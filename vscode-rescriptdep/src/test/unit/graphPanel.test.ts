import { beforeEach, test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as fs from 'fs';
import * as path from 'path';
import type * as vscode from 'vscode';
import type { HostMessage } from '../../protocol';
import { activateWith } from '../../extension';
import type { ExtensionServices } from '../../extension';
import { chooseEditorColumn } from '../../contextMenu';
import { MAX_HISTORY, MAX_HISTORY_ITEMS } from '../../graphPanel';
import { CliError } from '../../core/cliError';
import { FakeCli, cliDot, fakeContext, fakeEditor, makeTree, ndDot, projectFiles, tick, waitFor } from './helpers';
import { FakeWebviewPanel, TabInputText, Uri, resetStub, setWorkspaceFolders, stub } from './stub/vscode';

type RenderMessage = Extract<HostMessage, { type: 'render' }>;

interface Setup {
  root: string;
  bsDir: string;
  cli: FakeCli;
  services: ExtensionServices;
  extensionPath: string;
}

const MODULES = ['App', 'Logger', 'Math', 'React', 'Utils'];
const EDGES: [string, string][] = [
  ['App', 'Logger'],
  ['App', 'Math'],
  ['App', 'React'],
  ['Math', 'Utils'],
];

function setup(): Setup {
  resetStub();
  const root = makeTree({
    ...projectFiles('', ['src/app.res', 'src/logger.res', 'src/math.res', 'src/utils.res', 'src/tool.res']),
    'node_modules/@rescript/react/src/React.res': '',
  });
  setWorkspaceFolders(root);
  const tips: Record<string, string> = {
    App: path.join(root, 'src/app.res'),
    Logger: path.join(root, 'src/logger.res'),
    Math: path.join(root, 'src/math.res'),
    Utils: path.join(root, 'src/utils.res'),
    React: path.join(root, 'node_modules/@rescript/react/src'),
  };
  const cli = new FakeCli();
  cli.respond = (args) => {
    if (args.includes('--no-dependents')) {
      return ndDot(['App', 'Tool']);
    }
    const at = args.indexOf('--module');
    if (at >= 0) {
      const center = args[at + 1];
      if (!MODULES.includes(center)) {
        return cliDot([], []);
      }
      const edges = EDGES.filter(([a, b]) => a === center || b === center);
      const nodes = [...new Set([center, ...edges.flat()])];
      return cliDot(nodes, edges, tips);
    }
    return cliDot(MODULES, EDGES, tips);
  };
  const extensionPath = makeTree({ 'package.json': '{}' });
  const services = activateWith(fakeContext(extensionPath) as unknown as vscode.ExtensionContext, cli);
  return { root, bsDir: path.join(root, 'lib/bs'), cli, services, extensionPath };
}

const run = (id: string, ...args: unknown[]) => stub.commands.get(id)!(...args) as Promise<unknown>;
const panel = (): FakeWebviewPanel => stub.panels.at(-1)!;
const posted = (): HostMessage[] => panel().webview.posted as HostMessage[];
const renders = (): RenderMessage[] => posted().filter((m): m is RenderMessage => m.type === 'render');
const lastRender = (): RenderMessage => renders().at(-1)!;
const ready = () => panel().webview.send({ type: 'ready' });
const send = (message: unknown) => panel().webview.send(message);
const panelId = () => lastRender().panelId;

beforeEach(() => resetStub());

test('lifecycle: N commands → one panel, html set once, one listener, one CLI run each', async () => {
  const s = setup();
  for (let i = 0; i < 3; i++) {
    await run('bibimbob.showDependencyGraph');
  }
  assert.equal(stub.panels.length, 1);
  const p = panel();
  assert.equal(p.webview.htmlSets, 1);
  assert.equal(p.webview.messages.listeners.length, 1);
  assert.equal(p.disposeEmitter.listeners.length, 1);
  assert.deepEqual(s.cli.calls.map((c) => c.args), Array(3).fill(['--format=dot', s.bsDir]));
  assert.deepEqual(stub.progress.map((r) => r.options.title), Array(3).fill('Bibimbob: Analyzing dependency graph…'));
  assert.equal(stub.progress[0].options.cancellable, true);
  // Panel contract with the webview bundle.
  assert.equal(p.viewType, 'bibimbobVisualizer');
  assert.equal(p.options.enableScripts, true);
  assert.equal(p.options.retainContextWhenHidden, true);
  assert.deepEqual((p.options.localResourceRoots as Uri[]).map((u) => u.fsPath), [path.join(s.extensionPath, 'dist')]);
  assert.equal((p.iconPath as Uri).fsPath, path.join(s.extensionPath, 'images/rescriptdep_icon.png'));
  assert.ok(p.webview.html.includes(path.join(s.extensionPath, 'dist/webview.js')));
  assert.ok(p.webview.html.includes(path.join(s.extensionPath, 'dist/webview.css')));
  assert.match(p.webview.html, /nonce-[0-9a-f]{32}/);
  assert.equal(p.title, 'Dependency Graph');
  assert.equal(stub.messages.length, 0);
});

test('ready replays the current state; nothing is posted before it; no paths reach the webview', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  assert.deepEqual(posted(), []);
  ready();
  const render = lastRender();
  assert.equal(render.view, 'full');
  assert.equal(render.canGoBack, false);
  assert.equal(render.center, undefined);
  assert.deepEqual(render.layout, { tune: false, slow: false });
  assert.deepEqual(
    render.model.nodes.map((n) => n.id),
    ['App', 'Logger', 'Math', 'React', 'Utils'],
  );
  assert.deepEqual(render.model.nodes.find((n) => n.id === 'React'), { id: 'React', external: true, openable: true, secondary: '@rescript/react' });
  assert.ok(!JSON.stringify(posted()).includes(s.root), 'a path was posted to the webview');
  // Replay again (webview reload): the same render, once.
  posted().length = 0;
  ready();
  assert.deepEqual(posted().map((m) => m.type), ['render']);
});

test('node click: one CLI run with the panel bsDir, focus render with centre; Back runs nothing', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  const progressBefore = stub.progress.length;
  send({ type: 'focusModule', module: 'Math' });
  await waitFor(() => lastRender().view === 'focus');
  assert.deepEqual(s.cli.calls.at(-1)!.args, ['--format=dot', '--module', 'Math', s.bsDir]);
  assert.equal(s.cli.calls.length, 2);
  assert.equal(stub.progress.length, progressBefore, 'node clicks show in-panel progress only');
  const loading = posted().find((m) => m.type === 'loading');
  assert.deepEqual(loading, { type: 'loading', renderKey: lastRender().renderKey, text: 'Analyzing dependencies of Math…' });
  assert.equal(lastRender().center, 'Math');
  assert.equal(lastRender().canGoBack, true);
  assert.equal(panel().title, 'Dependencies: Math');

  // A tab switch / reload replays the navigated graph, not the original one.
  posted().length = 0;
  ready();
  assert.equal(renders().length, 1);
  assert.equal(lastRender().center, 'Math');

  send({ type: 'back' });
  assert.equal(lastRender().view, 'full');
  assert.equal(lastRender().canGoBack, false);
  assert.equal(panel().title, 'Dependency Graph');
  assert.equal(s.cli.calls.length, 2);

  // Clicking the centre is not a focus (the webview opens it instead).
  send({ type: 'focusModule', module: 'Math' });
  await waitFor(() => lastRender().view === 'focus');
  const calls = s.cli.calls.length;
  send({ type: 'focusModule', module: 'Math' });
  await tick(20);
  assert.equal(s.cli.calls.length, calls);
});

test('monorepo: a click uses the panel project, not the first config (ARCH-7)', async () => {
  resetStub();
  const root = makeTree({
    ...projectFiles('packages/admin', ['src/Header.res']),
    ...projectFiles('packages/web', ['src/App.res', 'src/Header.res']),
  });
  setWorkspaceFolders(root);
  const cli = new FakeCli();
  cli.respond = (args) => (args.includes('--module') ? cliDot(['Header'], []) : cliDot(['App', 'Header'], [['App', 'Header']]));
  activateWith(fakeContext(makeTree({})) as unknown as vscode.ExtensionContext, cli);
  stub.pick = (items) => items.find((i) => i.description === 'packages/web');
  await run('bibimbob.showDependencyGraph');
  ready();
  send({ type: 'focusModule', module: 'Header' });
  await waitFor(() => cli.calls.length === 2 && lastRender().view === 'focus');
  assert.deepEqual(cli.calls.map((c) => c.args.at(-1)), [path.join(root, 'packages/web/lib/bs'), path.join(root, 'packages/web/lib/bs')]);
});

test('last request wins: a superseded CLI run is aborted and never rendered (QA-7)', async () => {
  const s = setup();
  s.cli.delayMs = 80;
  const full = run('bibimbob.showDependencyGraph');
  await tick(20);
  const unused = run('bibimbob.showUnusedModules');
  ready();
  await Promise.all([full, unused]);
  assert.equal(s.cli.calls.length, 2);
  assert.equal(s.cli.calls[0].aborted, true);
  assert.equal(s.cli.calls[1].aborted, false);
  assert.equal(s.cli.live, 0);
  assert.deepEqual(renders().map((r) => r.view), ['unused']);
  assert.equal(panel().title, 'Modules Without Dependents');
  assert.equal(stub.messages.length, 0, 'no error toast for the superseded run');

  // Rapid clicks: one live CLI, the last one wins.
  send({ type: 'focusModule', module: 'App' });
  await tick(10);
  send({ type: 'focusModule', module: 'Tool' });
  await tick(10);
  assert.equal(s.cli.live, 1);
  await waitFor(() => lastRender().view === 'focus');
  assert.equal(lastRender().center, 'Tool');
  assert.equal(renders().filter((r) => r.view === 'focus').length, 1);
});

test('unused view: Focus pushes it onto the history and Back returns to it (CM-6)', async () => {
  const s = setup();
  await run('bibimbob.showUnusedModules');
  ready();
  assert.deepEqual(s.cli.calls[0].args, ['--format=dot', '--no-dependents', s.bsDir]);
  const unused = lastRender();
  assert.deepEqual(unused.model.nodes, [
    { id: 'App', external: false, openable: true },
    { id: 'Tool', external: false, openable: true },
  ]);
  await run('bibimbob.graph.focusModule', { webview: 'bibimbobVisualizer', webviewSection: 'module', panelId: unused.panelId, module: 'App' });
  assert.equal(lastRender().view, 'focus');
  await run('bibimbob.graph.back', { panelId: unused.panelId, webviewSection: 'canvas' });
  assert.equal(lastRender().view, 'unused');
  assert.equal(s.cli.calls.length, 2);
});

test('context menu: arguments are validated against the panel model (CM-8/14/15)', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  const id = panelId();
  const calls = s.cli.calls.length;

  await run('bibimbob.graph.copyModuleName', { webview: 'bibimbobVisualizer', panelId: id, module: 'Math' });
  assert.deepEqual(stub.clipboard, ['Math']);

  for (const arg of [
    { panelId: id, module: '../../etc/passwd' },
    { panelId: id, module: 'math' },
    { panelId: id, module: 42 },
    { panelId: id },
    { panelId: 'no-such-panel', module: 'Math' },
    { panelId: 7, module: 'Math' },
    'Math',
    undefined,
  ]) {
    await run('bibimbob.graph.copyModuleName', arg);
    await run('bibimbob.graph.focusModule', arg);
    await run('bibimbob.graph.openModule', arg);
    await run('bibimbob.graph.copyRelativePath', arg);
  }
  assert.deepEqual(stub.clipboard, ['Math']);
  assert.equal(s.cli.calls.length, calls);
  assert.equal(stub.shown.length, 0);

  // Canvas commands: an unknown panel is a no-op; no argument means the active panel.
  await run('bibimbob.graph.fitToView', { panelId: 'stale' });
  assert.equal(posted().filter((m) => m.type === 'fitToView').length, 0);
  await run('bibimbob.graph.fitToView', { panelId: id, webviewSection: 'canvas' });
  await run('bibimbob.graph.fitToView');
  assert.equal(posted().filter((m) => m.type === 'fitToView').length, 2);
});

test('context menu: focus, centre no-op, copy path, back, show full graph (CM-2/4/5)', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  const id = panelId();
  await run('bibimbob.graph.focusModule', { panelId: id, module: 'Math' });
  assert.deepEqual(s.cli.calls.at(-1)!.args, ['--format=dot', '--module', 'Math', s.bsDir]);
  assert.equal(lastRender().center, 'Math');

  const calls = s.cli.calls.length;
  await run('bibimbob.graph.focusModule', { panelId: id, module: 'Math', bibimbobIsCenter: true });
  assert.equal(s.cli.calls.length, calls, 'focus on the centre is a no-op');

  await run('bibimbob.graph.copyRelativePath', { panelId: id, module: 'Utils' });
  assert.deepEqual(stub.clipboard, ['src/utils.res']);

  await run('bibimbob.graph.showFullGraph', { panelId: id });
  assert.deepEqual(s.cli.calls.at(-1)!.args, ['--format=dot', s.bsDir]);
  assert.equal(lastRender().view, 'full');
  assert.equal(lastRender().canGoBack, true);

  await run('bibimbob.graph.back', { panelId: id });
  assert.equal(lastRender().center, 'Math');
  assert.equal(s.cli.calls.length, calls + 1);
});

test('Open Module: host-resolved path, placement beside the graph, refusal outside the workspace (CM-3/7/8)', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  const id = panelId();
  panel().viewColumn = 2;

  // No other group: Beside.
  await run('bibimbob.graph.openModule', { panelId: id, module: 'Math' });
  assert.equal(stub.shown.at(-1)!.uri.fsPath, path.join(s.root, 'src/math.res'));
  assert.deepEqual(stub.shown.at(-1)!.options, { viewColumn: -2, preview: true, preserveFocus: false });

  // The file is already open in another group: reveal it there. A tab behind the graph doesn't count.
  const mathUri = Uri.file(path.join(s.root, 'src/math.res'));
  stub.tabGroups = [
    { viewColumn: 1, tabs: [{ input: new TabInputText(mathUri) }] },
    { viewColumn: 2, tabs: [{ input: new TabInputText(mathUri) }] },
    { viewColumn: 3, tabs: [] },
  ];
  await run('bibimbob.graph.openModule', { panelId: id, module: 'Math' });
  assert.equal(stub.shown.at(-1)!.options.viewColumn, 1);
  stub.tabGroups[0].tabs = [];
  await run('bibimbob.graph.openModule', { panelId: id, module: 'Math' });
  assert.equal(stub.shown.at(-1)!.options.viewColumn, 1, 'nearest other group, left first');

  // External module: the package directory was searched for React.res.
  send({ type: 'openModule', module: 'React' });
  await waitFor(() => stub.shown.at(-1)!.uri.fsPath.endsWith('React.res'));
  assert.equal(stub.shown.at(-1)!.uri.fsPath, path.join(s.root, 'node_modules/@rescript/react/src/React.res'));

  // The file disappeared since the render: status-bar message, nothing opened.
  fs.rmSync(path.join(s.root, 'src/utils.res'));
  const shown = stub.shown.length;
  await run('bibimbob.graph.openModule', { panelId: id, module: 'Utils' });
  assert.equal(stub.shown.length, shown);
  assert.deepEqual(stub.statusBar, ["Source for Utils isn't available in this workspace."]);
});

test('Open Module refuses a graph path outside the project and every workspace folder', async () => {
  resetStub();
  const root = makeTree(projectFiles('', ['src/app.res']));
  const outside = makeTree({ 'src/Linked.res': '' });
  setWorkspaceFolders(root);
  const cli = new FakeCli();
  cli.respond = () => cliDot(['App', 'Linked'], [['App', 'Linked']], { App: path.join(root, 'src/app.res'), Linked: path.join(outside, 'src/Linked.res') });
  activateWith(fakeContext(makeTree({})) as unknown as vscode.ExtensionContext, cli);
  await run('bibimbob.showDependencyGraph');
  ready();
  assert.equal(lastRender().model.nodes.find((n) => n.id === 'Linked')?.openable, false);
  send({ type: 'openModule', module: 'Linked' });
  await waitFor(() => stub.statusBar.length > 0);
  assert.deepEqual(stub.statusBar, ["Source for Linked isn't available in this workspace."]);
  assert.equal(stub.shown.length, 0);
});

test('large graph: prompt before layout; Render Anyway tunes; dismiss on a fresh panel closes it', async () => {
  const s = setup();
  // chain-like graph: 151 nodes, L far above 20k.
  const nodes = Array.from({ length: 150 }, (_, i) => `M${String(i).padStart(3, '0')}`);
  const edges: [string, string][] = [];
  nodes.forEach((n, i) => {
    for (let j = 0; j < i; j += 3) {
      edges.push([n, nodes[j]]);
    }
    if (i > 0) {
      edges.push([n, nodes[i - 1]]);
    }
  });
  s.cli.respond = (args) => (args.includes('--module') ? cliDot([args[2]], []) : cliDot(nodes, edges));

  stub.respond = () => undefined;
  await run('bibimbob.showDependencyGraph');
  const warning = stub.messages.find((m) => m.level === 'warning')!;
  assert.equal(warning.text, 'This graph has 150 modules. Laying it out may take a while.');
  assert.deepEqual(warning.items, ['Focus on Module…', 'Render Anyway']);
  assert.equal(panel().disposed, true, 'nothing to restore: the new panel closes');
  assert.equal(s.services.registry.size, 0);

  stub.respond = (_text, items) => items[1];
  await run('bibimbob.showDependencyGraph');
  ready();
  assert.deepEqual(lastRender().layout, { tune: true, slow: true });

  // With a graph on screen, dismissing restores it silently.
  stub.respond = () => undefined;
  await run('bibimbob.showDependencyGraph');
  const cancelled = posted().at(-1)!;
  assert.equal(cancelled.type, 'cancelled');
  assert.equal(renders().length, 1);

  // Focus on Module…: the module QuickPick of the same project, then a focus run.
  stub.respond = (_text, items) => items[0];
  stub.pick = (items) => items.find((i) => i.label === 'Math');
  await run('bibimbob.showDependencyGraph');
  await waitFor(() => lastRender().view === 'focus');
  assert.deepEqual(s.cli.calls.at(-1)!.args, ['--format=dot', '--module', 'Math', s.bsDir]);
  assert.equal(stub.messages.filter((m) => m.level === 'error').length, 0);
});

test('CLI errors become the in-panel error state; Retry runs again (QA-6, DES-10)', async () => {
  const s = setup();
  let fail: Error | undefined = new CliError('timeout', 'timeout');
  const respond = s.cli.respond;
  s.cli.respond = (args) => fail ?? respond(args);
  await run('bibimbob.showDependencyGraph');
  ready();
  const error = posted().at(-1)!;
  assert.deepEqual(error, {
    type: 'error',
    renderKey: (error as { renderKey: number }).renderKey,
    title: "Couldn't render the graph",
    reason: 'The CLI took longer than 10 seconds.',
    canRetry: true,
    canGoBack: false,
  });
  assert.equal(stub.messages.length, 0, 'no toast for an in-panel error');

  fail = undefined;
  send({ type: 'retry' });
  await waitFor(() => renders().length === 1);
  assert.equal(lastRender().view, 'full');
  assert.equal(s.cli.calls.length, 2);

  s.cli.respond = () => 'Fatal error: exception Not_found';
  send({ type: 'focusModule', module: 'Math' });
  await waitFor(() => posted().at(-1)!.type === 'error');
  assert.equal((posted().at(-1) as { reason: string }).reason, "The graph data couldn't be read.");
});

test('layout failure in the webview: Retry re-sends the model tuned, without the CLI', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  const first = lastRender();
  send({ type: 'renderError', renderKey: first.renderKey, kind: 'tooLarge', message: 'RangeError' });
  send({ type: 'retry' });
  const again = lastRender();
  assert.notEqual(again.renderKey, first.renderKey);
  assert.deepEqual(again.layout, { tune: true, slow: true });
  assert.equal(s.cli.calls.length, 1);
});

test('Cancel: the progress button and the overlay button abort the CLI and end silently', async () => {
  const s = setup();
  s.cli.delayMs = 200;
  const first = run('bibimbob.showDependencyGraph');
  await tick(20);
  stub.progress[0].cancel();
  await first;
  assert.equal(s.cli.calls[0].aborted, true);
  assert.equal(panel().disposed, true);
  assert.equal(stub.messages.length, 0);

  s.cli.delayMs = 0;
  await run('bibimbob.showDependencyGraph');
  ready();
  s.cli.delayMs = 200;
  send({ type: 'focusModule', module: 'Math' });
  await tick(20);
  send({ type: 'cancel' });
  await tick(10);
  assert.equal(s.cli.calls.at(-1)!.aborted, true);
  assert.equal(posted().at(-1)!.type, 'cancelled');
  assert.equal(renders().length, 1);
  assert.equal(panel().disposed, false);
});

test('a focus on a module the build doesn not know still carries the centre (empty state)', async () => {
  const s = setup();
  s.cli.respond = () => cliDot([], []);
  stub.activeTextEditor = fakeEditor(path.join(s.root, 'src/tool.res'), ['let x = 1']);
  await run('bibimbob.focusModuleDependencies');
  ready();
  assert.equal(stub.progress[0].options.title, 'Bibimbob: Analyzing module dependencies…');
  assert.deepEqual(s.cli.calls[0].args, ['--format=dot', '--module', 'Tool', s.bsDir]);
  assert.equal(lastRender().center, 'Tool');
  assert.deepEqual(lastRender().model.nodes, []);
});

test('editor-title button: the passed URI is focused', async () => {
  const s = setup();
  stub.activeTextEditor = fakeEditor(path.join(s.root, 'src/app.res'), ['let x = 1']);
  await run('bibimbob.focusModuleDependencies', Uri.file(path.join(s.root, 'src/math.res')));
  assert.deepEqual(s.cli.calls[0].args, ['--format=dot', '--module', 'Math', s.bsDir]);
});

test('lib/bs missing: one error, no CLI run, no panel', async () => {
  resetStub();
  const root = makeTree({ 'rescript.json': '{}', 'src/App.res': '' });
  setWorkspaceFolders(root);
  const cli = new FakeCli();
  activateWith(fakeContext(makeTree({})) as unknown as vscode.ExtensionContext, cli);
  await run('bibimbob.showDependencyGraph');
  await run('bibimbob.showUnusedModules');
  assert.equal(cli.calls.length, 0);
  assert.equal(stub.panels.length, 0);
  assert.equal(stub.messages.length, 2);
  assert.match(stub.messages[0].text, /^No compiled output found in lib\/bs\./);
});

test('webview messages are validated', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  for (const message of [null, 'ready', { type: 42 }, { type: 'focusModule', module: 42 }, { type: 'focusModule', module: 'Nope' }, { type: 'openModule' }, { type: 'nope' }]) {
    send(message);
  }
  await tick(20);
  assert.equal(s.cli.calls.length, 1);
  assert.equal(stub.shown.length, 0);
});

test('history keeps the last 50 views', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  const targets = ['Math', 'Utils'];
  for (let i = 0; i < MAX_HISTORY + 5; i++) {
    send({ type: 'focusModule', module: targets[i % 2] });
    await waitFor(() => lastRender().center === targets[i % 2]);
  }
  let backs = 0;
  while (lastRender().canGoBack) {
    send({ type: 'back' });
    backs++;
  }
  assert.equal(backs, MAX_HISTORY);
  assert.equal(s.cli.calls.length, MAX_HISTORY + 6);
});

test('dispose: listeners released, registry emptied, stale commands are no-ops', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  const id = panelId();
  const p = panel();
  p.dispose();
  assert.equal(p.webview.messages.listeners.length, 0);
  assert.equal(s.services.registry.size, 0);
  await run('bibimbob.graph.copyModuleName', { panelId: id, module: 'Math' });
  await run('bibimbob.graph.back');
  assert.deepEqual(stub.clipboard, []);
  // The next command opens a new panel.
  await run('bibimbob.showDependencyGraph');
  assert.equal(stub.panels.length, 2);
});

test('chooseEditorColumn: reveal elsewhere → nearest group (left first) → beside', () => {
  const target = 'file:///p/a.res';
  assert.equal(chooseEditorColumn([{ viewColumn: 1, uris: [] }], 1, target), 'beside');
  assert.equal(chooseEditorColumn([{ viewColumn: 1, uris: [target] }], 1, target), 'beside');
  assert.equal(chooseEditorColumn([{ viewColumn: 1, uris: [] }, { viewColumn: 2, uris: [] }], 2, target), 1);
  assert.equal(chooseEditorColumn([{ viewColumn: 1, uris: [] }, { viewColumn: 2, uris: [] }], 1, target), 2);
  assert.equal(chooseEditorColumn([{ viewColumn: 1, uris: [] }, { viewColumn: 2, uris: [] }, { viewColumn: 3, uris: [] }], 2, target), 1);
  assert.equal(chooseEditorColumn([{ viewColumn: 1, uris: [] }, { viewColumn: 3, uris: [target] }, { viewColumn: 2, uris: [] }], 2, target), 3);
  assert.equal(chooseEditorColumn([{ viewColumn: 1, uris: [] }, { viewColumn: 2, uris: [] }, { viewColumn: 4, uris: [] }], 4, target), 2);
  assert.equal(chooseEditorColumn([], undefined, target), 'beside');
});


test('Back after a failed navigation restores the visible graph before popping history', async () => {
  const s = setup();
  await run('bibimbob.showDependencyGraph');
  ready();
  send({ type: 'focusModule', module: 'Math' });
  await waitFor(() => lastRender().center === 'Math');
  s.cli.respond = () => new CliError('timeout', 'timeout');
  send({ type: 'focusModule', module: 'Utils' });
  await waitFor(() => posted().at(-1)!.type === 'error');
  assert.equal((posted().at(-1) as { canGoBack?: boolean }).canGoBack, true);
  const count = s.cli.calls.length;
  send({ type: 'back' });
  assert.equal(lastRender().center, 'Math');
  assert.equal(lastRender().canGoBack, true);
  send({ type: 'back' });
  assert.equal(lastRender().view, 'full');
  assert.equal(lastRender().canGoBack, false);
  assert.equal(s.cli.calls.length, count);
});


test('history also bounds the total retained graph size', async () => {
  const s = setup();
  const edges: [string, string][] = Array.from({ length: 10_000 }, () => ['App', 'Math']);
  s.cli.respond = () => cliDot(['App', 'Math'], edges);
  await run('bibimbob.showDependencyGraph');
  ready();
  const host = s.services.registry.active()!;
  const project = { projectRoot: s.root, bsDir: s.bsDir };
  for (let i = 0; i < 8; i++) {
    await host.run({ view: i % 2 ? 'full' : 'unused', project }, { push: true });
  }
  let backs = 0;
  while (lastRender().canGoBack) {
    host.back();
    backs++;
  }
  assert.equal(backs, Math.floor(MAX_HISTORY_ITEMS / 10_002));
  assert.equal(s.cli.calls.length, 9);
});
