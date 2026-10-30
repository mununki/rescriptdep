import { beforeEach, test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as fs from 'fs';
import * as path from 'path';
import { ProjectService } from '../../project';
import { MAX_USAGE_CACHE, ValueUsageController } from '../../valueUsage';
import { CliError } from '../../core/cliError';
import { FakeCli, fakeEditor, makeTree, projectFiles, tick, waitFor } from './helpers';
import type { FakeEditor } from './helpers';
import { ConfigurationTarget, ThemeColor, resetStub, setWorkspaceFolders, stub, stubEvents } from './stub/vscode';

const RECUR = ['let rec fact = n => n <= 1 ? 1 : n * fact(n - 1)', 'let helper = 1', '// comment', 'and other = 2', 'let _ = ignore()'];
const DEBOUNCE = 350;

interface Setup {
  root: string;
  file: string;
  cli: FakeCli;
  controller: ValueUsageController;
  editor: FakeEditor;
}

function setup(counts: Record<string, number[]> = {}): Setup {
  resetStub();
  const root = makeTree({ ...projectFiles('', ['src/Recur.res']), 'lib/bs/.compiler.log': '' });
  setWorkspaceFolders(root);
  const file = path.join(root, 'src/Recur.res');
  const cli = new FakeCli();
  cli.respond = (args) => {
    const name = args[args.indexOf('-vb') + 1];
    const values = counts[name] ?? [0];
    return JSON.stringify({ modules: values.map((count, i) => ({ name: `M${i}`, count })) });
  };
  const controller = new ValueUsageController(cli, new ProjectService());
  const editor = fakeEditor(file, [...RECUR]);
  stub.activeTextEditor = editor;
  return { root, file, cli, controller, editor };
}

function moveTo(editor: FakeEditor, line: number): void {
  editor.selection = { active: { line } };
  stubEvents.selectionEmitter.fire({ textEditor: editor });
}

beforeEach(() => resetStub());

test('let rec: the bound name, not "rec"; one decoration type with the CodeLens colour', async () => {
  const s = setup({ fact: [2, 1] });
  moveTo(s.editor, 0);
  await waitFor(() => s.editor.shown !== undefined);
  assert.deepEqual(s.cli.calls[0].args, ['-m', 'Recur', '-vb', 'fact', '-vl', '1', '-f', 'json', path.join(s.root, 'lib/bs')]);
  assert.equal(s.editor.shown, 'Used 3 times');
  assert.equal(stub.decorationTypes.length, 1);
  const after = stub.decorationTypes[0].options.after as { color: ThemeColor; fontStyle: string };
  assert.equal(after.color.id, 'editorCodeLens.foreground');

  moveTo(s.editor, 3);
  await waitFor(() => s.editor.shown === 'Used 0 times');
  assert.equal(s.cli.calls[1].args[3], 'other');
  assert.equal(stub.decorationTypes.length, 1, 'the decoration type is reused');
  s.controller.dispose();
});

test('no request for non-bindings and `let _`; leaving the line clears the decoration', async () => {
  const s = setup({ fact: [1] });
  moveTo(s.editor, 2);
  moveTo(s.editor, 4);
  await tick(DEBOUNCE + 50);
  assert.equal(s.cli.calls.length, 0);
  moveTo(s.editor, 0);
  await waitFor(() => s.editor.shown === 'Used 1 time');
  moveTo(s.editor, 2);
  assert.deepEqual(s.editor.applied.at(-1)!.options, []);
  s.controller.dispose();
});

test('cache: (file, line, name, .compiler.log mtime); a rebuild invalidates it; no TTL', async () => {
  const s = setup({ fact: [1], helper: [4] });
  moveTo(s.editor, 0);
  await waitFor(() => s.editor.shown === 'Used 1 time');
  moveTo(s.editor, 1);
  await waitFor(() => s.editor.shown === 'Used 4 times');
  moveTo(s.editor, 0);
  await waitFor(() => s.editor.shown === 'Used 1 time');
  assert.equal(s.cli.calls.length, 2, 'cache hit');

  // Typing elsewhere in another document does not clear the cache.
  stubEvents.documentEmitter.fire({ document: { fileName: 'other.res' }, contentChanges: [] });
  moveTo(s.editor, 1);
  await waitFor(() => s.editor.shown === 'Used 4 times');
  assert.equal(s.cli.calls.length, 2);

  const log = path.join(s.root, 'lib/bs/.compiler.log');
  const later = new Date(Date.now() + 5000);
  fs.utimesSync(log, later, later);
  moveTo(s.editor, 0);
  await waitFor(() => s.cli.calls.length === 3);
  s.controller.dispose();
});

test('dirty documents are skipped', async () => {
  const s = setup({ fact: [1] });
  s.editor.document.isDirty = true;
  moveTo(s.editor, 0);
  await tick(DEBOUNCE + 50);
  assert.equal(s.cli.calls.length, 0);
  assert.equal(s.editor.shown, undefined);
  s.controller.dispose();
});

test('at most one -vb process: moving to another line aborts the running one', async () => {
  const s = setup({ fact: [1], helper: [2] });
  s.cli.delayMs = 300;
  moveTo(s.editor, 0);
  await waitFor(() => s.cli.calls.length === 1);
  moveTo(s.editor, 2);
  assert.equal(s.cli.calls[0].aborted, true);
  moveTo(s.editor, 1);
  await waitFor(() => s.cli.calls.length === 2);
  assert.equal(s.cli.live, 1);
  await waitFor(() => s.editor.shown === 'Used 2 times');
  assert.equal(s.editor.applied.filter((a) => a.options.length > 0).length, 1, 'the aborted result never shows');
  s.controller.dispose();
});

test('negative codes: all negative → nothing and one status-bar reason; some negative → N+', async () => {
  const s = setup({ fact: [-6, -6], helper: [-6], other: [2, -4] });
  moveTo(s.editor, 0);
  await waitFor(() => stub.statusBar.length === 1);
  moveTo(s.editor, 1);
  await waitFor(() => s.cli.calls.length === 2);
  await tick(20);
  assert.equal(s.editor.applied.filter((a) => a.options.length > 0).length, 0);
  assert.deepEqual(stub.statusBar, ["Bibimbob: usage counts aren't available for this ReScript version."]);
  moveTo(s.editor, 3);
  await waitFor(() => s.editor.shown === 'Used 2+ times');
  s.controller.dispose();
});

test('CLI failures show nothing (never "error" or "?")', async () => {
  const s = setup();
  s.cli.respond = () => new CliError('timeout', 'timeout');
  moveTo(s.editor, 0);
  await waitFor(() => s.cli.calls.length === 1);
  s.cli.respond = () => 'garbage';
  moveTo(s.editor, 1);
  await waitFor(() => s.cli.calls.length === 2);
  await tick(20);
  assert.equal(s.editor.applied.filter((a) => a.options.length > 0).length, 0);
  s.controller.dispose();
});

test('an edit on the decorated line clears it; an edit above moves it', async () => {
  const s = setup({ helper: [5] });
  moveTo(s.editor, 1);
  await waitFor(() => s.editor.shown === 'Used 5 times');
  const change = (startLine: number, endLine: number, text: string) =>
    stubEvents.documentEmitter.fire({ document: s.editor.document, contentChanges: [{ range: { start: { line: startLine }, end: { line: endLine } }, text }] });
  change(0, 0, 'x\n');
  assert.equal(s.editor.shown, 'Used 5 times');
  change(2, 2, 'y');
  assert.deepEqual(s.editor.applied.at(-1)!.options, []);
  s.controller.dispose();
});

test('toggle: persisted setting (Global), status-bar confirmation, aborts and clears', async () => {
  const s = setup({ fact: [1] });
  s.cli.delayMs = 200;
  moveTo(s.editor, 0);
  await waitFor(() => s.cli.calls.length === 1);
  await s.controller.toggle();
  assert.equal(s.cli.calls[0].aborted, true);
  assert.deepEqual(stub.configUpdates, [{ key: 'bibimbob.valueUsageCount.enabled', value: false, target: ConfigurationTarget.Global }]);
  assert.deepEqual(stub.statusBar, ['Bibimbob: inline usage count off']);
  assert.equal(stub.messages.length, 0, 'no toast');
  moveTo(s.editor, 1);
  await tick(DEBOUNCE + 50);
  assert.equal(s.cli.calls.length, 1);

  s.cli.delayMs = 0;
  await s.controller.toggle();
  assert.equal(stub.statusBar.at(-1), 'Bibimbob: inline usage count on');
  await waitFor(() => s.editor.shown !== undefined);
  s.controller.dispose();
});

test('the setting is read at start and followed live', async () => {
  resetStub();
  stub.config.set('bibimbob.valueUsageCount.enabled', false);
  const root = makeTree(projectFiles('', ['src/Recur.res']));
  setWorkspaceFolders(root);
  const cli = new FakeCli();
  cli.respond = () => '{"modules":[{"name":"A","count":1}]}';
  const controller = new ValueUsageController(cli, new ProjectService());
  const editor = fakeEditor(path.join(root, 'src/Recur.res'), [...RECUR]);
  stub.activeTextEditor = editor;
  moveTo(editor, 0);
  await tick(DEBOUNCE + 50);
  assert.equal(cli.calls.length, 0);
  stub.config.set('bibimbob.valueUsageCount.enabled', true);
  stubEvents.configEmitter.fire({ affectsConfiguration: (section: string) => section === 'bibimbob.valueUsageCount.enabled' });
  moveTo(editor, 0);
  await waitFor(() => editor.shown === 'Used 1 time');
  controller.dispose();
});

test('multi-root: the file uses its own project, not the first folder (QA-4)', async () => {
  resetStub();
  const root = makeTree({
    ...projectFiles('monorepo/packages/admin', ['src/mathUtil.res']),
    ...projectFiles('test/rescript12/basic', ['src/mathUtil.res']),
  });
  setWorkspaceFolders(path.join(root, 'monorepo/packages/admin'), path.join(root, 'test/rescript12/basic'));
  const cli = new FakeCli();
  cli.respond = () => '{"modules":[{"name":"App","count":1},{"name":"MathUtil","count":0}]}';
  const controller = new ValueUsageController(cli, new ProjectService());
  const editor = fakeEditor(path.join(root, 'test/rescript12/basic/src/mathUtil.res'), ['open Utils', '', 'let square = x => multiply(x, x)']);
  stub.activeTextEditor = editor;
  moveTo(editor, 2);
  await waitFor(() => editor.shown !== undefined);
  assert.deepEqual(cli.calls[0].args, ['-m', 'MathUtil', '-vb', 'square', '-vl', '3', '-f', 'json', path.join(root, 'test/rescript12/basic/lib/bs')]);
  assert.equal(editor.shown, 'Used 1 time');
  controller.dispose();
});


test('usage cache evicts old entries after the bound is reached', async () => {
  const s = setup();
  const controller = s.controller as unknown as {
    update(editor: typeof s.editor, line: number, name: string): Promise<void>;
    cache: Map<string, string | null>;
  };
  for (let i = 0; i < MAX_USAGE_CACHE + 2; i++) {
    s.editor.document.lines[0] = `let value${i} = 1`;
    await controller.update(s.editor, 0, `value${i}`);
  }
  assert.equal(controller.cache.size, MAX_USAGE_CACHE);
  s.editor.document.lines[0] = 'let value0 = 1';
  await controller.update(s.editor, 0, 'value0');
  assert.equal(s.cli.calls.length, MAX_USAGE_CACHE + 3, 'the evicted value is fetched again');
  s.controller.dispose();
});
