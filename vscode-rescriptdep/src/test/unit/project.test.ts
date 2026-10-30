import { beforeEach, test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as fs from 'fs';
import * as path from 'path';
import { NO_BUILD_OUTPUT_MESSAGE, ProjectService, findProjectRootForFile } from '../../project';
import { fakeEditor, makeTree, projectFiles } from './helpers';
import { Uri, resetStub, setWorkspaceFolders, stub, stubEvents } from './stub/vscode';

beforeEach(() => resetStub());

function monorepo(): string {
  return makeTree({
    'package.json': '{}',
    ...projectFiles('packages/admin', ['src/App.res', 'src/Header.res']),
    ...projectFiles('packages/web', ['src/App.res', 'src/Header.res', 'src/Footer.res']),
    'packages/web/lib/ocaml/App.res': 'copy',
    'packages/web/lib/bs/src/App.res': 'copy',
    'node_modules/pkg/src/Pkg.res': 'x',
  });
}

test('findProjectRootForFile: string-prefix siblings are not ancestors (QA-4)', async () => {
  const root = makeTree({
    ...projectFiles('test/rescript', ['src/utils.res']),
    ...projectFiles('test/rescript12/basic', ['src/mathUtil.res']),
  });
  const file = path.join(root, 'test/rescript12/basic/src/mathUtil.res');
  assert.equal(await findProjectRootForFile(file, path.join(root, 'test/rescript')), undefined);
  assert.equal(await findProjectRootForFile(file, root), path.join(root, 'test/rescript12/basic'));
  assert.equal(await findProjectRootForFile(path.join(root, 'test/rescript/src/utils.res'), root), path.join(root, 'test/rescript'));
  // Never above the boundary, even when a config exists higher up.
  const nested = makeTree({ 'rescript.json': '{}', 'apps/x/src/A.res': '' });
  assert.equal(await findProjectRootForFile(path.join(nested, 'apps/x/src/A.res'), path.join(nested, 'apps')), undefined);
  assert.equal(await findProjectRootForFile(path.join(nested, 'apps/x/src/A.res'), nested), nested);
});

test('multi-root: a file resolves inside its own workspace folder', async () => {
  const root = makeTree({
    ...projectFiles('monorepo/packages/admin', ['src/mathUtil.res']),
    ...projectFiles('test/rescript12/basic', ['src/mathUtil.res']),
  });
  setWorkspaceFolders(path.join(root, 'monorepo/packages/admin'), path.join(root, 'test/rescript12/basic'));
  const projects = new ProjectService();
  assert.equal(await projects.projectRootForFile(path.join(root, 'test/rescript12/basic/src/mathUtil.res')), path.join(root, 'test/rescript12/basic'));
  assert.equal(await projects.projectRootForFile(path.join(root, 'monorepo/packages/admin/src/mathUtil.res')), path.join(root, 'monorepo/packages/admin'));
});

test('focus from a camelCase file: MathUtil, no search, no error (QA-2)', async () => {
  const root = makeTree(projectFiles('', ['src/mathUtil.res', 'src/utils.res']));
  setWorkspaceFolders(root);
  stub.activeTextEditor = fakeEditor(path.join(root, 'src/mathUtil.res'), ['let square = x => x * x']);
  const target = await new ProjectService().resolveFocusTarget();
  assert.deepEqual(target, { project: { projectRoot: root, bsDir: path.join(root, 'lib/bs') }, module: 'MathUtil' });
  assert.equal(stub.messages.length, 0);
  assert.equal(stub.findFilesCalls.length, 0);
});

test('duplicate App.res: the active file decides the project (QA-3)', async () => {
  const root = monorepo();
  setWorkspaceFolders(root);
  const projects = new ProjectService();
  for (const pkg of ['web', 'admin']) {
    stub.activeTextEditor = fakeEditor(path.join(root, `packages/${pkg}/src/App.res`), ['let x = 1']);
    const target = await projects.resolveFocusTarget();
    assert.equal(target?.project.bsDir, path.join(root, `packages/${pkg}/lib/bs`));
    assert.equal(target?.module, 'App');
  }
});

test('editor-title URI wins over the active editor (split groups)', async () => {
  const root = monorepo();
  setWorkspaceFolders(root);
  stub.activeTextEditor = fakeEditor(path.join(root, 'packages/admin/src/App.res'), ['let x = 1']);
  const target = await new ProjectService().resolveFocusTarget(Uri.file(path.join(root, 'packages/web/src/Header.res')));
  assert.equal(target?.module, 'Header');
  assert.equal(target?.project.projectRoot, path.join(root, 'packages/web'));
  // Garbage arguments are ignored.
  const fromEditor = await new ProjectService().resolveFocusTarget({ fsPath: 42 });
  assert.equal(fromEditor?.project.projectRoot, path.join(root, 'packages/admin'));
});

test('no .res editor: a module QuickPick from one findFiles, build copies and node_modules excluded (C14)', async () => {
  const root = monorepo();
  setWorkspaceFolders(root);
  stub.pick = (items) => items.find((i) => i.description === 'packages/web/src/App.res');
  const target = await new ProjectService().resolveFocusTarget();
  assert.equal(stub.findFilesCalls.length, 1);
  const items = stub.quickPicks[0].items.map((i) => `${i.label} ${i.description}`);
  assert.deepEqual(items, [
    'App packages/admin/src/App.res',
    'App packages/web/src/App.res',
    'Footer packages/web/src/Footer.res',
    'Header packages/admin/src/Header.res',
    'Header packages/web/src/Header.res',
  ]);
  assert.equal(stub.quickPicks[0].options?.matchOnDescription, true);
  assert.deepEqual(target, { project: { projectRoot: path.join(root, 'packages/web'), bsDir: path.join(root, 'packages/web/lib/bs') }, module: 'App' });
});

test('a .res outside any project: one actionable error, nothing else', async () => {
  const root = makeTree({ 'loose/Thing.res': '' });
  setWorkspaceFolders(root);
  stub.activeTextEditor = fakeEditor(path.join(root, 'loose/Thing.res'), ['let x = 1']);
  assert.equal(await new ProjectService().resolveFocusTarget(), undefined);
  assert.equal(stub.messages.length, 1);
  assert.match(stub.messages[0].text, /Thing\.res isn't part of a ReScript project/);
});

test('lib/bs missing: one error and stop (DES-10)', async () => {
  const root = makeTree({ 'rescript.json': '{}', 'src/App.res': '' });
  setWorkspaceFolders(root);
  stub.activeTextEditor = fakeEditor(path.join(root, 'src/App.res'), ['let x = 1']);
  const projects = new ProjectService();
  assert.equal(await projects.resolveFocusTarget(), undefined);
  assert.equal(await projects.resolveForGraphCommand(), undefined);
  assert.deepEqual(stub.messages.map((m) => m.text), [NO_BUILD_OUTPUT_MESSAGE, NO_BUILD_OUTPUT_MESSAGE]);
});

test('graph commands: editor project, else the only project, else a project QuickPick', async () => {
  const root = monorepo();
  setWorkspaceFolders(root);
  const projects = new ProjectService();
  stub.activeTextEditor = fakeEditor(path.join(root, 'packages/web/src/Footer.res'), ['let x = 1']);
  assert.equal((await projects.resolveForGraphCommand())?.projectRoot, path.join(root, 'packages/web'));
  assert.equal(stub.quickPicks.length, 0);

  stub.activeTextEditor = undefined;
  stub.pick = (items) => items[1];
  assert.equal((await projects.resolveForGraphCommand())?.projectRoot, path.join(root, 'packages/web'));
  assert.deepEqual(stub.quickPicks[0].items.map((i) => i.description), ['packages/admin', 'packages/web']);
  // Cached until a config file is created or deleted.
  await projects.resolveForGraphCommand();
  assert.equal(stub.findFilesCalls.length, 1);
  stub.watchers.find((w) => w.glob.includes('rescript.json'))!.create.fire(Uri.file(path.join(root, 'x/rescript.json')));
  await projects.resolveForGraphCommand();
  assert.equal(stub.findFilesCalls.length, 2);

  stub.pick = () => undefined;
  assert.equal(await projects.resolveForGraphCommand(), undefined);

  const single = makeTree(projectFiles('', ['src/A.res']));
  setWorkspaceFolders(single);
  assert.equal((await new ProjectService().resolveForGraphCommand())?.projectRoot, single);

  setWorkspaceFolders(makeTree({ 'README.md': '' }));
  const before = stub.messages.length;
  assert.equal(await new ProjectService().resolveForGraphCommand(), undefined);
  assert.match(stub.messages[before].text, /Couldn't find a bsconfig\.json or rescript\.json/);
});

test('project picker uses workspace folder names for roots and project names for nested packages', async () => {
  const root = makeTree({
    ...projectFiles('app11-1000', ['src/Hub.res']),
    ...projectFiles('app11-1000/packages/tools', ['src/Tool.res']),
    ...projectFiles('rescript', ['src/App.res']),
  });
  const large = path.join(root, 'app11-1000');
  setWorkspaceFolders(large, path.join(root, 'rescript'));
  stub.workspaceFolders[0].name = 'large';
  stub.workspaceFolders[1].name = 'small';
  stub.pick = (items) => items.find((item) => item.label === 'large');

  const selected = await new ProjectService().resolveForGraphCommand();

  assert.deepEqual(stub.quickPicks[0].items.map((item) => item.label), ['large', 'tools', 'small']);
  assert.deepEqual(selected, { projectRoot: large, bsDir: path.join(large, 'lib/bs') });
});

test('resolveSources: tooltip file, package directory (case-insensitive), index for path-less nodes', async () => {
  const root = makeTree({
    ...projectFiles('', ['src/app.res', 'src/math/mathUtil.res', 'src/math/mathUtil.resi', 'src/ui/button.res']),
    'node_modules/@rescript/react/src/React.res': '',
    'node_modules/@rescript/react/src/reactDOM.res': '',
    'lib/ocaml/app.res': 'copy',
  });
  const outside = makeTree({ 'Elsewhere.res': '' });
  setWorkspaceFolders(root);
  const projects = new ProjectService();
  const sources = await projects.resolveSources(root, [
    { id: 'App', tooltip: path.join(root, 'src/app.res') },
    { id: 'React', tooltip: path.join(root, 'node_modules/@rescript/react/src') },
    { id: 'ReactDOM', tooltip: path.join(root, 'node_modules/@rescript/react/src') },
    { id: 'ReactEvent', tooltip: path.join(root, 'node_modules/@rescript/react/src') },
    { id: 'MathUtil' },
    { id: 'Button' },
    { id: 'Ghost' },
    { id: 'Elsewhere', tooltip: path.join(outside, 'Elsewhere.res') },
  ]);
  assert.deepEqual(sources.get('App'), { file: path.join(root, 'src/app.res'), openable: true, external: false });
  assert.deepEqual(sources.get('React'), {
    file: path.join(root, 'node_modules/@rescript/react/src/React.res'),
    openable: true,
    external: true,
    secondary: '@rescript/react',
  });
  assert.equal(sources.get('ReactDOM')?.file, path.join(root, 'node_modules/@rescript/react/src/reactDOM.res'));
  assert.deepEqual(sources.get('ReactEvent'), { openable: false, external: true, secondary: '@rescript/react' });
  assert.deepEqual(sources.get('MathUtil'), { file: path.join(root, 'src/math/mathUtil.res'), openable: true, external: false, secondary: 'math' });
  assert.deepEqual(sources.get('Button'), { file: path.join(root, 'src/ui/button.res'), openable: true, external: false, secondary: 'ui' });
  assert.deepEqual(sources.get('Ghost'), { openable: false, external: false });
  assert.equal(sources.get('Elsewhere')?.openable, false, 'outside the project and every workspace folder');

  // One findFiles per project per session; a created source file invalidates it.
  await projects.resolveSources(root, [{ id: 'App' }]);
  assert.equal(stub.findFilesCalls.length, 1);
  stub.watchers.find((w) => w.glob.includes('res,resi'))!.create.fire(Uri.file(path.join(root, 'src/New.res')));
  await projects.resolveSources(root, [{ id: 'App' }]);
  assert.equal(stub.findFilesCalls.length, 2);

  assert.equal(await projects.isOpenableFile(path.join(root, 'src/app.res'), root), true);
  assert.equal(await projects.isOpenableFile(path.join(root, 'src/gone.res'), root), false);
  assert.equal(await projects.isOpenableFile(path.join(outside, 'Elsewhere.res'), root), false);
  assert.equal(await projects.isOpenableFile(path.join(root, 'rescript.json'), root), false);
  fs.rmSync(outside, { recursive: true });
});


test('workspace folder changes invalidate project discovery', async () => {
  const a = makeTree(projectFiles('', ['src/A.res']));
  const b = makeTree(projectFiles('', ['src/B.res']));
  setWorkspaceFolders(a);
  const projects = new ProjectService();
  assert.deepEqual(await projects.configDirs(), [a]);
  setWorkspaceFolders(a, b);
  stubEvents.foldersEmitter.fire();
  assert.deepEqual(await projects.configDirs(), [a, b].sort());
  setWorkspaceFolders(b);
  stubEvents.foldersEmitter.fire();
  assert.deepEqual(await projects.configDirs(), [b]);
  projects.dispose();
});
