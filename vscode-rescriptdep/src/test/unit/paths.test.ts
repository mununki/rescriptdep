import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as path from 'path';
import {
  commonFolder,
  isExternalPath,
  isInside,
  isSourceFile,
  matchModuleFiles,
  moduleNameFromFile,
  packageName,
  secondLine,
  secondLineContext,
} from '../../core/paths';
import { parseDot } from '../../core/dot';

const P = path.posix;

test('isInside uses path.relative, not a string prefix (QA-4)', () => {
  assert.equal(isInside('/w/test/rescript12/basic/src/a.res', '/w/test/rescript', P), false);
  assert.equal(isInside('/w/test/rescript/src/a.res', '/w/test/rescript', P), true);
  assert.equal(isInside('/w/test/rescript', '/w/test/rescript', P), true);
  assert.equal(isInside('/w/test', '/w/test/rescript', P), false);
  assert.equal(isInside('/w/test/..rescript/x', '/w/test', P), true);
  assert.equal(isInside('/elsewhere/a.res', '/w', P), false);
  assert.equal(isInside('C:\\w\\proj\\src\\a.res', 'C:\\w\\proj', path.win32), true);
  assert.equal(isInside('D:\\w\\proj\\src\\a.res', 'C:\\w\\proj', path.win32), false);
});

test('G9: external iff the path contains /node_modules/', () => {
  assert.equal(isExternalPath('/p/node_modules/@rescript/react/src'), true);
  assert.equal(isExternalPath('/p/node_modules/@rescript/react/src/React.res'), true);
  assert.equal(isExternalPath('C:\\p\\node_modules\\pkg\\src\\A.res'), true);
  assert.equal(isExternalPath('/p/src/node_modules_like/A.res'), false);
  assert.equal(isExternalPath('/p/src/A.res'), false);
  assert.equal(isExternalPath(undefined), false);
});

test('G8: package name after the last node_modules/', () => {
  assert.equal(packageName('/p/node_modules/@rescript/react/src'), '@rescript/react');
  assert.equal(packageName('/p/node_modules/rescript-webapi/src/Webapi.res'), 'rescript-webapi');
  assert.equal(packageName('/p/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src'), 'pkg');
  assert.equal(packageName('/p/node_modules/.pnpm/@scope+pkg@2.0.0/node_modules/@scope/pkg/src/A.res'), '@scope/pkg');
  assert.equal(packageName('C:\\p\\node_modules\\@s\\pkg\\src'), '@s/pkg');
  assert.equal(packageName('/p/src/A.res'), undefined);
  assert.equal(packageName('/p/node_modules/@scope'), undefined);
});

test('G8: common prefix of the project module folders', () => {
  assert.equal(commonFolder(['/a/b/c', '/a/b/d', '/a/b'], P), '/a/b');
  assert.equal(commonFolder(['/a/x', '/b/y'], P), '/');
  assert.equal(commonFolder([], P), undefined);
  assert.equal(commonFolder(['/a/b'], P), '/a/b');
});

test('G8: single-folder project → no second line', () => {
  const files = ['/p/src/app.res', '/p/src/math.res', '/p/src/utils.res'];
  const ctx = secondLineContext(files, P);
  assert.equal(ctx.multiFolder, false);
  for (const f of files) {
    assert.equal(secondLine(f, ctx, P), undefined);
  }
  // A package still shows its name.
  assert.equal(secondLine('/p/node_modules/@rescript/react/src', ctx, P), '@rescript/react');
});

test('G8: realweb-like project → folder relative to the common prefix', () => {
  const files = ['/w/apps/web/src/pages/start/Start.res', '/w/apps/web/src/components/Button.res', '/w/apps/web/src/App.res'];
  const ctx = secondLineContext(files, P);
  assert.equal(ctx.prefix, '/w/apps/web/src');
  assert.equal(secondLine('/w/apps/web/src/pages/start/Start.res', ctx, P), 'pages/start');
  assert.equal(secondLine('/w/apps/web/src/components/Button.res', ctx, P), 'components');
  assert.equal(secondLine('/w/apps/web/src/App.res', ctx, P), undefined);
  assert.equal(secondLine('/elsewhere/X.res', ctx, P), undefined);
  assert.equal(secondLine(undefined, ctx, P), undefined);
  assert.equal(secondLine('/w/node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src', ctx, P), 'pkg');
});

test('G8: a path with " and \\ after QA-9 unescaping', () => {
  const dot = 'digraph dependencies {\n  "A" [label="A", tooltip="/t/quote\\"dir/src/a/A.res"];\n  "B" [label="B", tooltip="/t/back\\\\slash/src/b/B.res"];\n}\n';
  const [a, b] = parseDot(dot).nodes;
  assert.equal(a.tooltip, '/t/quote"dir/src/a/A.res');
  assert.equal(b.tooltip, '/t/back\\slash/src/b/B.res');
  const ctx = secondLineContext(['/t/quote"dir/src/a/A.res', '/t/quote"dir/src/c/C.res'], P);
  assert.equal(secondLine(a.tooltip, ctx, P), 'a');
});

test('module files match case-insensitively, .res before .resi (QA-2)', () => {
  const files = ['/p/src/mathUtil.resi', '/p/src/mathUtil.res', '/p/src/utils.res', '/p/lib/MathUtilities.res'];
  assert.deepEqual(matchModuleFiles('MathUtil', files), ['/p/src/mathUtil.res', '/p/src/mathUtil.resi']);
  assert.deepEqual(matchModuleFiles('Missing', files), []);
  assert.equal(moduleNameFromFile('/p/src/mathUtil.res'), 'MathUtil');
  assert.equal(moduleNameFromFile('/p/src/App.resi'), 'App');
  assert.equal(isSourceFile('/p/src/a.res'), true);
  assert.equal(isSourceFile('/p/node_modules/@rescript/react/src'), false);
});
