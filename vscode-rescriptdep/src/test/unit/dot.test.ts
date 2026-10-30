import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { DotParseError, compareIds, graphCounts, parseDot } from '../../core/dot';
import { buildModel } from '../../graphPanel';
import type { ModuleSource } from '../../project';
import rescript11 from './fixtures/rescript11.dot';
import app100 from './fixtures/app100.dot';
import mediumCycle from './fixtures/medium-cycle.dot';

test('full graph: nodes with tooltips, edges in CLI order', () => {
  const parsed = parseDot(rescript11);
  assert.deepEqual(graphCounts(parsed), { nodes: 7, edges: 7 });
  assert.deepEqual(
    parsed.nodes.map((n) => n.id),
    ['App', 'Comp0', 'Logger', 'Math', 'React', 'ReactDOM', 'Utils'],
  );
  assert.equal(parsed.nodes[0].tooltip, '/repo/test/rescript/src/app.res');
  assert.equal(parsed.nodes[4].tooltip, '/repo/test/rescript/node_modules/@rescript/react/src');
  assert.deepEqual(parsed.edges[0], ['App', 'Comp0']);
  assert.deepEqual(parsed.edges.at(-1), ['Math', 'Utils']);
  assert.deepEqual(parsed.cycles, []);
});

test('focus and --no-dependents shapes', () => {
  const focus = parseDot(
    'digraph dependencies {\n  rankdir=LR;\n  node [shape=box, style=filled, fillcolor=lightblue];\n\n  "App" [label="App", tooltip="/p/src/app.res"];\n  "Math" [label="Math", tooltip="/p/src/math.res"];\n\n  "App" -> "Math";\n}\n',
  );
  assert.deepEqual(graphCounts(focus), { nodes: 2, edges: 1 });
  const nd = parseDot('digraph G {\n  rankdir=LR;\n  node [shape=box];\n  "App";\n  "Tool";\n}\n');
  assert.deepEqual(nd.nodes, [{ id: 'App' }, { id: 'Tool' }]);
  assert.deepEqual(nd.edges, []);
  const empty = parseDot('digraph dependencies {\n  rankdir=LR;\n  node [shape=box, style=filled, fillcolor=lightblue];\n\n\n}\n');
  assert.deepEqual(graphCounts(empty), { nodes: 0, edges: 0 });
});

test('app100 fixture counts', () => {
  assert.deepEqual(graphCounts(parseDot(app100)), { nodes: 101, edges: 383 });
});

test('cycle clusters become cycles; their members stay plain nodes', () => {
  const parsed = parseDot(mediumCycle);
  assert.deepEqual(graphCounts(parsed), { nodes: 30, edges: 55 });
  assert.deepEqual(parsed.cycles, [['SettingsTheme', 'SharedDate', 'OrderPicker']]);
});

test('QA-9 escapes: \\" and \\\\ in IDs and tooltips; other backslashes kept', () => {
  const dot = [
    'digraph dependencies {',
    '  "Q" [label="Q", tooltip="/t/quote\\"dir/src/q.res"];',
    '  "B" [label="B", tooltip="/t/back\\\\slash/src/b.res"];',
    '  "W" [label="W", tooltip="C:\\\\Users\\\\me\\\\new\\\\W.res"];',
    '  "Old" [label="Old", tooltip="/t/back\\slash/old.res"];',
    '  "Q" -> "B";',
    '}',
  ].join('\n');
  const tips = Object.fromEntries(parseDot(dot).nodes.map((n) => [n.id, n.tooltip]));
  assert.equal(tips.Q, '/t/quote"dir/src/q.res');
  assert.equal(tips.B, '/t/back\\slash/src/b.res');
  assert.equal(tips.W, 'C:\\Users\\me\\new\\W.res');
  assert.equal(tips.Old, '/t/back\\slash/old.res');
});

test('robust to comments, edge chains, attributes on edges and leading noise', () => {
  const dot = 'Warning: Could not read directory x\n// header\ndigraph dependencies {\n  /* c */ "A" -> "B" -> "C" [color=red];\n  # comment line\n  subgraph { "D"; }\n  E -> A;\n}\n';
  const parsed = parseDot(dot);
  assert.deepEqual(parsed.edges, [['A', 'B'], ['B', 'C'], ['E', 'A']]);
  assert.deepEqual(parsed.nodes.map((n) => n.id), ['A', 'B', 'C', 'D', 'E']);
});

test('malformed output throws DotParseError', () => {
  assert.throws(() => parseDot(''), DotParseError);
  assert.throws(() => parseDot('rescriptdep: error'), DotParseError);
  assert.throws(() => parseDot('digraph x { "A" [label="A'), DotParseError);
  assert.throws(() => parseDot('digraph x { "A" -> }'), DotParseError);
  assert.throws(() => parseDot('digraph x { "A"'), DotParseError);
});

test('compareIds is code-point order, locale-independent (like the CLI)', () => {
  const ids = ['b', 'HeaderBar', 'Header_Cart', 'a', 'Z', 'Ä', '_x'];
  assert.deepEqual([...ids].sort(compareIds), ['HeaderBar', 'Header_Cart', 'Z', '_x', 'a', 'b', 'Ä']);
});

test('buildModel: sorted nodes, flags from the host, no paths for the webview', () => {
  const parsed = parseDot(rescript11);
  const sources = new Map<string, ModuleSource>([
    ['App', { file: '/repo/test/rescript/src/app.res', openable: true, external: false, secondary: 'src' }],
    ['React', { file: '/repo/test/rescript/node_modules/@rescript/react/src/React.res', openable: true, external: true, secondary: '@rescript/react' }],
  ]);
  const model = buildModel(parsed, sources);
  assert.deepEqual(model.nodes[0], { id: 'App', external: false, openable: true, secondary: 'src' });
  assert.deepEqual(model.nodes.find((n) => n.id === 'Utils'), { id: 'Utils', external: false, openable: false });
  assert.deepEqual(model.nodes.find((n) => n.id === 'React'), { id: 'React', external: true, openable: true, secondary: '@rescript/react' });
  assert.equal(model.edges.length, 7);
  assert.ok(!JSON.stringify(model).includes('/repo'), 'model must not carry paths');
  const cyc = buildModel(parseDot(mediumCycle), new Map());
  assert.deepEqual(cyc.cycles, [['SettingsTheme', 'SharedDate', 'OrderPicker']]);
  const ids = cyc.nodes.map((n) => n.id);
  assert.deepEqual(ids, [...ids].sort(compareIds));
});
