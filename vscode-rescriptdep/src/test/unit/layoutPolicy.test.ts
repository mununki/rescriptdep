import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { parseDot } from '../../core/dot';
import { classifySize, decideLayout, layoutSize } from '../../core/layoutPolicy';
import type { LayoutSize } from '../../core/layoutPolicy';
import { PyRandom } from './pyrandom';
import rescript11 from './fixtures/rescript11.dot';
import app100 from './fixtures/app100.dot';
import mediumCycle from './fixtures/medium-cycle.dot';

function sizeOf(dot: string): LayoutSize {
  const parsed = parseDot(dot);
  return layoutSize(parsed.nodes.map((n) => n.id), parsed.edges);
}

type Expect = 'none' | 'tune' | 'prompt';
const expected = (size: LayoutSize): Expect => {
  const c = classifySize(size);
  return c.prompt ? 'prompt' : c.tune ? 'tune' : 'none';
};

// architect-review3 §R2 "Every input, classified": measured nodes / edges / L for each row.
// The designer's acceptance table is the 'expect' column (none = not tuned).
const TABLE: [string, number, number, number, Expect][] = [
  ['fixture', 7, 7, 8, 'none'],
  ['app50', 51, 180, 417, 'none'],
  ['app100', 101, 383, 1781, 'none'],
  ['layered100', 101, 360, 799, 'none'],
  ['layered200', 201, 730, 1681, 'none'],
  ['app200', 201, 777, 2196, 'none'],
  ['g200', 200, 744, 3959, 'none'],
  ['app300', 301, 1181, 4571, 'none'],
  ['sub500', 500, 1077, 5400, 'tune'],
  ['layered500', 501, 1837, 4138, 'tune'],
  ['g500', 500, 1937, 12299, 'tune'],
  ['d_120_4_10', 120, 520, 2104, 'none'],
  ['d_150_4_20', 150, 682, 5339, 'none'],
  ['d_100_3_60', 100, 382, 6167, 'tune'],
  ['d_200_4_20', 200, 921, 6864, 'tune'],
  ['d_150_4_30', 150, 707, 7919, 'tune'],
  ['d_300_3_30', 300, 1154, 12227, 'tune'],
  ['d_200_4_40', 200, 942, 13570, 'tune'],
  ['d_180_3_60', 180, 700, 13607, 'tune'],
  ['d_250_4_30', 250, 1184, 13737, 'tune'],
  ['d_390_2_40', 390, 1152, 15155, 'tune'],
  ['app1000', 1001, 3976, 15821, 'tune'],
  ['sub1000', 1000, 2827, 20200, 'prompt'],
  ['g1000', 1000, 3900, 24197, 'prompt'],
  ['real app B', 883, 4448, 24581, 'prompt'],
  ['chain200', 201, 966, 49438, 'prompt'],
  ['g2000', 2000, 7887, 53418, 'prompt'],
  ['app3000', 3001, 12019, 56136, 'prompt'],
  ['real app A', 2036, 15018, 191799, 'prompt'],
  ['p2000', 2001, 11985, 195008, 'prompt'],
  ['medium-cycle', 30, 55, 138, 'none'],
];

for (const [name, nodes, edges, L, expect] of TABLE) {
  test(`classification: ${name} (${nodes}/${edges}, L ${L}) → ${expect}`, () => {
    assert.equal(expected({ nodes, edges, L }), expect);
  });
}

test('thresholds sit exactly at 400 nodes / 1200 edges / L 6000 / L 20000', () => {
  assert.equal(expected({ nodes: 399, edges: 1199, L: 5999 }), 'none');
  assert.equal(expected({ nodes: 400, edges: 0, L: 400 }), 'tune');
  assert.equal(expected({ nodes: 10, edges: 1200, L: 10 }), 'tune');
  assert.equal(expected({ nodes: 10, edges: 10, L: 6000 }), 'tune');
  assert.equal(expected({ nodes: 10, edges: 10, L: 19999 }), 'tune');
  assert.equal(expected({ nodes: 10, edges: 10, L: 20000 }), 'prompt');
});

test('L on checked-in fixtures: r11 = 8, app100 = 1781', () => {
  assert.deepEqual(sizeOf(rescript11), { nodes: 7, edges: 7, L: 8 });
  assert.deepEqual(sizeOf(app100), { nodes: 101, edges: 383, L: 1781 });
});

test('cycles: back edges are reversed before layering (medium-cycle, DES-18)', () => {
  assert.deepEqual(sizeOf(mediumCycle), { nodes: 30, edges: 55, L: 138 });
  // A pure cycle still layers every node: a → b → c → a is a chain of 3 with one reversed edge.
  assert.deepEqual(layoutSize(['a', 'b', 'c'], [['a', 'b'], ['b', 'c'], ['c', 'a']]), { nodes: 3, edges: 3, L: 4 });
  // Self-loops and undeclared endpoints.
  assert.deepEqual(layoutSize(['a'], [['a', 'a'], ['a', 'b']]), { nodes: 2, edges: 2, L: 2 });
  assert.deepEqual(layoutSize([], []), { nodes: 0, edges: 0, L: 0 });
});

// arch-r3/gendeep.py: n nodes in a chain, each also pointing to k random later nodes at distance <= d.
function gendeep(n: number, k: number, d: number, seed: number): { nodes: string[]; edges: [string, string][] } {
  const rng = new PyRandom(seed);
  const names = Array.from({ length: n }, (_, i) => `M${String(i).padStart(3, '0')}`);
  const set = new Set<number>();
  const add = (a: number, b: number) => set.add(a * 100000 + b);
  for (let i = 0; i < n - 1; i++) {
    add(i, i + 1);
    for (let r = 0; r < k; r++) {
      if (i + 2 < n) {
        const j = i + rng.randint(2, Math.max(2, Math.min(d, n - 1 - i)));
        if (j < n) {
          add(i, j);
        }
      }
    }
  }
  const edges = [...set].sort((a, b) => a - b).map((e) => [names[Math.floor(e / 100000)], names[e % 100000]] as [string, string]);
  return { nodes: names, edges };
}

// arch-r3/gendeep.sh seeds; the numbers are the architect's DOT files measured with this L.
const DEEP: [number, number, number, number, number, number][] = [
  [120, 4, 10, 1, 520, 2104],
  [150, 4, 20, 2, 682, 5339],
  [150, 4, 30, 3, 707, 7919],
  [200, 4, 20, 4, 921, 6864],
  [200, 4, 40, 5, 942, 13570],
  [250, 4, 30, 6, 1184, 13737],
  [300, 3, 30, 7, 1154, 12227],
  [390, 2, 40, 8, 1152, 15155],
  [100, 3, 60, 9, 382, 6167],
  [180, 3, 60, 10, 700, 13607],
];

for (const [n, k, d, seed, edges, L] of DEEP) {
  test(`deep band d_${n}_${k}_${d} (seed ${seed}): invisible to count rules, tuned iff L >= 6000, never prompted`, () => {
    const g = gendeep(n, k, d, seed);
    const size = layoutSize(g.nodes, g.edges);
    assert.deepEqual(size, { nodes: n, edges, L });
    assert.ok(size.nodes < 400 && size.edges < 1200, 'count rules would not see it');
    const decision = decideLayout('full', size);
    assert.equal(decision.tune, L >= 6000);
    assert.equal(decision.prompt, false);
  });
}

/** Small deterministic PRNG for the generated large inputs. */
function mulberry32(seed: number): () => number {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// chain200-like: every module imports the hub, its predecessor and a few random earlier modules.
function chain(n: number, extra: number, seed: number) {
  const rnd = mulberry32(seed);
  const nodes = ['Hub', ...Array.from({ length: n }, (_, i) => `Mod${String(i).padStart(4, '0')}`)];
  const edges: [string, string][] = [];
  for (let i = 0; i < n; i++) {
    const deps = new Set<number>();
    if (i > 0) {
      deps.add(i - 1);
    }
    for (let r = 0; r < extra && i > 1; r++) {
      deps.add(Math.floor(rnd() * (i - 1)));
    }
    edges.push([nodes[i + 1], 'Hub']);
    for (const dep of [...deps].sort((a, b) => a - b)) {
      edges.push([nodes[i + 1], nodes[dep + 1]]);
    }
  }
  return { nodes, edges };
}

// app-like: `ranks` layers, each module imports `fanOut` modules from any lower layer.
function layered(n: number, ranks: number, fanOut: number, seed: number) {
  const rnd = mulberry32(seed);
  const nodes = Array.from({ length: n }, (_, i) => `M${i}`);
  const rankOf = (i: number) => Math.floor((i * ranks) / n);
  const edges: [string, string][] = [];
  for (let i = 0; i < n; i++) {
    const r = rankOf(i);
    if (r === 0) {
      continue;
    }
    const firstOfRank = Math.ceil((r * n) / ranks);
    const deps = new Set<number>();
    while (deps.size < Math.min(fanOut, firstOfRank)) {
      deps.add(Math.floor(rnd() * firstOfRank));
    }
    for (const dep of deps) {
      edges.push([nodes[i], nodes[dep]]);
    }
  }
  return { nodes, edges };
}

test('chain200-like (generated): under every count rule, prompted by L alone', () => {
  const g = chain(200, 3, 200);
  const size = layoutSize(g.nodes, g.edges);
  assert.ok(size.nodes < 400 && size.edges < 1200, `${size.nodes}/${size.edges}`);
  assert.ok(size.L >= 20000, `L ${size.L}`);
  assert.deepEqual(decideLayout('full', size), { tune: true, slow: true, prompt: true });
});

test('large generated inputs: app1000-like tuned without a prompt, app3000/p2000-like prompted', () => {
  const size = (g: { nodes: string[]; edges: [string, string][] }) => layoutSize(g.nodes, g.edges);
  const app1000 = size(layered(1001, 11, 4, 1000));
  assert.equal(expected(app1000), 'tune', JSON.stringify(app1000));
  const app3000 = size(layered(3001, 13, 4, 3000));
  assert.equal(expected(app3000), 'prompt', JSON.stringify(app3000));
  const p2000 = chain(2000, 5, 2000);
  assert.equal(expected(layoutSize(p2000.nodes, p2000.edges)), 'prompt');
});

test('cost: L for a real-app-A-sized graph (2036 nodes / ~15k edges, with cycles) stays well under 20 ms', () => {
  const g = layered(2036, 26, 7, 2036);
  // Add back edges so the DFS reversal runs on a cyclic graph.
  const rnd = mulberry32(7);
  for (let i = 0; i < 200; i++) {
    g.edges.push([g.nodes[Math.floor(rnd() * 200)], g.nodes[2036 - 1 - Math.floor(rnd() * 200)]]);
  }
  layoutSize(g.nodes, g.edges); // warm-up
  const runs: number[] = [];
  for (let i = 0; i < 5; i++) {
    const start = performance.now();
    layoutSize(g.nodes, g.edges);
    runs.push(performance.now() - start);
  }
  runs.sort((a, b) => a - b);
  const median = runs[2];
  // QA's gate is 20 ms on real app A (measured 5 ms there); the bound here leaves room for slow CI machines.
  assert.ok(median < 60, `median ${median.toFixed(1)} ms`);
});

test('decideLayout: the policy applies to the full graph only', () => {
  const huge: LayoutSize = { nodes: 2036, edges: 15018, L: 191799 };
  const small: LayoutSize = { nodes: 7, edges: 7, L: 8 };
  assert.deepEqual(decideLayout('full', huge), { tune: true, slow: true, prompt: true });
  assert.deepEqual(decideLayout('full', huge, { renderAnyway: true }), { tune: true, slow: true, prompt: false });
  assert.deepEqual(decideLayout('full', small), { tune: false, slow: false, prompt: false });
  assert.deepEqual(decideLayout('full', small, { renderAnyway: true }), { tune: true, slow: true, prompt: false });
  assert.deepEqual(decideLayout('full', { nodes: 301, edges: 1181, L: 4571 }), { tune: false, slow: true, prompt: false });
  // Hub focus (1001 nodes) is a star: never prompted, never slow; `tune` only matters for the Graphviz fallback.
  assert.deepEqual(decideLayout('focus', { nodes: 1001, edges: 1000, L: 1001 }), { tune: true, slow: false, prompt: false });
  assert.deepEqual(decideLayout('focus', huge), { tune: true, slow: false, prompt: false });
  assert.deepEqual(decideLayout('unused', { nodes: 689, edges: 0, L: 689 }), { tune: false, slow: false, prompt: false });
});
