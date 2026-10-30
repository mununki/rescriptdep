// Large-graph policy (architect-review3 §R2). Must not import 'vscode'.
//
// L = |V| + Σ max(0, span(e) − 1) over a longest-path layering, i.e. real nodes plus the virtual
// nodes `dot` creates for long edges. Back edges found by one DFS are reversed first, as `dot` does.

import type { GraphView, LayoutOptions } from '../protocol';

/** Tune (`nslimit=2, nslimit1=2, mclimit=0.1`) at or above any of these. */
export const TUNE_MIN_NODES = 400;
export const TUNE_MIN_EDGES = 1200;
export const TUNE_MIN_L = 6_000;
/** Ask before laying out the full graph at or above this L. */
export const PROMPT_MIN_L = 20_000;
/** Below the tuning thresholds, layouts from here on take ~0.4 s or more: show the overlay first. */
export const SLOW_MIN_L = 4_000;

export interface LayoutSize {
  nodes: number;
  edges: number;
  /** Estimated layout size: nodes plus virtual nodes. */
  L: number;
}

/** O(V + E). Self-loops are ignored; edge endpoints missing from `nodeIds` are added. */
export function layoutSize(nodeIds: readonly string[], edges: readonly (readonly [string, string])[]): LayoutSize {
  const index = new Map<string, number>();
  const idOf = (id: string): number => {
    let i = index.get(id);
    if (i === undefined) {
      i = index.size;
      index.set(id, i);
    }
    return i;
  };
  for (const id of nodeIds) {
    idOf(id);
  }
  const from: number[] = [];
  const to: number[] = [];
  for (const [a, b] of edges) {
    const u = idOf(a);
    const v = idOf(b);
    if (u !== v) {
      from.push(u);
      to.push(v);
    }
  }
  const n = index.size;
  const m = from.length;

  // Adjacency in CSR form (edge ids per source node, in input order).
  const outStart = new Int32Array(n + 1);
  for (let e = 0; e < m; e++) {
    outStart[from[e] + 1]++;
  }
  for (let i = 0; i < n; i++) {
    outStart[i + 1] += outStart[i];
  }
  const outEdges = new Int32Array(m);
  const fill = outStart.slice(0, n);
  for (let e = 0; e < m; e++) {
    outEdges[fill[from[e]]++] = e;
  }

  // One iterative DFS in node order; an edge into a node still on the stack is a back edge.
  const state = new Uint8Array(n); // 0 unvisited, 1 on stack, 2 done
  const reversed = new Uint8Array(m);
  const stackNode = new Int32Array(n);
  const stackNext = new Int32Array(n);
  for (let root = 0; root < n; root++) {
    if (state[root] !== 0) {
      continue;
    }
    let top = 0;
    stackNode[0] = root;
    stackNext[0] = outStart[root];
    state[root] = 1;
    while (top >= 0) {
      const u = stackNode[top];
      if (stackNext[top] < outStart[u + 1]) {
        const e = outEdges[stackNext[top]++];
        const v = to[e];
        if (state[v] === 1) {
          reversed[e] = 1;
        } else if (state[v] === 0) {
          state[v] = 1;
          top++;
          stackNode[top] = v;
          stackNext[top] = outStart[v];
        }
      } else {
        state[u] = 2;
        top--;
      }
    }
  }

  // Longest-path layering with Kahn's algorithm (queue with an index pointer, not Array.shift).
  const src = new Int32Array(m);
  const dst = new Int32Array(m);
  const indeg = new Int32Array(n);
  const dagStart = new Int32Array(n + 1);
  for (let e = 0; e < m; e++) {
    src[e] = reversed[e] ? to[e] : from[e];
    dst[e] = reversed[e] ? from[e] : to[e];
    indeg[dst[e]]++;
    dagStart[src[e] + 1]++;
  }
  for (let i = 0; i < n; i++) {
    dagStart[i + 1] += dagStart[i];
  }
  const dagEdges = new Int32Array(m);
  const dagFill = dagStart.slice(0, n);
  for (let e = 0; e < m; e++) {
    dagEdges[dagFill[src[e]]++] = e;
  }
  const rank = new Int32Array(n);
  const queue = new Int32Array(n);
  let tail = 0;
  for (let i = 0; i < n; i++) {
    if (indeg[i] === 0) {
      queue[tail++] = i;
    }
  }
  for (let head = 0; head < tail; head++) {
    const u = queue[head];
    for (let k = dagStart[u]; k < dagStart[u + 1]; k++) {
      const v = dst[dagEdges[k]];
      if (rank[u] + 1 > rank[v]) {
        rank[v] = rank[u] + 1;
      }
      if (--indeg[v] === 0) {
        queue[tail++] = v;
      }
    }
  }

  let virtualNodes = 0;
  for (let e = 0; e < m; e++) {
    const span = rank[dst[e]] - rank[src[e]];
    if (span > 1) {
      virtualNodes += span - 1;
    }
  }
  return { nodes: n, edges: edges.length, L: n + virtualNodes };
}

export interface SizeClass {
  tune: boolean;
  prompt: boolean;
}

/** The two thresholds of §R2, independent of the view. */
export function classifySize(size: LayoutSize): SizeClass {
  return {
    tune: size.nodes >= TUNE_MIN_NODES || size.edges >= TUNE_MIN_EDGES || size.L >= TUNE_MIN_L,
    prompt: size.L >= PROMPT_MIN_L,
  };
}

export interface LayoutDecision extends LayoutOptions {
  /** Ask "Focus on Module… / Render Anyway" before sending the render. */
  prompt: boolean;
}

/**
 * Full graph: tune and prompt per §R2; "Render Anyway" always tunes and never prompts.
 * Focus (star) and unused (grid) never reach Graphviz, so they never prompt and are never slow;
 * focus still carries `tune` for the webview's Graphviz fallback when the star check fails.
 */
export function decideLayout(view: GraphView, size: LayoutSize, options: { renderAnyway?: boolean } = {}): LayoutDecision {
  const cls = classifySize(size);
  switch (view) {
    case 'full': {
      const tune = options.renderAnyway === true || cls.tune;
      return { tune, slow: tune || size.L >= SLOW_MIN_L, prompt: !options.renderAnyway && cls.prompt };
    }
    case 'focus':
      return { tune: cls.tune, slow: false, prompt: false };
    case 'unused':
      return { tune: false, slow: false, prompt: false };
  }
}
