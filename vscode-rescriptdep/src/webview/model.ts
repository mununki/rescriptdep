// Normalizes the host's GraphModel into the structure the layouts and the renderer share.
import type { GraphModel, GraphNode, GraphView } from '../protocol';

export interface PreparedGraph {
  view: GraphView;
  /** Focus view only, and only when the centre is a node of the model. */
  center?: string;
  /** Sorted by id in code-point order (the CLI's `String.compare`). */
  nodes: GraphNode[];
  byId: Map<string, GraphNode>;
  /** Deduplicated, CLI order, both endpoints present in `nodes`. */
  edges: [string, string][];
  cycles: string[][];
  inDegree: Map<string, number>;
  outDegree: Map<string, number>;
}

/** Locale-independent code-point order, identical to OCaml's byte-wise compare of UTF-8 strings. */
export function compareCodePoints(a: string, b: string): number {
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const ca = a.charCodeAt(i);
    const cb = b.charCodeAt(i);
    if (ca !== cb) {
      // Surrogates (astral code points) sort after every BMP code point in code-point order.
      const sa = ca >= 0xd800 && ca <= 0xdfff;
      const sb = cb >= 0xd800 && cb <= 0xdfff;
      if (sa !== sb) {
        return sa ? 1 : -1;
      }
      return ca - cb;
    }
  }
  return a.length - b.length;
}

export class GraphDataError extends Error {}

function isString(v: unknown): v is string {
  return typeof v === 'string';
}

export function prepareGraph(model: GraphModel, view: GraphView, center: string | undefined): PreparedGraph {
  if (!model || !Array.isArray(model.nodes) || !Array.isArray(model.edges)) {
    throw new GraphDataError('The render message has no graph model.');
  }
  const byId = new Map<string, GraphNode>();
  for (const n of model.nodes) {
    if (!n || !isString(n.id) || n.id === '') {
      throw new GraphDataError('A module has no name.');
    }
    if (!byId.has(n.id)) {
      byId.set(n.id, {
        id: n.id,
        secondary: isString(n.secondary) && n.secondary !== '' ? n.secondary : undefined,
        external: n.external === true,
        openable: n.openable === true,
      });
    }
  }
  const edges: [string, string][] = [];
  const seen = new Set<string>();
  const inDegree = new Map<string, number>();
  const outDegree = new Map<string, number>();
  for (const e of model.edges) {
    if (!Array.isArray(e) || !isString(e[0]) || !isString(e[1])) {
      throw new GraphDataError('A dependency is malformed.');
    }
    const [from, to] = e;
    const key = `${from}\u0000${to}`;
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    for (const id of [from, to]) {
      if (!byId.has(id)) {
        byId.set(id, { id, external: false, openable: false });
      }
    }
    edges.push([from, to]);
    outDegree.set(from, (outDegree.get(from) ?? 0) + 1);
    inDegree.set(to, (inDegree.get(to) ?? 0) + 1);
  }
  const nodes = [...byId.values()].sort((a, b) => compareCodePoints(a.id, b.id));
  const cycles = Array.isArray(model.cycles)
    ? model.cycles
        .filter((c): c is string[] => Array.isArray(c))
        .map((c) => c.filter((id) => isString(id) && byId.has(id)))
        .filter((c) => c.length > 1)
    : [];
  return {
    view,
    center: view === 'focus' && center !== undefined && byId.has(center) ? center : undefined,
    nodes,
    byId,
    edges,
    cycles,
    inDegree,
    outDegree,
  };
}
