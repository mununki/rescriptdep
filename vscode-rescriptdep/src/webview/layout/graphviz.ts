// Full-graph layout: Graphviz `dot` (viz-js, bundled) used for positions and edge splines only.
// Object input (no DOT text), `json0` output; we draw our own cards and edges from it.
import { instance, type Graph, type Viz } from '@viz-js/viz';
import type { LayoutOptions, RenderErrorKind } from '../../protocol';
import type { CardSize } from '../measure';
import type { PreparedGraph } from '../model';
import { r1 } from '../dom';
import { copy } from '../text';
import { ARROW_INSET, type ClusterBox, type EdgeGeom, type Layout, type NodeBox, type Rect, unionRect } from './types';

export class LayoutError extends Error {
  constructor(
    readonly kind: RenderErrorKind,
    message: string,
  ) {
    super(message);
  }
}

let vizPromise: Promise<Viz> | undefined;

function getViz(): Promise<Viz> {
  if (!vizPromise) {
    vizPromise = instance();
    // A failed instantiation must not poison later attempts (Retry).
    vizPromise.catch(() => {
      vizPromise = undefined;
    });
  }
  return vizPromise;
}

/** After a throw inside the wasm module (stack overflow, OOM) the instance is unusable; start fresh next time. */
function discardViz(): void {
  vizPromise = undefined;
}

const TUNING = { nslimit: 2, nslimit1: 2, mclimit: 0.1 };

interface Json0Object {
  _gvid: number;
  name: string;
  pos?: string;
  bb?: string;
  lp?: string;
  nodes?: number[];
}

interface Json0Edge {
  tail: number;
  head: number;
  pos?: string;
}

interface Json0 {
  bb?: string;
  objects?: Json0Object[];
  edges?: Json0Edge[];
}

function isTooLarge(e: unknown): boolean {
  if (e instanceof RangeError) {
    return true;
  }
  const msg = e instanceof Error ? e.message : String(e);
  return /stack|memory|out of bounds/i.test(msg);
}

export async function graphvizLayout(
  g: PreparedGraph,
  sizes: Map<string, CardSize>,
  options: LayoutOptions,
): Promise<Layout> {
  let viz: Viz;
  try {
    viz = await getViz();
  } catch (e) {
    throw new LayoutError('unknown', `Graphviz failed to load: ${e instanceof Error ? e.message : String(e)}`);
  }
  const clusterNodes = g.cycles.map((ids, i) => ({
    name: `cluster_${i}`,
    graphAttributes: { label: copy.cycleLabel, labeljust: 'l', fontname: 'Helvetica', fontsize: 11, margin: 10 },
    nodes: ids.map((name) => ({ name })),
  }));
  const graph: Graph = {
    directed: true,
    graphAttributes: {
      rankdir: 'LR',
      ranksep: 0.5,
      nodesep: 0.1,
      splines: 'true',
      ...(options.tune ? TUNING : {}),
    },
    nodeAttributes: { shape: 'box', fixedsize: 'true', label: '' },
    edgeAttributes: { arrowsize: 0.75 },
    nodes: g.nodes.map((n) => {
      const s = sizes.get(n.id) as CardSize;
      return { name: n.id, attributes: { width: s.w / 72, height: s.h / 72 } };
    }),
    edges: g.edges.map(([tail, head]) => ({ tail, head })),
    subgraphs: clusterNodes,
  };

  const t0 = performance.now();
  let output: string;
  try {
    const result = viz.render(graph, { format: 'json0' });
    if (result.status !== 'success') {
      throw new LayoutError('layout', result.errors.map((e) => e.message).join('; ') || 'Graphviz failed');
    }
    output = result.output;
  } catch (e) {
    if (e instanceof LayoutError) {
      throw e;
    }
    discardViz();
    if (isTooLarge(e)) {
      throw new LayoutError('tooLarge', e instanceof Error ? e.message : String(e));
    }
    throw new LayoutError('unknown', e instanceof Error ? e.message || e.name : String(e));
  }
  const layoutMs = performance.now() - t0;

  let json: Json0;
  try {
    json = JSON.parse(output) as Json0;
  } catch {
    throw new LayoutError('layout', 'Graphviz returned unreadable output');
  }
  return fromJson0(json, g, sizes, layoutMs);
}

function parsePoint(s: string): [number, number] {
  const [x, y] = s.split(',');
  return [Number(x), Number(y)];
}

function fromJson0(json: Json0, g: PreparedGraph, sizes: Map<string, CardSize>, layoutMs: number): Layout {
  const bb = (json.bb ?? '0,0,0,0').split(',').map(Number);
  const top = bb[3];
  const flip = (y: number) => top - y;
  const objects = json.objects ?? [];
  const byGvid = new Map<number, Json0Object>();
  for (const o of objects) {
    byGvid.set(o._gvid, o);
  }

  let bbox: Rect | null = null;
  const nodes: NodeBox[] = [];
  const clusters: ClusterBox[] = [];
  for (const o of objects) {
    if (o.nodes !== undefined) {
      if (o.bb) {
        const [x0, y0, x1, y1] = o.bb.split(',').map(Number);
        const [lx, ly] = o.lp ? parsePoint(o.lp) : [x0 + 8, y1 - 8];
        clusters.push({ x: x0, y: flip(y1), w: x1 - x0, h: y1 - y0, lx, ly: flip(ly) + 4 });
        bbox = unionRect(bbox, x0, flip(y1), x1 - x0, y1 - y0);
      }
      continue;
    }
    const size = sizes.get(o.name);
    if (!o.pos || !size) {
      continue;
    }
    const [x, y] = parsePoint(o.pos);
    const box = { id: o.name, x, y: flip(y), w: size.w, h: size.h };
    nodes.push(box);
    bbox = unionRect(bbox, box.x - box.w / 2, box.y - box.h / 2, box.w, box.h);
  }

  const edges: EdgeGeom[] = [];
  for (const e of json.edges ?? []) {
    const tail = byGvid.get(e.tail);
    const head = byGvid.get(e.head);
    if (!tail || !head || !e.pos) {
      continue;
    }
    const d = splinePath(e.pos, flip);
    if (d) {
      edges.push({ from: tail.name, to: head.name, d });
    }
  }
  return {
    engine: 'graphviz',
    nodes,
    edges,
    clusters,
    headers: [],
    bbox: bbox ?? { x: 0, y: 0, w: 0, h: 0 },
    layoutMs,
  };
}

/**
 * Converts a Graphviz spline (`[s,x,y ][e,x,y ]p0 c1 c2 p1 …`, splines separated by `;`) into path data.
 * The arrowhead tip is the `e,` point; the path ends ARROW_INSET short of it along the
 * last-point→tip direction so the marker (oriented by that final segment) covers the line end.
 */
function splinePath(pos: string, flip: (y: number) => number): string {
  let d = '';
  for (const spline of pos.split(';')) {
    let tip: [number, number] | null = null;
    const pts: [number, number][] = [];
    for (const tok of spline.trim().split(/\s+/)) {
      if (tok === '') {
        continue;
      }
      if (tok.startsWith('e,')) {
        const [x, y] = parsePoint(tok.slice(2));
        tip = [x, flip(y)];
      } else if (tok.startsWith('s,')) {
        continue; // arrow at the tail: not used (dir=forward)
      } else {
        const [x, y] = parsePoint(tok);
        pts.push([x, flip(y)]);
      }
    }
    if (pts.length === 0) {
      continue;
    }
    d += `M${r1(pts[0][0])},${r1(pts[0][1])}`;
    for (let i = 1; i + 2 < pts.length; i += 3) {
      d += `C${r1(pts[i][0])},${r1(pts[i][1])} ${r1(pts[i + 1][0])},${r1(pts[i + 1][1])} ${r1(pts[i + 2][0])},${r1(pts[i + 2][1])}`;
    }
    if (tip) {
      const last = pts[pts.length - 1];
      let ux = tip[0] - last[0];
      let uy = tip[1] - last[1];
      const len = Math.hypot(ux, uy);
      if (len < 1e-6) {
        ux = 1;
        uy = 0;
      } else {
        ux /= len;
        uy /= len;
      }
      d += `L${r1(tip[0] - ux * ARROW_INSET)},${r1(tip[1] - uy * ARROW_INSET)}`;
    }
  }
  return d;
}
