// Focus view: engine-less three-column star (USED BY · centre · USES), designer-review3 R1 "Focus view".
import type { CardSize } from '../measure';
import { fonts, textWidth, uiFontFamily } from '../measure';
import { compareCodePoints, type PreparedGraph } from '../model';
import { r1 } from '../dom';
import { copy } from '../text';
import {
  ARROW_INSET,
  ARROW_LENGTH,
  type ColumnHeader,
  type EdgeGeom,
  type Layout,
  type NodeBox,
  type Rect,
  unionRect,
} from './types';

const BUS_THRESHOLD = 12;
const ROW_GAP = 10;
const TRUNK_OFFSET = 28;
const CORNER = 8;
/** Vertical offset of a back edge (a USES module that also depends on the centre) so it doesn't overlap. */
const BACK_EDGE_OFFSET = 5;

/** S-curve from (sx, sy) to the arrow tip (tx, ty); control points offset horizontally by half the run. */
function sCurve(sx: number, sy: number, tx: number, ty: number): string {
  const dir = tx >= sx ? 1 : -1;
  const bx = tx - dir * ARROW_LENGTH;
  const dx = (bx - sx) * 0.5;
  return (
    `M${r1(sx)},${r1(sy)}C${r1(sx + dx)},${r1(sy)} ${r1(bx - dx)},${r1(ty)} ${r1(bx)},${r1(ty)}` +
    `L${r1(tx - dir * ARROW_INSET)},${r1(ty)}`
  );
}

/** Bus route: horizontal stub, vertical trunk at `trunkX`, horizontal into the tip; rounded corners. */
function busPath(sx: number, sy: number, tx: number, ty: number, trunkX: number): string {
  const endX = tx - ARROW_INSET;
  const dy = ty - sy;
  const r = Math.min(CORNER, Math.abs(dy) / 2);
  if (r < 1) {
    return `M${r1(sx)},${r1(sy)}H${r1(trunkX)}L${r1(endX)},${r1(ty)}`;
  }
  const dir = dy > 0 ? 1 : -1;
  return (
    `M${r1(sx)},${r1(sy)}H${r1(trunkX - r)}Q${r1(trunkX)},${r1(sy)} ${r1(trunkX)},${r1(sy + dir * r)}` +
    `V${r1(ty - dir * r)}Q${r1(trunkX)},${r1(ty)} ${r1(trunkX + r)},${r1(ty)}H${r1(endX)}`
  );
}

/**
 * Returns null when the graph is not a star around `center` (some edge doesn't touch it, or a module
 * isn't connected to it); the caller then falls back to Graphviz.
 */
export function starLayout(g: PreparedGraph, sizes: Map<string, CardSize>, center: string): Layout | null {
  const t0 = performance.now();
  if (!g.byId.has(center)) {
    return null;
  }
  const dependents = new Set<string>();
  const dependencies = new Set<string>();
  for (const [from, to] of g.edges) {
    if (from === center && to === center) {
      continue;
    }
    if (to === center) {
      dependents.add(from);
    } else if (from === center) {
      dependencies.add(to);
    } else {
      return null;
    }
  }
  for (const n of g.nodes) {
    if (n.id !== center && !dependents.has(n.id) && !dependencies.has(n.id)) {
      return null;
    }
  }
  // A module that is both a dependent and a dependency (a cycle) goes in USES and keeps both edges.
  const left = [...dependents].filter((id) => !dependencies.has(id)).sort(compareCodePoints);
  const right = [...dependencies].sort(compareCodePoints);

  const cs = sizes.get(center) as CardSize;
  const busLeft = left.length > BUS_THRESHOLD;
  const busRight = right.length > BUS_THRESHOLD;
  const gapX = busLeft || busRight ? 120 : 96;
  const nodes: NodeBox[] = [{ id: center, x: 0, y: 0, w: cs.w, h: cs.h }];
  const boxes = new Map<string, NodeBox>([[center, nodes[0]]]);
  let bbox: Rect | null = unionRect(null, -cs.w / 2, -cs.h / 2, cs.w, cs.h);

  const column = (ids: string[], side: -1 | 1): number => {
    const total = ids.reduce((s, id) => s + (sizes.get(id) as CardSize).h, 0) + ROW_GAP * Math.max(0, ids.length - 1);
    let y = -total / 2;
    for (const id of ids) {
      const { w, h } = sizes.get(id) as CardSize;
      const x = side < 0 ? -cs.w / 2 - gapX - w / 2 : cs.w / 2 + gapX + w / 2;
      const box = { id, x, y: y + h / 2, w, h };
      nodes.push(box);
      boxes.set(id, box);
      bbox = unionRect(bbox, x - w / 2, y, w, h);
      y += h + ROW_GAP;
    }
    return -total / 2;
  };
  const leftTop = column(left, -1);
  const rightTop = column(right, 1);

  const edges: EdgeGeom[] = [];
  const centreLeft = -cs.w / 2;
  const centreRight = cs.w / 2;
  const leftInner = -cs.w / 2 - gapX;
  for (const id of left) {
    const b = boxes.get(id) as NodeBox;
    const sx = b.x + b.w / 2;
    edges.push({
      from: id,
      to: center,
      kind: 'dependent',
      d: busLeft ? busPath(sx, b.y, centreLeft, 0, leftInner + TRUNK_OFFSET) : sCurve(sx, b.y, centreLeft, 0),
    });
  }
  for (const id of right) {
    const b = boxes.get(id) as NodeBox;
    const tx = b.x - b.w / 2;
    edges.push({
      from: center,
      to: id,
      kind: 'dependency',
      d: busRight ? busPath(centreRight, 0, tx, b.y, centreRight + TRUNK_OFFSET) : sCurve(centreRight, 0, tx, b.y),
    });
    if (dependents.has(id)) {
      edges.push({
        from: id,
        to: center,
        kind: 'dependent',
        d: sCurve(tx, b.y + BACK_EDGE_OFFSET, centreRight, BACK_EDGE_OFFSET),
      });
    }
  }
  // Keep the CLI's edge order for element order (search, harness and tracing don't depend on it).
  const order = new Map(g.edges.map(([a, b], i) => [`${a}\u0000${b}`, i]));
  edges.sort((a, b) => (order.get(`${a.from}\u0000${a.to}`) ?? 0) - (order.get(`${b.from}\u0000${b.to}`) ?? 0));

  const f = fonts(uiFontFamily());
  const headers: ColumnHeader[] = [];
  const header = (kind: 'in' | 'out', ids: string[], top: number) => {
    if (ids.length === 0) {
      return;
    }
    const x = Math.min(...ids.map((id) => (boxes.get(id) as NodeBox).x - (sizes.get(id) as CardSize).w / 2));
    const text = kind === 'in' ? copy.usedBy(ids.length) : copy.uses(ids.length);
    const y = top - 12;
    headers.push({ kind, x, y, text });
    const w = 18 + textWidth(text, f.header) + text.length * 0.44;
    bbox = unionRect(bbox, x, top - 26, w, 26);
  };
  header('in', left, leftTop);
  header('out', right, rightTop);

  let note: Layout['note'];
  if (nodes.length === 1) {
    const text = copy.isolated(center);
    const w = textWidth(text, f.note);
    note = { x: 0, y: cs.h / 2 + 28, text };
    bbox = unionRect(bbox, -w / 2, cs.h / 2 + 12, w, 22);
  }

  return {
    engine: 'star',
    nodes,
    edges,
    clusters: [],
    headers,
    note,
    bbox: bbox ?? { x: 0, y: 0, w: 0, h: 0 },
    layoutMs: performance.now() - t0,
  };
}
