// Unused view: engine-less card grid, sorted A→Z (code-point), filled column-major (designer-review3 R1).
import type { CardSize } from '../measure';
import type { PreparedGraph } from '../model';
import { type Layout, type NodeBox, type Rect, unionRect } from './types';

const COLUMN_GAP = 16;
const ROW_GAP = 10;

export function gridLayout(g: PreparedGraph, sizes: Map<string, CardSize>, viewport = { w: 1200, h: 800 }): Layout {
  const t0 = performance.now();
  const ids = g.nodes.map((n) => n.id); // already in code-point order
  const n = ids.length;
  const slotH = Math.max(...ids.map((id) => (sizes.get(id) as CardSize).h));
  const meanW = ids.reduce((sum, id) => sum + (sizes.get(id) as CardSize).w, 0) / Math.max(1, n);
  const rows = Math.max(1, Math.min(n, Math.max(Math.min(n, 6), Math.round(Math.sqrt(
    n * (meanW + COLUMN_GAP) * Math.max(1, viewport.h - 48) /
    ((slotH + ROW_GAP) * Math.max(1, viewport.w - 48)),
  )))));
  const nodes: NodeBox[] = [];
  let bbox: Rect | null = null;
  let x = 0;
  for (let start = 0; start < n; start += rows) {
    const col = ids.slice(start, start + rows);
    const colW = Math.max(...col.map((id) => (sizes.get(id) as CardSize).w));
    col.forEach((id, row) => {
      const { w, h } = sizes.get(id) as CardSize;
      const cy = row * (slotH + ROW_GAP) + slotH / 2;
      nodes.push({ id, x: x + w / 2, y: cy, w, h });
      bbox = unionRect(bbox, x, cy - h / 2, w, h);
    });
    x += colW + COLUMN_GAP;
  }
  return {
    engine: 'grid',
    nodes,
    edges: [],
    clusters: [],
    headers: [],
    bbox: bbox ?? { x: 0, y: 0, w: 0, h: 0 },
    layoutMs: performance.now() - t0,
  };
}
