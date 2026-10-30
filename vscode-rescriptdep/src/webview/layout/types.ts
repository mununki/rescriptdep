// Engine-neutral layout result consumed by the SVG renderer (one shape for Graphviz, star and grid).

export interface NodeBox {
  id: string;
  /** Card centre, in layout px (1 Graphviz point = 1 px at scale 1). */
  x: number;
  y: number;
  w: number;
  h: number;
}

export type EdgeKind = 'dependent' | 'dependency';

export interface EdgeGeom {
  from: string;
  to: string;
  /** SVG path data. The path ends 3.5 px short of the arrow tip; the marker draws the 7 px arrowhead. */
  d: string;
  /** Focus views: edges into the centre are `dependent`, edges out of it are `dependency`. */
  kind?: EdgeKind;
}

export interface ClusterBox {
  x: number;
  y: number;
  w: number;
  h: number;
  /** Label anchor (text centre baseline). */
  lx: number;
  ly: number;
}

export interface ColumnHeader {
  kind: 'in' | 'out';
  /** Left edge of the swatch and the baseline of the text. */
  x: number;
  y: number;
  text: string;
}

export interface Rect {
  x: number;
  y: number;
  w: number;
  h: number;
}

export interface Layout {
  engine: 'graphviz' | 'star' | 'grid';
  nodes: NodeBox[];
  edges: EdgeGeom[];
  clusters: ClusterBox[];
  headers: ColumnHeader[];
  /** A note under the single card of an isolated module. */
  note?: { x: number; y: number; text: string };
  bbox: Rect;
  layoutMs: number;
}

/** Arrowhead length and how far the path stops short of the tip (the marker covers the rest). */
export const ARROW_LENGTH = 7;
export const ARROW_INSET = 3.5;

export function unionRect(a: Rect | null, x: number, y: number, w: number, h: number): Rect {
  if (!a) {
    return { x, y, w, h };
  }
  const x0 = Math.min(a.x, x);
  const y0 = Math.min(a.y, y);
  const x1 = Math.max(a.x + a.w, x + w);
  const y1 = Math.max(a.y + a.h, y + h);
  return { x: x0, y: y0, w: x1 - x0, h: y1 - y0 };
}
