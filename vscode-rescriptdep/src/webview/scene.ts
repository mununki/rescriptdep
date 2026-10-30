// Own SVG renderer (R-B): one <g> per node (card + text, ring and tooltip added lazily), one <g><path> per edge.
// Arrowheads are markers (one per edge class). Two dimming overlays, a mask of "holes" and a raised-edge layer
// implement the C13 spotlight used by search and hover tracing (see highlight.ts).
import { ContextKeys, type GraphView } from '../protocol';
import { CARD } from './measure';
import type { PreparedGraph } from './model';
import { r1, svgEl } from './dom';
import { copy } from './text';
import type { Layout, NodeBox, Rect } from './layout/types';

export interface Scene {
  view: GraphView;
  center?: string;
  layout: Layout;
  graph: PreparedGraph;
  svg: SVGSVGElement;
  nodeEls: Map<string, SVGGElement>;
  boxes: Map<string, NodeBox>;
  edgeEls: SVGGElement[];
  edgeEnds: [string, string][];
  inEdges: Map<string, number[]>;
  outEdges: Map<string, number[]>;
  dimEdges: SVGRectElement;
  dimNodes: SVGRectElement;
  /** Black rects in the top overlay's luminance mask: highlighted nodes show through undimmed. */
  holes: SVGGElement;
  /** Highlighted edge groups are moved here (above the top overlay) and moved back afterwards. */
  edgesLayer: SVGGElement;
  raisedEdges: SVGGElement;
  bbox: Rect;
  /** Hover/focus tracing applies to views that draw edges. */
  traceable: boolean;
}

const MARKERS: [string, string][] = [
  ['bd-ah', 'marker-default'],
  ['bd-ah-in', 'marker-in'],
  ['bd-ah-out', 'marker-out'],
];

/** Overlay rectangles are made much larger than any reachable viewport. */
const OVERLAY_MARGIN = 1e6;

export function buildScene(g: PreparedGraph, layout: Layout): Scene {
  const center = g.center;
  const svg = svgEl('svg', {
    class: `graph-svg view-${g.view}`,
    role: 'group',
    'aria-label':
      g.view === 'focus' && center
        ? copy.svgLabelFocus(center)
        : g.view === 'unused'
          ? copy.svgLabelUnused(g.nodes.length)
          : copy.svgLabelFull(g.nodes.length),
  });
  const defs = svgEl('defs', undefined, svg);
  for (const [id, cls] of MARKERS) {
    const m = svgEl(
      'marker',
      {
        id,
        class: cls,
        viewBox: '-3.5 -3.2 7 6.4',
        refX: 0,
        refY: 0,
        markerWidth: 7,
        markerHeight: 6.4,
        markerUnits: 'userSpaceOnUse',
        orient: 'auto',
        overflow: 'visible',
      },
      defs,
    );
    svgEl('path', { d: 'M3.5,0L-3.5,-3.2L-3.5,3.2Z' }, m);
  }

  const clustersG = svgEl('g', { class: 'clusters' }, svg);
  for (const c of layout.clusters) {
    const cg = svgEl('g', { class: 'cluster' }, clustersG);
    svgEl('rect', { x: r1(c.x), y: r1(c.y), width: r1(c.w), height: r1(c.h), rx: 8 }, cg);
    svgEl('text', { x: r1(c.lx), y: r1(c.ly) }, cg).textContent = copy.cycleLabel;
  }

  const bbox = layout.bbox;
  const area = {
    x: bbox.x - OVERLAY_MARGIN,
    y: bbox.y - OVERLAY_MARGIN,
    width: bbox.w + 2 * OVERLAY_MARGIN,
    height: bbox.h + 2 * OVERLAY_MARGIN,
  };
  const overlay = (cls: string) => svgEl('rect', { class: `dim ${cls}`, ...area, display: 'none' });
  const mask = svgEl('mask', { id: 'bd-spot', maskUnits: 'userSpaceOnUse', ...area }, defs);
  svgEl('rect', { ...area, fill: 'white' }, mask);
  const holes = svgEl('g', { fill: 'black' }, mask);

  const edgesG = svgEl('g', { class: 'edges' }, svg);
  const edgeEls: SVGGElement[] = [];
  const edgeEnds: [string, string][] = [];
  const inEdges = new Map<string, number[]>();
  const outEdges = new Map<string, number[]>();
  const push = (m: Map<string, number[]>, k: string, i: number) => {
    const list = m.get(k);
    if (list) {
      list.push(i);
    } else {
      m.set(k, [i]);
    }
  };
  layout.edges.forEach((e, i) => {
    const cls = e.kind === 'dependent' ? 'edge dependent-edge' : e.kind === 'dependency' ? 'edge dependency-edge' : 'edge';
    const eg = svgEl('g', { class: cls, id: `e${i}`, 'data-source': e.from, 'data-target': e.to }, edgesG);
    svgEl('path', { d: e.d }, eg);
    edgeEls.push(eg);
    edgeEnds.push([e.from, e.to]);
    push(outEdges, e.from, i);
    push(inEdges, e.to, i);
  });

  const dimEdges = overlay('dim-edges');
  svg.appendChild(dimEdges);

  const nodesG = svgEl('g', { class: 'nodes' }, svg);
  const nodeEls = new Map<string, SVGGElement>();
  const boxes = new Map<string, NodeBox>();
  layout.nodes.forEach((b, i) => {
    const n = g.byId.get(b.id);
    if (!n) {
      return;
    }
    const isCenter = b.id === center;
    let cls = 'node';
    if (isCenter) {
      cls += ' center';
    }
    if (n.external) {
      cls += ' external';
    }
    const context = {
      [ContextKeys.section]: ContextKeys.sectionModule,
      [ContextKeys.module]: b.id,
      [ContextKeys.openable]: n.openable,
      [ContextKeys.isCenter]: isCenter,
    };
    const ng = svgEl(
      'g',
      {
        class: cls,
        id: `n${i}`,
        transform: `translate(${r1(b.x)},${r1(b.y)})`,
        tabindex: -1,
        role: 'button',
        'aria-label': copy.nodeLabel(b.id, n.secondary, g.inDegree.get(b.id) ?? 0, g.outDegree.get(b.id) ?? 0),
        'data-module': b.id,
        'data-vscode-context': JSON.stringify(context),
      },
      nodesG,
    );
    const w = b.w;
    const h = b.h;
    svgEl('rect', { class: 'card', x: r1(-w / 2), y: r1(-h / 2), width: w, height: h, rx: CARD.radius }, ng);
    const tx = r1(-w / 2 + CARD.padX);
    svgEl('text', { class: 'name', x: tx, y: n.secondary ? r1(-h / 2 + CARD.nameY2) : CARD.nameY1 }, ng).textContent =
      b.id;
    if (n.secondary) {
      svgEl('text', { class: 'sec', x: tx, y: r1(-h / 2 + CARD.secY2) }, ng).textContent = n.secondary;
    }
    nodeEls.set(b.id, ng);
    boxes.set(b.id, b);
  });

  const decor = svgEl('g', { class: 'decor' }, svg);
  for (const h of layout.headers) {
    svgEl('line', { class: `colhdr-swatch ${h.kind}`, x1: r1(h.x), x2: r1(h.x + 12), y1: r1(h.y - 4), y2: r1(h.y - 4) }, decor);
    svgEl('text', { class: 'colhdr', x: r1(h.x + 18), y: r1(h.y) }, decor).textContent = h.text;
  }
  if (layout.note) {
    svgEl('text', { class: 'note', x: r1(layout.note.x), y: r1(layout.note.y) }, decor).textContent = layout.note.text;
  }

  const dimNodes = overlay('dim-nodes');
  dimNodes.setAttribute('mask', 'url(#bd-spot)');
  svg.appendChild(dimNodes);
  const raisedEdges = svgEl('g', { class: 'raised raised-edges' }, svg);

  return {
    view: g.view,
    center,
    layout,
    graph: g,
    svg,
    nodeEls,
    boxes,
    edgeEls,
    edgeEnds,
    inEdges,
    outEdges,
    dimEdges,
    dimNodes,
    holes,
    edgesLayer: edgesG,
    raisedEdges,
    bbox,
    traceable: layout.edges.length > 0,
  };
}

/** Adds the selection/focus ring lazily (it is a sibling of the card, drawn on top, unfilled). */
export function ensureRing(node: SVGGElement): void {
  if (node.querySelector(':scope > .ring')) {
    return;
  }
  const card = node.querySelector(':scope > .card');
  if (!card) {
    return;
  }
  const x = Number(card.getAttribute('x'));
  const y = Number(card.getAttribute('y'));
  const w = Number(card.getAttribute('width'));
  const h = Number(card.getAttribute('height'));
  svgEl('rect', { class: 'ring', x: r1(x - 3.5), y: r1(y - 3.5), width: r1(w + 7), height: r1(h + 7), rx: 9 }, node);
}

export function dropRing(node: SVGGElement): void {
  node.querySelector(':scope > .ring')?.remove();
}
