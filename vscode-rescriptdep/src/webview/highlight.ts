// Highlight states on the current scene.
//
// Search dimming and hover tracing use the C13 overlay technique instead of per-element opacity: a
// translucent background-coloured rect is drawn over the graph, so dimming costs one layer however many
// elements are dimmed.
// - Highlighted nodes show through "holes" in the top overlay's luminance mask (one black rect per node),
//   so the real node elements are never cloned or moved: they keep their classes, `tabindex`,
//   `data-vscode-context`, focus and hit-testing.
// - Highlighted edges are moved (not cloned) into a layer above the overlay and moved back afterwards;
//   edges carry no focus or context state and their paint order is irrelevant.
// - Tracing also shows a second overlay between the edge and node layers, so non-highlighted edges end
//   at ~.10 and nodes at .28 (HC: .3 / .5). Search uses the top overlay only.
// All updates are incremental (only the difference to the previous state touches the DOM).
import { svgEl } from './dom';
import { dropRing, ensureRing, type Scene } from './scene';

/** Hole margin around a card: covers the 2 px match stroke and the selection ring (3.5 px + 1 px). */
const HOLE_PAD = 5;

export class Highlighter {
  private scene: Scene | null = null;
  private traceId: string | null = null;
  private traced: Element[] = [];
  private matches: Set<string> | null = null;
  private currentMatch: string | null = null;
  private selected: string | null = null;
  private hoverId: string | null = null;
  private focusTraceId: string | null = null;
  private holeEls = new Map<string, SVGRectElement>();
  private raised = new Set<number>();
  private overlays: 'none' | 'top' | 'both' = 'none';

  attach(scene: Scene | null): void {
    this.scene = scene;
    this.traceId = null;
    this.traced = [];
    this.matches = null;
    this.currentMatch = null;
    this.selected = null;
    this.hoverId = null;
    this.focusTraceId = null;
    this.holeEls = new Map();
    this.raised = new Set();
    this.overlays = 'none';
  }

  /** Hover tracing: the pointer is over `id` (null: over nothing). */
  setHover(id: string | null): void {
    if (id === this.hoverId) {
      return;
    }
    this.hoverId = id;
    this.updateTrace();
  }

  get hovered(): string | null {
    return this.hoverId;
  }

  /** Keyboard focus tracing (only for focus that should be visible). */
  setFocusTrace(id: string | null): void {
    if (id === this.focusTraceId) {
      return;
    }
    this.focusTraceId = id;
    this.updateTrace();
  }

  private updateTrace(): void {
    const s = this.scene;
    const next = s && s.traceable ? (this.hoverId ?? this.focusTraceId) : null;
    if (next === this.traceId) {
      return;
    }
    for (const el of this.traced) {
      el.classList.remove('hovered', 'hl-in', 'hl-out');
    }
    this.traced = [];
    this.traceId = next;
    if (s && next) {
      const node = s.nodeEls.get(next);
      if (node) {
        node.classList.add('hovered');
        this.traced.push(node);
        // Colours follow the direction relative to the traced node; in a focus view an edge keeps its
        // centre-relative colour (USED BY blue / USES orange) so hovering a card never recolours its stub.
        const mark = (i: number, relative: 'hl-in' | 'hl-out') => {
          const kind = s.layout.edges[i]?.kind;
          const cls = kind === 'dependent' ? 'hl-in' : kind === 'dependency' ? 'hl-out' : relative;
          s.edgeEls[i].classList.add(cls);
          this.traced.push(s.edgeEls[i]);
        };
        for (const i of s.inEdges.get(next) ?? []) {
          mark(i, 'hl-in');
        }
        for (const i of s.outEdges.get(next) ?? []) {
          mark(i, 'hl-out');
        }
      }
    }
    this.refreshSpotlight();
  }

  /** Search matches (null: no active query). */
  setMatches(matches: Set<string> | null): void {
    const s = this.scene;
    if (!s) {
      this.matches = matches;
      return;
    }
    const prev = this.matches ?? new Set<string>();
    const next = matches ?? new Set<string>();
    for (const id of prev) {
      if (!next.has(id)) {
        s.nodeEls.get(id)?.classList.remove('match');
      }
    }
    for (const id of next) {
      if (!prev.has(id)) {
        s.nodeEls.get(id)?.classList.add('match');
      }
    }
    this.matches = matches;
    if (this.currentMatch && !next.has(this.currentMatch)) {
      this.setCurrentMatch(null);
    }
    this.refreshSpotlight();
  }

  setCurrentMatch(id: string | null): void {
    const s = this.scene;
    const prev = this.currentMatch;
    this.currentMatch = id;
    if (!s) {
      return;
    }
    if (prev && prev !== id) {
      const el = s.nodeEls.get(prev);
      if (el) {
        el.classList.remove('current');
        this.syncRing(el);
      }
    }
    if (id) {
      const el = s.nodeEls.get(id);
      if (el) {
        el.classList.add('current');
        this.syncRing(el);
      }
    }
  }

  /** Context-menu target ring; follows VS Code's `vscode-context-menu-visible` (see main.ts). */
  select(id: string | null): void {
    const s = this.scene;
    const prev = this.selected;
    if (prev === id) {
      return;
    }
    this.selected = id;
    if (!s) {
      return;
    }
    if (prev) {
      const el = s.nodeEls.get(prev);
      if (el) {
        el.classList.remove('selected');
        this.syncRing(el);
      }
    }
    if (id) {
      const el = s.nodeEls.get(id);
      if (el) {
        el.classList.add('selected');
        this.syncRing(el);
      }
    }
    if (this.matches && !this.traceId) {
      this.refreshSpotlight();
    }
  }

  /** Creates or drops the lazy ring element of a node based on its states. */
  syncRing(el: SVGGElement): void {
    const needs =
      el.classList.contains('selected') || el.classList.contains('current') || el === document.activeElement;
    if (needs) {
      ensureRing(el);
    } else {
      dropRing(el);
    }
  }

  private refreshSpotlight(): void {
    const s = this.scene;
    if (!s) {
      return;
    }
    let nodes: Set<string> | null = null;
    let edges: number[] = [];
    let overlays: 'none' | 'top' | 'both' = 'none';
    if (this.traceId) {
      const id = this.traceId;
      edges = [...(s.inEdges.get(id) ?? []), ...(s.outEdges.get(id) ?? [])];
      nodes = new Set([id]);
      for (const i of edges) {
        nodes.add(s.edgeEnds[i][0]);
        nodes.add(s.edgeEnds[i][1]);
      }
      overlays = 'both';
    } else if (this.matches && this.matches.size < s.nodeEls.size) {
      nodes = this.matches;
      if (this.selected && !nodes.has(this.selected)) {
        nodes = new Set(nodes).add(this.selected); // keep the context-menu target readable
      }
      overlays = 'top';
    }

    // Holes for highlighted nodes (diff against the current set).
    const want = nodes ?? new Set<string>();
    for (const [id, hole] of this.holeEls) {
      if (!want.has(id)) {
        hole.remove();
        this.holeEls.delete(id);
      }
    }
    for (const id of want) {
      if (!this.holeEls.has(id)) {
        const b = s.boxes.get(id);
        if (b) {
          const hole = svgEl('rect', {
            x: b.x - b.w / 2 - HOLE_PAD,
            y: b.y - b.h / 2 - HOLE_PAD,
            width: b.w + 2 * HOLE_PAD,
            height: b.h + 2 * HOLE_PAD,
            rx: 9,
            'data-module': id,
          });
          s.holes.appendChild(hole);
          this.holeEls.set(id, hole);
        }
      }
    }

    // Highlighted edges above the overlays (diff against the current set).
    const wantEdges = new Set(edges);
    for (const i of this.raised) {
      if (!wantEdges.has(i)) {
        s.edgesLayer.appendChild(s.edgeEls[i]);
        this.raised.delete(i);
      }
    }
    for (const i of wantEdges) {
      if (!this.raised.has(i)) {
        s.raisedEdges.appendChild(s.edgeEls[i]);
        this.raised.add(i);
      }
    }

    if (overlays !== this.overlays) {
      const show = (el: SVGRectElement, on: boolean) => {
        if (on) {
          el.removeAttribute('display');
        } else {
          el.setAttribute('display', 'none');
        }
      };
      show(s.dimNodes, overlays !== 'none');
      show(s.dimEdges, overlays === 'both');
      s.svg.classList.toggle('spotlight', overlays !== 'none');
      this.overlays = overlays;
    }
  }
}
