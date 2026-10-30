// Graph webview entry point (bundled to dist/webview.js). Registers every DOM listener once at load,
// then renders whatever the host sends. The webview never sees file paths.
import { ContextKeys, type HostMessage, type RenderErrorKind } from '../protocol';
import { byId, isMac, yieldFrames } from './dom';
import { Highlighter } from './highlight';
import { gridLayout } from './layout/grid';
import { graphvizLayout, LayoutError } from './layout/graphviz';
import { starLayout } from './layout/star';
import type { Layout } from './layout/types';
import { measureCards } from './measure';
import { GraphDataError, prepareGraph, type PreparedGraph } from './model';
import { buildScene, type Scene } from './scene';
import { Search } from './search';
import { States, type StateAction } from './states';
import { copy } from './text';
import { Viewport } from './viewport';
import { post } from './vscode';

type RenderMessage = Extract<HostMessage, { type: 'render' }>;

const LOADING_DELAY_MS = 300;
const NAV_GUARD_MS = 400;
const DRAG_THRESHOLD = 4;
const ZOOM_STEP = 1.25;
const DBLCLICK_ZOOM = 2;
const WHEEL_ZOOM_CLAMP = 25;
const WHEEL_ZOOM_RATE = 0.01;
const CONTEXT_MENU_CLASS = 'vscode-context-menu-visible';

const container = byId<HTMLElement>('graph-container');
const graphHost = byId<HTMLElement>('graph');
const titleEl = byId<HTMLElement>('hdr-title');
const metaEl = byId<HTMLElement>('hdr-meta');
const findInput = byId<HTMLInputElement>('find');
const findCount = byId<HTMLElement>('find-count');
const backButton = byId<HTMLButtonElement>('btn-back');
const statusEl = byId<HTMLElement>('status');

const viewport = new Viewport(container);
const hl = new Highlighter();
const states = new States(graphHost);
const search = new Search(findInput, findCount, hl, {
  reveal: (id) => revealNode(id),
  focusNode: (id) => focusNode(id),
  announce: (text) => announce(text),
});

let scene: Scene | null = null;
let current: RenderMessage | null = null;
/** Highest renderKey seen in any host message; older render/loading/error messages are stale. */
let latestKey = -Infinity;
let renderJob = 0;
let loadingTimer = 0;
let loadingKey: number | null = null;
let loadingText = '';
let navGuardUntil = 0;
/** After a webview-initiated navigation, the next focus view moves keyboard focus to its centre. */
let focusCenterUntil = 0;

// ---------------------------------------------------------------------------------------------------
// Host messages

window.addEventListener('message', (event: MessageEvent) => {
  const msg = event.data as HostMessage | undefined;
  if (!msg || typeof msg !== 'object' || typeof (msg as { type?: unknown }).type !== 'string') {
    return;
  }
  switch (msg.type) {
    case 'render':
      onRender(msg);
      break;
    case 'loading':
      onLoading(msg.renderKey, msg.text);
      break;
    case 'error':
      onHostError(msg.renderKey, msg.title, msg.reason, msg.canRetry, msg.canGoBack);
      break;
    case 'cancelled':
      onCancelled(msg.renderKey);
      break;
    case 'fitToView':
      viewport.fit(true);
      break;
  }
});

function isStale(key: number): boolean {
  if (typeof key !== 'number' || key < latestKey) {
    return true;
  }
  latestKey = key;
  return false;
}

function clearLoadingTimer(): void {
  clearTimeout(loadingTimer);
  loadingTimer = 0;
}

function onLoading(key: number, text: string): void {
  if (isStale(key)) {
    return;
  }
  clearLoadingTimer();
  loadingKey = key;
  loadingText = String(text ?? '');
  loadingTimer = window.setTimeout(() => {
    loadingTimer = 0;
    if (loadingKey === key) {
      states.showLoading(String(text ?? ''), true);
    }
  }, LOADING_DELAY_MS);
}

function onCancelled(key: number): void {
  if (key !== loadingKey) {
    return;
  }
  // Restore the previous graph silently.
  clearLoadingTimer();
  loadingKey = null;
  states.hideLoading();
  focusCenterUntil = 0;
}

function onHostError(key: number, title: string, reason: string, canRetry: boolean, canGoBack?: boolean): void {
  if (isStale(key)) {
    return;
  }
  renderJob++; // abandon any render in flight
  clearLoadingTimer();
  loadingKey = null;
  states.hideLoading();
  unmountScene();
  if (current && canGoBack !== undefined) {
    current = { ...current, canGoBack };
    backButton.disabled = !canGoBack;
    setBodyContext(current);
  }
  metaEl.textContent = ''; // the counts described the graph that is no longer shown
  states.showPanel('error', String(title ?? copy.renderErrorTitle), String(reason ?? ''), States.errorActions(!!canRetry));
  announce(String(title ?? copy.renderErrorTitle));
}

function onRender(msg: RenderMessage): void {
  if (isStale(msg.renderKey)) {
    return;
  }
  clearLoadingTimer();
  loadingKey = null;
  const job = ++renderJob;
  void runRender(msg, job);
}

const fontsReady: Promise<unknown> = document.fonts ? document.fonts.ready.catch(() => undefined) : Promise.resolve();

async function runRender(msg: RenderMessage, job: number): Promise<void> {
  try {
    await fontsReady;
    if (job !== renderJob) {
      return;
    }
    const g = prepareGraph(msg.model, msg.view, msg.center);
    current = msg;
    setBodyContext(msg);
    backButton.disabled = !msg.canGoBack;

    if (g.nodes.length > 0) {
      const { title, meta } = describeGraph(g, msg);
      setHeader(title, meta);
    }
    if (g.nodes.length === 0) {
      states.hideLoading();
      unmountScene();
      showEmptyState(msg);
      post({ type: 'rendered', renderKey: msg.renderKey, layoutMs: 0, nodeCount: 0 });
      return;
    }

    const sizes = measureCards(g);
    let layout: Layout | null = null;
    if (g.view === 'focus' && g.center) {
      layout = starLayout(g, sizes, g.center); // null when the graph isn't a star: fall back to Graphviz
    } else if (g.view === 'unused') {
      layout = gridLayout(g, sizes, { w: container.clientWidth, h: container.clientHeight });
    }
    if (!layout) {
      if (msg.layout?.slow) {
        // Graphviz runs synchronously on this thread: paint the overlay first, then lay out.
        states.showLoading(copy.layingOut(g.nodes.length), false);
        await yieldFrames(2);
        if (job !== renderJob) {
          return;
        }
      }
      layout = await graphvizLayout(g, sizes, msg.layout ?? { tune: false, slow: false });
      if (job !== renderJob) {
        return;
      }
    }

    mountScene(g, layout, msg);
    post({ type: 'rendered', renderKey: msg.renderKey, layoutMs: Math.round(layout.layoutMs), nodeCount: g.nodes.length });
  } catch (e) {
    if (job !== renderJob) {
      return;
    }
    const { kind, reason, message } = describeError(e);
    states.hideLoading();
    unmountScene();
    states.showPanel('error', copy.renderErrorTitle, reason, States.errorActions(kind !== 'tooLarge'));
    announce(copy.renderErrorTitle);
    post({ type: 'renderError', renderKey: msg.renderKey, kind, message });
  }
}

function describeError(e: unknown): { kind: RenderErrorKind; reason: string; message: string } {
  const message = e instanceof Error ? e.message || e.name : String(e);
  if (e instanceof LayoutError) {
    if (e.kind === 'tooLarge') {
      return { kind: 'tooLarge', reason: copy.tooLarge, message };
    }
    if (e.kind === 'layout') {
      return { kind: 'layout', reason: copy.unreadable, message };
    }
    return { kind: 'unknown', reason: copy.unknownError(message), message };
  }
  if (e instanceof RangeError) {
    return { kind: 'tooLarge', reason: copy.tooLarge, message };
  }
  if (e instanceof GraphDataError) {
    return { kind: 'unknown', reason: copy.unreadable, message };
  }
  return { kind: 'unknown', reason: copy.unknownError(message), message };
}

function setBodyContext(msg: RenderMessage): void {
  document.body.setAttribute(
    'data-vscode-context',
    JSON.stringify({
      [ContextKeys.panelId]: msg.panelId,
      [ContextKeys.mode]: msg.view,
      [ContextKeys.canGoBack]: msg.canGoBack,
    }),
  );
}

function setHeader(title: string, meta: string): void {
  titleEl.textContent = title;
  titleEl.title = title;
  metaEl.textContent = meta;
}

function showEmptyState(msg: RenderMessage): void {
  if (msg.view === 'focus') {
    const name = msg.center ?? '';
    setHeader(name, '');
    states.showPanel('empty', copy.focusNotFoundTitle, copy.focusNotFound(name), [
      { action: 'pickModule', label: copy.pickModule, primary: true },
    ]);
    announce(copy.focusNotFound(name));
  } else if (msg.view === 'unused') {
    setHeader(copy.unusedTitle, copy.unusedMeta(0));
    states.showPanel('empty', copy.unusedEmptyTitle, copy.unusedEmpty, []);
    announce(copy.unusedEmpty);
  } else {
    setHeader(copy.fullTitle, copy.fullMeta(0, 0));
    states.showPanel('empty', copy.fullEmptyTitle, copy.fullEmpty, [
      { action: 'retry', label: copy.retry, primary: true },
    ]);
    announce(copy.fullEmpty);
  }
}

function mountScene(g: PreparedGraph, layout: Layout, msg: RenderMessage): void {
  tooltip.remove();
  const next = buildScene(g, layout);
  scene = next;
  hl.attach(next);
  graphHost.replaceChildren(next.svg);
  states.hidePanel();
  if (loadingKey === null || loadingKey <= msg.renderKey) {
    states.hideLoading();
  } else {
    states.showLoading(loadingText, true);
  }
  search.attach(next);

  const centerBox = g.center ? next.boxes.get(g.center) : undefined;
  viewport.setScene(next.svg, layout.bbox, g.view === 'focus' && centerBox ? { x: centerBox.x, y: centerBox.y } : null);
  announce(describeGraph(g, msg).announcement);
  if (g.center && performance.now() < focusCenterUntil && document.hasFocus()) {
    focusNode(g.center);
  }
  focusCenterUntil = 0;
}

/** Header title/meta and the screen-reader announcement for a non-empty graph. */
function describeGraph(g: PreparedGraph, msg: RenderMessage): { title: string; meta: string; announcement: string } {
  if (g.view === 'focus' && g.center) {
    let dependents = 0;
    let dependencies = 0;
    for (const [from, to] of g.edges) {
      if (to === g.center && from !== g.center) {
        dependents++;
      }
      if (from === g.center && to !== g.center) {
        dependencies++;
      }
    }
    return {
      title: g.center,
      meta: copy.focusMeta(dependents, dependencies),
      announcement: copy.announceFocus(g.center, dependents, dependencies),
    };
  }
  if (g.view === 'unused') {
    return {
      title: copy.unusedTitle,
      meta: copy.unusedMeta(g.nodes.length),
      announcement: copy.announceUnused(g.nodes.length),
    };
  }
  return {
    title: g.view === 'focus' ? (msg.center ?? copy.fullTitle) : copy.fullTitle,
    meta: copy.fullMeta(g.nodes.length, g.edges.length),
    announcement: copy.announceFull(g.nodes.length, g.edges.length),
  };
}

function unmountScene(): void {
  tooltip.remove();
  scene = null;
  hl.attach(null);
  search.attach(null);
  viewport.clear();
  graphHost.replaceChildren();
}

function announce(text: string): void {
  // Re-setting identical text is not announced; clear first.
  statusEl.textContent = '';
  statusEl.textContent = text;
}

// ---------------------------------------------------------------------------------------------------
// Navigation

function activateNode(id: string, openInstead: boolean): void {
  const now = performance.now();
  if (now < navGuardUntil || !scene) {
    return;
  }
  navGuardUntil = now + NAV_GUARD_MS;
  if (openInstead || (scene.view === 'focus' && id === scene.center)) {
    post({ type: 'openModule', module: id });
  } else {
    focusCenterUntil = now + 15_000;
    post({ type: 'focusModule', module: id });
  }
}

function goBack(): void {
  if (!current?.canGoBack) {
    return;
  }
  focusCenterUntil = performance.now() + 15_000;
  post({ type: 'back' });
}

function focusNode(id: string): void {
  const el = scene?.nodeEls.get(id);
  if (el) {
    el.focus({ preventScroll: true });
  }
}

function nearestToViewCentre(s: Scene): string | null {
  const c = viewport.worldCenter();
  let best: string | null = null;
  let bestD = Infinity;
  for (const [id, b] of s.boxes) {
    const d = (b.x - c.x) ** 2 + (b.y - c.y) ** 2;
    if (d < bestD) {
      bestD = d;
      best = id;
    }
  }
  return best;
}

function revealNode(id: string): void {
  const b = scene?.boxes.get(id);
  if (b) {
    viewport.centerOn(b.x, b.y, Math.max(viewport.scale, 1), true);
  }
}

function nodeIdOf(target: EventTarget | null): string | null {
  if (!(target instanceof Element)) {
    return null;
  }
  const g = target.closest('g.node');
  return g instanceof SVGGElement && graphHost.contains(g) ? g.getAttribute('data-module') : null;
}

function inPanel(target: EventTarget | null): boolean {
  return target instanceof Element && !!target.closest('.state, .loading-box');
}

// ---------------------------------------------------------------------------------------------------
// Pointer: drag to pan (4 px threshold swallows the click), click = focus, centre/Cmd/Ctrl+click = open

let drag: { id: number; x0: number; y0: number; x: number; y: number; moving: boolean } | null = null;
let dragEndedAt = -Infinity;

container.addEventListener('pointerdown', (e) => {
  hl.select(null);
  if ((e.button !== 0 && e.button !== 1) || inPanel(e.target)) {
    return;
  }
  if (e.button === 1) {
    e.preventDefault(); // no autoscroll
  }
  drag = { id: e.pointerId, x0: e.clientX, y0: e.clientY, x: e.clientX, y: e.clientY, moving: false };
});

container.addEventListener('pointermove', (e) => {
  if (!drag || e.pointerId !== drag.id) {
    return;
  }
  if ((e.buttons & 0b101) === 0) {
    endDrag(e);
    return;
  }
  if (!drag.moving) {
    if (Math.hypot(e.clientX - drag.x0, e.clientY - drag.y0) <= DRAG_THRESHOLD) {
      return;
    }
    drag.moving = true;
    container.setPointerCapture(e.pointerId);
    container.classList.add('dragging');
    hl.setHover(null);
  }
  viewport.panBy(e.clientX - drag.x, e.clientY - drag.y);
  drag.x = e.clientX;
  drag.y = e.clientY;
});

function endDrag(e: PointerEvent): void {
  if (!drag || e.pointerId !== drag.id) {
    return;
  }
  const moving = drag.moving;
  drag = null;
  if (moving) {
    dragEndedAt = performance.now();
    container.classList.remove('dragging');
    if (container.hasPointerCapture(e.pointerId)) {
      container.releasePointerCapture(e.pointerId);
    }
  }
  drag = null;
}
window.addEventListener('pointerup', endDrag);
window.addEventListener('pointercancel', endDrag);
container.addEventListener('lostpointercapture', endDrag);

container.addEventListener('click', (e) => {
  if (performance.now() - dragEndedAt < 100) {
    return; // the click that ends a drag
  }
  const id = nodeIdOf(e.target);
  if (id) {
    activateNode(id, e.metaKey || e.ctrlKey);
  }
});

container.addEventListener('dblclick', (e) => {
  if (nodeIdOf(e.target) || inPanel(e.target) || !scene) {
    return; // double-click on a node is ignored (the first click already navigated)
  }
  const r = container.getBoundingClientRect();
  viewport.zoomAt(DBLCLICK_ZOOM, e.clientX - r.left, e.clientY - r.top);
});

// Plain wheel / two-finger scroll pans (Shift: horizontal); Ctrl/Cmd+wheel and pinch zoom at the cursor.
container.addEventListener(
  'wheel',
  (e) => {
    if (inPanel(e.target)) {
      return;
    }
    e.preventDefault();
    if (!scene) {
      return;
    }
    const unit = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? container.clientHeight : 1;
    let dx = e.deltaX * unit;
    let dy = e.deltaY * unit;
    if (e.ctrlKey || e.metaKey) {
      const d = Math.max(-WHEEL_ZOOM_CLAMP, Math.min(WHEEL_ZOOM_CLAMP, dy));
      const r = container.getBoundingClientRect();
      viewport.zoomAt(Math.exp(-d * WHEEL_ZOOM_RATE), e.clientX - r.left, e.clientY - r.top);
      return;
    }
    if (e.shiftKey && dx === 0) {
      dx = dy;
      dy = 0;
    }
    viewport.panBy(-dx, -dy);
  },
  { passive: false },
);

// Hover tracing (full and focus views) and a lazily attached native tooltip.
const tooltip = document.createElementNS('http://www.w3.org/2000/svg', 'title');
container.addEventListener('pointerover', (e) => {
  if (drag?.moving || e.pointerType === 'touch') {
    return;
  }
  const id = nodeIdOf(e.target);
  if (id === hl.hovered) {
    return;
  }
  // Attach the tooltip before tracing so the raised clone of this node is built once.
  if (id && scene) {
    const n = scene.graph.byId.get(id);
    tooltip.textContent = n?.secondary ? `${id}\n${n.secondary}` : id;
    scene.nodeEls.get(id)?.prepend(tooltip);
  } else {
    tooltip.remove();
  }
  hl.setHover(id);
});
container.addEventListener('pointerleave', () => hl.setHover(null));

// Context menu: VS Code's preload shows the native menu from data-vscode-context. Never preventDefault.
container.addEventListener('contextmenu', (e) => {
  hl.select(nodeIdOf(e.target));
});

// The selection ring follows the native menu: it clears when VS Code removes its body class.
let contextMenuVisible = document.body.classList.contains(CONTEXT_MENU_CLASS);
new MutationObserver(() => {
  const visible = document.body.classList.contains(CONTEXT_MENU_CLASS);
  if (contextMenuVisible && !visible) {
    hl.select(null);
  }
  contextMenuVisible = visible;
}).observe(document.body, { attributes: true, attributeFilter: ['class'] });

// Keyboard focus on nodes: ring + tracing for keyboard focus only.
container.addEventListener('focusin', (e) => {
  const t = e.target;
  if (t instanceof SVGGElement && t.classList.contains('node')) {
    hl.syncRing(t);
    hl.setFocusTrace(t.matches(':focus-visible') ? t.getAttribute('data-module') : null);
  }
});
container.addEventListener('focusout', (e) => {
  const t = e.target;
  if (t instanceof SVGGElement && t.classList.contains('node')) {
    // activeElement still points at the old node during focusout; recheck after the move.
    setTimeout(() => hl.syncRing(t), 0);
    hl.setFocusTrace(null);
  }
});

// Focusing an SVG child must never scroll the overflow-hidden canvas.
container.addEventListener('scroll', () => {
  container.scrollTop = 0;
  container.scrollLeft = 0;
});

// ---------------------------------------------------------------------------------------------------
// Toolbar, panel actions, keyboard

backButton.addEventListener('click', goBack);
byId<HTMLButtonElement>('btn-zoom-out').addEventListener('click', () => viewport.zoomBy(1 / ZOOM_STEP));
byId<HTMLButtonElement>('btn-zoom-in').addEventListener('click', () => viewport.zoomBy(ZOOM_STEP));
byId<HTMLButtonElement>('btn-fit').addEventListener('click', () => viewport.fit(true));
findInput.placeholder = copy.findPlaceholder(isMac);

container.addEventListener('click', (e) => {
  const target = e.target instanceof Element ? e.target.closest<HTMLElement>('[data-action]') : null;
  if (!target) {
    return;
  }
  const action = target.dataset.action as StateAction;
  if (action === 'retry') {
    post({ type: 'retry' });
  } else if (action === 'pickModule') {
    post({ type: 'pickModule' });
  } else if (action === 'cancel') {
    post({ type: 'cancel' });
  }
});

document.addEventListener('keydown', (e) => {
  const inFind = e.target === findInput;
  const isMenuKey = (e.key === 'F10' && e.shiftKey) || e.key === 'ContextMenu';
  if (!isMenuKey && !['Shift', 'Control', 'Alt', 'Meta'].includes(e.key)) {
    hl.select(null);
  }
  const mod = e.metaKey || e.ctrlKey;

  if (mod && !e.altKey && !e.shiftKey && e.key.toLowerCase() === 'f') {
    e.preventDefault();
    search.focusInput();
    return;
  }
  if (inFind) {
    return; // Enter / Esc are handled by the find box
  }
  if (e.key === 'ArrowLeft' && e.altKey && !mod) {
    e.preventDefault();
    goBack();
    return;
  }
  if (isMenuKey) {
    if (isMac) {
      openContextMenuFromKeyboard(e);
    }
    return; // other platforms fire contextmenu natively at the focused element
  }
  const nodeId = nodeIdOf(e.target);
  if (e.key === 'Enter' && e.target === container && scene && !mod) {
    // Keyboard entry into the graph (arrow-key navigation is deferred): the centre, else the node nearest
    // the middle of the view. From there Enter focuses, Cmd/Ctrl+Enter opens, Shift+F10 opens the menu.
    e.preventDefault();
    const id = scene.center ?? nearestToViewCentre(scene);
    if (id) {
      focusNode(id);
    }
    return;
  }
  if ((e.key === 'Enter' || (e.key === ' ' && !mod && !e.altKey)) && nodeId) {
    e.preventDefault();
    activateNode(nodeId, mod);
    return;
  }
  if (e.key === 'Escape') {
    if (search.active) {
      search.clear();
    }
    return;
  }
  if (mod || e.altKey) {
    return; // leave Cmd/Ctrl/Alt chords to VS Code
  }
  if (!(e.target instanceof Element) || !(container.contains(e.target) || e.target === document.body || e.target.closest('.hdr'))) {
    return;
  }
  if (e.key === '+' || e.key === '=') {
    e.preventDefault();
    viewport.zoomBy(ZOOM_STEP);
  } else if (e.key === '-' || e.key === '_') {
    e.preventDefault();
    viewport.zoomBy(1 / ZOOM_STEP);
  } else if (e.key === '0') {
    e.preventDefault();
    viewport.fit(true);
  }
});

/** macOS has no keyboard contextmenu event: dispatch one at the focused node (or canvas) centre. */
function openContextMenuFromKeyboard(e: KeyboardEvent): void {
  const target = document.activeElement;
  let rect: DOMRect;
  if (target instanceof SVGGElement && target.classList.contains('node')) {
    rect = (target.querySelector('.card') ?? target).getBoundingClientRect();
  } else if (target === container) {
    rect = container.getBoundingClientRect();
  } else {
    return;
  }
  e.preventDefault();
  target.dispatchEvent(
    new MouseEvent('contextmenu', {
      bubbles: true,
      cancelable: true,
      view: window,
      clientX: rect.left + rect.width / 2,
      clientY: rect.top + rect.height / 2,
    }),
  );
}

post({ type: 'ready' });
