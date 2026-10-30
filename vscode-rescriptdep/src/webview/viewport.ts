// Pan/zoom through the SVG viewBox (screen = world · scale + t, in container px), the D6 fit rule,
// 200 ms ease-out moves, and the dot grid that follows the viewport.
import { prefersReducedMotion } from './dom';
import type { Rect } from './layout/types';

const FIT_PADDING = 24;
const MAX_SCALE = 4;
const FOCUS_MIN_FIT = 0.6;
const MOVE_MS = 200;
const GRID = 18;

interface View {
  scale: number;
  tx: number;
  ty: number;
}

export class Viewport {
  scale = 1;
  tx = 0;
  ty = 0;
  private width = 0;
  private height = 0;
  private svg: SVGSVGElement | null = null;
  private bbox: Rect = { x: 0, y: 0, w: 0, h: 0 };
  private focusPoint: { x: number; y: number } | null = null;
  private needsInitial = false;
  /** What a resize re-applies: the initial view, a fit, or nothing (the user moved; keep the centre). */
  private mode: 'initial' | 'fit' | 'user' = 'initial';
  private frame = 0;
  private anim = 0;

  constructor(private readonly container: HTMLElement) {
    new ResizeObserver(() => this.onResize()).observe(container);
  }

  get size(): { width: number; height: number } {
    return { width: this.width, height: this.height };
  }

  /** Attaches a freshly rendered scene and shows its initial view (fit, or the focus-view rule). */
  setScene(svg: SVGSVGElement, bbox: Rect, focusPoint: { x: number; y: number } | null): void {
    this.cancelAnimation();
    this.svg = svg;
    this.bbox = bbox;
    this.focusPoint = focusPoint;
    this.mode = 'initial';
    this.measure();
    if (this.width === 0 || this.height === 0) {
      this.needsInitial = true; // hidden panel: apply when it gets a size
      return;
    }
    this.showInitial();
  }

  clear(): void {
    this.cancelAnimation();
    this.svg = null;
  }

  private measure(): void {
    this.width = this.container.clientWidth;
    this.height = this.container.clientHeight;
  }

  private fitScaleUncapped(): number {
    const { w, h } = this.bbox;
    const sx = w > 0 ? (this.width - 2 * FIT_PADDING) / w : Infinity;
    const sy = h > 0 ? (this.height - 2 * FIT_PADDING) / h : Infinity;
    const s = Math.min(sx, sy);
    return Number.isFinite(s) && s > 0 ? s : 1;
  }

  private minScale(): number {
    return Math.min(0.5, this.fitScaleUncapped() / 2);
  }

  private clampScale(s: number): number {
    return Math.min(MAX_SCALE, Math.max(this.minScale(), s));
  }

  private fitView(): View {
    const s = Math.min(this.fitScaleUncapped(), 1);
    const cx = this.bbox.x + this.bbox.w / 2;
    const cy = this.bbox.y + this.bbox.h / 2;
    return { scale: s, tx: this.width / 2 - cx * s, ty: this.height / 2 - cy * s };
  }

  private initialView(): View {
    const fit = this.fitView();
    if (this.focusPoint && fit.scale < FOCUS_MIN_FIT) {
      const { x, y } = this.focusPoint;
      return { scale: 1, tx: this.width / 2 - x, ty: this.height / 2 - y };
    }
    return fit;
  }

  private showInitial(): void {
    this.needsInitial = false;
    this.set(this.initialView());
    this.apply();
  }

  fit(animate: boolean): void {
    if (!this.svg) {
      return;
    }
    this.mode = 'fit';
    this.moveTo(this.fitView(), animate);
  }

  /** Centres a world point at `scale` (used by search). */
  centerOn(x: number, y: number, scale: number, animate: boolean): void {
    if (!this.svg) {
      return;
    }
    this.mode = 'user';
    const s = this.clampScale(scale);
    this.moveTo({ scale: s, tx: this.width / 2 - x * s, ty: this.height / 2 - y * s }, animate);
  }

  /** Zooms by `factor` keeping the container point (px) fixed. */
  zoomAt(factor: number, px: number, py: number): void {
    if (!this.svg) {
      return;
    }
    this.cancelAnimation();
    this.mode = 'user';
    const s = this.clampScale(this.scale * factor);
    const wx = (px - this.tx) / this.scale;
    const wy = (py - this.ty) / this.scale;
    this.set({ scale: s, tx: px - wx * s, ty: py - wy * s });
    this.requestApply();
  }

  zoomBy(factor: number): void {
    this.zoomAt(factor, this.width / 2, this.height / 2);
  }

  panBy(dx: number, dy: number): void {
    if (!this.svg) {
      return;
    }
    this.cancelAnimation();
    this.mode = 'user';
    this.tx += dx;
    this.ty += dy;
    this.requestApply();
  }

  /** World point at the middle of the view. */
  worldCenter(): { x: number; y: number } {
    return this.toWorldCenter(this);
  }

  /** World → container px. */
  toScreen(x: number, y: number): { x: number; y: number } {
    return { x: x * this.scale + this.tx, y: y * this.scale + this.ty };
  }

  private set(v: View): void {
    this.scale = v.scale;
    this.tx = v.tx;
    this.ty = v.ty;
  }

  private onResize(): void {
    const oldW = this.width;
    const oldH = this.height;
    this.measure();
    if (!this.svg || this.width === 0 || this.height === 0) {
      return;
    }
    if (this.needsInitial || this.mode === 'initial') {
      this.showInitial();
      return;
    }
    if (this.mode === 'fit') {
      this.cancelAnimation();
      this.set(this.fitView());
      this.apply();
      return;
    }
    // Keep the world point at the centre of the view.
    this.tx += (this.width - oldW) / 2;
    this.ty += (this.height - oldH) / 2;
    this.apply();
  }

  private moveTo(target: View, animate: boolean): void {
    this.cancelAnimation();
    if (!animate || prefersReducedMotion()) {
      this.set(target);
      this.apply();
      return;
    }
    // Interpolate the world point at the view centre linearly and the scale logarithmically.
    const from = { ...this.toWorldCenter(this), ls: Math.log(this.scale) };
    const to = { ...this.toWorldCenter(target), ls: Math.log(target.scale) };
    const start = performance.now();
    const step = (now: number) => {
      const t = Math.min(1, (now - start) / MOVE_MS);
      const e = 1 - Math.pow(1 - t, 3);
      const s = Math.exp(from.ls + (to.ls - from.ls) * e);
      const cx = from.x + (to.x - from.x) * e;
      const cy = from.y + (to.y - from.y) * e;
      this.set({ scale: s, tx: this.width / 2 - cx * s, ty: this.height / 2 - cy * s });
      this.apply();
      this.anim = t < 1 ? requestAnimationFrame(step) : 0;
    };
    this.anim = requestAnimationFrame(step);
  }

  private toWorldCenter(v: View): { x: number; y: number } {
    return { x: (this.width / 2 - v.tx) / v.scale, y: (this.height / 2 - v.ty) / v.scale };
  }

  private cancelAnimation(): void {
    if (this.anim) {
      cancelAnimationFrame(this.anim);
      this.anim = 0;
    }
  }

  private requestApply(): void {
    if (!this.frame) {
      this.frame = requestAnimationFrame(() => {
        this.frame = 0;
        this.apply();
      });
    }
  }

  private apply(): void {
    if (!this.svg) {
      return;
    }
    const s = this.scale;
    this.svg.setAttribute('viewBox', `${-this.tx / s} ${-this.ty / s} ${this.width / s} ${this.height / s}`);
    // The dot grid follows pan and zoom, with its spacing kept between 12 and 36 px.
    let g = GRID * s;
    while (g < 12) {
      g *= 2;
    }
    while (g > 36) {
      g /= 2;
    }
    const mod = (v: number) => ((v % g) + g) % g;
    this.container.style.backgroundSize = `${g}px ${g}px`;
    this.container.style.backgroundPosition = `${mod(this.tx)}px ${mod(this.ty)}px`;
  }
}
