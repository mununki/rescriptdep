// Card metrics (designer-review3 R1 "Card"): text is measured in the body's UI font, so labels fit by construction.
import type { PreparedGraph } from './model';

export const CARD = {
  padX: 12,
  height1: 28,
  height2: 40,
  minWidth: 44,
  radius: 6,
  /** Name baseline for one-line cards, from the card centre. */
  nameY1: 4.2,
  /** Name and second-line baselines for two-line cards, from the card top. */
  nameY2: 17,
  secY2: 31,
} as const;

export interface CardSize {
  w: number;
  h: number;
}

let ctx: CanvasRenderingContext2D | null = null;
let cachedFamily = '';
const widthCache = new Map<string, number>();

export function uiFontFamily(): string {
  return getComputedStyle(document.body).fontFamily || 'sans-serif';
}

export function textWidth(text: string, font: string): number {
  if (!ctx) {
    ctx = document.createElement('canvas').getContext('2d');
  }
  const key = `${font}\u0000${text}`;
  let w = widthCache.get(key);
  if (w === undefined) {
    if (!ctx) {
      // No 2D context (should not happen in a webview): a conservative estimate.
      w = text.length * 7.5;
    } else {
      ctx.font = font;
      w = ctx.measureText(text).width;
    }
    widthCache.set(key, w);
  }
  return w;
}

export function fonts(family: string) {
  return {
    name: `500 12px ${family}`,
    center: `600 12px ${family}`,
    sec: `400 10.5px ${family}`,
    header: `600 11px ${family}`,
    note: `400 12px ${family}`,
  };
}

export function measureCards(g: PreparedGraph): Map<string, CardSize> {
  const family = uiFontFamily();
  if (family !== cachedFamily) {
    widthCache.clear();
    cachedFamily = family;
  }
  const f = fonts(family);
  const sizes = new Map<string, CardSize>();
  for (const n of g.nodes) {
    const nameW = textWidth(n.id, n.id === g.center ? f.center : f.name);
    const secW = n.secondary ? textWidth(n.secondary, f.sec) : 0;
    sizes.set(n.id, {
      w: Math.max(CARD.minWidth, Math.ceil(Math.max(nameW, secW)) + CARD.padX * 2),
      h: n.secondary ? CARD.height2 : CARD.height1,
    });
  }
  return sizes;
}
