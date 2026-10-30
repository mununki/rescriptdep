// Small DOM helpers. All text goes through textContent; no HTML strings are parsed at runtime.

export const SVG_NS = 'http://www.w3.org/2000/svg';

export function svgEl<K extends keyof SVGElementTagNameMap>(
  tag: K,
  attrs?: Record<string, string | number>,
  parent?: Element,
): SVGElementTagNameMap[K] {
  const el = document.createElementNS(SVG_NS, tag);
  if (attrs) {
    for (const key in attrs) {
      el.setAttribute(key, String(attrs[key]));
    }
  }
  if (parent) {
    parent.appendChild(el);
  }
  return el;
}

export function byId<T extends HTMLElement>(id: string): T {
  const el = document.getElementById(id);
  if (!el) {
    throw new Error(`Missing #${id} in the webview shell`);
  }
  return el as T;
}

/** Rounds to 0.1 px for compact path data. */
export function r1(n: number): number {
  return Math.round(n * 10) / 10;
}

export const isMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent);

export function prefersReducedMotion(): boolean {
  return (
    document.body.classList.contains('vscode-reduce-motion') ||
    matchMedia('(prefers-reduced-motion: reduce)').matches
  );
}

/** Resolves after `frames` animation frames, or after `maxMs` when frames are not delivered (hidden panel). */
export function yieldFrames(frames: number, maxMs = 250): Promise<void> {
  return new Promise((resolve) => {
    let done = false;
    const finish = () => {
      if (!done) {
        done = true;
        resolve();
      }
    };
    const step = (left: number) =>
      requestAnimationFrame(() => (left <= 1 ? setTimeout(finish, 0) : step(left - 1)));
    step(frames);
    setTimeout(finish, maxMs);
  });
}
