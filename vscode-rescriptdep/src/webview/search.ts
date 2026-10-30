// Find box (C13 / WP-UI-5): filter as you type, Enter / Shift+Enter cycle, counter, Esc clears.
//
// The 100 ms debounce has a leading edge: the first keystroke after a quiet period is applied at once
// (so a single keystroke paints in the same frame), later keystrokes within 100 ms are coalesced into
// one trailing update.
import type { Highlighter } from './highlight';
import type { Scene } from './scene';
import { copy } from './text';

const DEBOUNCE_MS = 100;

export interface SearchHost {
  /** Centres a module (CTM-correct, at scale ≥ 1) with the 200 ms move. */
  reveal(id: string): void;
  /** Moves keyboard focus to a node. */
  focusNode(id: string): void;
  announce(text: string): void;
}

export class Search {
  private scene: Scene | null = null;
  private lowerIds: [string, string][] = [];
  private ordered: string[] = [];
  private index = -1;
  private applied = '';
  private timer = 0;
  private lastApply = -Infinity;

  constructor(
    private readonly input: HTMLInputElement,
    private readonly counter: HTMLElement,
    private readonly hl: Highlighter,
    private readonly host: SearchHost,
  ) {
    input.addEventListener('input', () => this.onInput());
    input.addEventListener('keydown', (e) => this.onKeyDown(e));
  }

  /** A new graph replaces the old one; the query is cleared. */
  attach(scene: Scene | null): void {
    this.scene = scene;
    this.lowerIds = scene ? [...scene.nodeEls.keys()].map((id) => [id, id.toLowerCase()]) : [];
    this.cancelTimer();
    this.input.value = '';
    this.applied = '';
    this.ordered = [];
    this.index = -1;
    this.renderCounter();
  }

  get active(): boolean {
    return this.applied !== '';
  }

  focusInput(): void {
    this.input.focus();
    this.input.select();
  }

  clear(): void {
    this.cancelTimer();
    this.input.value = '';
    this.apply();
  }

  private onInput(): void {
    const now = performance.now();
    if (!this.timer && now - this.lastApply >= DEBOUNCE_MS) {
      this.apply();
      return;
    }
    if (!this.timer) {
      this.timer = window.setTimeout(() => {
        this.timer = 0;
        this.apply();
      }, Math.max(0, DEBOUNCE_MS - (now - this.lastApply)));
    }
  }

  private cancelTimer(): void {
    if (this.timer) {
      clearTimeout(this.timer);
      this.timer = 0;
    }
  }

  /** Applies the input's current value (no-op when unchanged). */
  private apply(): void {
    this.lastApply = performance.now();
    const q = this.input.value.trim().toLowerCase();
    if (q === this.applied) {
      return;
    }
    this.applied = q;
    this.index = -1;
    const s = this.scene;
    if (!s || q === '') {
      this.ordered = [];
      this.hl.setCurrentMatch(null);
      this.hl.setMatches(null);
      this.renderCounter();
      return;
    }
    const matches = new Set<string>();
    for (const [id, lower] of this.lowerIds) {
      if (lower.includes(q)) {
        matches.add(id);
      }
    }
    // Cycle in reading order of the LR layouts: left to right, then top to bottom.
    this.ordered = [...matches].sort((a, b) => {
      const pa = s.boxes.get(a);
      const pb = s.boxes.get(b);
      if (!pa || !pb) {
        return 0;
      }
      return Math.round(pa.x - pa.w / 2) - Math.round(pb.x - pb.w / 2) || pa.y - pb.y;
    });
    this.hl.setCurrentMatch(null);
    this.hl.setMatches(matches);
    this.renderCounter();
    if (matches.size === 0) {
      this.host.announce(copy.noResults);
    }
  }

  private onKeyDown(e: KeyboardEvent): void {
    if (e.key === 'Enter' && !e.metaKey && !e.ctrlKey && !e.altKey) {
      e.preventDefault();
      this.cancelTimer();
      this.apply();
      this.step(e.shiftKey ? -1 : 1);
    } else if (e.key === 'Escape') {
      e.preventDefault();
      e.stopPropagation();
      const current = this.index >= 0 ? this.ordered[this.index] : null;
      this.clear();
      if (current) {
        this.host.focusNode(current);
      }
    }
  }

  private step(dir: 1 | -1): void {
    const n = this.ordered.length;
    if (n === 0) {
      return;
    }
    this.index = this.index < 0 ? (dir > 0 ? 0 : n - 1) : (this.index + dir + n) % n;
    const id = this.ordered[this.index];
    this.hl.setCurrentMatch(id);
    this.host.reveal(id);
    this.renderCounter();
    this.host.announce(copy.announceMatchOf(this.index + 1, n));
  }

  private renderCounter(): void {
    const c = this.counter;
    c.classList.remove('none');
    if (this.applied === '') {
      c.textContent = '';
      return;
    }
    const n = this.ordered.length;
    if (n === 0) {
      c.textContent = copy.noResults;
      c.classList.add('none');
    } else if (this.index < 0) {
      c.textContent = copy.matchCount(n);
    } else {
      c.textContent = copy.matchOf(this.index + 1, n);
    }
  }
}
