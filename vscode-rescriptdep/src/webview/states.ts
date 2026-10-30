// Loading overlay and the empty/error panel (DES-10 / WP-UI-6). Buttons carry `data-action`; one delegated
// click listener (registered once in main.ts) handles them.
import { byId } from './dom';
import { copy } from './text';

export type StateAction = 'retry' | 'pickModule' | 'cancel';

export interface PanelAction {
  action: StateAction;
  label: string;
  primary?: boolean;
}

const SLOW_HINT_MS = 10_000;

export class States {
  private readonly loading = byId<HTMLElement>('loading');
  private readonly loadingText = byId<HTMLElement>('loading-text');
  private readonly loadingHint = byId<HTMLElement>('loading-hint');
  private readonly loadingCancel = byId<HTMLButtonElement>('loading-cancel');
  private readonly panel = byId<HTMLElement>('state');
  private readonly panelTitle = byId<HTMLElement>('state-title');
  private readonly panelBody = byId<HTMLElement>('state-body');
  private readonly panelActions = byId<HTMLElement>('state-actions');
  private hintTimer = 0;

  constructor(private readonly graphHost: HTMLElement) {}

  get loadingVisible(): boolean {
    return !this.loading.hidden;
  }

  /** Shows the overlay and dims the previous graph to 40%. `cancel` is offered only while waiting for the CLI. */
  showLoading(text: string, cancel: boolean): void {
    this.loadingText.textContent = text;
    this.loadingCancel.hidden = !cancel;
    this.loading.hidden = false;
    this.graphHost.classList.add('stale');
    clearTimeout(this.hintTimer);
    this.loadingHint.hidden = true;
    if (cancel) {
      this.hintTimer = window.setTimeout(() => {
        this.loadingHint.hidden = false;
      }, SLOW_HINT_MS);
    }
  }

  hideLoading(): void {
    clearTimeout(this.hintTimer);
    this.loading.hidden = true;
    this.loadingHint.hidden = true;
    this.graphHost.classList.remove('stale');
  }

  showPanel(kind: 'empty' | 'error', title: string, body: string, actions: PanelAction[]): void {
    this.panel.dataset.kind = kind;
    this.panelTitle.textContent = title;
    this.panelBody.textContent = body;
    const buttons = actions.map((a) => {
      const b = document.createElement('button');
      b.type = 'button';
      b.className = a.primary ? 'primary' : 'secondary';
      b.dataset.action = a.action;
      b.textContent = a.label;
      return b;
    });
    this.panelActions.replaceChildren(...buttons);
    this.panel.hidden = false;
    this.panel.setAttribute('role', kind === 'error' ? 'alert' : 'status');
  }

  hidePanel(): void {
    this.panel.hidden = true;
    this.panelActions.replaceChildren();
  }

  static errorActions(canRetry: boolean): PanelAction[] {
    const actions: PanelAction[] = [];
    if (canRetry) {
      actions.push({ action: 'retry', label: copy.retry, primary: true });
    }
    actions.push({ action: 'pickModule', label: copy.pickModule, primary: !canRetry });
    return actions;
  }
}
