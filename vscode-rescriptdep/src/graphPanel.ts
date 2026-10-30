// The graph webview panel: lifecycle, host-owned view state, history and request handling
// (ARCH-2/3, FE-1/4, QA-7, C5, C11).
//
// - `webview.html` is set once; every later update is a typed message (`protocol.ts`).
// - One message listener and one dispose listener per panel, in panel-scoped disposables.
// - The host owns the state; `ready` replays it (also after a webview reload).
// - Last request wins: each request gets a new `renderKey` and aborts the superseded CLI run.
import * as vscode from 'vscode';
import * as crypto from 'crypto';
import { WEBVIEW_VIEW_TYPE } from './protocol';
import type { GraphModel, GraphNode, GraphView, HostMessage, LayoutOptions } from './protocol';
import { renderWebviewHtml } from './webviewHtml';
import type { CliRunner } from './cli';
import { classifyCliError, cliErrorReason, CliError } from './core/cliError';
import { compareIds, parseDot } from './core/dot';
import type { ParsedDot } from './core/dot';
import { decideLayout, layoutSize } from './core/layoutPolicy';
import type { LayoutSize } from './core/layoutPolicy';
import type { ModuleSource, ProjectRef, ProjectService } from './project';
import { openModuleSource } from './contextMenu';

export const MAX_HISTORY = 50;
/** Bound retained graph data as well as the number of navigation entries. */
export const MAX_HISTORY_ITEMS = 50_000;
export const FOCUS_ON_MODULE = 'Focus on Module…';
export const RENDER_ANYWAY = 'Render Anyway';
const ERROR_TITLE = "Couldn't render the graph";

export type GraphRequest =
  | { view: 'full'; project: ProjectRef }
  | { view: 'unused'; project: ProjectRef }
  | { view: 'focus'; project: ProjectRef; module: string };

export interface RunOptions {
  /** Started from a command: show the cancellable notification. In-panel navigation shows only the overlay. */
  notify?: boolean;
  /** Push the current view onto the Back history once this one renders. */
  push?: boolean;
  /** "Render Anyway" was chosen: tune, and don't ask again. */
  renderAnyway?: boolean;
}

export interface GraphPanelDeps {
  extensionUri: vscode.Uri;
  cli: CliRunner;
  projects: ProjectService;
}

interface ViewEntry {
  request: GraphRequest;
  /** The focus view's centre as it appears in the model. */
  center?: string;
  model: GraphModel;
  moduleIds: Set<string>;
  sources: Map<string, ModuleSource>;
  layout: LayoutOptions;
  renderKey: number;
}

interface Pending {
  key: number;
  request: GraphRequest;
  options: RunOptions;
  abort: AbortController;
  text: string;
}

interface Fetched {
  parsed: ParsedDot;
  sources: Map<string, ModuleSource>;
  model: GraphModel;
  size: LayoutSize;
}

export function cliArgs(request: GraphRequest): string[] {
  switch (request.view) {
    case 'full':
      return ['--format=dot', request.project.bsDir];
    case 'unused':
      return ['--format=dot', '--no-dependents', request.project.bsDir];
    case 'focus':
      return ['--format=dot', '--module', request.module, request.project.bsDir];
  }
}

function progressTitle(request: GraphRequest): string {
  switch (request.view) {
    case 'full':
      return 'Bibimbob: Analyzing dependency graph…';
    case 'unused':
      return 'Bibimbob: Finding unused modules…';
    case 'focus':
      return 'Bibimbob: Analyzing module dependencies…';
  }
}

function loadingText(request: GraphRequest): string {
  switch (request.view) {
    case 'full':
      return 'Analyzing the dependency graph…';
    case 'unused':
      return 'Finding modules without dependents…';
    case 'focus':
      return `Analyzing dependencies of ${request.module}…`;
  }
}

export function panelTitle(view: GraphView, center?: string): string {
  switch (view) {
    case 'full':
      return 'Dependency Graph';
    case 'unused':
      return 'Modules Without Dependents';
    case 'focus':
      return `Dependencies: ${center ?? ''}`;
  }
}

function sameView(a: GraphRequest, b: GraphRequest): boolean {
  return (
    a.view === b.view &&
    a.project.bsDir === b.project.bsDir &&
    (a.view !== 'focus' || (b.view === 'focus' && a.module === b.module))
  );
}

/** Nodes sorted in code-point order; the webview never receives a path. */
export function buildModel(parsed: ParsedDot, sources: Map<string, ModuleSource>): GraphModel {
  const ids = new Set(parsed.nodes.map((n) => n.id));
  const nodes: GraphNode[] = parsed.nodes
    .map((n) => {
      const source = sources.get(n.id);
      const node: GraphNode = { id: n.id, external: source?.external ?? false, openable: source?.openable ?? false };
      if (source?.secondary !== undefined) {
        node.secondary = source.secondary;
      }
      return node;
    })
    .sort((a, b) => compareIds(a.id, b.id));
  return {
    nodes,
    edges: parsed.edges.map(([from, to]) => [from, to] as [string, string]),
    cycles: parsed.cycles.map((c) => c.filter((id) => ids.has(id))).filter((c) => c.length > 1),
  };
}

function makeNonce(): string {
  return crypto.randomBytes(16).toString('hex');
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null;
}

export class GraphPanel implements vscode.Disposable {
  readonly id: string = crypto.randomUUID();
  private readonly panel: vscode.WebviewPanel;
  private readonly disposables: vscode.Disposable[] = [];
  private webviewReady = false;
  private disposed = false;
  private renderKey = 0;
  private current: ViewEntry | undefined;
  private readonly history: ViewEntry[] = [];
  private pending: Pending | undefined;
  /** The last request that failed before rendering; shown until the next render. */
  private lastError: { message: HostMessage; request: GraphRequest; options: RunOptions } | undefined;
  /** `renderKey` of the current view when the webview reported a layout failure for it. */
  private failedRenderKey: number | undefined;

  constructor(
    private readonly deps: GraphPanelDeps,
    private readonly events: { onDidDispose(panel: GraphPanel): void; onDidBecomeActive(panel: GraphPanel): void },
  ) {
    const dist = vscode.Uri.joinPath(deps.extensionUri, 'dist');
    this.panel = vscode.window.createWebviewPanel(WEBVIEW_VIEW_TYPE, panelTitle('full'), vscode.ViewColumn.One, {
      enableScripts: true,
      retainContextWhenHidden: true,
      localResourceRoots: [dist],
    });
    this.panel.iconPath = vscode.Uri.joinPath(deps.extensionUri, 'images', 'rescriptdep_icon.png');
    const webview = this.panel.webview;
    webview.html = renderWebviewHtml({
      cspSource: webview.cspSource,
      scriptUri: webview.asWebviewUri(vscode.Uri.joinPath(dist, 'webview.js')).toString(),
      styleUri: webview.asWebviewUri(vscode.Uri.joinPath(dist, 'webview.css')).toString(),
      nonce: makeNonce(),
    });
    this.disposables.push(
      webview.onDidReceiveMessage((message: unknown) => this.onMessage(message)),
      this.panel.onDidChangeViewState((e) => {
        if (e.webviewPanel.active) {
          this.events.onDidBecomeActive(this);
        }
      }),
      this.panel.onDidDispose(() => this.dispose()),
    );
  }

  dispose(): void {
    if (this.disposed) {
      return;
    }
    this.disposed = true;
    this.pending?.abort.abort();
    this.pending = undefined;
    for (const d of this.disposables) {
      d.dispose();
    }
    this.disposables.length = 0;
    this.panel.dispose();
    this.events.onDidDispose(this);
  }

  get isDisposed(): boolean {
    return this.disposed;
  }

  get viewColumn(): vscode.ViewColumn | undefined {
    return this.panel.viewColumn;
  }

  /** The project of what the panel shows, or of the request it is waiting for. */
  get project(): ProjectRef | undefined {
    return this.current?.request.project ?? this.pending?.request.project ?? this.lastError?.request.project;
  }

  /** Reveal in the panel's current column. */
  reveal(): void {
    this.panel.reveal(undefined, false);
  }

  hasModule(module: string): boolean {
    return this.current?.moduleIds.has(module) ?? false;
  }

  isCenter(module: string): boolean {
    return this.current?.request.view === 'focus' && this.current.center === module;
  }

  // ---- Webview messages ------------------------------------------------------------------

  private onMessage(message: unknown): void {
    if (!isRecord(message) || typeof message.type !== 'string') {
      return;
    }
    switch (message.type) {
      case 'ready':
        this.webviewReady = true;
        this.replay();
        break;
      case 'focusModule':
        if (typeof message.module === 'string') {
          void this.focusModule(message.module);
        }
        break;
      case 'openModule':
        if (typeof message.module === 'string') {
          void this.openModule(message.module);
        }
        break;
      case 'back':
        this.back();
        break;
      case 'showFullGraph':
        void this.showFullGraph();
        break;
      case 'pickModule':
        void this.pickAndFocus();
        break;
      case 'retry':
        void this.retry();
        break;
      case 'cancel':
        if (this.pending) {
          this.cancel(this.pending.key);
        }
        break;
      case 'rendered':
        if (message.renderKey === this.current?.renderKey) {
          this.failedRenderKey = undefined;
        }
        break;
      case 'renderError':
        if (typeof message.renderKey === 'number' && message.renderKey === this.current?.renderKey) {
          this.failedRenderKey = message.renderKey;
        }
        break;
    }
  }

  private post(message: HostMessage): void {
    // Until `ready`, state is only recorded; `ready` replays it.
    if (this.webviewReady && !this.disposed) {
      void this.panel.webview.postMessage(message);
    }
  }

  private replay(): void {
    if (this.current) {
      this.postRender(this.current);
    }
    if (this.lastError) {
      this.post(this.lastError.message);
    }
    if (this.pending) {
      this.post({ type: 'loading', renderKey: this.pending.key, text: this.pending.text });
    }
  }

  private postRender(entry: ViewEntry): void {
    this.post({
      type: 'render',
      renderKey: entry.renderKey,
      panelId: this.id,
      view: entry.request.view,
      center: entry.center,
      canGoBack: this.history.length > 0,
      model: entry.model,
      layout: entry.layout,
    });
  }

  // ---- Navigation ------------------------------------------------------------------------

  /** Left-click, the context menu and Enter: the same path. Unknown modules and the centre are no-ops. */
  async focusModule(module: string): Promise<void> {
    const entry = this.current;
    if (!entry || !entry.moduleIds.has(module) || this.isCenter(module)) {
      return;
    }
    await this.run({ view: 'focus', project: entry.request.project, module }, { push: true });
  }

  async showFullGraph(): Promise<void> {
    const project = this.project;
    if (project) {
      await this.run({ view: 'full', project }, { push: true });
    }
  }

  async pickAndFocus(): Promise<void> {
    const project = this.project;
    if (!project) {
      return;
    }
    const module = await this.deps.projects.pickModule(project);
    if (module) {
      await this.run({ view: 'focus', project, module }, { push: true });
    }
  }

  /** Pop the history and re-render the cached model: no CLI run. */
  back(): void {
    const entry = this.lastError && this.current ? this.current : this.history.pop();
    if (!entry || this.disposed) {
      return;
    }
    this.supersedePending();
    entry.renderKey = ++this.renderKey;
    this.show(entry);
  }

  fitToView(): void {
    this.post({ type: 'fitToView' });
  }

  async retry(): Promise<void> {
    if (this.lastError) {
      const { request, options } = this.lastError;
      await this.run(request, { push: options.push, renderAnyway: options.renderAnyway });
      return;
    }
    const entry = this.current;
    if (entry && this.failedRenderKey === entry.renderKey) {
      // The layout failed in the webview: send the same model again, tuned.
      entry.layout = { tune: true, slow: true };
      entry.renderKey = ++this.renderKey;
      this.show(entry);
      return;
    }
    if (entry) {
      await this.run(entry.request, {});
    }
  }

  async openModule(module: string): Promise<void> {
    const entry = this.current;
    if (!entry || !entry.moduleIds.has(module)) {
      return;
    }
    const file = entry.sources.get(module)?.file;
    if (!file || !(await this.deps.projects.isOpenableFile(file, entry.request.project.projectRoot))) {
      vscode.window.setStatusBarMessage(`Source for ${module} isn't available in this workspace.`, 5000);
      return;
    }
    await openModuleSource(vscode.Uri.file(file), this.panel.viewColumn);
  }

  /** Workspace-relative path for "Copy Relative Path", only for openable modules. */
  relativePath(module: string): string | undefined {
    const source = this.current?.moduleIds.has(module) ? this.current.sources.get(module) : undefined;
    return source?.openable && source.file ? vscode.workspace.asRelativePath(source.file, false) : undefined;
  }

  // ---- Requests --------------------------------------------------------------------------

  private isPending(key: number): boolean {
    return !this.disposed && this.pending?.key === key;
  }

  /** Abort the running request without telling the webview (a newer message replaces it). */
  private supersedePending(): void {
    if (this.pending) {
      this.pending.abort.abort();
      this.pending = undefined;
    }
  }

  /** User cancel: end silently, restore the previous view; close a panel that never showed anything. */
  private cancel(key: number): void {
    if (!this.isPending(key)) {
      return;
    }
    this.supersedePending();
    if (!this.current && !this.lastError) {
      this.dispose();
      return;
    }
    this.post({ type: 'cancelled', renderKey: key });
  }

  private fail(pending: Pending, reason: string): void {
    this.pending = undefined;
    const message: HostMessage = { type: 'error', renderKey: pending.key, title: ERROR_TITLE, reason, canRetry: true, canGoBack: !!this.current || this.history.length > 0 };
    this.lastError = { message, request: pending.request, options: pending.options };
    this.post(message);
  }

  private show(entry: ViewEntry): void {
    this.current = entry;
    this.lastError = undefined;
    this.failedRenderKey = undefined;
    this.panel.title = panelTitle(entry.request.view, entry.center);
    this.postRender(entry);
  }

  /** Run the CLI for `request` and render the result. A newer request (or Back) supersedes this one. */
  async run(request: GraphRequest, options: RunOptions = {}): Promise<void> {
    if (this.disposed) {
      return;
    }
    this.supersedePending();
    const pending: Pending = { key: ++this.renderKey, request, options, abort: new AbortController(), text: loadingText(request) };
    this.pending = pending;
    this.post({ type: 'loading', renderKey: pending.key, text: pending.text });

    let fetched: Fetched | undefined;
    if (options.notify) {
      fetched = await vscode.window.withProgress(
        { location: vscode.ProgressLocation.Notification, title: progressTitle(request), cancellable: true },
        async (_progress, token) => {
          const subscription = token.onCancellationRequested(() => this.cancel(pending.key));
          try {
            return await this.fetch(pending);
          } finally {
            subscription.dispose();
          }
        },
      );
    } else {
      fetched = await this.fetch(pending);
    }
    if (fetched && this.isPending(pending.key)) {
      await this.present(pending, fetched);
    }
  }

  private async fetch(pending: Pending): Promise<Fetched | undefined> {
    let stdout: string;
    try {
      stdout = await this.deps.cli.run(cliArgs(pending.request), { signal: pending.abort.signal });
    } catch (error) {
      if (!this.isPending(pending.key)) {
        return undefined; // superseded, cancelled or disposed: silent
      }
      const kind = classifyCliError(error);
      if (kind === 'cancelled') {
        this.cancel(pending.key);
      } else {
        this.fail(pending, cliErrorReason(kind, error instanceof CliError ? error.stderr : ''));
      }
      return undefined;
    }
    if (!this.isPending(pending.key)) {
      return undefined;
    }
    let parsed: ParsedDot;
    try {
      parsed = parseDot(stdout);
    } catch {
      this.fail(pending, "The graph data couldn't be read.");
      return undefined;
    }
    const sources = await this.deps.projects.resolveSources(pending.request.project.projectRoot, parsed.nodes);
    if (!this.isPending(pending.key)) {
      return undefined;
    }
    const model = buildModel(parsed, sources);
    return { parsed, sources, model, size: layoutSize(model.nodes.map((n) => n.id), model.edges) };
  }

  private async present(pending: Pending, fetched: Fetched): Promise<void> {
    const { request } = pending;
    let decision = decideLayout(request.view, fetched.size, { renderAnyway: pending.options.renderAnyway });
    if (decision.prompt) {
      const choice = await vscode.window.showWarningMessage(
        `This graph has ${fetched.size.nodes.toLocaleString('en-US')} modules. Laying it out may take a while.`,
        FOCUS_ON_MODULE,
        RENDER_ANYWAY,
      );
      if (!this.isPending(pending.key)) {
        return;
      }
      if (choice === RENDER_ANYWAY) {
        pending.options = { ...pending.options, renderAnyway: true };
        decision = decideLayout(request.view, fetched.size, { renderAnyway: true });
      } else if (choice === FOCUS_ON_MODULE) {
        const module = await this.deps.projects.pickModule(request.project);
        if (!this.isPending(pending.key)) {
          return;
        }
        if (!module) {
          this.cancel(pending.key);
          return;
        }
        await this.run({ view: 'focus', project: request.project, module }, { push: pending.options.push });
        return;
      } else {
        this.cancel(pending.key);
        return;
      }
    }

    const ids = new Set(fetched.model.nodes.map((n) => n.id));
    let center: string | undefined;
    if (request.view === 'focus') {
      // The CLI matches `--module` case-insensitively; show the name as the graph spells it.
      const lower = request.module.toLowerCase();
      center = ids.has(request.module) ? request.module : ([...ids].find((id) => id.toLowerCase() === lower) ?? request.module);
    }
    const entry: ViewEntry = {
      request,
      center,
      model: fetched.model,
      moduleIds: ids,
      sources: fetched.sources,
      layout: { tune: decision.tune, slow: decision.slow },
      renderKey: pending.key,
    };
    this.pending = undefined;
    if (pending.options.push && this.current && !sameView(this.current.request, request)) {
      this.history.push(this.current);
      const weight = (entry: ViewEntry) => entry.model.nodes.length + entry.model.edges.length;
      let retained = this.history.reduce((sum, entry) => sum + weight(entry), 0);
      while (this.history.length > MAX_HISTORY || retained > MAX_HISTORY_ITEMS) {
        retained -= weight(this.history.shift()!);
      }
    }
    this.show(entry);
  }
}

/** Panels by id. Today `getOrCreate` keeps a single panel; every lookup still goes through the registry. */
export class GraphPanelRegistry implements vscode.Disposable {
  private readonly panels = new Map<string, GraphPanel>();
  private activeId: string | undefined;

  constructor(private readonly deps: GraphPanelDeps) {}

  get(id: string): GraphPanel | undefined {
    return this.panels.get(id);
  }

  /** The most recently active panel, else any open one. */
  active(): GraphPanel | undefined {
    const active = this.activeId === undefined ? undefined : this.panels.get(this.activeId);
    return active ?? [...this.panels.values()].pop();
  }

  get size(): number {
    return this.panels.size;
  }

  getOrCreate(): { panel: GraphPanel; created: boolean } {
    const existing = this.active();
    if (existing) {
      return { panel: existing, created: false };
    }
    const panel = new GraphPanel(this.deps, {
      onDidDispose: (p) => {
        this.panels.delete(p.id);
        if (this.activeId === p.id) {
          this.activeId = undefined;
        }
      },
      onDidBecomeActive: (p) => {
        this.activeId = p.id;
      },
    });
    this.panels.set(panel.id, panel);
    this.activeId = panel.id;
    return { panel, created: true };
  }

  /** Entry point of the public commands: reveal (or create) the panel and run the request. */
  async show(request: GraphRequest): Promise<void> {
    const { panel, created } = this.getOrCreate();
    if (!created) {
      panel.reveal();
    }
    await panel.run(request, { notify: true, push: true });
  }

  dispose(): void {
    for (const panel of [...this.panels.values()]) {
      panel.dispose();
    }
  }
}
