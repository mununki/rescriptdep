// A minimal stand-in for the `vscode` module, aliased in by `test:unit` (esbuild `--alias:vscode=`).
// It implements only what the host uses, records every UI call in `stub`, and backs `findFiles`
// with the real file system (case-sensitive globs, like VS Code).
import * as fs from 'fs';
import * as path from 'path';

type Listener<T> = (e: T) => unknown;

export class Disposable {
  constructor(private readonly fn: () => void) {}
  dispose(): void {
    this.fn();
  }
}

export class EventEmitter<T> {
  listeners: Listener<T>[] = [];
  event = (listener: Listener<T>, _thisArg?: unknown, disposables?: Disposable[]): Disposable => {
    this.listeners.push(listener);
    const d = new Disposable(() => {
      this.listeners = this.listeners.filter((l) => l !== listener);
    });
    disposables?.push(d);
    return d;
  };
  fire(e: T): void {
    for (const l of [...this.listeners]) {
      l(e);
    }
  }
  dispose(): void {
    this.listeners = [];
  }
}

export class Uri {
  private constructor(
    readonly scheme: string,
    readonly fsPath: string,
    private readonly raw?: string,
  ) {}
  static file(p: string): Uri {
    return new Uri('file', path.resolve(p));
  }
  static parse(value: string): Uri {
    const scheme = value.includes(':') ? value.slice(0, value.indexOf(':')) : 'file';
    return new Uri(scheme, value, value);
  }
  static joinPath(base: Uri, ...segments: string[]): Uri {
    return new Uri(base.scheme, path.join(base.fsPath, ...segments));
  }
  get path(): string {
    return this.fsPath;
  }
  toString(): string {
    return this.raw ?? `${this.scheme}://${this.fsPath}`;
  }
}

export class Position {
  constructor(
    readonly line: number,
    readonly character: number,
  ) {}
}

export class Range {
  readonly start: Position;
  readonly end: Position;
  constructor(startLine: number, startCharacter: number, endLine: number, endCharacter: number) {
    this.start = new Position(startLine, startCharacter);
    this.end = new Position(endLine, endCharacter);
  }
}

export class ThemeColor {
  constructor(readonly id: string) {}
}

export class RelativePattern {
  readonly base: string;
  constructor(
    base: string | Uri | { uri: Uri },
    readonly pattern: string,
  ) {
    this.base = typeof base === 'string' ? base : base instanceof Uri ? base.fsPath : base.uri.fsPath;
  }
}

export class TabInputText {
  constructor(readonly uri: Uri) {}
}

export enum ViewColumn {
  Active = -1,
  Beside = -2,
  One = 1,
  Two = 2,
  Three = 3,
}

export enum ProgressLocation {
  SourceControl = 1,
  Window = 10,
  Notification = 15,
}

export enum ConfigurationTarget {
  Global = 1,
  Workspace = 2,
  WorkspaceFolder = 3,
}

// ---- Globs (enough for the patterns the host uses) ----------------------------------------

function expandBraces(glob: string): string[] {
  const open = glob.indexOf('{');
  if (open < 0) {
    return [glob];
  }
  let depth = 0;
  let close = -1;
  const parts: string[] = [];
  let start = open + 1;
  for (let i = open; i < glob.length; i++) {
    if (glob[i] === '{') {
      depth++;
    } else if (glob[i] === '}') {
      depth--;
      if (depth === 0) {
        parts.push(glob.slice(start, i));
        close = i;
        break;
      }
    } else if (glob[i] === ',' && depth === 1) {
      parts.push(glob.slice(start, i));
      start = i + 1;
    }
  }
  const head = glob.slice(0, open);
  const tail = glob.slice(close + 1);
  return parts.flatMap((p) => expandBraces(head + p + tail));
}

export function globToRegExp(glob: string): RegExp {
  const alternatives = expandBraces(glob).map((g) => {
    let re = '';
    for (let i = 0; i < g.length; i++) {
      const c = g[i];
      if (c === '*' && g[i + 1] === '*') {
        if (g[i + 2] === '/') {
          re += '(?:.*/)?';
          i += 2;
        } else {
          re += '.*';
          i += 1;
        }
      } else if (c === '*') {
        re += '[^/]*';
      } else if (c === '?') {
        re += '[^/]';
      } else {
        re += c.replace(/[.+^$()|[\]\\]/g, '\\$&');
      }
    }
    return re;
  });
  return new RegExp(`^(?:${alternatives.join('|')})$`);
}

function walk(dir: string, out: string[]): void {
  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      walk(full, out);
    } else if (entry.isFile()) {
      out.push(full);
    }
  }
}

// ---- Fakes -----------------------------------------------------------------------------

export class FakeWebview {
  cspSource = 'vscode-webview-resource:';
  posted: unknown[] = [];
  htmlSets = 0;
  private htmlValue = '';
  readonly messages = new EventEmitter<unknown>();
  onDidReceiveMessage = this.messages.event;
  set html(value: string) {
    this.htmlValue = value;
    this.htmlSets++;
  }
  get html(): string {
    return this.htmlValue;
  }
  postMessage(message: unknown): Promise<boolean> {
    this.posted.push(JSON.parse(JSON.stringify(message)));
    return Promise.resolve(true);
  }
  asWebviewUri(uri: Uri): Uri {
    return uri;
  }
  /** Simulate a message from the webview script. */
  send(message: unknown): void {
    this.messages.fire(message);
  }
}

export class FakeWebviewPanel {
  readonly webview = new FakeWebview();
  iconPath: unknown;
  disposed = false;
  reveals: unknown[][] = [];
  readonly disposeEmitter = new EventEmitter<void>();
  readonly viewStateEmitter = new EventEmitter<{ webviewPanel: FakeWebviewPanel }>();
  onDidDispose = this.disposeEmitter.event;
  onDidChangeViewState = this.viewStateEmitter.event;
  active = true;
  constructor(
    readonly viewType: string,
    public title: string,
    public viewColumn: number,
    readonly options: Record<string, unknown>,
  ) {}
  reveal(...args: unknown[]): void {
    this.reveals.push(args);
  }
  dispose(): void {
    if (!this.disposed) {
      this.disposed = true;
      this.disposeEmitter.fire();
    }
  }
}

export interface ProgressRecord {
  options: { title?: string; cancellable?: boolean };
  cancel(): void;
  done: boolean;
}

type MessageResponder = (message: string, items: string[]) => string | undefined;
type QuickPickResponder = (items: Record<string, unknown>[], options: Record<string, unknown> | undefined) => unknown;

function createState() {
  return {
    panels: [] as FakeWebviewPanel[],
    messages: [] as { level: 'error' | 'warning' | 'info'; text: string; items: string[] }[],
    statusBar: [] as string[],
    progress: [] as ProgressRecord[],
    quickPicks: [] as { items: Record<string, unknown>[]; options: Record<string, unknown> | undefined }[],
    shown: [] as { uri: Uri; options: Record<string, unknown> }[],
    clipboard: [] as string[],
    opened: [] as string[],
    decorationTypes: [] as { options: Record<string, unknown>; disposed: boolean }[],
    watchers: [] as { glob: string; create: EventEmitter<Uri>; delete: EventEmitter<Uri> }[],
    findFilesCalls: [] as { include: string; base: string | undefined; exclude: string | undefined }[],
    commands: new Map<string, (...args: unknown[]) => unknown>(),
    config: new Map<string, unknown>(),
    configUpdates: [] as { key: string; value: unknown; target: unknown }[],
    workspaceFolders: [] as { uri: Uri; name: string; index: number }[],
    tabGroups: [] as { viewColumn: number; tabs: { input: unknown }[] }[],
    respond: (() => undefined) as MessageResponder,
    pick: (() => undefined) as QuickPickResponder,
    activeTextEditor: undefined as unknown,
  };
}

export const stub = createState();

const selectionEmitter = new EventEmitter<{ textEditor: unknown }>();
const activeEditorEmitter = new EventEmitter<unknown>();
const documentEmitter = new EventEmitter<unknown>();
const configEmitter = new EventEmitter<{ affectsConfiguration(section: string): boolean }>();

const foldersEmitter = new EventEmitter<void>();

export const stubEvents = { foldersEmitter, selectionEmitter, activeEditorEmitter, documentEmitter, configEmitter };

/** Fresh state for each test; listeners registered by earlier tests are dropped. */
export function resetStub(): void {
  Object.assign(stub, createState());
  selectionEmitter.dispose();
  activeEditorEmitter.dispose();
  documentEmitter.dispose();
  configEmitter.dispose();
  foldersEmitter.dispose();
}

export function setWorkspaceFolders(...folders: string[]): void {
  stub.workspaceFolders = folders.map((f, index) => ({ uri: Uri.file(f), name: path.basename(f), index }));
}

function folderOf(fsPath: string) {
  let best: (typeof stub.workspaceFolders)[number] | undefined;
  for (const folder of stub.workspaceFolders) {
    const rel = path.relative(folder.uri.fsPath, fsPath);
    if (!rel.startsWith('..') && !path.isAbsolute(rel) && (!best || folder.uri.fsPath.length > best.uri.fsPath.length)) {
      best = folder;
    }
  }
  return best;
}

export const window = {
  get activeTextEditor(): unknown {
    return stub.activeTextEditor;
  },
  get tabGroups() {
    return { all: stub.tabGroups };
  },
  createWebviewPanel(viewType: string, title: string, column: number, options: Record<string, unknown>): FakeWebviewPanel {
    const panel = new FakeWebviewPanel(viewType, title, column, options);
    stub.panels.push(panel);
    return panel;
  },
  async withProgress<R>(
    options: { title?: string; cancellable?: boolean },
    task: (progress: { report(): void }, token: { isCancellationRequested: boolean; onCancellationRequested: EventEmitter<void>['event'] }) => Promise<R>,
  ): Promise<R> {
    const cancelEmitter = new EventEmitter<void>();
    const token = { isCancellationRequested: false, onCancellationRequested: cancelEmitter.event };
    const record: ProgressRecord = {
      options,
      done: false,
      cancel: () => {
        token.isCancellationRequested = true;
        cancelEmitter.fire();
      },
    };
    stub.progress.push(record);
    try {
      return await task({ report() {} }, token);
    } finally {
      record.done = true;
    }
  },
  showErrorMessage(text: string, ...items: string[]): Promise<string | undefined> {
    stub.messages.push({ level: 'error', text, items });
    return Promise.resolve(stub.respond(text, items));
  },
  showWarningMessage(text: string, ...items: string[]): Promise<string | undefined> {
    stub.messages.push({ level: 'warning', text, items });
    return Promise.resolve(stub.respond(text, items));
  },
  showInformationMessage(text: string, ...items: string[]): Promise<string | undefined> {
    stub.messages.push({ level: 'info', text, items });
    return Promise.resolve(stub.respond(text, items));
  },
  showQuickPick(items: Record<string, unknown>[], options?: Record<string, unknown>): Promise<unknown> {
    stub.quickPicks.push({ items, options });
    return Promise.resolve(stub.pick(items, options));
  },
  setStatusBarMessage(text: string): Disposable {
    stub.statusBar.push(text);
    return new Disposable(() => {});
  },
  showTextDocument(uri: Uri, options: Record<string, unknown>): Promise<unknown> {
    stub.shown.push({ uri, options });
    return Promise.resolve({});
  },
  createTextEditorDecorationType(options: Record<string, unknown>) {
    const record = { options, disposed: false };
    stub.decorationTypes.push(record);
    return { key: `decoration${stub.decorationTypes.length}`, dispose: () => (record.disposed = true) };
  },
  onDidChangeTextEditorSelection: selectionEmitter.event,
  onDidChangeActiveTextEditor: activeEditorEmitter.event,
};

export const workspace = {
  get workspaceFolders() {
    return stub.workspaceFolders.length > 0 ? stub.workspaceFolders : undefined;
  },
  getWorkspaceFolder(uri: Uri) {
    return folderOf(uri.fsPath);
  },
  async findFiles(include: string | RelativePattern, exclude?: string | null): Promise<Uri[]> {
    const pattern = typeof include === 'string' ? include : include.pattern;
    const bases = typeof include === 'string' ? stub.workspaceFolders.map((f) => f.uri.fsPath) : [include.base];
    stub.findFilesCalls.push({ include: pattern, base: typeof include === 'string' ? undefined : include.base, exclude: exclude ?? undefined });
    const includeRe = globToRegExp(pattern);
    const excludeRe = exclude ? globToRegExp(exclude) : undefined;
    const result: Uri[] = [];
    for (const base of bases) {
      const files: string[] = [];
      walk(base, files);
      for (const file of files.sort()) {
        // Like VS Code, `exclude` is matched against the path relative to the workspace folder.
        const folder = folderOf(file);
        const rel = path.relative(base, file).split(path.sep).join('/');
        const relToFolder = path.relative(folder ? folder.uri.fsPath : base, file).split(path.sep).join('/');
        if (includeRe.test(rel) && !(excludeRe && excludeRe.test(relToFolder))) {
          result.push(Uri.file(file));
        }
      }
    }
    return result;
  },
  asRelativePath(pathOrUri: string | Uri, includeWorkspaceFolder?: boolean): string {
    const fsPath = typeof pathOrUri === 'string' ? pathOrUri : pathOrUri.fsPath;
    const folder = folderOf(fsPath);
    if (!folder) {
      return fsPath;
    }
    const rel = path.relative(folder.uri.fsPath, fsPath).split(path.sep).join('/');
    const withFolder = includeWorkspaceFolder ?? stub.workspaceFolders.length > 1;
    return withFolder ? `${folder.name}/${rel}` : rel;
  },
  getConfiguration(section: string) {
    return {
      get<T>(key: string, defaultValue?: T): T | undefined {
        const full = `${section}.${key}`;
        return stub.config.has(full) ? (stub.config.get(full) as T) : defaultValue;
      },
      async update(key: string, value: unknown, target: unknown): Promise<void> {
        const full = `${section}.${key}`;
        stub.config.set(full, value);
        stub.configUpdates.push({ key: full, value, target });
        configEmitter.fire({ affectsConfiguration: (s: string) => full === s || full.startsWith(`${s}.`) });
      },
    };
  },
  createFileSystemWatcher(glob: string) {
    const record = { glob, create: new EventEmitter<Uri>(), delete: new EventEmitter<Uri>() };
    stub.watchers.push(record);
    const change = new EventEmitter<Uri>();
    return { onDidCreate: record.create.event, onDidDelete: record.delete.event, onDidChange: change.event, dispose() {} };
  },
  onDidChangeWorkspaceFolders: foldersEmitter.event,
  onDidChangeTextDocument: documentEmitter.event,
  onDidChangeConfiguration: configEmitter.event,
};

export const commands = {
  registerCommand(id: string, fn: (...args: unknown[]) => unknown): Disposable {
    stub.commands.set(id, fn);
    return new Disposable(() => stub.commands.delete(id));
  },
  executeCommand(id: string, ...args: unknown[]): Promise<unknown> {
    const fn = stub.commands.get(id);
    if (!fn) {
      return Promise.reject(new Error(`command '${id}' not found`));
    }
    return Promise.resolve(fn(...args));
  },
};

export const env = {
  clipboard: {
    writeText(text: string): Promise<void> {
      stub.clipboard.push(text);
      return Promise.resolve();
    },
    readText(): Promise<string> {
      return Promise.resolve(stub.clipboard.at(-1) ?? '');
    },
  },
  openExternal(uri: Uri): Promise<boolean> {
    stub.opened.push(uri.toString());
    return Promise.resolve(true);
  },
};
