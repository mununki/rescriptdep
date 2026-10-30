// Project resolution (QA-2/3/4, ARCH-7) and module source lookup for Open Module (ARCH §2.4).
import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import type { DotNode } from './core/dot';
import { compareIds } from './core/dot';
import {
  SOURCE_EXTENSIONS,
  SecondLineContext,
  isExternalPath,
  isInside,
  isSourceFile,
  matchModuleFiles,
  moduleNameFromFile,
  secondLine,
  secondLineContext,
} from './core/paths';

export interface ProjectRef {
  /** Directory holding `bsconfig.json` or `rescript.json`. */
  projectRoot: string;
  /** `<projectRoot>/lib/bs`, checked to exist. */
  bsDir: string;
}

/** Host-side facts about one graph node. The webview only ever sees `external`, `openable`, `secondary`. */
export interface ModuleSource {
  file?: string;
  openable: boolean;
  external: boolean;
  secondary?: string;
}

interface ModuleIndex {
  files: string[];
  secondLine: SecondLineContext;
}

const CONFIG_FILES = ['bsconfig.json', 'rescript.json'];
const SOURCE_GLOB = `**/*.{${SOURCE_EXTENSIONS.map((e) => e.slice(1)).join(',')}}`;
const IMPLEMENTATION_EXTENSIONS = new Set(['.res', '.re', '.ml']);
// Build output keeps copies of the sources (`lib/ocaml/*.res`), so it is never part of a module list.
const WORKSPACE_EXCLUDE = '{**/node_modules/**,**/lib/bs/**,**/lib/ocaml/**}';
export const NO_BUILD_OUTPUT_MESSAGE = 'No compiled output found in lib/bs. Build the project (e.g. rescript build) and try again.';

async function exists(p: string): Promise<boolean> {
  try {
    await fs.promises.access(p);
    return true;
  } catch {
    return false;
  }
}

async function hasConfig(dir: string): Promise<boolean> {
  for (const name of CONFIG_FILES) {
    if (await exists(path.join(dir, name))) {
      return true;
    }
  }
  return false;
}

/**
 * Nearest directory with `bsconfig.json`/`rescript.json`, from the file's folder upwards, never above
 * `boundary` (the file's workspace folder) when one is given. Containment uses `path.relative`.
 */
export async function findProjectRootForFile(filePath: string, boundary?: string): Promise<string | undefined> {
  let dir = path.dirname(path.resolve(filePath));
  for (;;) {
    if (boundary !== undefined && !isInside(dir, boundary)) {
      return undefined;
    }
    if (await hasConfig(dir)) {
      return dir;
    }
    const parent = path.dirname(dir);
    if (parent === dir || (boundary !== undefined && path.relative(boundary, dir) === '')) {
      return undefined;
    }
    dir = parent;
  }
}

/** The innermost workspace folder (file scheme) containing `file`. */
export function workspaceFolderPath(file: string): string | undefined {
  let best: string | undefined;
  for (const folder of vscode.workspace.workspaceFolders ?? []) {
    if (folder.uri.scheme !== undefined && folder.uri.scheme !== 'file') {
      continue;
    }
    const root = folder.uri.fsPath;
    if (isInside(file, root) && (best === undefined || root.length > best.length)) {
      best = root;
    }
  }
  return best;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null;
}

/** A `file:` URI passed as a command argument (the `editor/title` button passes the editor's resource). */
function fileUriArg(arg: unknown): vscode.Uri | undefined {
  if (!isRecord(arg) || typeof arg.fsPath !== 'string') {
    return undefined;
  }
  const scheme = arg.scheme;
  return scheme === undefined || scheme === 'file' ? (arg as unknown as vscode.Uri) : undefined;
}

interface ModulePick extends vscode.QuickPickItem {
  file: string;
}

export class ProjectService implements vscode.Disposable {
  private configDirsPromise: Promise<string[]> | undefined;
  private readonly indexes = new Map<string, Promise<ModuleIndex>>();
  private readonly rootsByDir = new Map<string, Promise<string | undefined>>();
  private readonly disposables: vscode.Disposable[] = [];

  constructor() {
    const configWatcher = vscode.workspace.createFileSystemWatcher(`**/{${CONFIG_FILES.join(',')}}`, false, true, false);
    const sourceWatcher = vscode.workspace.createFileSystemWatcher(SOURCE_GLOB, false, true, false);
    const onConfig = () => {
      this.configDirsPromise = undefined;
      this.rootsByDir.clear();
      this.indexes.clear();
    };
    const onSource = (uri: vscode.Uri) => {
      for (const root of [...this.indexes.keys()]) {
        if (isInside(uri.fsPath, root)) {
          this.indexes.delete(root);
        }
      }
    };
    this.disposables.push(
      vscode.workspace.onDidChangeWorkspaceFolders(onConfig),
      configWatcher,
      sourceWatcher,
      configWatcher.onDidCreate(onConfig),
      configWatcher.onDidDelete(onConfig),
      sourceWatcher.onDidCreate(onSource),
      sourceWatcher.onDidDelete(onSource),
    );
  }

  dispose(): void {
    for (const d of this.disposables) {
      d.dispose();
    }
  }

  /** Project root of a file inside its own workspace folder (or anywhere above it when it has none). Cached per folder. */
  projectRootForFile(file: string): Promise<string | undefined> {
    const dir = path.dirname(file);
    let root = this.rootsByDir.get(dir);
    if (!root) {
      root = findProjectRootForFile(file, workspaceFolderPath(file));
      this.rootsByDir.set(dir, root);
    }
    return root;
  }

  /** `lib/bs` must exist; otherwise one actionable error and no CLI run (DES-10). */
  async ensureBuilt(projectRoot: string): Promise<ProjectRef | undefined> {
    const bsDir = path.join(projectRoot, 'lib', 'bs');
    try {
      if ((await fs.promises.stat(bsDir)).isDirectory()) {
        return { projectRoot, bsDir };
      }
    } catch {
      // reported below
    }
    void vscode.window.showErrorMessage(NO_BUILD_OUTPUT_MESSAGE);
    return undefined;
  }

  /** Every ReScript project in the workspace (one `findFiles`, cached until a config file is created or deleted). */
  configDirs(): Promise<string[]> {
    this.configDirsPromise ??= (async () => {
      const uris = await vscode.workspace.findFiles(`**/{${CONFIG_FILES.join(',')}}`, '**/node_modules/**');
      return [...new Set(uris.map((u) => path.dirname(u.fsPath)))].sort(compareIds);
    })();
    return this.configDirsPromise;
  }

  /**
   * Project for the full-graph and unused-modules commands: the active `.res` editor's project,
   * else the only project, else a QuickPick of projects (monorepos).
   */
  async resolveForGraphCommand(): Promise<ProjectRef | undefined> {
    const editorFile = this.activeSourceFile();
    if (editorFile) {
      const root = await this.projectRootForFile(editorFile);
      if (root) {
        return this.ensureBuilt(root);
      }
    }
    const dirs = await this.configDirs();
    if (dirs.length === 0) {
      void vscode.window.showErrorMessage("Couldn't find a bsconfig.json or rescript.json in this workspace.");
      return undefined;
    }
    let root = dirs[0];
    if (dirs.length > 1) {
      const picked = await vscode.window.showQuickPick(
        dirs.map((dir) => {
          const folder = vscode.workspace.workspaceFolders?.find(
            (folder) => folder.uri.scheme === 'file' && path.relative(folder.uri.fsPath, dir) === '',
          );
          return { label: folder?.name ?? path.basename(dir), description: vscode.workspace.asRelativePath(dir), dir };
        }),
        { placeHolder: 'Select a ReScript project to analyze', title: 'Monorepo Projects', matchOnDescription: true },
      );
      if (!picked) {
        return undefined;
      }
      root = picked.dir;
    }
    return this.ensureBuilt(root);
  }

  /**
   * Module and project for "Focus On Module Dependencies": the URI passed by the editor-title button,
   * else the active editor, else a QuickPick of the workspace's modules.
   */
  async resolveFocusTarget(arg?: unknown): Promise<{ project: ProjectRef; module: string } | undefined> {
    const passed = fileUriArg(arg);
    let file = passed && isSourceFile(passed.fsPath) ? passed.fsPath : this.activeSourceFile();
    if (!file) {
      const picked = await this.pickWorkspaceModule();
      if (!picked) {
        return undefined;
      }
      file = picked;
    }
    const root = await this.projectRootForFile(file);
    if (!root) {
      void vscode.window.showErrorMessage(`${path.basename(file)} isn't part of a ReScript project (no bsconfig.json or rescript.json found).`);
      return undefined;
    }
    const project = await this.ensureBuilt(root);
    return project && { project, module: moduleNameFromFile(file) };
  }

  /** QuickPick over one project's modules; returns the module name. */
  async pickModule(project: ProjectRef): Promise<string | undefined> {
    const index = await this.moduleIndex(project.projectRoot);
    const picked = await this.showModulePick(index.files);
    return picked && moduleNameFromFile(picked);
  }

  private async pickWorkspaceModule(): Promise<string | undefined> {
    const uris = await vscode.workspace.findFiles('**/*.{res,re}', WORKSPACE_EXCLUDE);
    if (uris.length === 0) {
      void vscode.window.showErrorMessage('No ReScript modules found in this workspace.');
      return undefined;
    }
    return this.showModulePick(uris.map((u) => u.fsPath));
  }

  private async showModulePick(files: readonly string[]): Promise<string | undefined> {
    const items: ModulePick[] = files
      .filter((f) => IMPLEMENTATION_EXTENSIONS.has(path.extname(f).toLowerCase()))
      .map((file) => ({ label: moduleNameFromFile(file), description: vscode.workspace.asRelativePath(file), file }))
      .sort((a, b) => compareIds(a.label, b.label) || compareIds(a.description ?? '', b.description ?? ''));
    const picked = await vscode.window.showQuickPick(items, {
      placeHolder: 'Select a module to focus on',
      title: 'Focus on Module',
      matchOnDescription: true,
    });
    return picked?.file;
  }

  private activeSourceFile(): string | undefined {
    const doc = vscode.window.activeTextEditor?.document;
    if (!doc || (doc.uri !== undefined && doc.uri.scheme !== undefined && doc.uri.scheme !== 'file')) {
      return undefined;
    }
    return isSourceFile(doc.fileName) ? doc.fileName : undefined;
  }

  /** One `findFiles` per project per session, invalidated when a source file is created or deleted. */
  moduleIndex(projectRoot: string): Promise<ModuleIndex> {
    let index = this.indexes.get(projectRoot);
    if (!index) {
      index = (async () => {
        let files: string[] = [];
        try {
          const uris = await vscode.workspace.findFiles(new vscode.RelativePattern(projectRoot, SOURCE_GLOB), '**/node_modules/**');
          const buildDir = path.join(projectRoot, 'lib');
          files = uris.map((u) => u.fsPath).filter((f) => !isInside(f, buildDir) && !isExternalPath(f));
        } catch {
          // An empty index only costs the second line and resolution of path-less modules.
        }
        return { files, secondLine: secondLineContext(files) };
      })();
      this.indexes.set(projectRoot, index);
    }
    return index;
  }

  /** A path the host may open: inside the project or a workspace folder. */
  isAllowedPath(file: string, projectRoot: string): boolean {
    return isInside(file, projectRoot) || workspaceFolderPath(file) !== undefined;
  }

  /** Re-checked right before opening: an existing file in an allowed location. */
  async isOpenableFile(file: string, projectRoot: string): Promise<boolean> {
    if (!isSourceFile(file) || !this.isAllowedPath(file, projectRoot)) {
      return false;
    }
    try {
      return (await fs.promises.stat(file)).isFile();
    } catch {
      return false;
    }
  }

  /**
   * Source file, `openable`, `external` and second line for each node (once per render):
   * 1. a tooltip naming a source file is used as is;
   * 2. a tooltip naming a directory (an external package) is searched case-insensitively for `<Module>.res|.resi`;
   * 3. no tooltip (`--no-dependents`, unresolved) → the project's module index, case-insensitive.
   */
  async resolveSources(projectRoot: string, nodes: readonly DotNode[]): Promise<Map<string, ModuleSource>> {
    const index = await this.moduleIndex(projectRoot);
    const listings = new Map<string, Promise<string[]>>();
    const listDir = (dir: string): Promise<string[]> => {
      let listing = listings.get(dir);
      if (!listing) {
        listing = fs.promises.readdir(dir).then(
          (names) => names.map((n) => path.join(dir, n)),
          () => [],
        );
        listings.set(dir, listing);
      }
      return listing;
    };
    const result = new Map<string, ModuleSource>();
    await Promise.all(
      nodes.map(async (node) => {
        const tooltip = node.tooltip;
        let file: string | undefined;
        if (tooltip !== undefined && isSourceFile(tooltip)) {
          file = tooltip;
        } else if (tooltip !== undefined) {
          file = matchModuleFiles(node.id, await listDir(tooltip)).find((f) => /\.resi?$/i.test(f));
        } else {
          file = matchModuleFiles(node.id, index.files)[0];
        }
        const located = file ?? tooltip;
        const source: ModuleSource = {
          openable: file !== undefined && this.isAllowedPath(file, projectRoot),
          external: isExternalPath(located),
        };
        if (file !== undefined) {
          source.file = file;
        }
        const secondary = secondLine(located, index.secondLine);
        if (secondary !== undefined) {
          source.secondary = secondary;
        }
        result.set(node.id, source);
      }),
    );
    return result;
  }
}
