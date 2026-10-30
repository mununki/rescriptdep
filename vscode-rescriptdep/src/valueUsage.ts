// Inline "Used N times" decoration on value bindings (ARCH-4/5, FE-10, QA-8, D10).
import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import type { CliRunner } from './cli';
import type { ProjectService } from './project';
import { matchBinding } from './core/binding';
import { formatUsage, parseUsageJson, unavailableReason, usageCacheKey } from './core/usage';
import { moduleNameFromFile } from './core/paths';

export const VALUE_USAGE_SECTION = 'bibimbob';
export const VALUE_USAGE_SETTING = 'valueUsageCount.enabled';
const DEBOUNCE_MS = 350;
export const MAX_USAGE_CACHE = 256;

interface Decorated {
  editor: vscode.TextEditor;
  line: number;
}

interface InFlight {
  document: vscode.TextDocument;
  line: number;
  abort: AbortController;
}

async function mtime(p: string): Promise<number | undefined> {
  try {
    return (await fs.promises.stat(p)).mtimeMs;
  } catch {
    return undefined;
  }
}

function isResFile(document: vscode.TextDocument): boolean {
  return document.languageId === 'rescript' && document.fileName.endsWith('.res');
}

export class ValueUsageController implements vscode.Disposable {
  /** One decoration type for the session; the text goes in `renderOptions.after.contentText`. */
  private readonly decoration: vscode.TextEditorDecorationType;
  private readonly disposables: vscode.Disposable[] = [];
  /** Keyed by `usageCacheKey`; `null` = the CLI reported only negative codes (nothing to show). */
  private readonly cache = new Map<string, string | null>();
  private enabled: boolean;
  private timer: ReturnType<typeof setTimeout> | undefined;
  private inFlight: InFlight | undefined;
  private decorated: Decorated | undefined;
  private reasonShown = false;

  constructor(
    private readonly cli: CliRunner,
    private readonly projects: ProjectService,
  ) {
    this.decoration = vscode.window.createTextEditorDecorationType({
      isWholeLine: true,
      after: {
        color: new vscode.ThemeColor('editorCodeLens.foreground'),
        margin: '0 0 0 8px',
        fontStyle: 'italic',
      },
    });
    this.enabled = this.readSetting();
    this.disposables.push(
      this.decoration,
      vscode.window.onDidChangeTextEditorSelection((e) => this.onSelection(e.textEditor)),
      vscode.window.onDidChangeActiveTextEditor(() => this.reset()),
      vscode.workspace.onDidChangeTextDocument((e) => this.onEdit(e)),
      vscode.workspace.onDidChangeConfiguration((e) => {
        if (e.affectsConfiguration(`${VALUE_USAGE_SECTION}.${VALUE_USAGE_SETTING}`)) {
          this.setEnabled(this.readSetting());
        }
      }),
    );
  }

  dispose(): void {
    this.reset();
    for (const d of this.disposables) {
      d.dispose();
    }
  }

  private readSetting(): boolean {
    return vscode.workspace.getConfiguration(VALUE_USAGE_SECTION).get<boolean>(VALUE_USAGE_SETTING, true) !== false;
  }

  private setEnabled(enabled: boolean): void {
    this.enabled = enabled;
    if (!enabled) {
      this.reset();
    }
  }

  /** The toggle command: persist the setting (Global) and confirm in the status bar, not with a toast. */
  async toggle(): Promise<void> {
    const next = !this.enabled;
    this.setEnabled(next);
    await vscode.workspace.getConfiguration(VALUE_USAGE_SECTION).update(VALUE_USAGE_SETTING, next, vscode.ConfigurationTarget.Global);
    vscode.window.setStatusBarMessage(`Bibimbob: inline usage count ${next ? 'on' : 'off'}`, 2500);
    if (next && vscode.window.activeTextEditor) {
      this.onSelection(vscode.window.activeTextEditor);
    }
  }

  /** Drop the decoration, the pending timer and any running CLI. */
  private reset(): void {
    this.clearTimer();
    this.abortInFlight();
    this.clearDecoration();
  }

  private clearTimer(): void {
    if (this.timer !== undefined) {
      clearTimeout(this.timer);
      this.timer = undefined;
    }
  }

  private abortInFlight(): void {
    this.inFlight?.abort.abort();
    this.inFlight = undefined;
  }

  private clearDecoration(): void {
    if (this.decorated) {
      this.decorated.editor.setDecorations(this.decoration, []);
      this.decorated = undefined;
    }
  }

  private onSelection(editor: vscode.TextEditor): void {
    if (!this.enabled) {
      return;
    }
    const document = editor.document;
    if (!isResFile(document)) {
      return;
    }
    const line = editor.selection.active.line;
    if (line >= document.lineCount) {
      return;
    }
    const name = matchBinding(document.lineAt(line).text);
    if (this.decorated && (this.decorated.editor !== editor || this.decorated.line !== line || !name)) {
      this.clearDecoration();
    }
    // At most one -vb process: leaving the line aborts it.
    if (this.inFlight && (this.inFlight.document !== document || this.inFlight.line !== line || !name)) {
      this.abortInFlight();
    }
    this.clearTimer();
    if (!name || document.isDirty) {
      // The .cmt files describe the saved, compiled file; a dirty buffer's lines may not match them.
      return;
    }
    if (this.decorated?.editor === editor && this.decorated.line === line) {
      return;
    }
    this.timer = setTimeout(() => {
      this.timer = undefined;
      void this.update(editor, line, name);
    }, DEBOUNCE_MS);
  }

  /** Clear when an edit touches the decorated line; edits above it only move it. */
  private onEdit(event: vscode.TextDocumentChangeEvent): void {
    const decorated = this.decorated;
    if (!decorated || event.document !== decorated.editor.document) {
      return;
    }
    let line = decorated.line;
    for (const change of event.contentChanges) {
      const start = change.range.start.line;
      const end = change.range.end.line;
      if (start <= line && line <= end) {
        this.clearDecoration();
        return;
      }
      if (end < line) {
        const added = change.text.split('\n').length - 1;
        line += added - (end - start);
      }
    }
    decorated.line = line;
  }

  private stillCurrent(editor: vscode.TextEditor, line: number, name: string): boolean {
    const document = editor.document;
    return (
      this.enabled &&
      vscode.window.activeTextEditor === editor &&
      !document.isDirty &&
      editor.selection.active.line === line &&
      line < document.lineCount &&
      matchBinding(document.lineAt(line).text) === name
    );
  }

  private async update(editor: vscode.TextEditor, line: number, name: string): Promise<void> {
    if (!this.stillCurrent(editor, line, name)) {
      return;
    }
    const document = editor.document;
    const file = document.fileName;
    // The file's own workspace folder bounds the project search (multi-root, QA-4).
    const projectRoot = await this.projects.projectRootForFile(file);
    if (!projectRoot) {
      return;
    }
    const bsDir = path.join(projectRoot, 'lib', 'bs');
    const [compilerLogMtime, sourceMtime, bsMtime] = await Promise.all([
      mtime(path.join(bsDir, '.compiler.log')),
      mtime(file),
      mtime(bsDir),
    ]);
    if (bsMtime === undefined) {
      return;
    }
    const key = usageCacheKey({
      file,
      line: line + 1,
      name,
      compilerLogMtime,
      fallback: { projectRoot, sourceMtime: sourceMtime ?? 0, bsMtime, documentVersion: document.version },
    });

    let text: string | null | undefined = this.cache.get(key);
    if (text === undefined) {
      // Project discovery and stat calls can outlive the cursor selection.
      if (!this.stillCurrent(editor, line, name)) {
        return;
      }
      this.abortInFlight();
      const abort = new AbortController();
      const inFlight: InFlight = { document, line, abort };
      this.inFlight = inFlight;
      const args = ['-m', moduleNameFromFile(file), '-vb', name, '-vl', String(line + 1), '-f', 'json', bsDir];
      let output: string;
      try {
        output = await this.cli.run(args, { signal: abort.signal });
      } catch {
        // Cancelled, timed out, failed or missing (reported once by the CLI runner): show nothing.
        return;
      } finally {
        if (this.inFlight === inFlight) {
          this.inFlight = undefined;
        }
      }
      if (abort.signal.aborted) {
        return;
      }
      const result = parseUsageJson(output);
      if (!result) {
        return;
      }
      text = formatUsage(result) ?? null;
      this.cache.set(key, text);
      if (this.cache.size > MAX_USAGE_CACHE) {
        this.cache.delete(this.cache.keys().next().value!);
      }
      if (result.unavailable && !this.reasonShown) {
        this.reasonShown = true;
        vscode.window.setStatusBarMessage(unavailableReason(result.codes), 8000);
      }
    }

    if (text === null || !this.stillCurrent(editor, line, name)) {
      return;
    }
    if (this.decorated && this.decorated.editor !== editor) {
      this.clearDecoration();
    }
    editor.setDecorations(this.decoration, [
      { range: new vscode.Range(line, 0, line, 0), renderOptions: { after: { contentText: text } } },
    ]);
    this.decorated = { editor, line };
  }
}
