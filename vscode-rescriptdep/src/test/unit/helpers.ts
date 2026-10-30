// Shared helpers for the unit tests (bundled with the `vscode` stub).
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import type { CliRunOptions, CliRunner } from '../../cli';
import { CliError } from '../../core/cliError';
import { Uri } from './stub/vscode';

/** Creates a temp directory with the given files (`content` or `null` for an empty directory). */
export function makeTree(files: Record<string, string | null>): string {
  const root = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), 'bibimbob-test-')));
  for (const [rel, content] of Object.entries(files)) {
    const full = path.join(root, rel);
    if (content === null) {
      fs.mkdirSync(full, { recursive: true });
    } else {
      fs.mkdirSync(path.dirname(full), { recursive: true });
      fs.writeFileSync(full, content);
    }
  }
  return root;
}

/** A ReScript project skeleton: config, `lib/bs` and the given source files. */
export function projectFiles(prefix: string, sources: string[], config = 'rescript.json'): Record<string, string | null> {
  const files: Record<string, string | null> = {
    [path.join(prefix, config)]: '{}',
    [path.join(prefix, 'lib', 'bs')]: null,
  };
  for (const src of sources) {
    files[path.join(prefix, src)] = 'let x = 1\n';
  }
  return files;
}

/** DOT in the CLI's shape. `tooltips` maps module → path (omit for `--no-dependents`-style nodes). */
export function cliDot(modules: string[], edges: [string, string][], tooltips: Record<string, string> = {}, cycles: string[][] = []): string {
  const lines = ['digraph dependencies {', '  rankdir=LR;', '  node [shape=box, style=filled, fillcolor=lightblue];', ''];
  const esc = (s: string) => s.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
  for (const m of [...modules].sort()) {
    const tip = tooltips[m];
    lines.push(`  "${esc(m)}" [label="${esc(m)}"${tip === undefined ? '' : `, tooltip="${esc(tip)}"`}];`);
  }
  lines.push('');
  for (const [a, b] of edges) {
    lines.push(`  "${esc(a)}" -> "${esc(b)}";`);
  }
  if (cycles.length > 0) {
    lines.push('', '  /* Cycles */');
    cycles.forEach((c, i) => {
      lines.push(`  subgraph cluster_${i} {`, '    style=filled;', '    color=pink;', '    label="Cyclic dependency";');
      for (const m of c) {
        lines.push(`    "${esc(m)}";`);
      }
      lines.push('  }');
    });
  }
  lines.push('}', '');
  return lines.join('\n');
}

export function ndDot(modules: string[]): string {
  return ['digraph G {', '  rankdir=LR;', '  node [shape=box];', ...modules.map((m) => `  "${m}";`), '}', ''].join('\n');
}

export interface FakeCall {
  args: string[];
  aborted: boolean;
  settled: boolean;
}

/** Records every run; answers from `respond` after `delayMs`; honours the abort signal like `execFile`. */
export class FakeCli implements CliRunner {
  calls: FakeCall[] = [];
  delayMs = 0;
  respond: (args: string[]) => string | Error = () => '';

  run(args: string[], options: CliRunOptions = {}): Promise<string> {
    const call: FakeCall = { args, aborted: false, settled: false };
    this.calls.push(call);
    return new Promise((resolve, reject) => {
      const abort = () => {
        clearTimeout(timer);
        call.aborted = true;
        call.settled = true;
        reject(new CliError('cancelled', 'cancelled'));
      };
      const timer = setTimeout(() => {
        options.signal?.removeEventListener('abort', abort);
        call.settled = true;
        const out = this.respond(args);
        if (out instanceof Error) {
          reject(out);
        } else {
          resolve(out);
        }
      }, this.delayMs);
      if (options.signal?.aborted) {
        abort();
      } else {
        options.signal?.addEventListener('abort', abort, { once: true });
      }
    });
  }

  /** Runs still waiting for their result. */
  get live(): number {
    return this.calls.filter((c) => !c.settled).length;
  }
}

export interface FakeDocument {
  languageId: string;
  fileName: string;
  uri: Uri;
  version: number;
  isDirty: boolean;
  lines: string[];
  readonly lineCount: number;
  lineAt(i: number): { text: string };
}

export interface FakeEditor {
  document: FakeDocument;
  selection: { active: { line: number } };
  applied: { type: unknown; options: unknown[] }[];
  setDecorations(type: unknown, options: unknown[]): void;
  /** Text of the decoration currently applied, if any. */
  readonly shown: string | undefined;
}

export function fakeEditor(file: string, lines: string[], line = 0): FakeEditor {
  const document: FakeDocument = {
    languageId: 'rescript',
    fileName: file,
    uri: Uri.file(file),
    version: 1,
    isDirty: false,
    lines,
    get lineCount() {
      return this.lines.length;
    },
    lineAt(i: number) {
      return { text: this.lines[i] };
    },
  };
  const editor: FakeEditor = {
    document,
    selection: { active: { line } },
    applied: [],
    setDecorations(type, options) {
      this.applied.push({ type, options });
    },
    get shown() {
      const last = this.applied.at(-1);
      const first = last?.options[0] as { renderOptions?: { after?: { contentText?: string } } } | undefined;
      return first?.renderOptions?.after?.contentText;
    },
  };
  return editor;
}

export const tick = (ms = 0) => new Promise((resolve) => setTimeout(resolve, ms));

export async function waitFor(condition: () => boolean, timeoutMs = 2000): Promise<void> {
  const start = Date.now();
  while (!condition()) {
    if (Date.now() - start > timeoutMs) {
      throw new Error('waitFor: timed out');
    }
    await tick(5);
  }
}

export function fakeContext(extensionPath: string) {
  return { extensionPath, extensionUri: Uri.file(extensionPath), subscriptions: [] as { dispose(): unknown }[] };
}
