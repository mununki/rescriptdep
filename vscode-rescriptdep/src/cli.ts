// Locating and running the rescriptdep CLI (ARCH-3, QA-5, QA-6, FE-15).
import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { CLI_TIMEOUT_MS, CliError, classifyCliError } from './core/cliError';

export const INSTALL_URL = 'https://github.com/mununki/rescriptdep#installation';
const MAX_BUFFER = 64 * 1024 * 1024;

export interface CliRunOptions {
  signal?: AbortSignal;
  timeoutMs?: number;
}

/** What the panel and the value-usage controller need; tests substitute a fake. */
export interface CliRunner {
  /** Resolves with stdout. Rejects with a `CliError` (`cancelled` when `signal` aborts). */
  run(args: string[], options?: CliRunOptions): Promise<string>;
}

/**
 * Runs `command args` (behind `nice` on Unix, so the CLI gets a lower CPU priority).
 * `nice` execs the CLI, so aborting the signal kills the CLI process itself.
 */
export function execCli(command: string, args: string[], options: CliRunOptions = {}): Promise<string> {
  const useNice = os.platform() !== 'win32';
  const file = useNice ? 'nice' : command;
  const finalArgs = useNice ? [command, ...args] : args;
  return new Promise((resolve, reject) => {
    if (options.signal?.aborted) {
      reject(new CliError('cancelled', 'Cancelled'));
      return;
    }
    cp.execFile(
      file,
      finalArgs,
      { maxBuffer: MAX_BUFFER, timeout: options.timeoutMs ?? CLI_TIMEOUT_MS, signal: options.signal, encoding: 'utf8' },
      (error, stdout, stderr) => {
        if (error) {
          const kind = classifyCliError(error);
          reject(new CliError(kind, kind === 'failed' ? `${path.basename(command)} failed: ${error.message}` : kind, String(stderr ?? '')));
          return;
        }
        resolve(String(stdout));
      },
    );
  });
}

export function isDevelopmentMode(): boolean {
  const isDebug = process.env.VSCODE_DEBUG_MODE === 'true';
  const isDevHostPath = __dirname.includes('.vscode-test') || __dirname.includes('.vscode-insiders');
  const devFileExists = fs.existsSync(path.resolve(__dirname, '../../dune-project'));
  return isDebug || isDevHostPath || devFileExists;
}

export function getBundledCLIPath(extensionPath: string, platform: NodeJS.Platform = os.platform()): string {
  const binaryName = platform === 'win32' ? 'rescriptdep.exe' : 'rescriptdep';
  const platformDir = platform === 'win32' ? 'win32' : platform === 'darwin' ? 'darwin' : 'linux';
  return path.join(extensionPath, 'bin', platformDir, binaryName);
}

async function isFile(p: string): Promise<boolean> {
  try {
    return (await fs.promises.stat(p)).isFile();
  } catch {
    return false;
  }
}

/** Looks the binary up on `PATH` without blocking the extension host (no `execSync('which')`). */
export async function findOnPath(name: string, envPath = process.env.PATH ?? ''): Promise<string | undefined> {
  const exe = os.platform() === 'win32' ? `${name}.exe` : name;
  for (const dir of envPath.split(path.delimiter)) {
    if (!dir) {
      continue;
    }
    const candidate = path.join(dir, exe);
    try {
      await fs.promises.access(candidate, fs.constants.X_OK);
      if (await isFile(candidate)) {
        return candidate;
      }
    } catch {
      // not here
    }
  }
  return undefined;
}

/**
 * Resolution order (unchanged): development build → bundled binary → `PATH`.
 * The result is memoized as a promise for the session; the not-found message is shown at most once.
 */
export class RescriptDepCli implements CliRunner {
  private pathPromise: Promise<string | undefined> | undefined;
  private missingShown = false;

  constructor(
    private readonly extensionPath: string,
    private readonly devMode: () => boolean = isDevelopmentMode,
  ) {}

  resolvePath(): Promise<string | undefined> {
    this.pathPromise ??= this.lookup();
    return this.pathPromise;
  }

  private async lookup(): Promise<string | undefined> {
    if (this.devMode()) {
      // The OCaml project root is the parent of the extension directory.
      return path.join(path.resolve(this.extensionPath, '..'), '_build', 'default', 'bin', 'main.exe');
    }
    const bundled = getBundledCLIPath(this.extensionPath);
    if (await isFile(bundled)) {
      if (os.platform() !== 'win32') {
        try {
          await fs.promises.access(bundled, fs.constants.X_OK);
        } catch {
          try {
            await fs.promises.chmod(bundled, 0o755);
          } catch {
            // Reported as `missing` when the spawn fails.
          }
        }
      }
      return bundled;
    }
    return findOnPath('rescriptdep');
  }

  async run(args: string[], options: CliRunOptions = {}): Promise<string> {
    const cliPath = await this.resolvePath();
    if (!cliPath) {
      this.reportMissing();
      throw new CliError('missing', 'rescriptdep CLI not found');
    }
    try {
      return await execCli(cliPath, args, options);
    } catch (error) {
      if (error instanceof CliError && error.kind === 'missing') {
        this.reportMissing();
      }
      throw error;
    }
  }

  /** At most once per session (QA-5). */
  reportMissing(): void {
    if (this.missingShown) {
      return;
    }
    this.missingShown = true;
    if (os.platform() === 'win32') {
      void vscode.window.showErrorMessage("Bibimbob doesn't support Windows yet.");
      return;
    }
    const install = 'Installation Guide';
    void vscode.window
      .showErrorMessage('rescriptdep CLI cannot be found. This extension requires rescriptdep to analyze dependencies.', install)
      .then((choice) => {
        if (choice === install) {
          void vscode.env.openExternal(vscode.Uri.parse(INSTALL_URL));
        }
      });
  }
}
