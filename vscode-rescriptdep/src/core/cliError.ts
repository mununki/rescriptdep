// Classification of rescriptdep CLI failures. Must not import 'vscode'.

export type CliErrorKind = 'cancelled' | 'timeout' | 'missing' | 'tooLarge' | 'failed';

/** Every CLI run is killed after this long (the user sees the `timeout` reason). */
export const CLI_TIMEOUT_MS = 10_000;

export class CliError extends Error {
  constructor(
    readonly kind: CliErrorKind,
    message: string,
    readonly stderr = '',
  ) {
    super(message);
    this.name = 'CliError';
  }
}

function field(error: unknown, key: string): unknown {
  return typeof error === 'object' && error !== null ? (error as Record<string, unknown>)[key] : undefined;
}

/**
 * Maps an `execFile` callback error to a kind.
 * - An aborted signal gives `AbortError` (checked first: an abort also kills the child).
 * - `maxBuffer` overflow kills the child too, so it is checked before the timeout.
 * - A timeout is `killed && signal === 'SIGTERM'`; the message never mentions "timeout" (QA-6).
 * - `nice` exits with 127 (not found) or 126 (not executable); a direct spawn gives `ENOENT`.
 */
export function classifyCliError(error: unknown): CliErrorKind {
  if (error instanceof CliError) {
    return error.kind;
  }
  const code = field(error, 'code');
  if (field(error, 'name') === 'AbortError' || code === 'ABORT_ERR') {
    return 'cancelled';
  }
  if (code === 'ERR_CHILD_PROCESS_STDIO_MAXBUFFER') {
    return 'tooLarge';
  }
  if (field(error, 'killed') === true && field(error, 'signal') === 'SIGTERM') {
    return 'timeout';
  }
  if (code === 'ENOENT' || code === 'EACCES' || code === 127 || code === 126) {
    return 'missing';
  }
  return 'failed';
}

/** The reason line of the webview error state for a failed CLI run. */
export function cliErrorReason(kind: CliErrorKind, stderr = ''): string {
  switch (kind) {
    case 'timeout':
      return 'The CLI took longer than 10 seconds.';
    case 'missing':
      return "The rescriptdep CLI couldn't be found.";
    case 'tooLarge':
      return 'The CLI output is too large to display.';
    case 'cancelled':
      return 'The request was cancelled.';
    case 'failed': {
      const line = stderr
        .split(/\r?\n/)
        .map((l) => l.trim())
        .find((l) => l.length > 0);
      return line ? `The CLI failed: ${line.slice(0, 200)}` : 'The CLI exited with an error.';
    }
  }
}
