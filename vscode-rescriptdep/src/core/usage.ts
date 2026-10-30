// Parsing and wording of the `-vb` value-usage result (ARCH-5, D10/C9). Must not import 'vscode'.

export interface UsageResult {
  /** Sum of the non-negative counts. */
  total: number;
  /** Some modules reported a negative code, so the total is a lower bound. */
  partial: boolean;
  /** Every module reported a negative code: nothing can be shown. */
  unavailable: boolean;
  /** The negative codes, in output order. */
  codes: number[];
}

/** Parses `rescriptdep -vb … -f json`. Returns `undefined` for output that isn't the expected JSON. */
export function parseUsageJson(output: string): UsageResult | undefined {
  let parsed: unknown;
  try {
    parsed = JSON.parse(output);
  } catch {
    return undefined;
  }
  const modules = typeof parsed === 'object' && parsed !== null ? (parsed as { modules?: unknown }).modules : undefined;
  if (!Array.isArray(modules)) {
    return undefined;
  }
  let total = 0;
  let counted = 0;
  const codes: number[] = [];
  for (const entry of modules) {
    const count = typeof entry === 'object' && entry !== null ? (entry as { count?: unknown }).count : undefined;
    if (typeof count !== 'number' || !Number.isFinite(count)) {
      continue;
    }
    if (count >= 0) {
      total += count;
      counted++;
    } else {
      codes.push(count);
    }
  }
  return {
    total,
    partial: codes.length > 0 && counted > 0,
    unavailable: codes.length > 0 && counted === 0,
    codes,
  };
}

/** Decoration text: `Used 0 times`, `Used 1 time`, `Used N times`, `Used N+ times`; nothing when unavailable. */
export function formatUsage(result: UsageResult): string | undefined {
  if (result.unavailable) {
    return undefined;
  }
  if (result.partial) {
    return `Used ${result.total}+ times`;
  }
  return result.total === 1 ? 'Used 1 time' : `Used ${result.total} times`;
}

// README code table: -2 no implementation, -3 unreadable .cmt, -4 no .cmt, -5 no file path,
// -6 compiler not supported by value usage analysis.
const REASONS: Record<number, string> = {
  [-2]: "Bibimbob: usage counts aren't available (no implementation file found).",
  [-3]: "Bibimbob: usage counts aren't available (a .cmt file couldn't be read).",
  [-4]: "Bibimbob: usage counts aren't available (build output not found).",
  [-5]: "Bibimbob: usage counts aren't available (no file path for the module).",
  [-6]: "Bibimbob: usage counts aren't available for this ReScript version.",
};

/** The one status-bar line explaining why nothing is shown, from the most frequent code. */
export function unavailableReason(codes: readonly number[]): string {
  const frequency = new Map<number, number>();
  for (const code of codes) {
    frequency.set(code, (frequency.get(code) ?? 0) + 1);
  }
  let best: number | undefined;
  for (const [code, n] of frequency) {
    if (best === undefined || n > (frequency.get(best) ?? 0)) {
      best = code;
    }
  }
  return (best !== undefined && REASONS[best]) || "Bibimbob: usage counts aren't available for this module.";
}

/**
 * Cache key: `(file, line, name, mtime of lib/bs/.compiler.log)`. When `.compiler.log` is missing,
 * fall back to the previous key shape (source and `lib/bs` mtimes plus the document version).
 */
export function usageCacheKey(parts: {
  file: string;
  line: number;
  name: string;
  compilerLogMtime?: number;
  fallback: { projectRoot: string; sourceMtime: number; bsMtime: number; documentVersion: number };
}): string {
  if (parts.compilerLogMtime !== undefined) {
    return ['log', parts.file, parts.line, parts.name, parts.compilerLogMtime].join('\0');
  }
  const f = parts.fallback;
  return ['old', f.projectRoot, parts.file, f.sourceMtime, f.bsMtime, f.documentVersion, parts.line, parts.name].join('\0');
}
