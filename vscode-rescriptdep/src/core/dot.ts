// Parser for the DOT the rescriptdep CLI prints (full, `--module`, `--no-dependents`).
// Must not import 'vscode'.
//
// Handles quoted IDs with `\"` and `\\` escapes (QA-9), attribute lists, edge chains,
// comments and `subgraph cluster_*` blocks (the CLI's cyclic-dependency clusters).

export interface DotNode {
  id: string;
  /** The `tooltip` attribute: a source file, or a package directory for externals. */
  tooltip?: string;
}

export interface ParsedDot {
  /** In order of first appearance (declarations first, then undeclared edge endpoints). */
  nodes: DotNode[];
  /** `[from, to]`, in output order. */
  edges: [string, string][];
  /** Members of each `subgraph cluster_*` block. */
  cycles: string[][];
}

export class DotParseError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'DotParseError';
  }
}

type Token = { kind: 'id'; value: string } | { kind: 'punct'; value: string };

const PUNCT = new Set(['{', '}', '[', ']', ';', ',', '=', ':']);

function isIdChar(c: string): boolean {
  const code = c.charCodeAt(0);
  return (
    (code >= 48 && code <= 57) || // 0-9
    (code >= 65 && code <= 90) || // A-Z
    (code >= 97 && code <= 122) || // a-z
    code === 95 || // _
    code === 46 || // .
    code >= 0x80
  );
}

/** Unescapes a DOT quoted string: `\"` → `"`, `\\` → `\`, a line continuation is dropped, other `\x` stay as is. */
function readQuoted(src: string, start: number): { value: string; end: number } {
  let value = '';
  let segment = start + 1;
  let i = segment;
  while (i < src.length) {
    const code = src.charCodeAt(i);
    if (code === 34 /* " */) {
      return { value: value + src.slice(segment, i), end: i + 1 };
    }
    if (code === 92 /* \ */) {
      value += src.slice(segment, i);
      const next = src[i + 1];
      if (next === '"' || next === '\\') {
        value += next;
        i += 2;
      } else if (next === '\n') {
        i += 2;
      } else if (next === '\r' && src[i + 2] === '\n') {
        i += 3;
      } else {
        value += '\\';
        i += 1;
      }
      segment = i;
      continue;
    }
    i++;
  }
  throw new DotParseError('Unterminated string');
}

function tokenize(src: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let lineStart = true;
  while (i < src.length) {
    const c = src[i];
    if (c === '\n') {
      lineStart = true;
      i++;
      continue;
    }
    if (c === ' ' || c === '\t' || c === '\r') {
      i++;
      continue;
    }
    if (lineStart && c === '#') {
      while (i < src.length && src[i] !== '\n') {
        i++;
      }
      continue;
    }
    lineStart = false;
    if (c === '/' && src[i + 1] === '/') {
      while (i < src.length && src[i] !== '\n') {
        i++;
      }
      continue;
    }
    if (c === '/' && src[i + 1] === '*') {
      const end = src.indexOf('*/', i + 2);
      i = end < 0 ? src.length : end + 2;
      continue;
    }
    if (c === '"') {
      const { value, end } = readQuoted(src, i);
      tokens.push({ kind: 'id', value });
      i = end;
      continue;
    }
    if (c === '-' && (src[i + 1] === '>' || src[i + 1] === '-')) {
      tokens.push({ kind: 'punct', value: src.slice(i, i + 2) });
      i += 2;
      continue;
    }
    if (PUNCT.has(c)) {
      tokens.push({ kind: 'punct', value: c });
      i++;
      continue;
    }
    if (isIdChar(c) || c === '-') {
      let j = i + 1;
      while (j < src.length && isIdChar(src[j])) {
        j++;
      }
      tokens.push({ kind: 'id', value: src.slice(i, j) });
      i = j;
      continue;
    }
    throw new DotParseError(`Unexpected character '${c}'`);
  }
  return tokens;
}

/** Index of the graph header, skipping anything printed before it (old CLIs wrote warnings to stdout). */
function graphStart(src: string): number {
  const match = /(^|\n)\s*(strict\s+)?(di)?graph\b/i.exec(src);
  if (!match) {
    throw new DotParseError('No graph found in the CLI output');
  }
  return match.index + match[1].length;
}

export function parseDot(src: string): ParsedDot {
  const tokens = tokenize(src.slice(graphStart(src)));
  let pos = 0;
  const peek = (offset = 0): Token | undefined => tokens[pos + offset];
  const isPunct = (t: Token | undefined, value: string) => t !== undefined && t.kind === 'punct' && t.value === value;
  const expectPunct = (value: string) => {
    const t = tokens[pos];
    if (!isPunct(t, value)) {
      throw new DotParseError(`Expected '${value}'`);
    }
    pos++;
  };

  const nodes = new Map<string, DotNode>();
  const edges: [string, string][] = [];
  const cycles: string[][] = [];
  const touch = (id: string): DotNode => {
    let node = nodes.get(id);
    if (!node) {
      node = { id };
      nodes.set(id, node);
    }
    return node;
  };

  const parseAttrList = (): Record<string, string> => {
    const attrs: Record<string, string> = {};
    while (isPunct(peek(), '[')) {
      pos++;
      while (!isPunct(peek(), ']')) {
        const key = peek();
        if (!key) {
          throw new DotParseError('Unterminated attribute list');
        }
        if (key.kind === 'punct' && (key.value === ',' || key.value === ';')) {
          pos++;
          continue;
        }
        if (key.kind !== 'id') {
          throw new DotParseError('Expected an attribute name');
        }
        pos++;
        if (isPunct(peek(), '=')) {
          pos++;
          const value = peek();
          if (!value || value.kind !== 'id') {
            throw new DotParseError('Expected an attribute value');
          }
          pos++;
          attrs[key.value] = value.value;
        } else {
          attrs[key.value] = 'true';
        }
      }
      pos++;
    }
    return attrs;
  };

  const readNodeId = (): string => {
    const t = peek();
    if (!t || t.kind !== 'id') {
      throw new DotParseError('Expected a node id');
    }
    pos++;
    // Port syntax (`"A":p:n`) is not produced by the CLI; skip it if present.
    while (isPunct(peek(), ':') && peek(1)?.kind === 'id') {
      pos += 2;
    }
    return t.value;
  };

  // Parses statements until the closing brace; `members` collects node ids for clusters.
  const parseStmtList = (members?: string[]): void => {
    while (pos < tokens.length && !isPunct(peek(), '}')) {
      const t = peek()!;
      if (t.kind === 'punct' && (t.value === ';' || t.value === ',')) {
        pos++;
        continue;
      }
      if (t.kind === 'punct' && t.value === '{') {
        // Anonymous subgraph.
        pos++;
        parseStmtList(members);
        expectPunct('}');
        continue;
      }
      if (t.kind !== 'id') {
        throw new DotParseError(`Unexpected '${t.value}'`);
      }
      const keyword = t.value.toLowerCase();
      if (keyword === 'subgraph') {
        pos++;
        let name = '';
        if (peek()?.kind === 'id') {
          name = peek()!.value;
          pos++;
        }
        expectPunct('{');
        const inner: string[] = [];
        parseStmtList(inner);
        expectPunct('}');
        if (name.startsWith('cluster')) {
          cycles.push(inner);
        }
        members?.push(...inner);
        continue;
      }
      if ((keyword === 'graph' || keyword === 'node' || keyword === 'edge') && isPunct(peek(1), '[')) {
        pos++;
        parseAttrList();
        continue;
      }
      if (isPunct(peek(1), '=')) {
        // Graph attribute: `rankdir=LR`.
        pos += 2;
        if (peek()?.kind !== 'id') {
          throw new DotParseError('Expected an attribute value');
        }
        pos++;
        continue;
      }
      const chain = [readNodeId()];
      while (isPunct(peek(), '->') || isPunct(peek(), '--')) {
        pos++;
        chain.push(readNodeId());
      }
      const attrs = parseAttrList();
      if (chain.length === 1) {
        const node = touch(chain[0]);
        if (attrs.tooltip !== undefined) {
          node.tooltip = attrs.tooltip;
        }
        members?.push(chain[0]);
      } else {
        for (let i = 0; i + 1 < chain.length; i++) {
          touch(chain[i]);
          touch(chain[i + 1]);
          edges.push([chain[i], chain[i + 1]]);
        }
      }
    }
  };

  // Header: [strict] (di)graph [id] {
  if (peek()?.kind === 'id' && peek()!.value.toLowerCase() === 'strict') {
    pos++;
  }
  const header = peek();
  if (!header || header.kind !== 'id' || !/^(di)?graph$/i.test(header.value)) {
    throw new DotParseError('Expected a graph header');
  }
  pos++;
  if (peek()?.kind === 'id') {
    pos++;
  }
  expectPunct('{');
  parseStmtList();
  expectPunct('}');

  return { nodes: [...nodes.values()], edges, cycles };
}

/** Code-point order, the same as the CLI's `String.compare` on its (ASCII) module names. */
export function compareIds(a: string, b: string): number {
  if (a === b) {
    return 0;
  }
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const ca = a.codePointAt(i)!;
    const cb = b.codePointAt(i)!;
    if (ca !== cb) {
      return ca - cb;
    }
    if (ca > 0xffff) {
      i++;
    }
  }
  return a.length - b.length;
}

export interface GraphCounts {
  nodes: number;
  edges: number;
}

export function graphCounts(parsed: ParsedDot): GraphCounts {
  return { nodes: parsed.nodes.length, edges: parsed.edges.length };
}
