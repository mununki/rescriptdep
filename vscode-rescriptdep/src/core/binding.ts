// The value-binding matcher shared by the cursor listener and the request path (QA-8).
// Must not import 'vscode'.

// Optional leading attributes (`@genType`, `@react.component`, `@module("x")`), then `let` or
// `and`, an optional `rec`, and a lowercase identifier that may contain primes (`x'`).
const BINDING_RE = /^\s*(?:@[A-Za-z_][\w.]*(?:\([^)]*\))?\s+)*(?:let|and)\s+(?:rec\s+)?([a-z_][A-Za-z0-9_']*)(?=[\s:=]|$)/;

// Words the identifier group can capture that are never value bindings.
const NOT_A_BINDING = new Set(['_', 'rec', 'and', 'let', 'module', 'type', 'open', 'include', 'external', 'exception']);

/** The value name bound on this line, or `undefined` when the line does not bind one. */
export function matchBinding(lineText: string): string | undefined {
  const match = BINDING_RE.exec(lineText);
  if (!match) {
    return undefined;
  }
  const name = match[1];
  return NOT_A_BINDING.has(name) ? undefined : name;
}
