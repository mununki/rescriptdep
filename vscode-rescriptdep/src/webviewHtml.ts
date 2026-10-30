// HTML shell of the graph webview. Imported by the extension host bundle, so it must not touch the DOM.
// The webview script (src/webview/main.ts) looks these elements up by id once at load.

import { ContextKeys } from './protocol';

export interface WebviewHtmlOptions {
  /** `webview.cspSource` */
  cspSource: string;
  /** `webview.asWebviewUri(dist/webview.js)` as a string */
  scriptUri: string;
  /** `webview.asWebviewUri(dist/webview.css)` as a string */
  styleUri: string;
  /** Random nonce for the script tag and the CSP */
  nonce: string;
}

function escapeAttr(value: string): string {
  return value
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

export function renderWebviewHtml(options: WebviewHtmlOptions): string {
  const csp = escapeAttr(options.cspSource);
  const nonce = escapeAttr(options.nonce);
  const policy = [
    `default-src 'none'`,
    `img-src ${csp} data:`,
    `style-src ${csp} 'unsafe-inline'`,
    `script-src 'nonce-${nonce}' 'wasm-unsafe-eval'`,
    `font-src ${csp}`,
  ].join('; ');
  // The canvas context carries preventDefaultContextMenuItems, so nodes and the canvas get only our
  // menu items while the find box (outside the canvas) keeps Cut/Copy/Paste.
  const canvasContext = escapeAttr(
    JSON.stringify({ [ContextKeys.section]: ContextKeys.sectionCanvas, preventDefaultContextMenuItems: true }),
  );
  return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy" content="${policy};">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link rel="stylesheet" href="${escapeAttr(options.styleUri)}">
<title>Bibimbob</title>
</head>
<body>
<header class="hdr">
  <div class="hdr-text"><span class="title" id="hdr-title"></span><span class="meta" id="hdr-meta"></span></div>
  <div class="find" role="search">
    <input id="find" type="text" spellcheck="false" autocomplete="off" aria-label="Find module" aria-describedby="find-count" placeholder="Find module">
    <span class="count" id="find-count"></span>
  </div>
  <div class="tb" role="toolbar" aria-label="Graph view">
    <button type="button" id="btn-back" title="Back (Alt+←)" aria-label="Back" disabled>←</button>
    <span class="sep" aria-hidden="true"></span>
    <button type="button" id="btn-zoom-out" title="Zoom Out (−)" aria-label="Zoom out">−</button>
    <button type="button" id="btn-zoom-in" title="Zoom In (+)" aria-label="Zoom in">+</button>
    <button type="button" id="btn-fit" class="txt" title="Fit to View (0)" aria-label="Fit to view">Fit</button>
  </div>
</header>
<main id="graph-container" tabindex="0" aria-label="Graph canvas" data-vscode-context="${canvasContext}">
  <div id="graph"></div>
  <div id="state" class="state" hidden>
    <div class="state-box">
      <div class="state-title" id="state-title"></div>
      <p class="state-body" id="state-body"></p>
      <div class="state-actions" id="state-actions"></div>
    </div>
  </div>
  <div id="loading" class="loading" hidden>
    <div class="loading-bar"></div>
    <div class="loading-box">
      <div class="loading-text" id="loading-text"></div>
      <p class="loading-hint" id="loading-hint" hidden>This is taking longer than usual. Focusing on one module is much faster. <button type="button" class="link" data-action="pickModule">Focus on Module…</button></p>
      <button type="button" class="secondary" id="loading-cancel" data-action="cancel" hidden>Cancel</button>
    </div>
  </div>
</main>
<div id="status" class="sr-only" role="status" aria-live="polite"></div>
<script nonce="${nonce}" src="${escapeAttr(options.scriptUri)}"></script>
</body>
</html>`;
}
