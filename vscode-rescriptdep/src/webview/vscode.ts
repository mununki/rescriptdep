// Typed access to the VS Code webview API. `acquireVsCodeApi` may be called only once per page.
import type { WebviewMessage } from '../protocol';

interface VsCodeApi {
  postMessage(message: unknown): void;
}

declare function acquireVsCodeApi(): VsCodeApi;

const api = acquireVsCodeApi();

export function post(message: WebviewMessage): void {
  api.postMessage(message);
}
