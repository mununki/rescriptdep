// Message contract between the extension host and the graph webview.
// This file is imported by both bundles, so it must not import 'vscode' or touch the DOM.

export type GraphView = 'full' | 'focus' | 'unused';

export interface GraphNode {
  /** Module name as printed by the CLI (the DOT node id). */
  id: string;
  /**
   * Second card line: the module's folder relative to the common prefix of the project's
   * module folders (omitted when the project has a single folder), or the package name for
   * modules under node_modules. Computed by the host; the webview never sees a path.
   */
  secondary?: string;
  /** The module comes from node_modules (drawn with a dashed border). */
  external: boolean;
  /** The host resolved an existing source file inside the project or a workspace folder. */
  openable: boolean;
}

export interface GraphModel {
  /** Sorted by id in code-point order. */
  nodes: GraphNode[];
  /** `[from, to]` means `from` depends on `to`, in CLI output order. */
  edges: [string, string][];
  /** Module ids of each cyclic-dependency cluster emitted by the CLI. */
  cycles: string[][];
}

export interface LayoutOptions {
  /** Apply the Graphviz tuning attributes (nslimit=2, nslimit1=2, mclimit=0.1). Decided by the host policy. */
  tune: boolean;
  /** The host policy expects a slow layout: show the loading overlay immediately and yield two frames first. */
  slow: boolean;
}

export type HostMessage =
  | {
      type: 'render';
      renderKey: number;
      panelId: string;
      view: GraphView;
      /** Focus view only. Present even when the CLI returned an empty graph (module not found). */
      center?: string;
      canGoBack: boolean;
      model: GraphModel;
      layout: LayoutOptions;
    }
  /** Waiting for the CLI. The webview shows its loading overlay after 300 ms and keeps the previous graph dimmed. */
  | { type: 'loading'; renderKey: number; text: string }
  /** The request for `renderKey` failed before anything could be rendered. */
  | { type: 'error'; renderKey: number; title: string; reason: string; canRetry: boolean; canGoBack?: boolean }
  /** The user cancelled the request for `renderKey`; restore the previous graph silently. */
  | { type: 'cancelled'; renderKey: number }
  /** From the "Fit to View" context-menu command. */
  | { type: 'fitToView' };

export type RenderErrorKind = 'tooLarge' | 'layout' | 'unknown';

export type WebviewMessage =
  | { type: 'ready' }
  | { type: 'focusModule'; module: string }
  | { type: 'openModule'; module: string }
  | { type: 'back' }
  | { type: 'showFullGraph' }
  /** "Focus on Module…" actions in empty/error states: the host shows the module QuickPick. */
  | { type: 'pickModule' }
  | { type: 'retry' }
  /** Cancel button on the loading overlay while waiting for the CLI. */
  | { type: 'cancel' }
  | { type: 'rendered'; renderKey: number; layoutMs: number; nodeCount: number }
  | { type: 'renderError'; renderKey: number; kind: RenderErrorKind; message: string };

/** Webview panel view type; also the `webviewId` in `webview/context` menu `when` clauses. */
export const WEBVIEW_VIEW_TYPE = 'bibimbobVisualizer';

/** `data-vscode-context` keys shared by the webview markup and the `package.json` menu `when` clauses. */
export const ContextKeys = {
  section: 'webviewSection',
  sectionModule: 'module',
  sectionCanvas: 'canvas',
  module: 'module',
  panelId: 'panelId',
  mode: 'bibimbobMode',
  canGoBack: 'bibimbobCanGoBack',
  openable: 'bibimbobOpenable',
  isCenter: 'bibimbobIsCenter',
} as const;

/** Argument VS Code passes to `webview/context` menu commands: the merged `data-vscode-context` plus `webview`. */
export interface WebviewContextArg {
  webview?: string;
  webviewSection?: string;
  panelId?: string;
  module?: string;
  bibimbobMode?: GraphView;
  bibimbobCanGoBack?: boolean;
  bibimbobOpenable?: boolean;
  bibimbobIsCenter?: boolean;
}
