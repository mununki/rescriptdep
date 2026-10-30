// `webview/context` menu commands (U1, designer-review3 §R5 with the round-3 corrections).
//
// VS Code calls these with the merged `data-vscode-context` of the right-clicked element plus
// `webview` (the view type). Only `panelId` and `module` are read, both validated: `module` must be
// a node of that panel's current graph. Paths never come from the webview.
import * as vscode from 'vscode';
import * as path from 'path';
import type { GraphPanel, GraphPanelRegistry } from './graphPanel';

export const GRAPH_COMMANDS = {
  openModule: 'bibimbob.graph.openModule',
  focusModule: 'bibimbob.graph.focusModule',
  copyModuleName: 'bibimbob.graph.copyModuleName',
  copyRelativePath: 'bibimbob.graph.copyRelativePath',
  fitToView: 'bibimbob.graph.fitToView',
  back: 'bibimbob.graph.back',
  showFullGraph: 'bibimbob.graph.showFullGraph',
} as const;

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null;
}

/**
 * The panel named by `arg.panelId`. An unknown or malformed id (a stale menu after the panel closed)
 * is a no-op; no argument at all (palette, a future keybinding) means the active panel.
 */
export function panelForArg(registry: GraphPanelRegistry, arg: unknown): GraphPanel | undefined {
  if (isRecord(arg) && 'panelId' in arg) {
    return typeof arg.panelId === 'string' ? registry.get(arg.panelId) : undefined;
  }
  return registry.active();
}

/** Panel and module, or `undefined` unless the module is a node of that panel's current graph. */
export function moduleTarget(registry: GraphPanelRegistry, arg: unknown): { panel: GraphPanel; module: string } | undefined {
  const panel = panelForArg(registry, arg);
  const module = isRecord(arg) ? arg.module : undefined;
  if (!panel || typeof module !== 'string' || !panel.hasModule(module)) {
    return undefined;
  }
  return { panel, module };
}

export function registerGraphCommands(registry: GraphPanelRegistry): vscode.Disposable[] {
  const withModule = (fn: (panel: GraphPanel, module: string) => unknown) => async (arg: unknown) => {
    const target = moduleTarget(registry, arg);
    if (target) {
      await fn(target.panel, target.module);
    }
  };
  const withPanel = (fn: (panel: GraphPanel) => unknown) => async (arg: unknown) => {
    const panel = panelForArg(registry, arg);
    if (panel) {
      await fn(panel);
    }
  };
  return [
    vscode.commands.registerCommand(GRAPH_COMMANDS.openModule, withModule((panel, module) => panel.openModule(module))),
    vscode.commands.registerCommand(GRAPH_COMMANDS.focusModule, withModule((panel, module) => panel.focusModule(module))),
    // Copy actions show no toast, like Explorer's Copy Path.
    vscode.commands.registerCommand(GRAPH_COMMANDS.copyModuleName, withModule((_panel, module) => vscode.env.clipboard.writeText(module))),
    vscode.commands.registerCommand(
      GRAPH_COMMANDS.copyRelativePath,
      withModule((panel, module) => {
        const relative = panel.relativePath(module);
        return relative === undefined ? undefined : vscode.env.clipboard.writeText(relative);
      }),
    ),
    vscode.commands.registerCommand(GRAPH_COMMANDS.fitToView, withPanel((panel) => panel.fitToView())),
    vscode.commands.registerCommand(GRAPH_COMMANDS.back, withPanel((panel) => panel.back())),
    vscode.commands.registerCommand(GRAPH_COMMANDS.showFullGraph, withPanel((panel) => panel.showFullGraph())),
  ];
}

export interface EditorGroupInfo {
  viewColumn: number;
  /** `Uri.toString()` of each text tab. */
  uris: readonly string[];
}

/**
 * Where "Open Module" shows a file (designer rule 6 with the FE/ARCH correction):
 * 1. a tab already showing it in a group other than the graph's → reveal it there;
 * 2. else the nearest other editor group (left first on a tie);
 * 3. else `Beside`, which splits next to the graph.
 */
export function chooseEditorColumn(groups: readonly EditorGroupInfo[], graphColumn: number | undefined, target: string): number | 'beside' {
  const others = groups.filter((g) => g.viewColumn !== graphColumn);
  const open = others.find((g) => g.uris.includes(target));
  if (open) {
    return open.viewColumn;
  }
  if (graphColumn === undefined || others.length === 0) {
    return 'beside';
  }
  const nearest = [...others].sort(
    (a, b) => Math.abs(a.viewColumn - graphColumn) - Math.abs(b.viewColumn - graphColumn) || a.viewColumn - b.viewColumn,
  )[0];
  return nearest.viewColumn;
}

export async function openModuleSource(uri: vscode.Uri, graphColumn: vscode.ViewColumn | undefined): Promise<void> {
  const groups: EditorGroupInfo[] = vscode.window.tabGroups.all.map((group) => ({
    viewColumn: group.viewColumn,
    uris: group.tabs.flatMap((tab) => (tab.input instanceof vscode.TabInputText ? [tab.input.uri.toString()] : [])),
  }));
  const column = chooseEditorColumn(groups, graphColumn, uri.toString());
  try {
    await vscode.window.showTextDocument(uri, {
      viewColumn: column === 'beside' ? vscode.ViewColumn.Beside : column,
      preview: true,
      preserveFocus: false,
    });
  } catch {
    void vscode.window.showErrorMessage(`Couldn't open ${path.basename(uri.fsPath)}.`);
  }
}
