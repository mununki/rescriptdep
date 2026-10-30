// Activation and command registration. The behaviour lives in graphPanel.ts, project.ts,
// valueUsage.ts and contextMenu.ts.
import * as vscode from 'vscode';
import { RescriptDepCli } from './cli';
import type { CliRunner } from './cli';
import { registerGraphCommands } from './contextMenu';
import { GraphPanelRegistry } from './graphPanel';
import { ProjectService } from './project';
import { ValueUsageController } from './valueUsage';

// Public command IDs (unchanged).
export const SHOW_DEPENDENCY_GRAPH = 'bibimbob.showDependencyGraph';
export const FOCUS_MODULE_DEPENDENCIES = 'bibimbob.focusModuleDependencies';
export const SHOW_UNUSED_MODULES = 'bibimbob.showUnusedModules';
export const TOGGLE_VALUE_USAGE_COUNT = 'bibimbob.toggleValueUsageCount';

export interface ExtensionServices {
  registry: GraphPanelRegistry;
  projects: ProjectService;
  valueUsage: ValueUsageController;
}

/** Wires everything to `context`; tests pass a fake CLI runner. */
export function activateWith(context: vscode.ExtensionContext, cli: CliRunner): ExtensionServices {
  const projects = new ProjectService();
  const extensionUri = context.extensionUri ?? vscode.Uri.file(context.extensionPath);
  const registry = new GraphPanelRegistry({ extensionUri, cli, projects });
  const valueUsage = new ValueUsageController(cli, projects);

  context.subscriptions.push(
    projects,
    registry,
    valueUsage,
    vscode.commands.registerCommand(SHOW_DEPENDENCY_GRAPH, async () => {
      const project = await projects.resolveForGraphCommand();
      if (project) {
        await registry.show({ view: 'full', project });
      }
    }),
    // The editor-title button passes the editor's URI; prefer it over the active editor (split groups).
    vscode.commands.registerCommand(FOCUS_MODULE_DEPENDENCIES, async (uri?: unknown) => {
      const target = await projects.resolveFocusTarget(uri);
      if (target) {
        await registry.show({ view: 'focus', project: target.project, module: target.module });
      }
    }),
    vscode.commands.registerCommand(SHOW_UNUSED_MODULES, async () => {
      const project = await projects.resolveForGraphCommand();
      if (project) {
        await registry.show({ view: 'unused', project });
      }
    }),
    vscode.commands.registerCommand(TOGGLE_VALUE_USAGE_COUNT, () => valueUsage.toggle()),
    ...registerGraphCommands(registry),
  );
  return { registry, projects, valueUsage };
}

export function activate(context: vscode.ExtensionContext): void {
  activateWith(context, new RescriptDepCli(context.extensionPath));
}

export function deactivate(): void {}
