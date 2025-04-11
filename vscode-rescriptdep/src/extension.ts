import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

// Command IDs
const SHOW_DEPENDENCY_GRAPH = 'bibimbob.showDependencyGraph';
const FOCUS_MODULE_DEPENDENCIES = 'bibimbob.focusModuleDependencies';
const SHOW_UNUSED_MODULES = 'bibimbob.showUnusedModules';

// Track the current webview panel
let currentPanel: vscode.WebviewPanel | undefined = undefined;
// Track current DOT content and module state as global variables
let currentDotContent: string = '';
let currentIsFocusedMode: boolean = false;
let currentCenterModule: string | undefined = undefined;

export function activate(context: vscode.ExtensionContext) {
  // Command for full dependency graph
  let fullGraphCommand = vscode.commands.registerCommand(SHOW_DEPENDENCY_GRAPH, async () => {
    await generateDependencyGraph(context);
  });

  // Command for module-focused dependency graph
  let focusModuleCommand = vscode.commands.registerCommand(FOCUS_MODULE_DEPENDENCIES, async () => {
    await generateDependencyGraph(context, true);
  });

  // Command for showing modules with no dependents (unused modules)
  let unusedModulesCommand = vscode.commands.registerCommand(SHOW_UNUSED_MODULES, async () => {
    await generateDependencyGraph(context, false, true);
  });

  context.subscriptions.push(fullGraphCommand);
  context.subscriptions.push(focusModuleCommand);
  context.subscriptions.push(unusedModulesCommand);
}

// Helper function to get current module name from active editor
function getCurrentModuleNameFromActiveEditor(): string | undefined {
  const editor = vscode.window.activeTextEditor;
  if (!editor) { return undefined; }

  const document = editor.document;
  const fileName = document.fileName;

  // Check if it's a ReScript file
  if (path.extname(fileName) === '.res') {
    // Get the base filename without extension and capitalize first letter
    // ReScript modules start with capital letter
    const baseName = path.basename(fileName, '.res');
    return baseName.charAt(0).toUpperCase() + baseName.slice(1);
  }

  return undefined;
}

// Restore findConfigFile function definition at the top level
async function findConfigFile(workspaceRoot: string): Promise<vscode.Uri | undefined> {
  // Prioritize bsconfig.json, then rescript.json
  const bsconfigFiles = await vscode.workspace.findFiles(
    new vscode.RelativePattern(workspaceRoot, '**/bsconfig.json'),
    '**/node_modules/**', // exclude node_modules
    1 // find only the first one
  );
  if (bsconfigFiles.length > 0) {
    return bsconfigFiles[0];
  }

  const rescriptFiles = await vscode.workspace.findFiles(
    new vscode.RelativePattern(workspaceRoot, '**/rescript.json'),
    '**/node_modules/**', // exclude node_modules
    1 // find only the first one
  );
  if (rescriptFiles.length > 0) {
    return rescriptFiles[0];
  }

  return undefined; // Should not happen if activationEvents worked
}

// Helper function to find the nearest project root (containing bsconfig/rescript.json)
// starting from a given file path and going upwards.
async function findProjectRootForFile(filePath: string, workspaceRoot: string): Promise<string | undefined> {
  let currentDir = path.dirname(filePath);

  // Iterate upwards until we find a config file or hit the workspace root
  while (currentDir.startsWith(workspaceRoot) && currentDir !== workspaceRoot) {
    const bsconfigPath = path.join(currentDir, 'bsconfig.json');
    const rescriptPath = path.join(currentDir, 'rescript.json');

    try {
      // Check if either config file exists in the current directory
      if (fs.existsSync(bsconfigPath) || fs.existsSync(rescriptPath)) {
        return currentDir; // Found the project root
      }
    } catch (err) {
      // Ignore errors (e.g., permission issues) and continue upwards
      console.warn(`Error checking for config files in ${currentDir}:`, err);
    }

    // Move one directory up
    const parentDir = path.dirname(currentDir);
    // Avoid infinite loop if dirname doesn't change (e.g., at root)
    if (parentDir === currentDir) {
      break;
    }
    currentDir = parentDir;
  }

  // If not found in subdirectories, check the workspace root itself
  const bsconfigPath = path.join(workspaceRoot, 'bsconfig.json');
  const rescriptPath = path.join(workspaceRoot, 'rescript.json');
  if (fs.existsSync(bsconfigPath) || fs.existsSync(rescriptPath)) {
    return workspaceRoot;
  }

  // Return undefined if no config file found up to the workspace root
  return undefined;
}

// Helper function to detect monorepo and find all projects with ReScript config
async function detectMonorepoProjects(workspaceRoot: string): Promise<vscode.Uri[]> {
  // Find all bsconfig.json or rescript.json files in the workspace
  const bsconfigFiles = await vscode.workspace.findFiles(
    new vscode.RelativePattern(workspaceRoot, '**/bsconfig.json'),
    '**/node_modules/**', // exclude node_modules
  );

  const rescriptFiles = await vscode.workspace.findFiles(
    new vscode.RelativePattern(workspaceRoot, '**/rescript.json'),
    '**/node_modules/**', // exclude node_modules
  );

  // Combine both types of config files
  const allConfigFiles = [...bsconfigFiles, ...rescriptFiles];

  // If there's more than one config file, it might be a monorepo
  if (allConfigFiles.length > 1) {
    return allConfigFiles;
  }

  return [];
}

// Function to prompt user to select a project from a monorepo
async function selectMonorepoProject(projects: vscode.Uri[]): Promise<string | undefined> {
  // Create QuickPick items from project paths
  const items = projects.map(uri => {
    const relativePath = vscode.workspace.asRelativePath(uri);
    const projectDir = path.dirname(uri.fsPath);
    const projectName = path.basename(projectDir);

    return {
      label: projectName,
      description: relativePath,
      projectRoot: projectDir
    };
  });

  // Show QuickPick to user
  const selectedItem = await vscode.window.showQuickPick(items, {
    placeHolder: 'Select a ReScript project to analyze',
    title: 'Monorepo Projects'
  });

  return selectedItem?.projectRoot;
}

// Integrated common logic into a single function
async function generateDependencyGraph(context: vscode.ExtensionContext, focusOnModule: boolean = false, showUnusedModules: boolean = false) {
  // Use withProgress API to show a progress notification in the bottom right
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    title: focusOnModule ? 'ReScript: Analyzing module dependencies...' :
      showUnusedModules ? 'ReScript: Finding unused modules...' :
        'ReScript: Analyzing dependency graph...',
    cancellable: true
  }, async (progress, token) => {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders || workspaceFolders.length === 0) { // Ensure folder exists
      vscode.window.showErrorMessage('No workspace folder open');
      return;
    }

    // Use the first workspace folder as the root for searching
    const workspaceRoot = workspaceFolders[0].uri.fsPath;

    let projectRoot: string;
    let moduleName: string | undefined;

    if (focusOnModule) {
      // --- Logic when focusing on a specific module ---
      progress.report({ message: 'Getting module information...' });
      if (token.isCancellationRequested) { return; }

      // Get module name (from editor or input)
      moduleName = getCurrentModuleNameFromActiveEditor();
      if (!moduleName) {
        moduleName = await vscode.window.showInputBox({
          prompt: 'Enter module name to focus on',
          placeHolder: 'ModuleName'
        });
      }
      if (!moduleName) { return; } // User cancelled

      // Find the source file for the target module
      progress.report({ message: `Finding source file for ${moduleName}...` });
      const moduleInfo = await findModuleInProject(moduleName);
      if (!moduleInfo) {
        vscode.window.showErrorMessage(`Could not find the source file for module: ${moduleName}`);
        return;
      }

      // Find the project root specific to this module's source file
      progress.report({ message: `Finding project root for ${moduleName}...` });
      const moduleProjectRoot = await findProjectRootForFile(moduleInfo.path, workspaceRoot);
      if (!moduleProjectRoot) {
        vscode.window.showErrorMessage(`Could not determine the project root for module: ${moduleName} (no bsconfig/rescript.json found in parent directories).`);
        return;
      }

      // Calculate bsDir based on the module's specific project root
      projectRoot = moduleProjectRoot;
    } else {
      // Only check for monorepo and ask for project selection when not focusing on a specific module
      // Check if this is a monorepo with multiple projects
      progress.report({ message: 'Checking workspace structure...' });
      const monorepoProjects = await detectMonorepoProjects(workspaceRoot);

      // If it's a monorepo with multiple projects, ask user to select one
      if (monorepoProjects.length > 1) {
        progress.report({ message: 'Monorepo detected. Please select a project...' });
        const selectedProjectRoot = await selectMonorepoProject(monorepoProjects);

        if (!selectedProjectRoot) {
          // User cancelled the selection
          return;
        }

        projectRoot = selectedProjectRoot;
      } else {
        // Find the initial config file (used for full graph or as fallback)
        progress.report({ message: 'Finding ReScript config file...' });
        const initialConfigFileUri = await findConfigFile(workspaceRoot);

        if (token.isCancellationRequested) { return; }

        // If no config file is found anywhere, exit (should be caught by activationEvents)
        if (!initialConfigFileUri) {
          vscode.window.showErrorMessage('Could not find any bsconfig.json or rescript.json in the workspace (excluding node_modules).');
          return;
        }

        // Use the initially found config file's location
        projectRoot = path.dirname(initialConfigFileUri.fsPath);
      }
    }

    let bsDir: string;

    try {
      // Calculate bsDir based on the determined project root
      bsDir = path.join(projectRoot, 'lib', 'bs');

      // Check if the determined bsDir exists (common check)
      if (!fs.existsSync(bsDir)) {
        vscode.window.showWarningMessage(`ReScript build directory not found: ${bsDir}. Please ensure the project is compiled.`);
        // Consider returning if bsDir is essential for the CLI command
      }

      // Find CLI path
      progress.report({ message: 'Finding CLI path...' });
      if (token.isCancellationRequested) { return; }
      const cliPath = await findRescriptDepCLI(context);

      // Check project size first if not focusing on a specific module
      if (!focusOnModule && !showUnusedModules) {
        progress.report({ message: 'Checking project size...' });
        try {
          // Get a simple DOT output to estimate the number of modules
          const sizeCheckArgs = ['--format=dot', bsDir];
          const dotOutput = await runRescriptDep(cliPath, sizeCheckArgs, context);

          // Count nodes in DOT format by finding quoted node names
          // Each module appears as "ModuleName" [label="ModuleName", tooltip="..."] in DOT format
          const nodeMatches = dotOutput.match(/"([^"]+)"\s*\[/g) || [];

          // Filter out style definitions (like "node [shape=box...]")
          const moduleNodes = nodeMatches.filter(match => !match.startsWith('"node ['));
          const moduleCount = moduleNodes.length;

          // Prompt user if module count is high
          if (moduleCount > 1000) {
            const response = await vscode.window.showWarningMessage(
              `This project contains approximately ${moduleCount} modules, which may cause performance issues or visualization errors.`,
              'Continue Anyway', 'Focus on Module', 'Show Unused Modules', 'Cancel'
            );

            if (response === 'Focus on Module') {
              // User chose to focus on a specific module
              await vscode.commands.executeCommand(FOCUS_MODULE_DEPENDENCIES);
              return;
            } else if (response === 'Show Unused Modules') {
              // User chose to show unused modules
              await vscode.commands.executeCommand(SHOW_UNUSED_MODULES);
              return;
            } else if (response !== 'Continue Anyway') {
              // User chose to cancel
              return;
            }

            // If continuing, use the DOT output we already have
            progress.report({ message: 'Generating visualization...' });
            if (token.isCancellationRequested) { return; }

            if (dotOutput) {
              showDotGraphWebview(context, dotOutput, focusOnModule, moduleName, showUnusedModules);
              return;
            }
          }
        } catch (error) {
          // If check operation fails, continue anyway with regular flow
          console.warn('Failed to estimate project size:', error);
        }
      }

      // Run the CLI command with the determined bsDir and moduleName (if applicable)
      progress.report({ message: showUnusedModules ? 'Finding unused modules...' : 'Running dependency analysis...' });
      if (token.isCancellationRequested) { return; }

      // Define CLI arguments based on the analysis type
      let args: string[];

      if (showUnusedModules) {
        // 1. Unused modules analysis
        args = ['--format=dot', '--no-dependents'];
      } else if (focusOnModule) {
        // 2. Focus on specific module
        args = ['--format=dot', '--module', moduleName!];
      } else {
        // 3. Full dependency graph
        args = ['--format=dot'];
      }

      // Add bsDir target
      args.push(bsDir);

      // Get DOT format data
      const dotContent = await runRescriptDep(cliPath, args, context);

      // Display webview
      progress.report({ message: 'Generating visualization...' });
      if (token.isCancellationRequested) { return; }

      if (dotContent) {
        showDotGraphWebview(context, dotContent, focusOnModule, moduleName, showUnusedModules);
      } else {
        vscode.window.showErrorMessage('Failed to generate dependency visualization (CLI returned no content).');
      }

    } catch (error) {
      if (!token.isCancellationRequested) {
        // Create or update the webview to show the error
        if (error instanceof Error) {
          // For known memory errors, show a more helpful message
          if (error.message.includes('Cannot enlarge memory arrays') ||
            error.message.includes('TOTAL_MEMORY') ||
            error.message.includes('ALLOW_MEMORY_GROWTH')) {

            vscode.window.showErrorMessage(
              'The project is too large to visualize in full. Try focusing on specific modules instead.',
              'Focus on Module'
            ).then(selection => {
              if (selection === 'Focus on Module') {
                // Trigger the focus module command
                vscode.commands.executeCommand(FOCUS_MODULE_DEPENDENCIES);
              }
            });

            // If webview is already open, show error there
            if (currentPanel) {
              currentPanel.webview.postMessage({
                command: 'showError',
                errorMessage: 'The project is too large to visualize in full. Try focusing on specific modules instead.'
              });
            }
          } else {
            vscode.window.showErrorMessage(`Error generating dependency visualization: ${error.message}`);
          }
        } else {
          vscode.window.showErrorMessage(`Error generating dependency visualization: ${error}`);
        }
      }
    }
  });
}

// Get path to CLI, with fallback strategies
async function findRescriptDepCLI(context: vscode.ExtensionContext): Promise<string> {
  if (isDevelopmentMode()) {
    // Move from VSCode extension directory to parent directory (OCaml project root)
    const ocamlProjectRoot = path.resolve(context.extensionPath, '..');

    // Use _build path - directly reference compiled binary
    return path.join(ocamlProjectRoot, '_build', 'default', 'bin', 'main.exe');
  }

  // 1. Try bundled CLI first
  const bundledPath = getBundledCLIPath(context.extensionPath);

  // Ensure execute permission if on Unix-like OS
  if (os.platform() !== 'win32' && fs.existsSync(bundledPath)) {
    try {
      await fs.promises.access(bundledPath, fs.constants.X_OK);
    } catch {
      // Set execute permission
      await fs.promises.chmod(bundledPath, 0o755);
    }
    return bundledPath;
  }

  if (fs.existsSync(bundledPath)) {
    return bundledPath;
  }

  // 2. Try from PATH
  try {
    if (os.platform() === 'win32') {
      cp.execSync('where rescriptdep', { stdio: 'ignore' });
    } else {
      cp.execSync('which rescriptdep', { stdio: 'ignore' });
    }
    return 'rescriptdep';
  } catch {
    // 3. If not found, suggest installing
    const choice = await vscode.window.showErrorMessage(
      'rescriptdep CLI cannot be found. This extension requires rescriptdep to analyze dependencies.',
      'Use bundled version (recommended)', 'Manual installation'
    );

    if (choice === 'Manual installation') {
      vscode.env.openExternal(
        vscode.Uri.parse('https://github.com/your-username/rescriptdep#installation')
      );
      throw new Error('Please install rescriptdep CLI to use this extension');
    }

    // Try to use bundled version even if it doesn't exist yet
    // (This allows future-proofing when bundling is implemented)
    return bundledPath;
  }
}

function isDevelopmentMode(): boolean {
  const isDebug = process.env.VSCODE_DEBUG_MODE === 'true';
  const isDevHostPath = __dirname.includes('.vscode-test') || __dirname.includes('.vscode-insiders');
  const devFileExists = fs.existsSync(path.resolve(__dirname, '../../dune-project'));

  return isDebug || isDevHostPath || devFileExists;
}

// Get path to bundled CLI based on platform
function getBundledCLIPath(extensionPath: string): string {
  const platform = os.platform();
  let binaryName = 'rescriptdep';

  if (platform === 'win32') {
    binaryName += '.exe';
  }

  // Different paths based on platform
  const platformDir = platform === 'win32' ? 'win32' :
    platform === 'darwin' ? 'darwin' : 'linux';

  return path.join(extensionPath, 'bin', platformDir, binaryName);
}

// Run CLI with arguments
async function runRescriptDep(cliPath: string, args: string[], context?: vscode.ExtensionContext): Promise<string> {
  // Restore the correct Promise-based implementation using cp.execFile
  return new Promise((resolve, reject) => {
    const command = cliPath;
    const options: cp.ExecFileOptions = {
      maxBuffer: 10 * 1024 * 1024, // 10MB buffer size
      timeout: 10000, // 10 second timeout
    };

    // Platform-specific CPU limiting wrapper
    let cpuLimitedCommand = command;
    let cpuLimitedArgs = [...args];

    if (os.platform() !== 'win32') {
      // On Unix systems (macOS/Linux), use 'nice' to limit CPU priority
      cpuLimitedArgs = [command, ...args];
      cpuLimitedCommand = 'nice';
    }

    cp.execFile(cpuLimitedCommand, cpuLimitedArgs, options, (error, stdout, stderr) => {
      if (error) {
        console.error(`rescriptdep stderr: ${stderr}`);

        // Handle timeout error specifically
        if (error.message.includes('timeout')) {
          reject(new Error('The operation timed out after 10 seconds. The project may be too large to analyze.'));
          return;
        }

        // Provide more specific error message for buffer exceeded case
        if (error.message.includes('maxBuffer')) {
          reject(new Error('The project has too many modules to visualize as a graph. This feature works best with smaller projects or when focusing on specific modules.'));
          return;
        }

        // Include stderr in the rejection for better debugging
        reject(new Error(`Command failed: ${command} ${args.join(' ')}\n${error.message}\nStderr: ${stderr}`));
        return;
      }

      if (stderr) {
        console.warn(`rescriptdep stderr: ${stderr}`);
      }
      resolve(stdout.toString());
    });
  });
}

// Function to display DOT format graph in webview
function showDotGraphWebview(context: vscode.ExtensionContext, dotContent: string, isFocusedMode: boolean = false, centerModuleName?: string, isUnusedModulesMode: boolean = false) {
  // Save current state to global variables
  currentDotContent = dotContent;
  currentIsFocusedMode = isFocusedMode;
  currentCenterModule = centerModuleName;

  // Detect if the current theme is dark
  const isDarkTheme = vscode.window.activeColorTheme && vscode.window.activeColorTheme.kind === vscode.ColorThemeKind.Dark;

  // Set appropriate graph attributes based on theme
  const themeAttributes = isDarkTheme ?
    'bgcolor="transparent" fontcolor="#e0e0e0"' :
    'bgcolor="transparent" fontcolor="#333333"';

  // Node background color and padding settings - apply clearer colors and styles
  const nodeStyle = isDarkTheme ?
    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#1e1e1e", margin="0.3,0.2", color="#aaaaaa", penwidth=1]' :
    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#f0f0f0", margin="0.3,0.2", color="#666666", penwidth=1]';

  // Line and arrow style settings
  const edgeStyle = isDarkTheme ?
    'edge[color="#555555", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]' :
    'edge[color="#cccccc", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]';

  // Update DOT content - apply node styles directly
  let themedDotContent = dotContent;

  // Set basic properties
  themedDotContent = themedDotContent.replace(/^(digraph\s+\w+\s*\{)/m,
    `$1\n  ${themeAttributes}\n  ${nodeStyle}\n  ${edgeStyle}\n  splines=true\n  overlap=false\n  sep="+10"`);

  // Force apply node styles - add style directly to all nodes
  if (isDarkTheme) {
    // Apply dark background in dark theme
    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#1e1e1e", ');
  } else {
    // Apply light gray background in light theme
    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#f0f0f0", ');
  }

  // Add center module style in focus mode
  if (isFocusedMode && centerModuleName) {
    // Change the style of the center module node
    const centerNodePattern = new RegExp(`\\s+(${centerModuleName})\\s*\\[`);
    themedDotContent = themedDotContent.replace(centerNodePattern, ` $1 [style="filled", fillcolor="lightgreen", `);

    // Enhanced edge color change logic - modified to match DOT format
    const lines = themedDotContent.split('\n');
    const coloredLines = lines.map(line => {
      // More accurate edge line pattern matching
      if (line.includes('->') && !line.includes('//')) { // Edge line that's not a comment
        const trimmed = line.trim();

        // Find source -> target pattern (handling cases with and without attributes)
        const parts = trimmed.split('->');
        if (parts.length === 2) {
          const source = parts[0].trim();
          let target = parts[1].trim();

          // Extract pure target name by removing semicolons or attributes
          const targetName = target.split(/[\[\s;]/)[0].trim();

          // Check if attributes already exist
          const hasAttributes = target.includes('[');

          // 1. dependents -> center (arrows coming into the center module)
          if (targetName === centerModuleName) {
            if (hasAttributes) {
              // Add color to existing attributes - use darker color in dark theme
              const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
              return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
            } else {
              // Add new attributes - use darker color in dark theme
              const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
              return line.replace(/;/, ` [color="${arrowColor}", penwidth=1.5];`);
            }
          }
          // 2. center -> dependencies (arrows going out from the center module)
          else if (source === centerModuleName) {
            if (hasAttributes) {
              // Add color to existing attributes - use darker color in dark theme
              const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
              return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
            } else {
              // Add new attributes - use darker color in dark theme
              const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
              return line.replace(/;/, ` [color="${arrowColor}", penwidth=1.5];`);
            }
          }
        }
      }
      return line;
    });

    themedDotContent = coloredLines.join('\n');
  }

  // Create HTML content without embedding the data directly
  const htmlContent = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ReScript Dependency Graph</title>
    <script src="https://cdn.jsdelivr.net/npm/@viz-js/viz@3.12.0/lib/viz-standalone.min.js"></script>
    <style>
        /* Only keep basic styles */
        body {
            margin: 0;
            padding: 10px;
            font-family: -apple-system, BlinkMacSystemFont, sans-serif;
            background-color: var(--vscode-editor-background);
            color: var(--vscode-editor-foreground);
            overflow: hidden;
            box-sizing: border-box;
            width: 100%;
            height: 100vh;
            user-select: none;
        }
        
        body.vscode-dark {
            background-color: var(--vscode-editor-background, #1e1e1e) !important;
        }
        
        body.vscode-light {
            background-color: var(--vscode-editor-background, #ffffff) !important;
        }
        
        #graph-container {
            width: 100%;
            height: calc(100vh - 60px);
            overflow: hidden;
            display: flex;
            justify-content: center;
            align-items: center;
            position: relative;
            user-select: none;
            background-color: transparent;
        }
        
        #graph {
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            width: 100%;
            height: 100%;
            cursor: move;
            user-select: none;
        }
        
        /* Top controls wrapper */
        .top-controls-wrapper {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 10px;
            width: 100%;
            padding: 0 20px;
            box-sizing: border-box;
        }
        
        /* Legend styles - changed to lines */
        .legend {
            display: flex;
            justify-content: flex-start;
            gap: 20px;
            position: relative;
            z-index: 10;
        }
        
        .legend-item {
            display: flex;
            align-items: center;
        }
        
        .legend-line {
            width: 24px;
            height: 2px;
            margin-right: 5px;
        }
        
        /* Search filter styles */
        .search-container {
            display: flex;
            justify-content: flex-end;
            align-items: center;
            position: relative;
            z-index: 10;
        }
        
        button {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 6px 12px;
            border-radius: 4px;
            cursor: pointer;
        }
        
        button:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        
        .zoom-controls {
            display: flex;
            gap: 5px;
        }
        
        .search-input {
            padding: 4px 8px;
            border-radius: 4px;
            border: 1px solid var(--vscode-input-border);
            background-color: var(--vscode-input-background);
            color: var(--vscode-input-foreground);
            margin-right: 5px;
            width: 200px;
        }
        
        .search-input:focus {
            outline: 1px solid var(--vscode-focusBorder);
        }
        
        .search-input.not-found {
            border-color: var(--vscode-inputValidation-errorBorder);
            outline: 1px solid var(--vscode-inputValidation-errorBorder);
        }
        
        .search-button {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 4px 8px;
            border-radius: 4px;
            cursor: pointer;
        }
        
        .search-button:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        
        .search-message {
            position: absolute;
            top: 100%;
            left: 0;
            font-size: 12px;
            color: var(--vscode-inputValidation-errorForeground);
            margin-top: 3px;
            background-color: var(--vscode-inputValidation-errorBackground);
            padding: 2px 6px;
            border-radius: 3px;
            opacity: 0;
            transition: opacity 0.3s;
            pointer-events: none;
        }
        
        .search-message.visible {
            opacity: 1;
            white-space: nowrap;
        }
        
        /* Define colors for arrows that match the legend */
        :root {
            /* These variables will be assigned at runtime based on theme */
            --dependents-color: lightblue;
            --dependencies-color: lightcoral;
            --unused-color: #ff6666;
        }
        
        /* Dark theme arrow colors */
        body.vscode-dark .edge.dependent-edge path {
            stroke: var(--dependents-color, steelblue) !important;
        }
        
        body.vscode-dark .edge.dependent-edge polygon {
            fill: var(--dependents-color, steelblue) !important;
            stroke: var(--dependents-color, steelblue) !important;
        }
        
        body.vscode-dark .edge.dependency-edge path {
            stroke: var(--dependencies-color, indianred) !important;
        }
        
        body.vscode-dark .edge.dependency-edge polygon {
            fill: var(--dependencies-color, indianred) !important;
            stroke: var(--dependencies-color, indianred) !important;
        }
        
        /* Light theme arrow colors */
        body.vscode-light .edge.dependent-edge path {
            stroke: var(--dependents-color, lightblue) !important;
        }
        
        body.vscode-light .edge.dependent-edge polygon {
            fill: var(--dependents-color, lightblue) !important;
            stroke: var(--dependents-color, lightblue) !important;
        }
        
        body.vscode-light .edge.dependency-edge path {
            stroke: var(--dependencies-color, lightcoral) !important;
        }
        
        body.vscode-light .edge.dependency-edge polygon {
            fill: var(--dependencies-color, lightcoral) !important;
            stroke: var(--dependencies-color, lightcoral) !important;
        }
        
        /* Error message container */
        #error-container {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            background-color: var(--vscode-editor-background);
            border: 1px solid var(--vscode-editorError-foreground);
            padding: 20px;
            border-radius: 5px;
            max-width: 80%;
            text-align: center;
            display: none;
            z-index: 100;
        }
        
        #error-message {
            color: var(--vscode-editorError-foreground);
            margin-bottom: 15px;
        }
        
        #error-tip {
            font-style: italic;
            margin-top: 10px;
            font-size: 0.9em;
        }
        
        /* Directly specify SVG styles */
        svg {
            background-color: transparent !important;
        }
        
        /* SVG element styles for dark theme */
        body.vscode-dark svg .node rect,
        body.vscode-dark svg .node polygon {
            fill: #1e1e1e !important;
            stroke: #aaaaaa !important;
        }
        
        body.vscode-dark svg .node text {
            fill: #cccccc !important;
        }
        
        body.vscode-dark svg .edge path {
            stroke: #555555 !important;
        }
        
        body.vscode-dark svg .edge polygon {
            fill: #555555 !important;
            stroke: #555555 !important;
        }
        
        /* SVG element styles for light theme */
        body.vscode-light svg .node rect,
        body.vscode-light svg .node polygon {
            fill: #f0f0f0 !important;
            stroke: #666666 !important;
        }
        
        body.vscode-light svg .node text {
            fill: #333333 !important;
        }
        
        body.vscode-light svg .edge path {
            stroke: #cccccc !important;
        }
        
        body.vscode-light svg .edge polygon {
            fill: #cccccc !important;
            stroke: #cccccc !important;
        }
    </style>
</head>
<body class="${isDarkTheme ? 'vscode-dark' : 'vscode-light'}">
    <div class="top-controls-wrapper">
        <div class="legend">
            <div class="legend-item" id="dependents-legend" style="display: none;">
                <div class="legend-line" style="background-color: var(--dependents-color, lightblue);"></div>
                <span>Dependents (modules that use the center module)</span>
            </div>
            <div class="legend-item" id="dependencies-legend" style="display: none;">
                <div class="legend-line" style="background-color: var(--dependencies-color, lightcoral);"></div>
                <span>Dependencies (modules used by the center module)</span>
            </div>
            <div class="legend-item" id="unused-modules-legend" style="display: none;">
                <div class="legend-line" style="background-color: var(--unused-color, #ff6666);"></div>
                <span>Modules with no dependents (unused modules)</span>
            </div>
        </div>
        <div class="search-container">
            <input type="text" class="search-input" id="module-search" placeholder="Search for module..." />
            <button class="search-button" id="search-button">Search</button>
            <div class="search-message" id="search-message">Module not found</div>
        </div>
    </div>
    <div id="graph-container">
        <div id="graph"></div>
        <div id="error-container">
            <div id="error-message"></div>
            <div id="error-tip">Try focusing on a specific module instead of viewing the entire graph.</div>
        </div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();
        let svgElement;
        // Zoom-related variables
        let currentZoom = 1;
        const MIN_ZOOM = 0.1;
        const MAX_ZOOM = 5;
        const ZOOM_SPEED = 0.1;
        
        // Drag-related variables
        let isDragging = false;
        let lastX = 0;
        let lastY = 0;
        let viewBox = { x: 0, y: 0, width: 1000, height: 1000 };
        
        // Initial data placeholders - will be populated via message
        let dotSrc = '';
        let isFocusedMode = false;
        let centerModule = null;
        let isUnusedModulesMode = false;
        let allModuleNodes = []; // Store all available module names
        
        // Theme-related variables - detect theme from body class during initialization
        let isDarkTheme = document.body.classList.contains('vscode-dark');
        
        // Set colors based on theme
        document.documentElement.style.setProperty('--dependents-color', isDarkTheme ? 'steelblue' : 'lightblue');
        document.documentElement.style.setProperty('--dependencies-color', isDarkTheme ? 'indianred' : 'lightcoral');
        document.documentElement.style.setProperty('--unused-color', '#ff6666');
        
        // Function to update SVG styles to match the theme
        function updateSvgStylesForTheme(svg, isDark) {
            if (!svg) return;
            
            // Set SVG background color
            svg.style.backgroundColor = 'transparent';
            
            // Update node styles
            const nodeRects = svg.querySelectorAll('.node rect, .node polygon');
            const bgColor = isDark ? '#1e1e1e' : '#f0f0f0';
            nodeRects.forEach(rect => {
                rect.setAttribute('fill', bgColor);
                rect.setAttribute('stroke', isDark ? '#aaaaaa' : '#666666');
                rect.setAttribute('stroke-width', '1px');
                // Add rounded corners
                if (rect.tagName.toLowerCase() === 'rect') {
                    rect.setAttribute('rx', '4');
                    rect.setAttribute('ry', '4');
                }
            });
            
            // Update text colors
            const nodeTexts = svg.querySelectorAll('.node text');
            nodeTexts.forEach(text => {
                text.setAttribute('fill', isDark ? '#cccccc' : '#333333');
            });
            
            // Update edge colors
            const edgePaths = svg.querySelectorAll('.edge path');
            const arrowColor = isDark ? '#555555' : '#cccccc';
            edgePaths.forEach(path => {
                path.setAttribute('stroke', arrowColor);
                path.setAttribute('fill', 'none');
                path.setAttribute('stroke-width', '1.2');
            });
            
            const arrowHeads = svg.querySelectorAll('.edge polygon');
            arrowHeads.forEach(head => {
                head.setAttribute('fill', arrowColor);
                head.setAttribute('stroke', arrowColor);
            });
            
            // Update focused module styles
            if (isFocusedMode && centerModule) {
                updateFocusedModuleStyles(svg, isDark);
            }
        }
        
        // Function to update styles for focused module
        function updateFocusedModuleStyles(svg, isDark) {
            if (!svg || !centerModule) return;
            
            const edges = svg.querySelectorAll('.edge');
            edges.forEach(edge => {
                const titleEl = edge.querySelector('title');
                if (titleEl && titleEl.textContent) {
                    const titleText = titleEl.textContent;
                    const parts = titleText.split('->');
                    if (parts.length === 2) {
                        const target = parts[1].trim();
                        const source = parts[0].trim();
                        
                        // 1. Dependents -> Center direction (arrows coming into center)
                        if (target === centerModule) {
                            // Add dependent-edge class for CSS targeting
                            edge.classList.add('dependent-edge');
                            
                            const paths = edge.querySelectorAll('path');
                            const polygons = edge.querySelectorAll('polygon');
                            const arrowColor = isDark ? 'steelblue' : 'lightblue';
                            
                            paths.forEach(path => {
                                path.setAttribute('stroke', arrowColor);
                                path.setAttribute('stroke-width', '1.5');
                            });
                            
                            polygons.forEach(polygon => {
                                polygon.setAttribute('fill', arrowColor);
                                polygon.setAttribute('stroke', arrowColor);
                            });
                        }
                        // 2. Center -> Dependencies direction (arrows going out from the center module)
                        else if (source === centerModule) {
                            // Add dependency-edge class for CSS targeting
                            edge.classList.add('dependency-edge');
                            
                            const paths = edge.querySelectorAll('path');
                            const polygons = edge.querySelectorAll('polygon');
                            const arrowColor = isDark ? 'indianred' : 'lightcoral';
                            
                            paths.forEach(path => {
                                path.setAttribute('stroke', arrowColor);
                                path.setAttribute('stroke-width', '1.5');
                            });
                            
                            polygons.forEach(polygon => {
                                polygon.setAttribute('fill', arrowColor);
                                polygon.setAttribute('stroke', arrowColor);
                            });
                        }
                    }
                }
            });
        }
        
        // Function to show error with better UI
        function showErrorMessage(error) {
            // Hide graph
            const graphElem = document.getElementById('graph');
            if (graphElem) {
                graphElem.style.display = 'none';
            }
            
            // Show error container
            const errorContainer = document.getElementById('error-container');
            const errorMessage = document.getElementById('error-message');
            
            if (errorContainer && errorMessage) {
                // Check for common memory-related errors
                if (error.message && (
                    error.message.includes('Cannot enlarge memory arrays') || 
                    error.message.includes('TOTAL_MEMORY') ||
                    error.message.includes('ALLOW_MEMORY_GROWTH')
                )) {
                    errorMessage.innerHTML = 'The project is too large to visualize in the browser.<br/>Memory limit exceeded.';
                } else {
                    errorMessage.innerHTML = error.message || 'Unknown error occurred';
                }
                
                errorContainer.style.display = 'block';
            }
            
            console.error('Graph rendering error:', error);
        }
        
        // Function to render the DOT data - only called after we receive data
        function renderGraph() {
            if (!dotSrc) {
                console.log('No DOT data yet');
                return;
            }
            
            // Check current theme state again (for accurate application when theme changes)
            isDarkTheme = document.body.classList.contains('vscode-dark');
            
            // Update theme-based color settings
            document.documentElement.style.setProperty('--dependents-color', isDarkTheme ? 'steelblue' : 'lightblue');
            document.documentElement.style.setProperty('--dependencies-color', isDarkTheme ? 'indianred' : 'lightcoral');
            document.documentElement.style.setProperty('--unused-color', '#ff6666');
            
            // Directly set body background color
            document.body.style.backgroundColor = isDarkTheme ? '#1e1e1e' : '#ffffff';
            
            // Set graph container background color
            const graphContainer = document.getElementById('graph-container');
            if (graphContainer) {
                graphContainer.style.backgroundColor = 'transparent';
            }
            
            Viz.instance().then(viz => {
              const element = viz.renderSVGElement(dotSrc);
              
              try {
                // Hide any previous error
                const errorContainer = document.getElementById('error-container');
                if (errorContainer) {
                    errorContainer.style.display = 'none';
                }
                
                // Show graph
                const graphElem = document.getElementById('graph');
                if (graphElem) {
                    graphElem.style.display = 'block';
                }
                
                const container = document.getElementById('graph');
                // Remove any existing graph
                while (container.firstChild) {
                    container.removeChild(container.firstChild);
                }
                
                container.appendChild(element);
                svgElement = element;
                
                // Add selection prevention styles to SVG element
                svgElement.style.userSelect = 'none';
                svgElement.style.webkitUserSelect = 'none';
                svgElement.style.msUserSelect = 'none';
                
                // Collect all module names for search functionality
                allModuleNodes = [];
                const nodes = svgElement.querySelectorAll('.node');
                nodes.forEach(node => {
                    const titleEl = node.querySelector('title');
                    if (titleEl && titleEl.textContent) {
                        allModuleNodes.push(titleEl.textContent.trim());
                    }
                });
                
                // Detect theme and add class
                if (isDarkTheme) {
                    document.body.classList.add('dark-theme');
                }
                
                // Direct SVG modification - change all node background colors
                const nodeRects = svgElement.querySelectorAll('.node rect, .node polygon');
                
                // Base colors based on theme
                const standardBgColor = isDarkTheme ? '#1e1e1e' : '#f0f0f0';
                const unusedModulesBgColor = isDarkTheme ? '#4b1d1d' : '#ffeeee'; // Reddish background
                
                // Choose color based on mode
                const bgColor = isUnusedModulesMode ? unusedModulesBgColor : standardBgColor;
                
                nodeRects.forEach(rect => {
                    rect.setAttribute('fill', bgColor);
                    // Also set border clearly
                    const borderColor = isUnusedModulesMode ? 
                                      (isDarkTheme ? '#cc6666' : '#cc6666') : 
                                      (isDarkTheme ? '#aaaaaa' : '#666666');
                    rect.setAttribute('stroke', borderColor);
                    rect.setAttribute('stroke-width', isUnusedModulesMode ? '1.5px' : '1px');
                    // Add rounded corners
                    if (rect.tagName.toLowerCase() === 'rect') {
                        rect.setAttribute('rx', '4');
                        rect.setAttribute('ry', '4');
                    }
                });
                
                // Change text colors
                const nodeTexts = svgElement.querySelectorAll('.node text');
                if (isDarkTheme) {
                    nodeTexts.forEach(text => {
                        text.setAttribute('fill', '#cccccc'); // Light gray
                    });
                } else {
                    nodeTexts.forEach(text => {
                        text.setAttribute('fill', '#333333'); // Dark gray
                    });
                }
                
                // Set SVG background color - set background color for the top-level g element
                const rootG = svgElement.querySelector('g');
                if (rootG) {
                    // Use transparent background in SVG and container background color
                    rootG.style.backgroundColor = 'transparent';
                }
                
                // Set background for SVG (entire SVG area)
                svgElement.style.backgroundColor = 'transparent';
                
                // Update document and container background colors
                document.body.style.backgroundColor = isDarkTheme ? '#1e1e1e' : '#ffffff';
                const graphContainer = document.getElementById('graph-container');
                if (graphContainer) {
                    graphContainer.style.backgroundColor = 'transparent';
                }
                
                // Modify arrow styles - set border and background color identically
                const edgePaths = svgElement.querySelectorAll('.edge path');
                const arrowColor = isDarkTheme ? '#555555' : '#cccccc';
                edgePaths.forEach(path => {
                    path.setAttribute('stroke', arrowColor);
                    // Set fill to none so it only affects arrow heads
                    path.setAttribute('fill', 'none');
                    path.setAttribute('stroke-width', '1.2');
                });
                
                // Adjust arrow head styles
                const arrowHeads = svgElement.querySelectorAll('.edge polygon');
                arrowHeads.forEach(head => {
                    head.setAttribute('fill', arrowColor);
                    head.setAttribute('stroke', arrowColor);
                });
                
                // Handle edge colors for center module in focus mode
                if (isFocusedMode && centerModule) {
                    // Identify center module node (title text matches center module name)
                    const centerNode = Array.from(svgElement.querySelectorAll('.node')).find(node => {
                        const titleEl = node.querySelector('title');
                        return titleEl && titleEl.textContent === centerModule;
                    });
                    
                    if (centerNode) {
                        // Process arrows coming into the center module (Dependents)
                        const edgesTo = svgElement.querySelectorAll('.edge');
                        edgesTo.forEach(edge => {
                            const titleEl = edge.querySelector('title');
                            if (titleEl && titleEl.textContent) {
                                const titleText = titleEl.textContent;
                                // Arrow titles typically in "source->target" format
                                const parts = titleText.split('->');
                                if (parts.length === 2) {
                                    const source = parts[0].trim();
                                    const target = parts[1].trim();
                                    
                                    // 1. Dependents -> Center direction (arrows coming into center)
                                    if (target === centerModule) {
                                        // Add class for CSS styling
                                        edge.classList.add('dependent-edge');
                                        
                                        const paths = edge.querySelectorAll('path');
                                        const polygons = edge.querySelectorAll('polygon');
                                        
                                        // Use colors that match the legend
                                        const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
                                        
                                        paths.forEach(path => {
                                            path.setAttribute('stroke', arrowColor);
                                            path.setAttribute('stroke-width', '1.5');
                                        });
                                        
                                        polygons.forEach(polygon => {
                                            polygon.setAttribute('fill', arrowColor);
                                            polygon.setAttribute('stroke', arrowColor);
                                        });
                                    }
                                    // 2. Center -> Dependencies direction (arrows going out from center)
                                    else if (source === centerModule) {
                                        // Add class for CSS styling
                                        edge.classList.add('dependency-edge');
                                        
                                        const paths = edge.querySelectorAll('path');
                                        const polygons = edge.querySelectorAll('polygon');
                                        
                                        // Use colors that match the legend
                                        const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
                                        
                                        paths.forEach(path => {
                                            path.setAttribute('stroke', arrowColor);
                                            path.setAttribute('stroke-width', '1.5');
                                        });
                                        
                                        polygons.forEach(polygon => {
                                            polygon.setAttribute('fill', arrowColor);
                                            polygon.setAttribute('stroke', arrowColor);
                                        });
                                    }
                                }
                            }
                        });
                    }
                } else {
                    // Distinguish colors in full mode - dependencies (outgoing arrows) and dependents (incoming arrows)
                    const edges = svgElement.querySelectorAll('.edge');
                    const visitedNodes = new Set();
                    
                    // First pass: collect all node names
                    edges.forEach(edge => {
                        const titleEl = edge.querySelector('title');
                        if (titleEl && titleEl.textContent) {
                            const titleText = titleEl.textContent;
                            const parts = titleText.split('->');
                            if (parts.length === 2) {
                                const source = parts[0].trim();
                                const target = parts[1].trim();
                                visitedNodes.add(source);
                                visitedNodes.add(target);
                            }
                        }
                    });
                    
                    // Second pass: modify all edge colors
                    edges.forEach(edge => {
                        const titleEl = edge.querySelector('title');
                        if (titleEl && titleEl.textContent) {
                            const titleText = titleEl.textContent;
                            const parts = titleText.split('->');
                            if (parts.length === 2) {
                                const source = parts[0].trim();
                                const target = parts[1].trim();
                                
                                // Outgoing arrows use dependencies color
                                const paths = edge.querySelectorAll('path');
                                const polygons = edge.querySelectorAll('polygon');
                                
                                // Darker color for dark theme
                                const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
                                
                                paths.forEach(path => {
                                    path.setAttribute('stroke', arrowColor);
                                    path.setAttribute('stroke-width', '1.2');
                                });
                                
                                polygons.forEach(polygon => {
                                    polygon.setAttribute('fill', arrowColor);
                                    polygon.setAttribute('stroke', arrowColor);
                                });
                            }
                        }
                    });
                }
                
                // Initial viewBox setup
                const bbox = svgElement.getBBox();
                viewBox = {
                    x: bbox.x,
                    y: bbox.y,
                    width: bbox.width,
                    height: bbox.height
                };
                svgElement.setAttribute('width', '100%');
                svgElement.setAttribute('height', '100%');
                
                // Add styles to SVG element to fill the entire area
                svgElement.style.display = 'block';
                svgElement.style.width = '100%';
                svgElement.style.height = '100%';
                svgElement.style.margin = '0';
                svgElement.style.padding = '0';
                
                // Set preserveAspectRatio attribute to fit to screen
                svgElement.setAttribute('preserveAspectRatio', 'xMidYMid meet');
                
                updateViewBox();
                
                // Add click handlers to nodes
                setupNodeClickHandlers();
                
                // Setup dragging
                setupDragHandlers();
                
                // Adjust initial view to fit screen
                fitGraphToContainer();
                
                // Set up scroll zoom
                setupScrollZoom();
              } catch (error) {
                showErrorMessage(error);
              }
            })
        }
        
        function setupNodeClickHandlers() {
            if (!svgElement) return;
            
            // Process all nodes
            const nodes = svgElement.querySelectorAll('.node');
            nodes.forEach(node => {
                const titleEl = node.querySelector('title');
                if (!titleEl || !titleEl.textContent) return;
                
                const moduleName = titleEl.textContent.trim();
                if (!moduleName) return;
                
                // Check if this is the center module (only in focused mode)
                if (isFocusedMode && centerModule && moduleName === centerModule) {
                    // Only apply click prevention style to center module
                    node.style.cursor = 'default';
                    
                    // Disable click event
                    node.addEventListener('click', (e) => {
                        e.preventDefault();
                        e.stopPropagation();
                        return false;
                    }, true);
                } else {
                    // Add click event to regular modules
                    node.style.cursor = 'pointer';
                    node.addEventListener('click', (e) => {
                        e.preventDefault();
                        e.stopPropagation();
                        
                        vscode.postMessage({
                            command: 'focusModule',
                            moduleName: moduleName
                        });
                    });
                }
            });
        }
        
        function updateViewBox() {
            if (!svgElement) return;
            svgElement.setAttribute('viewBox', 
                viewBox.x + " " + viewBox.y + " " + viewBox.width + " " + viewBox.height);
        }
        
        function setupDragHandlers() {
            if (!svgElement) return;
            
            let isDragging = false;
            let lastX = 0;
            let lastY = 0;
            
            // Mouse down - start dragging
            svgElement.addEventListener('mousedown', (e) => {
                e.preventDefault(); // Prevent text selection
                isDragging = true;
                lastX = e.clientX;
                lastY = e.clientY;
                svgElement.style.cursor = 'grabbing';
            });
            
            // Mouse move - during dragging
            document.addEventListener('mousemove', (e) => {
                if (!isDragging) return;
                
                const dx = e.clientX - lastX;
                const dy = e.clientY - lastY;
                lastX = e.clientX;
                lastY = e.clientY;
                
                // Move with fixed ratio - Transform to SVG coordinate system
                const svgRect = svgElement.getBoundingClientRect();
                
                // Calculate SVG scale (how many SVG coordinates are represented per screen pixel)
                const scaleX = viewBox.width / svgRect.width;
                const scaleY = viewBox.height / svgRect.height;
                
                // Apply consistent movement ratio
                // Use max value for scale to maintain consistent movement in both x and y
                const scale = Math.max(scaleX, scaleY);
                
                // Update viewBox
                viewBox.x -= dx * scale;
                viewBox.y -= dy * scale;
                
                updateViewBox();
            });
            
            // Mouse up - end dragging
            document.addEventListener('mouseup', () => {
                isDragging = false;
                svgElement.style.cursor = 'grab';
            });
            
            // End dragging if mouse leaves SVG area
            document.addEventListener('mouseleave', () => {
                isDragging = false;
                svgElement.style.cursor = 'grab';
            });
            
            // Initial cursor style
            svgElement.style.cursor = 'grab';
        }
        
        function setupScrollZoom() {
            // Mouse wheel event
            document.getElementById('graph-container').addEventListener('wheel', (e) => {
                e.preventDefault();
                
                // Get mouse position
                const svgRect = svgElement.getBoundingClientRect();
                const mouseX = e.clientX - svgRect.left;
                const mouseY = e.clientY - svgRect.top;
                
                // Calculate relative position within SVG (0-1 range)
                const relativeX = mouseX / svgRect.width;
                const relativeY = mouseY / svgRect.height;
                
                // Calculate actual coordinates in viewBox
                const pointX = viewBox.x + (viewBox.width * relativeX);
                const pointY = viewBox.y + (viewBox.height * relativeY);
                
                // Zoom in/out (based on wheel direction)
                const zoomFactor = e.deltaY > 0 ? (1 - ZOOM_SPEED) : (1 + ZOOM_SPEED);
                zoomByFactor(zoomFactor, { x: pointX, y: pointY });
            });
            
            // Double-click to zoom in
            document.getElementById('graph-container').addEventListener('dblclick', (e) => {
                e.preventDefault(); // Prevent text selection
                // Get mouse position
                const svgRect = svgElement.getBoundingClientRect();
                const mouseX = e.clientX - svgRect.left;
                const mouseY = e.clientY - svgRect.top;
                
                // Calculate relative position within SVG
                const relativeX = mouseX / svgRect.width;
                const relativeY = mouseY / svgRect.height;
                
                // Calculate actual coordinates in viewBox
                const pointX = viewBox.x + (viewBox.width * relativeX);
                const pointY = viewBox.y + (viewBox.height * relativeY);
                
                // Zoom in
                zoomByFactor(0.7, { x: pointX, y: pointY });
            });
            
            // Right-click to reset zoom
            document.getElementById('graph-container').addEventListener('contextmenu', (e) => {
                e.preventDefault(); // Prevent context menu
                resetZoom();
            });
        }
        
        function zoomByFactor(factor, point) {
            if (!svgElement) return;
            
            // Current zoom calculation
            const oldZoom = currentZoom;
            currentZoom = Math.max(MIN_ZOOM, Math.min(MAX_ZOOM, currentZoom * factor));
            
            // Calculate actual scaling factor
            const realFactor = currentZoom / oldZoom;
            
            // Calculate zoom at the current pointer position
            const newWidth = viewBox.width / realFactor;
            const newHeight = viewBox.height / realFactor;
            
            // Calculate new coordinates based on pointer position
            const mouseRatioX = (point.x - viewBox.x) / viewBox.width;
            const mouseRatioY = (point.y - viewBox.y) / viewBox.height;
            
            const newX = point.x - mouseRatioX * newWidth;
            const newY = point.y - mouseRatioY * newHeight;
            
            // Update viewBox
            viewBox = {
                x: newX,
                y: newY,
                width: newWidth,
                height: newHeight
            };
            
            updateViewBox();
        }
        
        function resetZoom() {
            if (!svgElement) return;
            
            // Adjust graph size to fit the screen
            fitGraphToContainer();
        }
        
        function fitGraphToContainer() {
            if (!svgElement) return;
            
            // Get container size
            const container = document.getElementById('graph-container');
            const containerRect = container.getBoundingClientRect();
            
            // Get SVG's viewBox
            const bbox = svgElement.getBBox();
            
            // Calculate ratio between container size and graph size
            const widthRatio = containerRect.width / bbox.width;
            const heightRatio = containerRect.height / bbox.height;
            
            // Use smaller ratio to fit graph completely
            const ratio = Math.min(widthRatio, heightRatio) * 0.7; // 70% margin
            
            // Limit maximum zoom (prevents excessive zoom for small graphs)
            const limitedRatio = Math.min(ratio, 0.7);
            
            // Calculate new viewBox
            const newWidth = bbox.width * 1.5; // Add some padding on both sides
            const newHeight = bbox.height * 1.5; // Add some padding on both sides
            const centerX = bbox.x + bbox.width / 2;
            const centerY = bbox.y + bbox.height / 2;
            
            // Set viewBox to center the graph on the screen
            viewBox = {
                x: centerX - newWidth / 2,
                y: centerY - newHeight / 2,
                width: newWidth,
                height: newHeight
            };
            
            // Apply calculated viewBox
            updateViewBox();
            
            // Set initial zoom level
            currentZoom = limitedRatio;
        }
        
        // Function to locate and center a specific module
        function findAndCenterModule(moduleName) {
            if (!svgElement || !moduleName) return false;
            
            // Clear any previous error state
            const searchInput = document.getElementById('module-search');
            const searchMessage = document.getElementById('search-message');
            if (searchInput) {
                searchInput.classList.remove('not-found');
            }
            if (searchMessage) {
                searchMessage.classList.remove('visible');
            }
            
            // Normalize module name for case-insensitive search
            const searchName = moduleName.trim();
            
            // Find matching module (exact match first)
            let matchingNode = Array.from(svgElement.querySelectorAll('.node')).find(node => {
                const titleEl = node.querySelector('title');
                return titleEl && titleEl.textContent === searchName;
            });
            
            // If no exact match, try case-insensitive search
            if (!matchingNode) {
                const lowerSearchName = searchName.toLowerCase();
                matchingNode = Array.from(svgElement.querySelectorAll('.node')).find(node => {
                    const titleEl = node.querySelector('title');
                    return titleEl && titleEl.textContent && titleEl.textContent.toLowerCase() === lowerSearchName;
                });
            }
            
            // If still no match, try partial match
            if (!matchingNode && searchName.length >= 2) { // Only for searches with at least 2 chars
                matchingNode = Array.from(svgElement.querySelectorAll('.node')).find(node => {
                    const titleEl = node.querySelector('title');
                    return titleEl && titleEl.textContent && 
                           titleEl.textContent.toLowerCase().includes(searchName.toLowerCase());
                });
            }
            
            if (matchingNode) {
                // Get the bounding box of the found node
                const nodeBBox = matchingNode.getBBox();
                
                // Calculate center point of the node
                const centerX = nodeBBox.x + nodeBBox.width / 2;
                const centerY = nodeBBox.y + nodeBBox.height / 2;
                
                // Set a specific zoom level for better visibility
                const targetZoom = 0.7; // Adjust as needed
                const zoomPoint = { x: centerX, y: centerY };
                
                // Apply zoom first
                const zoomFactor = targetZoom / currentZoom;
                zoomByFactor(zoomFactor, zoomPoint);
                
                // Then center the node in the viewport
                centerNodeInView(centerX, centerY);
                
                // Highlight the node visually
                highlightNode(matchingNode);
                
                return true;
            }
            
            return false;
        }
        
        // Center a specific point in the current viewport
        function centerNodeInView(centerX, centerY) {
            if (!svgElement) return;
            
            // Calculate target viewBox values
            const currentWidth = viewBox.width;
            const currentHeight = viewBox.height;
            
            const targetViewBox = {
                x: centerX - currentWidth / 2,
                y: centerY + currentHeight / 2,
                width: currentWidth,
                height: currentHeight
            };
            
            // Animate from current viewBox to target viewBox
            animateViewBox(targetViewBox);
        }
        
        // Animate viewBox changes smoothly
        function animateViewBox(targetViewBox, duration = 500) {
            if (!svgElement) return;
            
            const startViewBox = { ...viewBox };
            const startTime = performance.now();
            
            function easeInOutCubic(t) {
                return t < 0.5
                    ? 4 * t * t * t
                    : 1 - Math.pow(-2 * t + 2, 3) / 2;
            }
            
            function animate(currentTime) {
                const elapsed = currentTime - startTime;
                const progress = Math.min(elapsed / duration, 1);
                
                // Calculate current viewBox values using easing
                const easedProgress = easeInOutCubic(progress);
                
                viewBox = {
                    x: startViewBox.x + (targetViewBox.x - startViewBox.x) * easedProgress,
                    y: startViewBox.y + (targetViewBox.y - startViewBox.y) * easedProgress,
                    width: startViewBox.width + (targetViewBox.width - startViewBox.width) * easedProgress,
                    height: startViewBox.height + (targetViewBox.height - startViewBox.height) * easedProgress
                };
                
                updateViewBox();
                
                if (progress < 1) {
                    requestAnimationFrame(animate);
                }
            }
            
            requestAnimationFrame(animate);
        }
        
        // Highlight a node temporarily to make it more noticeable
        function highlightNode(node) {
            if (!node) return;
            
            // Save original styles
            const rects = node.querySelectorAll('rect, polygon');
            const originalStyles = [];
            
            rects.forEach(rect => {
                originalStyles.push({
                    element: rect,
                    fill: rect.getAttribute('fill'),
                    stroke: rect.getAttribute('stroke'),
                    strokeWidth: rect.getAttribute('stroke-width')
                });
                
                // Apply highlight styles
                rect.setAttribute('fill', '#ffff99');
                rect.setAttribute('stroke', '#ff9900');
                rect.setAttribute('stroke-width', '2px');
            });
            
            // Restore original styles after a delay
            setTimeout(() => {
                originalStyles.forEach(style => {
                    style.element.setAttribute('fill', style.fill || '');
                    style.element.setAttribute('stroke', style.stroke || '');
                    style.element.setAttribute('stroke-width', style.strokeWidth || '');
                });
            }, 2000); // Highlight for 2 seconds
        }
        
        // Setup search functionality
        function setupSearchFunctionality() {
            const searchInput = document.getElementById('module-search');
            const searchButton = document.getElementById('search-button');
            
            if (!searchInput || !searchButton) return;
            
            // Clear error state when typing in search field
            searchInput.addEventListener('input', () => {
                searchInput.classList.remove('not-found');
                const searchMessage = document.getElementById('search-message');
                if (searchMessage) {
                    searchMessage.classList.remove('visible');
                }
            });
            
            // Search function
            const performSearch = () => {
                const searchInput = document.getElementById('module-search');
                if (!searchInput) return;
                
                const searchTerm = searchInput.value.trim();
                if (searchTerm.length === 0) return;
                
                const found = findAndCenterModule(searchTerm);
                if (!found) {
                    // Show visual feedback when module not found
                    if (searchInput) {
                        searchInput.classList.add('not-found');
                    }
                    
                    // Show error message
                    const searchMessage = document.getElementById('search-message');
                    if (searchMessage) {
                        searchMessage.textContent = 'Module "' + searchTerm + '" not found';
                        searchMessage.classList.add('visible');
                        
                        // Hide message after a delay
                        setTimeout(() => {
                            if (searchMessage) {
                                searchMessage.classList.remove('visible');
                            }
                            if (searchInput) {
                                searchInput.classList.remove('not-found');
                            }
                        }, 3000);
                    }
                    
                    // Also log to console
                    console.log('Module not found: ' + searchTerm);
                }
            };
            
            // Search button click handler
            searchButton.addEventListener('click', performSearch);
            
            // Enter key in search input
            searchInput.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') {
                    performSearch();
                }
            });
            
            // Handle Cmd+F or Ctrl+F to focus search input
            document.addEventListener('keydown', (e) => {
                if ((e.metaKey || e.ctrlKey) && e.key === 'f') {
                    e.preventDefault(); // Prevent browser's default search
                    searchInput.focus();
                }
            });
        }
        
        // Listen for messages from VS Code
        window.addEventListener('message', event => {
            const message = event.data;
            if (message.command === 'initGraph') {
                // Receive the graph data from the extension
                dotSrc = message.dotContent;
                isFocusedMode = message.isFocusedMode;
                centerModule = message.centerModule;
                isUnusedModulesMode = message.isUnusedModulesMode || false;
                
                // Update legend display based on mode
                updateLegendDisplay();
                
                // Now that we have data, render the graph
                renderGraph();
                
                // Setup search after graph is rendered
                setupSearchFunctionality();
            } else if (message.command === 'updateGraph') {
                // Update graph with new data
                dotSrc = message.dotContent;
                isFocusedMode = message.isFocusedMode;
                centerModule = message.centerModule;
                isUnusedModulesMode = message.isUnusedModulesMode || false;
                
                // Update legend display based on mode
                updateLegendDisplay();
                
                // Clear search input when graph is redrawn
                const searchInput = document.getElementById('module-search');
                if (searchInput) {
                    searchInput.value = '';
                    searchInput.classList.remove('not-found');
                    
                    // Clear any error message
                    const searchMessage = document.getElementById('search-message');
                    if (searchMessage) {
                        searchMessage.classList.remove('visible');
                    }
                }
                
                // Re-render with new data
                renderGraph();
                
                // Refresh search setup
                setupSearchFunctionality();
            } else if (message.command === 'updateTheme') {
                // Refresh page instead of applying theme to graph
                dotSrc = message.dotContent;
                isFocusedMode = message.isFocusedMode;
                centerModule = message.centerModule;
                
                // Update current theme state
                isDarkTheme = message.isDarkTheme;
                
                // Apply theme changes
                document.documentElement.style.setProperty('--dependents-color', 
                    isDarkTheme ? 'steelblue' : 'lightblue');
                document.documentElement.style.setProperty('--dependencies-color', 
                    isDarkTheme ? 'indianred' : 'lightcoral');
                document.documentElement.style.setProperty('--unused-color', '#ff6666');
                
                // Update class of document
                if (isDarkTheme) {
                    document.body.classList.add('vscode-dark');
                    document.body.classList.remove('vscode-light');
                    document.body.style.backgroundColor = '#1e1e1e';  // Directly set background color
                } else {
                    document.body.classList.add('vscode-light');
                    document.body.classList.remove('vscode-dark');
                    document.body.style.backgroundColor = '#ffffff';  // Directly set background color
                }
                
                // Set graph container background color
                const graphContainer = document.getElementById('graph-container');
                if (graphContainer) {
                    graphContainer.style.backgroundColor = 'transparent';
                }
                
                // Update existing SVG elements directly
                if (svgElement) {
                    updateSvgStylesForTheme(svgElement, isDarkTheme);
                } else {
                    // If SVG doesn't exist, try rendering again
                    renderGraph();
                }
            } else if (message.command === 'searchModule') {
                // Handle search request from extension
                const moduleName = message.moduleName;
                if (moduleName) {
                    findAndCenterModule(moduleName);
                    // Update search input to show the search term
                    const searchInput = document.getElementById('module-search');
                    if (searchInput instanceof HTMLInputElement) {
                        searchInput.value = moduleName;
                    }
                }
            } else if (message.command === 'showError') {
                // Display an error message without rendering graph
                showErrorMessage({ message: message.errorMessage });
            }
        });
        
        // Function to update legend display based on current mode
        function updateLegendDisplay() {
            const dependentsLegend = document.getElementById('dependents-legend');
            const dependenciesLegend = document.getElementById('dependencies-legend');
            const unusedModulesLegend = document.getElementById('unused-modules-legend');
            
            if (!dependentsLegend || !dependenciesLegend || !unusedModulesLegend) return;
            
            if (isUnusedModulesMode) {
                // Show only unused modules legend in unused modules mode
                dependentsLegend.style.display = 'none';
                dependenciesLegend.style.display = 'none';
                unusedModulesLegend.style.display = 'flex';
            } else if (isFocusedMode) {
                // Show both legends in focused mode
                dependentsLegend.style.display = 'flex';
                dependenciesLegend.style.display = 'flex';
                unusedModulesLegend.style.display = 'none';
            } else {
                // Show no legend in regular mode
                dependentsLegend.style.display = 'none';
                dependenciesLegend.style.display = 'none';
                unusedModulesLegend.style.display = 'none';
            }
        }
        
        // Notify VS Code that webview is ready
        vscode.postMessage({ command: 'webviewReady' });
    </script>
</body>
</html>`;

  // Check if we already have a panel
  if (currentPanel) {
    // If we already have a panel, just update its html
    currentPanel.webview.html = htmlContent;

    // Set appropriate title based on mode
    if (isUnusedModulesMode) {
      currentPanel.title = 'ReScript: Unused Modules';
    } else if (isFocusedMode && centerModuleName) {
      currentPanel.title = `Module: ${centerModuleName} Dependencies`;
    } else {
      currentPanel.title = 'ReScript Dependencies';
    }

    currentPanel.reveal(vscode.ViewColumn.One);

    // Send the graph data after the webview is loaded
    currentPanel.webview.onDidReceiveMessage(
      message => {
        if (message.command === 'webviewReady') {
          currentPanel?.webview.postMessage({
            command: 'initGraph',
            dotContent: themedDotContent,
            isFocusedMode: isFocusedMode,
            centerModule: centerModuleName,
            isUnusedModulesMode: isUnusedModulesMode
          });
        }
      }
    );
  } else {
    // Create a new panel
    currentPanel = vscode.window.createWebviewPanel(
      'bibimbobVisualizer',
      isUnusedModulesMode ? 'ReScript: Unused Modules' :
        (isFocusedMode && centerModuleName ? `Module: ${centerModuleName} Dependencies` : 'ReScript Dependencies'),
      vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [
          vscode.Uri.file(path.join(context.extensionPath, 'media'))
        ]
      }
    );

    currentPanel.webview.html = htmlContent;

    // Handle panel disposal
    currentPanel.onDidDispose(() => {
      currentPanel = undefined;
    }, null, context.subscriptions);

    // Send the graph data after the webview is loaded
    currentPanel.webview.onDidReceiveMessage(
      message => {
        if (message.command === 'webviewReady') {
          currentPanel?.webview.postMessage({
            command: 'initGraph',
            dotContent: themedDotContent,
            isFocusedMode: isFocusedMode,
            centerModule: centerModuleName,
            isUnusedModulesMode: isUnusedModulesMode
          });
        }
      }
    );
  }

  // Listen for theme changes
  context.subscriptions.push(
    vscode.window.onDidChangeActiveColorTheme(theme => {
      const newIsDarkTheme = theme.kind === vscode.ColorThemeKind.Dark;
      if (currentPanel && currentDotContent) {
        // Regenerate DOT content based on new theme
        let updatedDotContent = currentDotContent;

        // Set appropriate graph attributes based on new theme
        const themeAttributes = newIsDarkTheme ?
          'bgcolor="transparent" fontcolor="#e0e0e0"' :
          'bgcolor="transparent" fontcolor="#333333"';

        // Set node styles based on new theme
        const nodeStyle = newIsDarkTheme ?
          'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#1e1e1e", margin="0.3,0.2", color="#aaaaaa", penwidth=1]' :
          'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#f0f0f0", margin="0.3,0.2", color="#666666", penwidth=1]';

        // Set edge styles based on new theme
        const edgeStyle = newIsDarkTheme ?
          'edge[color="#555555", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]' :
          'edge[color="#cccccc", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]';

        // Set basic properties
        updatedDotContent = updatedDotContent.replace(/^(digraph\s+\w+\s*\{)/m,
          `$1\n  ${themeAttributes}\n  ${nodeStyle}\n  ${edgeStyle}\n  splines=true\n  overlap=false\n  sep="+10"`);

        // Force apply node styles
        if (newIsDarkTheme) {
          updatedDotContent = updatedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#1e1e1e", ');
        } else {
          updatedDotContent = updatedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#f0f0f0", ');
        }

        // Send webview a command to update theme
        currentPanel.webview.postMessage({
          command: 'updateTheme',
          dotContent: updatedDotContent,
          isFocusedMode: currentIsFocusedMode,
          centerModule: currentCenterModule,
          isDarkTheme: newIsDarkTheme
        });
      }
    })
  );

  // Restore the message handler for webview communication
  if (currentPanel) { // Ensure panel exists before adding listener
    currentPanel.webview.onDidReceiveMessage(
      async message => {
        switch (message.command) {
          case 'openFile':
            try {
              let filePath = message.path;
              let lineNumber = message.line || 1;

              // Try to find the file in the project if path is not provided
              if (!filePath) {
                const moduleInfo = await findModuleInProject(message.moduleName);
                if (moduleInfo) {
                  filePath = moduleInfo.path;
                  lineNumber = moduleInfo.line;
                } else {
                  vscode.window.showWarningMessage(`File not found: ${message.moduleName}`);
                  return;
                }
              }

              // Create URI and open file
              const fileUri = vscode.Uri.file(filePath);
              const document = await vscode.workspace.openTextDocument(fileUri);
              await vscode.window.showTextDocument(document);

              // Move to specified line
              const editor = vscode.window.activeTextEditor;
              if (editor) {
                const position = new vscode.Position(lineNumber - 1, 0);
                editor.selection = new vscode.Selection(position, position);
                editor.revealRange(
                  new vscode.Range(position, position),
                  vscode.TextEditorRevealType.InCenter
                );
              }
            } catch (error) {
              vscode.window.showErrorMessage(`Failed to open file: ${error}`);
            }
            break;

          case 'searchModule':
            // Handle search request from extension UI
            if (message.moduleName) {
              // Simply forward the request back to the webview
              currentPanel?.webview.postMessage({
                command: 'searchModule',
                moduleName: message.moduleName
              });
            }
            break;

          case 'focusModule':
            try {
              const moduleName = message.moduleName;
              if (!moduleName) {
                vscode.window.showErrorMessage('No module name provided');
                return;
              }

              // Run a new dependency analysis focused on this module
              await vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: `ReScript: Analyzing dependencies for ${moduleName}...`,
                cancellable: true
              }, async (progress, token) => {

                // Re-calculate project root and bsDir for this focus request
                const workspaceFolders = vscode.workspace.workspaceFolders;
                if (!workspaceFolders || workspaceFolders.length === 0) {
                  vscode.window.showErrorMessage('No workspace folder open');
                  return;
                }
                const workspaceRoot = workspaceFolders[0].uri.fsPath;
                const configFileUri = await findConfigFile(workspaceRoot);
                if (!configFileUri) {
                  vscode.window.showErrorMessage('Could not find config file when focusing module.');
                  return;
                }
                const projectRoot = path.dirname(configFileUri.fsPath);
                const bsDir = path.join(projectRoot, 'lib', 'bs');

                if (!fs.existsSync(bsDir)) {
                  vscode.window.showWarningMessage(`Build directory not found: ${bsDir}`);
                  // Optionally return if bsDir is critical
                }

                // Find CLI path
                const cliPath = await findRescriptDepCLI(context);

                // Define CLI arguments - Use DOT format for better performance
                const args: string[] = ['--format=dot'];

                // Add module focus
                args.push('--module', moduleName);

                // Add bsDir target
                args.push(bsDir);

                // Get DOT format data
                const dotContent = await runRescriptDep(cliPath, args, context);

                if (token.isCancellationRequested) { return; }

                if (dotContent && currentPanel) { // Check panel existence again
                  // Detect if the current theme is dark
                  const isDarkTheme = vscode.window.activeColorTheme && vscode.window.activeColorTheme.kind === vscode.ColorThemeKind.Dark;

                  // Set theme style
                  const themeAttributes = isDarkTheme ?
                    'bgcolor="transparent" fontcolor="#e0e0e0"' :
                    'bgcolor="transparent" fontcolor="#333333"';

                  const nodeStyle = isDarkTheme ?
                    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#1e1e1e", margin="0.3,0.2", color="#aaaaaa", penwidth=1]' :
                    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#f0f0f0", margin="0.3,0.2", color="#666666", penwidth=1]';

                  const edgeStyle = isDarkTheme ?
                    'edge[color="#555555", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]' :
                    'edge[color="#cccccc", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]';

                  // Update DOT content - apply node styles directly
                  let themedDotContent = dotContent;

                  // Set basic properties
                  themedDotContent = themedDotContent.replace(/^(digraph\s+\w+\s*\{)/m,
                    `$1\n  ${themeAttributes}\n  ${nodeStyle}\n  ${edgeStyle}\n  splines=true\n  overlap=false\n  sep="+10"`);

                  // Force apply node styles
                  if (isDarkTheme) {
                    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#1e1e1e", ');
                  } else {
                    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#f0f0f0", ');
                  }

                  // Process center module and edge styles in focus mode
                  // Change center module node style
                  const centerNodePattern = new RegExp(`\\s+(${moduleName})\\s*\\[`);
                  themedDotContent = themedDotContent.replace(centerNodePattern, ` $1 [style="filled", fillcolor="lightgreen", `);

                  // Change edge colors
                  const lines = themedDotContent.split('\n');
                  const coloredLines = lines.map(line => {
                    // Edge line pattern matching
                    if (line.includes('->') && !line.includes('//')) {
                      const trimmed = line.trim();

                      // Find source -> target pattern
                      const parts = trimmed.split('->');
                      if (parts.length === 2) {
                        const source = parts[0].trim();
                        let target = parts[1].trim();

                        // Extract pure target name
                        const targetName = target.split(/[\[\s;]/)[0].trim();

                        // Check if attributes exist
                        const hasAttributes = target.includes('[');

                        // 1. dependents -> center (arrows coming into the center module)
                        if (targetName === moduleName) {
                          if (hasAttributes) {
                            // Add color to existing attributes - use darker color in dark theme
                            const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
                            return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
                          } else {
                            // Add new attributes - use darker color in dark theme
                            const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
                            return line.replace(/;/, ` [color="${arrowColor}", penwidth=1.5];`);
                          }
                        }
                        // 2. center -> dependencies (arrows going out from the center module)
                        else if (source === moduleName) {
                          if (hasAttributes) {
                            // Add color to existing attributes - use darker color in dark theme
                            const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
                            return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
                          } else {
                            // Add new attributes - use darker color in dark theme
                            const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
                            return line.replace(/;/, ` [color="${arrowColor}", penwidth=1.5];`);
                          }
                        }
                      }
                    }
                    return line;
                  });

                  themedDotContent = coloredLines.join('\n');

                  // Update panel title
                  currentPanel.title = `Module: ${moduleName} Dependencies`;

                  // Send message to update the graph
                  currentPanel.webview.postMessage({
                    command: 'updateGraph',
                    dotContent: themedDotContent,
                    isFocusedMode: true,
                    centerModule: moduleName,
                    isDarkTheme: isDarkTheme
                  });
                } else if (!dotContent) {
                  vscode.window.showErrorMessage(`Failed to generate dependency data for ${moduleName}`);
                } else {
                  console.warn("Panel closed before focusModule update could be sent.");
                }
              });
            } catch (error) {
              vscode.window.showErrorMessage(`Error analyzing module dependencies: ${error}`);
            }
            break;

          case 'webviewReady':
            break;
        }
      },
      undefined,
      context.subscriptions
    );
  }
}

// Helper function to try to find the module in the project when file path is unavailable
async function findModuleInProject(moduleName: string): Promise<{ path: string, line: number } | null> {
  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (!workspaceFolders) { return null; }

  // File extensions and directory list
  const extensions = ['.res', '.resi', '.re', '.rei', '.ml', '.mli']; // Common ReScript/OCaml extensions
  const rootPath = workspaceFolders[0].uri.fsPath; // Use first workspace root

  try {
    // Search for files named ModuleName.ext or moduleName.ext
    // Construct a glob pattern that matches case-insensitively if possible, or covers common casings
    const patterns = extensions.map(ext => `**/${moduleName}${ext}`);
    // Add lowercased version if different (simple case-insensitivity approximation)
    if (moduleName !== moduleName.toLowerCase()) {
      patterns.push(...extensions.map(ext => `**/${moduleName.toLowerCase()}${ext}`));
    }

    // Use findFiles with multiple patterns if needed, or a single complex one
    const filePattern = `{${patterns.join(',')}}`;

    const files = await vscode.workspace.findFiles(
      filePattern,
      '**/node_modules/**', // Standard exclusion
      10 // Limit results
    );

    if (files.length > 0) {
      // Prioritize .res over .resi, then .re over .rei, etc.
      const preferredOrder = ['.res', '.re', '.ml'];
      let bestMatch: vscode.Uri | undefined;

      for (const ext of preferredOrder) {
        bestMatch = files.find(f => f.fsPath.endsWith(`${moduleName}${ext}`));
        if (bestMatch) { break; }
      }
      // Fallback to the first file found if no preferred extension matches
      const targetFile = bestMatch || files[0];

      return {
        path: targetFile.fsPath,
        line: 1 // Default to first line
      };
    } else {
      // console.log(`Module file not found via glob: ${moduleName}`); // Removed log
    }
  } catch (error) {
    console.error("Error finding module via findFiles:", error);
  }

  return null;
}
