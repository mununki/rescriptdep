import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

// Command IDs
const SHOW_DEPENDENCY_GRAPH = 'bibimbob.showDependencyGraph';
const FOCUS_MODULE_DEPENDENCIES = 'bibimbob.focusModuleDependencies';
const CLEAR_CACHE = 'bibimbob.clearCache';

// Track the current webview panel
let currentPanel: vscode.WebviewPanel | undefined = undefined;

export function activate(context: vscode.ExtensionContext) {
  // console.log('Bibimbob is activated'); // Removed log
  // Command for full dependency graph
  let fullGraphCommand = vscode.commands.registerCommand(SHOW_DEPENDENCY_GRAPH, async () => {
    await generateDependencyGraph(context);
  });

  // Command for module-focused dependency graph
  let focusModuleCommand = vscode.commands.registerCommand(FOCUS_MODULE_DEPENDENCIES, async () => {
    await generateDependencyGraph(context, true);
  });

  // Command to clear the rescriptdep cache
  let clearCacheCommand = vscode.commands.registerCommand(CLEAR_CACHE, async () => {
    await clearRescriptDepCache(context);
  });

  context.subscriptions.push(fullGraphCommand);
  context.subscriptions.push(focusModuleCommand);
  context.subscriptions.push(clearCacheCommand);
}

// Function to clear the rescriptdep cache
async function clearRescriptDepCache(context: vscode.ExtensionContext): Promise<void> {
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    title: 'ReScript: Clearing dependency cache...',
    cancellable: false
  }, async (progress) => {
    try {
      // Get cache directory using globalStorageUri instead of globalStoragePath
      const cacheDir = vscode.Uri.joinPath(context.globalStorageUri, 'cache');
      const cacheDirPath = cacheDir.fsPath;

      if (fs.existsSync(cacheDirPath)) {
        // Read all files in the cache directory
        const files = fs.readdirSync(cacheDirPath);

        // Delete all cache files
        let deletedCount = 0;
        for (const file of files) {
          if (file.endsWith('.rescriptdep_cache.marshal')) {
            fs.unlinkSync(path.join(cacheDirPath, file));
            deletedCount++;
          }
        }

        if (deletedCount > 0) {
          vscode.window.showInformationMessage(`ReScript Dependency: Cleared ${deletedCount} cache file(s)`);
        } else {
          vscode.window.showInformationMessage('No ReScript dependency cache files found');
        }
      } else {
        vscode.window.showInformationMessage('No ReScript dependency cache directory found');
      }
    } catch (error) {
      console.error('Error clearing cache:', error);
      vscode.window.showErrorMessage(`Failed to clear cache: ${error instanceof Error ? error.message : String(error)}`);
    }
  });
}

// Helper function to get current module name from active editor
function getCurrentModuleNameFromActiveEditor(): string | undefined {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return undefined;

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

// Integrated common logic into a single function
async function generateDependencyGraph(context: vscode.ExtensionContext, focusOnModule: boolean = false) {
  // Use withProgress API to show a progress notification in the bottom right
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    title: focusOnModule ? 'ReScript: Analyzing module dependencies...' : 'ReScript: Analyzing dependency graph...',
    cancellable: true
  }, async (progress, token) => {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders || workspaceFolders.length === 0) { // Ensure folder exists
      vscode.window.showErrorMessage('No workspace folder open');
      return;
    }

    // Use the first workspace folder as the root for searching
    const workspaceRoot = workspaceFolders[0].uri.fsPath;

    // Find the initial config file (used for full graph or as fallback)
    progress.report({ message: 'Finding ReScript config file...' });
    const initialConfigFileUri = await findConfigFile(workspaceRoot);

    if (token.isCancellationRequested) return;

    // If no config file is found anywhere, exit (should be caught by activationEvents)
    if (!initialConfigFileUri) {
      vscode.window.showErrorMessage('Could not find any bsconfig.json or rescript.json in the workspace (excluding node_modules).');
      return;
    }

    let projectRoot: string;
    let bsDir: string;

    try {
      let moduleName: string | undefined;

      // --- Logic when focusing on a specific module ---
      if (focusOnModule) {
        progress.report({ message: 'Getting module information...' });
        if (token.isCancellationRequested) return;

        // Get module name (from editor or input)
        moduleName = getCurrentModuleNameFromActiveEditor();
        if (!moduleName) {
          moduleName = await vscode.window.showInputBox({
            prompt: 'Enter module name to focus on',
            placeHolder: 'ModuleName'
          });
        }
        if (!moduleName) return; // User cancelled

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
        bsDir = path.join(projectRoot, 'lib', 'bs');

      } else {
        // --- Logic for full dependency graph ---
        // Use the initially found config file's location
        projectRoot = path.dirname(initialConfigFileUri.fsPath);
        bsDir = path.join(projectRoot, 'lib', 'bs');
      }

      // Check if the determined bsDir exists (common check)
      if (!fs.existsSync(bsDir)) {
        vscode.window.showWarningMessage(`ReScript build directory not found: ${bsDir}. Please ensure the project is compiled.`);
        // Consider returning if bsDir is essential for the CLI command
      }

      // Find CLI path
      progress.report({ message: 'Finding CLI path...' });
      if (token.isCancellationRequested) return;
      const cliPath = await findRescriptDepCLI(context);

      // Run the CLI command with the determined bsDir and moduleName (if applicable)
      progress.report({ message: 'Running rescriptdep CLI...' });
      if (token.isCancellationRequested) return;

      const cliArgs = [
        '--format=json',
        ...(moduleName ? ['--module', moduleName] : []),
        bsDir // Use the bsDir calculated based on focus mode
      ];

      const jsonContent = await runRescriptDep(cliPath, cliArgs, context);

      // Display webview
      progress.report({ message: 'Generating visualization...' });
      if (token.isCancellationRequested) return;

      if (jsonContent) {
        showGraphWebview(context, jsonContent, focusOnModule, moduleName);
      } else {
        vscode.window.showErrorMessage('Failed to generate dependency visualization (CLI returned no content).');
      }

    } catch (error) {
      if (!token.isCancellationRequested) {
        vscode.window.showErrorMessage(`Error generating dependency visualization: ${error}`);
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
    const options: cp.ExecFileOptions = {}; // Add options like cwd if needed later

    cp.execFile(command, args, options, (error, stdout, stderr) => {
      if (error) {
        console.error(`rescriptdep stderr: ${stderr}`);
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

// JSON module type extension
interface ModuleNode {
  name: string;
  dependencies: { name: string, path?: string }[] | string[];
  dependents: { name: string, path?: string }[] | string[];
  fan_in: number;
  fan_out: number;
  in_cycle: boolean;
  file_path?: string | null;
  path?: string;
  location?: { start: number; end: number } | null;
}

interface DependencyData {
  modules: ModuleNode[];
  cycles: string[][];
  metrics: {
    total_modules: number;
    average_fan_in?: number;
    average_fan_out?: number;
    avg_fan_in?: number;
    avg_fan_out?: number;
    max_fan_in?: number;
    max_fan_out?: number;
    cyclic_modules?: number;
    most_depended_upon?: { module: string, count: number };
    most_dependencies?: { module: string, count: number };
    cycles_count?: number;
  };
}

// Graph webview display function
function showGraphWebview(context: vscode.ExtensionContext, jsonContent: string, isFocusedMode: boolean = false, centerModuleName?: string) {
  const data = JSON.parse(jsonContent);
  const modules = data.modules || [];

  // Only find a center module if in focused mode
  let centerModule = null;
  if (isFocusedMode) {
    // If centerModuleName is provided, find the module with that name
    if (centerModuleName) {
      // Maintain TypeScript type checking here
      const foundModule = modules.find((m: ModuleNode) => m.name === centerModuleName);
      centerModule = foundModule || null;
    }

    // Fallback to first module if not found
    if (!centerModule && modules.length > 0) {
      centerModule = modules[0];
    }

    if (!centerModule) {
      vscode.window.showErrorMessage('No module data available');
      return;
    }
  }

  // Detect if the current theme is dark
  const isDarkTheme = vscode.window.activeColorTheme && vscode.window.activeColorTheme.kind === vscode.ColorThemeKind.Dark;

  // Define color sets based on theme
  const colors = {
    // Light theme colors
    light: {
      nodeBg: '#f3f4f6',
      nodeBorder: '#9ca3af',
      linkStroke: '#999',
      textColor: '#333333',
      centerColor: '#4CAF50',
      dependentColor: '#2196F3',
      dependencyColor: '#F44336',
    },
    // Dark theme colors
    dark: {
      nodeBg: '#2d2d2d',
      nodeBorder: '#555555',
      linkStroke: '#aaaaaa',
      textColor: '#e0e0e0',
      centerColor: '#5CCC60', // Brighter green for dark mode
      dependentColor: '#42A5F5', // Brighter blue for dark mode
      dependencyColor: '#FF5252', // Brighter red for dark mode
    }
  };

  // Select appropriate color set
  const theme = isDarkTheme ? colors.dark : colors.light;

  // Create HTML content with theme-appropriate colors
  const htmlContent = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ReScript Dependency Graph</title>
    <script src="https://d3js.org/d3.v7.min.js"></script>
    <style>
        body {
            margin: 0;
            padding: 10px;
            font-family: -apple-system, BlinkMacSystemFont, sans-serif;
            background-color: var(--vscode-editor-background);
            color: var(--vscode-editor-foreground);
        }
        
        /* CSS Variables for theme colors */
        :root {
            --node-bg: ${theme.nodeBg};
            --node-border: ${theme.nodeBorder};
            --link-stroke: ${theme.linkStroke};
            --text-color: ${theme.textColor};
            --center-color: ${theme.centerColor};
            --dependent-color: ${theme.dependentColor};
            --dependency-color: ${theme.dependencyColor};
        }
        
        #graph-container {
            width: 100%;
            height: calc(100vh - 60px);
            overflow: hidden;
        }
        .legend {
            display: flex;
            justify-content: center;
            margin-bottom: 10px;
        }
        .legend-item {
            display: flex;
            align-items: center;
            margin-right: 20px;
        }
        .legend-color {
            width: 15px;
            height: 15px;
            margin-right: 5px;
            border-radius: 3px;
            background-color: var(--node-bg);
            border: 1px solid var(--node-border);
            position: relative;
        }
        .legend-color::before {
            content: '';
            position: absolute;
            top: -1px;
            left: -1px;
            width: calc(100% + 2px);
            height: 4px;
        }
        .legend-color.center-color::before {
            background-color: var(--center-color);
        }
        .legend-color.dependent-color::before {
            background-color: var(--dependent-color);
        }
        .legend-color.dependency-color::before {
            background-color: var(--dependency-color);
        }
        .node {
            cursor: pointer;
        }
        .node rect {
            border-radius: 6px;
            fill: var(--node-bg);
            stroke: var(--node-border);
            stroke-width: 1.5px;
        }
        .node text {
            font-size: 12px;
            fill: var(--text-color);
            text-anchor: middle;
            dominant-baseline: middle;
        }
        .link {
            fill: none;
            stroke: var(--link-stroke);
            stroke-opacity: 0.6;
            stroke-width: 1.5px;
        }
        .center-node rect {
            stroke-width: 1.5px;
            stroke: var(--node-border);
        }
        .dependent-node rect {
            stroke-width: 1.5px;
            stroke: var(--node-border);
        }
        .dependency-node rect {
            stroke-width: 1.5px;
            stroke: var(--node-border);
        }
    </style>
</head>
<body>
    <div class="legend">
        <div class="legend-item">
            <div class="legend-color dependent-color"></div>
            <span>Dependents (uses center module)</span>
        </div>
        <div class="legend-item">
            <div class="legend-color center-color"></div>
            <span>Center Module</span>
        </div>
        <div class="legend-item">
            <div class="legend-color dependency-color"></div>
            <span>Dependencies (used by center module)</span>
        </div>
    </div>
    <div id="graph-container">
        <svg id="graph"></svg>
    </div>
    <script>
        const vscode = acquireVsCodeApi();
        
        // Parse JSON data
        const data = ${JSON.stringify(data)};
        const isFocusedMode = ${isFocusedMode};
        const centerModule = ${JSON.stringify(centerModule)};
        const isDarkTheme = ${isDarkTheme};
        
        // Theme colors
        const theme = ${JSON.stringify(theme)};
        
        // Color definitions for both themes
        const colors = ${JSON.stringify(colors)};
        
        // Create and render the dependency graph
        function createGraph() {
            const container = document.getElementById('graph-container');
            const width = container.clientWidth;
            const height = container.clientHeight;
            
            // Different graph rendering based on mode
            if (isFocusedMode && centerModule) {
                renderFocusedGraph(width, height);
            } else {
                renderFullGraph(width, height);
            }
        }
        
        function renderFocusedGraph(width, height) {
            // Create nodes array
            const nodes = [
                // Center node
                {
                    id: centerModule.name,
                    type: 'center',
                    label: centerModule.name,
                    width: Math.max(centerModule.name.length * 10, 100),
                    height: 40
                }
            ];
            
            // Links array
            const links = [];
            
            // Add dependent modules (those that use the center module)
            centerModule.dependents.forEach((dep, i) => {
                const name = typeof dep === 'object' ? dep.name : dep;
                nodes.push({
                    id: name,
                    type: 'dependent',
                    label: name,
                    width: Math.max(name.length * 10, 100),
                    height: 40
                });
                
                links.push({
                    source: name,
                    target: centerModule.name,
                    type: 'dependent'
                });
            });
            
            // Add dependency modules (those that the center module uses)
            centerModule.dependencies.forEach((dep, i) => {
                const name = typeof dep === 'object' ? dep.name : dep;
                nodes.push({
                    id: name,
                    type: 'dependency',
                    label: name,
                    width: Math.max(name.length * 10, 100),
                    height: 40
                });
                
                links.push({
                    source: centerModule.name,
                    target: name,
                    type: 'dependency'
                });
            });
            
            // Create SVG element
            const svg = d3.select('#graph')
                .attr('width', width)
                .attr('height', height);
            
            // Clear any existing content
            svg.selectAll('*').remove();
            
            // Add group for graph content with zoom behavior
            const g = svg.append('g');
            
            // Add arrow markers for direction
            svg.append('defs').selectAll('marker')
                .data(['dependent', 'dependency'])
                .enter().append('marker')
                .attr('id', d => 'arrow-' + d)
                .attr('viewBox', '0 -5 10 10')
                .attr('refX', 20)
                .attr('refY', 0)
                .attr('markerWidth', 6)
                .attr('markerHeight', 6)
                .attr('orient', 'auto')
                .append('path')
                .attr('d', 'M0,-5L10,0L0,5')
                .attr('fill', 'var(--link-stroke)');
            
            // Create the force simulation
            const simulation = d3.forceSimulation(nodes)
                .force('link', d3.forceLink(links).id(d => d.id).distance(200))
                .force('charge', d3.forceManyBody().strength(-500))
                .force('center', d3.forceCenter(width / 2, height / 2))
                .force('x', d3.forceX().x(d => {
                    if (d.type === 'center') return width / 2;
                    if (d.type === 'dependent') return width / 4;
                    if (d.type === 'dependency') return width * 3/4;
                    return width / 2;
                }).strength(0.3))
                .force('y', d3.forceY().y(height / 2).strength(0.1))
                .force('collision', d3.forceCollide().radius(d => Math.max(d.width, d.height) / 2 + 20))
                .on('tick', ticked);
            
            // Create links
            const link = g.append('g')
                .selectAll('line')
                .data(links)
                .enter().append('path')
                .attr('class', 'link')
                .attr('marker-end', d => 'url(#arrow-' + d.type + ')');
            
            // Create node groups
            const node = g.append('g')
                .selectAll('.node')
                .data(nodes)
                .enter().append('g')
                .attr('class', d => 'node ' + d.type + '-node')
                .call(d3.drag()
                    .on('start', dragStarted)
                    .on('drag', dragged)
                    .on('end', dragEnded));
            
            // Add rectangles to nodes
            node.append('rect')
                .attr('width', d => d.width)
                .attr('height', d => d.height)
                .attr('x', d => -d.width / 2)
                .attr('y', d => -d.height / 2)
                .attr('rx', 6)
                .attr('ry', 6);
            
            // Add top border to nodes based on type
            node.append('rect')
                .attr('width', d => d.width)
                .attr('height', 4)
                .attr('x', d => -d.width / 2)
                .attr('y', d => -d.height / 2)
                .attr('class', 'top-border')
                .style('fill', d => {
                    if (d.type === 'center') return 'var(--center-color)';
                    if (d.type === 'dependent') return 'var(--dependent-color)';
                    if (d.type === 'dependency') return 'var(--dependency-color)';
                    return 'var(--center-color)';
                })
                .style('stroke', d => {
                    if (d.type === 'center') return 'var(--center-color)';
                    if (d.type === 'dependent') return 'var(--dependent-color)';
                    if (d.type === 'dependency') return 'var(--dependency-color)';
                    return 'var(--center-color)';
                });
            
            // Add text labels
            node.append('text')
                .text(d => d.label);
            
            // Add click behavior
            node.on('click', function(event, d) {
                if (d.type !== 'center') {
                    vscode.postMessage({
                        command: 'focusModule',
                        moduleName: d.id
                    });
                }
            });
            
            // Add zoom behavior
            const zoom = d3.zoom()
                .scaleExtent([0.2, 3])
                .on('zoom', event => {
                    g.attr('transform', event.transform);
                });
            
            svg.call(zoom);
            
            // Initial zoom to fit content
            const initialTransform = d3.zoomIdentity
                .translate(width / 2, height / 2)
                .scale(0.8)
                .translate(-width / 2, -height / 2);
            
            svg.call(zoom.transform, initialTransform);
            
            // Update positions on each tick
            function ticked() {
                // Update link paths
                link.attr('d', d => {
                    // Direct connection line with slight curve
                    const sourceNode = nodes.find(n => n.id === d.source.id || n.id === d.source);
                    const targetNode = nodes.find(n => n.id === d.target.id || n.id === d.target);
                    
                    if (!sourceNode || !targetNode) return '';
                    
                    const sourceX = d.source.x || sourceNode.x || 0;
                    const sourceY = d.source.y || sourceNode.y || 0;
                    const targetX = d.target.x || targetNode.x || 0;
                    const targetY = d.target.y || targetNode.y || 0;
                    
                    const dx = targetX - sourceX;
                    const dy = targetY - sourceY;
                    const dr = Math.sqrt(dx * dx + dy * dy) * 1.5;
                    
                    return "M" + sourceX + "," + sourceY + "A" + dr + "," + dr + " 0 0,1 " + targetX + "," + targetY;
                });
                
                // Update node positions
                node.attr('transform', d => "translate(" + (d.x || 0) + "," + (d.y || 0) + ")");
            }
            
            // Drag functions
            function dragStarted(event, d) {
                if (!event.active) simulation.alphaTarget(0.3).restart();
                d.fx = d.x;
                d.fy = d.y;
            }
            
            function dragged(event, d) {
                d.fx = event.x;
                d.fy = event.y;
            }
            
            function dragEnded(event, d) {
                if (!event.active) simulation.alphaTarget(0);
                d.fx = null;
                d.fy = null;
            }
        }
        
        function renderFullGraph(width, height) {
            // Basic visualization for all modules
            // Create nodes for all modules
            const nodes = data.modules.map(module => ({
                id: module.name,
                label: module.name,
                width: Math.max(module.name.length * 10, 100),
                height: 40
            }));
            // Create a set of node IDs for quick lookup
            const nodeIds = new Set(nodes.map(n => n.id)); // Add set for efficient lookup

            // Create links for all dependencies, filtering out external ones
            const links = [];
            data.modules.forEach(module => {
                // Ensure dependencies is an array before iterating
                const dependencies = Array.isArray(module.dependencies) ? module.dependencies : [];
                dependencies.forEach(dep => {
                    const targetName = typeof dep === 'object' ? dep.name : dep;
                    // Check if both source and target nodes exist in our node list
                    if (nodeIds.has(module.name) && nodeIds.has(targetName)) { // Filter links
                        links.push({
                            source: module.name,
                            target: targetName
                        });
                    }
                });
            });

            // Create SVG element
            const svg = d3.select('#graph')
                .attr('width', width)
                .attr('height', height);
            
            // Clear any existing content
            svg.selectAll('*').remove();
            
            // Add group for graph content with zoom behavior
            const g = svg.append('g');
            
            // Add arrow markers for direction
            svg.append('defs').append('marker')
                .attr('id', 'arrow')
                .attr('viewBox', '0 -5 10 10')
                .attr('refX', 20)
                .attr('refY', 0)
                .attr('markerWidth', 6)
                .attr('markerHeight', 6)
                .attr('orient', 'auto')
                .append('path')
                .attr('d', 'M0,-5L10,0L0,5')
                .attr('fill', 'var(--link-stroke)');
            
            // Create the force simulation
            const simulation = d3.forceSimulation(nodes)
                .force('link', d3.forceLink(links).id(d => d.id).distance(150))
                .force('charge', d3.forceManyBody().strength(-300))
                .force('center', d3.forceCenter(width / 2, height / 2))
                .force('collision', d3.forceCollide().radius(d => Math.max(d.width, d.height) / 2 + 10))
                .on('tick', ticked);
            
            // Create links
            const link = g.append('g')
                .selectAll('line')
                .data(links)
                .enter().append('path')
                .attr('class', 'link')
                .attr('marker-end', 'url(#arrow)');
            
            // Create node groups
            const node = g.append('g')
                .selectAll('.node')
                .data(nodes)
                .enter().append('g')
                .attr('class', 'node')
                .call(d3.drag()
                    .on('start', dragStarted)
                    .on('drag', dragged)
                    .on('end', dragEnded));
            
            // Add rectangles to nodes
            node.append('rect')
                .attr('width', d => d.width)
                .attr('height', d => d.height)
                .attr('x', d => -d.width / 2)
                .attr('y', d => -d.height / 2)
                .attr('rx', 6)
                .attr('ry', 6)
                .attr('class', 'node-rect');
            
            // Add top border to nodes
            node.append('rect')
                .attr('width', d => d.width)
                .attr('height', 4)
                .attr('x', d => -d.width / 2)
                .attr('y', d => -d.height / 2)
                .attr('class', 'top-border')
                .style('fill', 'var(--center-color)')
                .style('stroke', 'var(--center-color)');
            
            // Add text labels
            node.append('text')
                .text(d => d.label);
            
            // Add click behavior
            node.on('click', function(event, d) {
                vscode.postMessage({
                    command: 'focusModule',
                    moduleName: d.id
                });
            });
            
            // Add zoom behavior
            const zoom = d3.zoom()
                .scaleExtent([0.1, 3])
                .on('zoom', event => {
                    g.attr('transform', event.transform);
                });
            
            svg.call(zoom);
            
            // Update positions on each tick
            function ticked() {
                link.attr('d', d => {
                    const dx = d.target.x - d.source.x;
                    const dy = d.target.y - d.source.y;
                    const dr = Math.sqrt(dx * dx + dy * dy) * 1.5;
                    return "M" + d.source.x + "," + d.source.y + "A" + dr + "," + dr + " 0 0,1 " + d.target.x + "," + d.target.y;
                });
                
                node.attr('transform', d => "translate(" + d.x + "," + d.y + ")");
            }
            
            // Drag functions
            function dragStarted(event, d) {
                if (!event.active) simulation.alphaTarget(0.3).restart();
                d.fx = d.x;
                d.fy = d.y;
            }
            
            function dragged(event, d) {
                d.fx = event.x;
                d.fy = event.y;
            }
            
            function dragEnded(event, d) {
                if (!event.active) simulation.alphaTarget(0);
                d.fx = null;
                d.fy = null;
            }
        }
        
        // Initialize graph
        createGraph();
        
        // Handle resize
        window.addEventListener('resize', () => {
            createGraph();
        });
        
        // Update graph colors for theme change without redrawing
        function updateGraphColors(isDark) {
            // Get the appropriate color theme
            const newTheme = isDark ? colors.dark : colors.light;
            
            // Update CSS variables
            document.documentElement.style.setProperty('--node-bg', newTheme.nodeBg);
            document.documentElement.style.setProperty('--node-border', newTheme.nodeBorder);
            document.documentElement.style.setProperty('--link-stroke', newTheme.linkStroke);
            document.documentElement.style.setProperty('--text-color', newTheme.textColor);
            document.documentElement.style.setProperty('--center-color', newTheme.centerColor);
            document.documentElement.style.setProperty('--dependent-color', newTheme.dependentColor);
            document.documentElement.style.setProperty('--dependency-color', newTheme.dependencyColor);
            
            // Force update for any SVG elements that might not be using the CSS variables
            // Update node backgrounds
            d3.selectAll('.node rect:not(.top-border)').style('fill', newTheme.nodeBg).style('stroke', newTheme.nodeBorder);
            
            // Update node borders
            d3.selectAll('.center-node .top-border').style('fill', newTheme.centerColor).style('stroke', newTheme.centerColor);
            d3.selectAll('.dependent-node .top-border').style('fill', newTheme.dependentColor).style('stroke', newTheme.dependentColor);
            d3.selectAll('.dependency-node .top-border').style('fill', newTheme.dependencyColor).style('stroke', newTheme.dependencyColor);
            
            // Update text color
            d3.selectAll('.node text').style('fill', newTheme.textColor);
            
            // Update links
            d3.selectAll('.link').style('stroke', newTheme.linkStroke);
            
            // Update marker colors (arrow heads)
            d3.selectAll('marker path').style('fill', newTheme.linkStroke);
        }
        
        // Handle messages from VS Code
        window.addEventListener('message', event => {
            const message = event.data;
            if (message.command === 'updateGraph') {
                try {
                    const updatedData = JSON.parse(message.jsonContent);
                    data.modules = updatedData.modules;
                    data.cycles = updatedData.cycles;
                    data.metrics = updatedData.metrics;
                    
                    // Update center module if in focused mode
                    if (isFocusedMode && message.focusedModule) {
                      const foundModule = updatedData.modules.find(m => m.name === message.focusedModule);
                      if (foundModule) {
                        Object.assign(centerModule, foundModule);
                      }
                    }
                    
                    createGraph();
                } catch (error) {
                    console.error('Error updating graph:', error);
                }
            } else if (message.command === 'updateTheme') {
                // Update colors without redrawing the graph
                updateGraphColors(message.isDarkTheme);
            }
        });
        
        // Notify VS Code that webview is ready
        vscode.postMessage({ command: 'webviewReady' });
    </script>
</body>
</html>`;

  // Check if we already have a panel
  if (currentPanel) {
    // If we already have a panel, just update its html
    currentPanel.webview.html = htmlContent;
    currentPanel.title = isFocusedMode && centerModule ? `Module: ${centerModule.name} Dependencies` : 'ReScript Dependencies';
    currentPanel.reveal(vscode.ViewColumn.One);
  } else {
    // Create a new panel
    currentPanel = vscode.window.createWebviewPanel(
      'bibimbobVisualizer',
      isFocusedMode && centerModule ? `Module: ${centerModule.name} Dependencies` : 'ReScript Dependencies',
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
  }

  // Listen for theme changes
  context.subscriptions.push(
    vscode.window.onDidChangeActiveColorTheme(theme => {
      const newIsDarkTheme = theme.kind === vscode.ColorThemeKind.Dark;
      if (newIsDarkTheme !== isDarkTheme && currentPanel) {
        console.log(`Theme changed to ${newIsDarkTheme ? 'dark' : 'light'}`);

        // Instead of recreating the webview, just send a message to update colors
        currentPanel.webview.postMessage({
          command: 'updateTheme',
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

                // Setup cache args (requires context)
                let cacheArgs: string[] = [];
                if (context) {
                  try {
                    const cacheDir = vscode.Uri.joinPath(context.globalStorageUri, 'cache');
                    const cacheDirPath = cacheDir.fsPath;
                    const storageDir = context.globalStorageUri.fsPath;
                    if (!fs.existsSync(storageDir)) fs.mkdirSync(storageDir, { recursive: true });
                    if (!fs.existsSync(cacheDirPath)) fs.mkdirSync(cacheDirPath, { recursive: true });
                    const workspaceName = vscode.workspace.workspaceFolders?.[0]?.name || 'default';
                    const cacheFilePath = path.join(cacheDirPath, `${workspaceName}.rescriptdep_cache.marshal`);
                    cacheArgs = ['--cache-file', cacheFilePath];
                  } catch (error) { console.error('Error setting up cache dir for focus:', error); }
                }

                // Define the core arguments for the CLI
                const coreArgs = [
                  '--format=json',
                  '--module',
                  moduleName,
                  bsDir
                ];
                const fullArgs = [...cacheArgs, ...coreArgs];

                // Get JSON format data
                const jsonContent = await runRescriptDep(cliPath, fullArgs, context); // Pass context

                if (token.isCancellationRequested) return;

                if (jsonContent && currentPanel) { // Check panel existence again
                  // Parse data to update title correctly
                  let panelTitle = 'ReScript Dependencies';
                  try {
                    const parsedData = JSON.parse(jsonContent);
                    const foundModule = parsedData.modules?.find((m: ModuleNode) => m.name === moduleName);
                    if (foundModule) {
                      panelTitle = `Module: ${moduleName} Dependencies`;
                    } else {
                      console.warn(`Focused module ${moduleName} not found in CLI response for title update.`);
                    }
                  } catch (e) {
                    console.error("Error parsing JSON for title update:", e);
                  }
                  currentPanel.title = panelTitle;

                  const currentIsDarkTheme = vscode.window.activeColorTheme?.kind === vscode.ColorThemeKind.Dark;
                  currentPanel.webview.postMessage({
                    command: 'updateGraph',
                    jsonContent: jsonContent,
                    focusedModule: moduleName,
                    isDarkTheme: currentIsDarkTheme
                  });
                } else if (!jsonContent) {
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

// Helper function to get webview URIs for local files
function getWebviewUri(webview: vscode.Webview, extensionUri: vscode.Uri, ...pathSegments: string[]): vscode.Uri {
  return webview.asWebviewUri(vscode.Uri.joinPath(extensionUri, ...pathSegments));
}

// Helper function to try to find the module in the project when file path is unavailable
async function findModuleInProject(moduleName: string): Promise<{ path: string, line: number } | null> {
  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (!workspaceFolders) return null;

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
        if (bestMatch) break;
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
