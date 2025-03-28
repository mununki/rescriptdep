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
  console.log('Bibimbob is activated');
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

// Integrated common logic into a single function
async function generateDependencyGraph(context: vscode.ExtensionContext, focusOnModule: boolean = false) {
  // Use withProgress API to show a progress notification in the bottom right
  return vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    title: focusOnModule ? 'ReScript: Analyzing module dependencies...' : 'ReScript: Analyzing dependency graph...',
    cancellable: true
  }, async (progress, token) => {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders) {
      vscode.window.showErrorMessage('No workspace folder open');
      return;
    }

    const workspaceRoot = workspaceFolders[0].uri.fsPath;

    // Check if it's a ReScript project
    const hasBsconfig = fs.existsSync(path.join(workspaceRoot, 'bsconfig.json'));
    const hasRescriptConfig = fs.existsSync(path.join(workspaceRoot, 'rescript.json'));

    if (!hasBsconfig && !hasRescriptConfig) {
      vscode.window.showErrorMessage('Not a ReScript project (neither bsconfig.json nor rescript.json found)');
      return;
    }

    try {
      progress.report({ message: 'Finding CLI path...' });
      if (token.isCancellationRequested) return;

      // Find CLI path
      const cliPath = await findRescriptDepCLI(context);

      // bs directory path
      const bsDir = path.join(workspaceRoot, 'lib', 'bs');

      let moduleName: string | undefined;

      // Request module name if in focused mode
      if (focusOnModule) {
        progress.report({ message: 'Getting module information...' });
        if (token.isCancellationRequested) return;

        // First try to get module name from active editor
        moduleName = getCurrentModuleNameFromActiveEditor();

        // If no active ReScript file is open or couldn't determine module name,
        // fall back to asking the user
        if (!moduleName) {
          moduleName = await vscode.window.showInputBox({
            prompt: 'Enter module name to focus on',
            placeHolder: 'ModuleName'
          });
        }

        if (!moduleName) return; // User cancelled or no module name available
      }

      if (token.isCancellationRequested) return;

      // Get data in JSON format
      const jsonContent = await runRescriptDep(cliPath, [
        '--format=json',
        ...(moduleName ? ['--module', moduleName] : []),
        bsDir
      ], context);

      // Display webview with the json data
      progress.report({ message: 'Generating visualization...' });
      if (token.isCancellationRequested) return;

      if (jsonContent) {
        showGraphWebview(context, jsonContent, focusOnModule, moduleName);
      } else {
        vscode.window.showErrorMessage('Failed to generate dependency visualization');
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
  // Setup cache directory
  let cacheArgs: string[] = [];

  if (context) {
    try {
      // Create cache directory if it doesn't exist using globalStorageUri
      const cacheDir = vscode.Uri.joinPath(context.globalStorageUri, 'cache');
      const cacheDirPath = cacheDir.fsPath;
      const storageDir = context.globalStorageUri.fsPath;

      if (!fs.existsSync(storageDir)) {
        fs.mkdirSync(storageDir, { recursive: true });
      }

      if (!fs.existsSync(cacheDirPath)) {
        fs.mkdirSync(cacheDirPath, { recursive: true });
      }

      // Get workspace name to create a unique cache file per project
      const workspaceName = vscode.workspace.workspaceFolders?.[0]?.name || 'default';
      const cacheFilePath = path.join(cacheDirPath, `${workspaceName}.rescriptdep_cache.marshal`);

      // Add cache file argument
      cacheArgs = ['--cache-file', cacheFilePath];
    } catch (error) {
      console.error('Error setting up cache directory:', error);
      // Continue without cache if there's an error
    }
  }

  // Merge the cache arguments with the provided arguments
  const fullArgs = [...cacheArgs, ...args];

  return new Promise((resolve, reject) => {
    if (cliPath.includes('_build/default/bin') && cliPath.endsWith('main.exe')) {
      // Direct execution of built binary
      cp.execFile(cliPath, fullArgs, (error, stdout) => {
        if (error) {
          reject(error);
          return;
        }
        resolve(stdout.toString());
      });
      return;
    }

    if (cliPath === 'rescriptdep') {
      cp.exec(`rescriptdep ${fullArgs.join(' ')}`, (error, stdout) => {
        if (error) {
          reject(error);
          return;
        }
        resolve(stdout.toString());
      });
    } else {
      cp.execFile(cliPath, fullArgs, (error, stdout) => {
        if (error) {
          reject(error);
          return;
        }
        resolve(stdout.toString());
      });
    }
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
            background-color: ${theme.nodeBg};
            border: 1px solid ${theme.nodeBorder};
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
            background-color: ${theme.centerColor};
        }
        .legend-color.dependent-color::before {
            background-color: ${theme.dependentColor};
        }
        .legend-color.dependency-color::before {
            background-color: ${theme.dependencyColor};
        }
        .node {
            cursor: pointer;
        }
        .node rect {
            border-radius: 6px;
            fill: ${theme.nodeBg};
            stroke: ${theme.nodeBorder};
            stroke-width: 1.5px;
        }
        .node text {
            font-size: 12px;
            fill: ${theme.textColor};
            text-anchor: middle;
            dominant-baseline: middle;
        }
        .link {
            fill: none;
            stroke: ${theme.linkStroke};
            stroke-opacity: 0.6;
            stroke-width: 1.5px;
        }
        .center-node rect {
            stroke-width: 1.5px;
            stroke: ${theme.nodeBorder};
        }
        .dependent-node rect {
            stroke-width: 1.5px;
            stroke: ${theme.nodeBorder};
        }
        .dependency-node rect {
            stroke-width: 1.5px;
            stroke: ${theme.nodeBorder};
        }
    </style>
</head>
<body>
    <div class="legend">
        <div class="legend-item">
            <div class="legend-color center-color"></div>
            <span>Center Module</span>
        </div>
        <div class="legend-item">
            <div class="legend-color dependent-color"></div>
            <span>Dependents (uses center module)</span>
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
                .attr('fill', theme.linkStroke);
            
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
                    if (d.type === 'center') return theme.centerColor;
                    if (d.type === 'dependent') return theme.dependentColor;
                    if (d.type === 'dependency') return theme.dependencyColor;
                    return theme.centerColor;
                })
                .style('stroke', d => {
                    if (d.type === 'center') return theme.centerColor;
                    if (d.type === 'dependent') return theme.dependentColor;
                    if (d.type === 'dependency') return theme.dependencyColor;
                    return theme.centerColor;
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
            
            // Create links for all dependencies
            const links = [];
            data.modules.forEach(module => {
                module.dependencies.forEach(dep => {
                    const targetName = typeof dep === 'object' ? dep.name : dep;
                    links.push({
                        source: module.name,
                        target: targetName
                    });
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
                .attr('fill', theme.linkStroke);
            
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
                .style('fill', theme.nodeBg)
                .style('stroke', theme.nodeBorder)
                .style('stroke-width', '1.5px');
            
            // Add top border to nodes
            node.append('rect')
                .attr('width', d => d.width)
                .attr('height', 4)
                .attr('x', d => -d.width / 2)
                .attr('y', d => -d.height / 2)
                .style('fill', theme.centerColor)
                .style('stroke', theme.centerColor);
            
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
        
        // Handle messages from VS Code
        window.addEventListener('message', event => {
            const message = event.data;
            if (message.command === 'updateGraph') {
                try {
                    const updatedData = JSON.parse(message.jsonContent);
                    data.modules = updatedData.modules;
                    data.cycles = updatedData.cycles;
                    data.metrics = updatedData.metrics;
                    
                    // Update theme if provided
                    if (message.isDarkTheme !== undefined) {
                        // No need to reload, we'll handle theme updates externally
                        // by recreating the webview with the right theme colors
                    }
                    
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
        // Recreate the webview with the new theme
        showGraphWebview(context, jsonContent, isFocusedMode, centerModuleName);
      }
    })
  );

  // Handle messages from webview
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

            console.log(`Received focusModule request for: ${moduleName}`);

            // Run a new dependency analysis focused on this module
            await vscode.window.withProgress({
              location: vscode.ProgressLocation.Notification,
              title: `ReScript: Analyzing dependencies for ${moduleName}...`,
              cancellable: true
            }, async (progress, token) => {
              const workspaceFolders = vscode.workspace.workspaceFolders;
              if (!workspaceFolders) {
                vscode.window.showErrorMessage('No workspace folder open');
                return;
              }

              const workspaceRoot = workspaceFolders[0].uri.fsPath;
              const bsDir = path.join(workspaceRoot, 'lib', 'bs');

              // Find CLI path
              const cliPath = await findRescriptDepCLI(context);

              // Get JSON format data
              const jsonContent = await runRescriptDep(cliPath, [
                '--format=json',
                '--module',
                moduleName,
                bsDir
              ], context);

              if (jsonContent) {
                // Get current theme status when updating
                const currentIsDarkTheme = vscode.window.activeColorTheme &&
                  vscode.window.activeColorTheme.kind === vscode.ColorThemeKind.Dark;

                // Send the new json content back to the webview for update
                console.log(`Sending updated data for module: ${moduleName}`);
                if (currentPanel) {
                  currentPanel.webview.postMessage({
                    command: 'updateGraph',
                    jsonContent: jsonContent,
                    focusedModule: moduleName,
                    isDarkTheme: currentIsDarkTheme
                  });
                }
              } else {
                vscode.window.showErrorMessage(`Failed to generate dependency data for ${moduleName}`);
              }
            });
          } catch (error) {
            vscode.window.showErrorMessage(`Error analyzing module dependencies: ${error}`);
          }
          break;

        case 'webviewReady':
          console.log("Webview is ready to receive messages");
          break;
      }
    },
    undefined,
    context.subscriptions
  );
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
  const extensions = ['.res', '.re', '.ml'];
  const rootPath = workspaceFolders[0].uri.fsPath;

  // File search pattern
  const filePattern = `**/{${moduleName},${moduleName.toLowerCase()}}${extensions.join(',')}`;

  try {
    const files = await vscode.workspace.findFiles(filePattern, '**/node_modules/**');
    if (files.length > 0) {
      return {
        path: files[0].fsPath,
        line: 1 // Default to first line
      };
    }
  } catch (error) {
    console.error("Error finding module:", error);
  }

  return null;
}
