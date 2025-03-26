import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

export function activate(context: vscode.ExtensionContext) {
  // Command for full dependency graph
  let fullGraphCommand = vscode.commands.registerCommand('rescriptdep-visualizer.showDependencyGraph', async () => {
    await generateDependencyGraph(context);
  });

  // Command for module-focused dependency graph
  let focusModuleCommand = vscode.commands.registerCommand('rescriptdep-visualizer.focusModuleDependencies', async () => {
    await generateDependencyGraph(context, true);
  });

  context.subscriptions.push(fullGraphCommand);
  context.subscriptions.push(focusModuleCommand);
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
      ]);

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

    // Method 1: Use dune exec - execute command with dune exec
    return `dune exec -- rescriptdep`;

    // Or
    // Method 2: Use _build path - directly reference compiled binary
    // return path.join(ocamlProjectRoot, '_build', 'default', 'bin', 'main.exe');
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
async function runRescriptDep(cliPath: string, args: string[]): Promise<string> {
  return new Promise((resolve, reject) => {
    if (cliPath.startsWith('dune exec')) {
      const fullCommand = `${cliPath} ${args.join(' ')}`;

      const rootDir = path.resolve(__dirname, '../..');

      cp.exec(fullCommand, { cwd: rootDir }, (error, stdout) => {
        if (error) {
          reject(error);
          return;
        }
        resolve(stdout.toString());
      });
      return;
    }

    if (cliPath === 'rescriptdep') {
      cp.exec(`rescriptdep ${args.join(' ')}`, (error, stdout) => {
        if (error) {
          reject(error);
          return;
        }
        resolve(stdout.toString());
      });
    } else {
      cp.execFile(cliPath, args, (error, stdout) => {
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

  // Create HTML content without problematic CSS properties
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
        }
        .node {
            cursor: pointer;
        }
        .node rect {
            border-radius: 6px;
        }
        .node text {
            font-size: 12px;
            fill: var(--vscode-editor-foreground);
            text-anchor: middle;
            dominant-baseline: middle;
        }
        .link {
            fill: none;
            stroke: #999;
            stroke-opacity: 0.6;
            stroke-width: 1.5px;
        }
        .center-node rect {
            fill: #4CAF50;
            stroke: #2E7D32;
        }
        .dependent-node rect {
            fill: #2196F3;
            stroke: #1565C0;
        }
        .dependency-node rect {
            fill: #F44336;
            stroke: #C62828;
        }
    </style>
</head>
<body>
    <div class="legend">
        <div class="legend-item">
            <div class="legend-color" style="background-color: #4CAF50;"></div>
            <span>Center Module</span>
        </div>
        <div class="legend-item">
            <div class="legend-color" style="background-color: #2196F3;"></div>
            <span>Dependents (uses center module)</span>
        </div>
        <div class="legend-item">
            <div class="legend-color" style="background-color: #F44336;"></div>
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
                .attr('fill', '#999');
            
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
                .attr('y', d => -d.height / 2);
            
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
                .attr('fill', '#999');
            
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
                .style('fill', '#9CCC65')
                .style('stroke', '#7CB342');
            
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

  // Create and display webview panel
  const panel = vscode.window.createWebviewPanel(
    'rescriptDepVisualizer',
    isFocusedMode && centerModule ? `Module: ${centerModule.name} Dependencies` : 'ReScript Dependencies',
    vscode.ViewColumn.One,
    {
      enableScripts: true,
      localResourceRoots: [
        vscode.Uri.file(path.join(context.extensionPath, 'media'))
      ]
    }
  );

  panel.webview.html = htmlContent;

  // Handle messages from webview
  panel.webview.onDidReceiveMessage(
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
              ]);

              if (jsonContent) {
                // Send the new json content back to the webview for update
                console.log(`Sending updated data for module: ${moduleName}`);
                panel.webview.postMessage({
                  command: 'updateGraph',
                  jsonContent: jsonContent,
                  focusedModule: moduleName
                });
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
