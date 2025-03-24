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
      // Check for cancellation
      if (token.isCancellationRequested) {
        return;
      }

      // Find CLI path
      const cliPath = await findRescriptDepCLI(context);

      // Temporary file path
      const tempDir = path.join(workspaceRoot, '.vscode');
      if (!fs.existsSync(tempDir)) {
        fs.mkdirSync(tempDir);
      }

      // bs directory path
      const bsDir = path.join(workspaceRoot, 'lib', 'bs');

      let moduleArgs: string[] = [];

      // Request module name if in focused mode
      if (focusOnModule) {
        progress.report({ message: 'Getting module information...' });
        // Check for cancellation
        if (token.isCancellationRequested) {
          return;
        }

        // First try to get module name from active editor
        let moduleName = getCurrentModuleNameFromActiveEditor();

        // If no active ReScript file is open or couldn't determine module name,
        // fall back to asking the user
        if (!moduleName) {
          moduleName = await vscode.window.showInputBox({
            prompt: 'Enter module name to focus on',
            placeHolder: 'ModuleName'
          });
        }

        if (!moduleName) return; // User cancelled or no module name available

        moduleArgs = ['--module', moduleName];
      }

      // Check for cancellation
      if (token.isCancellationRequested) {
        return;
      }

      const dotContent = await runRescriptDep(cliPath, [
        '--format=dot',
        ...moduleArgs,
        bsDir
      ]);

      // Display webview with the dot content
      progress.report({ message: 'Generating graph...' });
      // Check for cancellation
      if (token.isCancellationRequested) {
        return;
      }

      if (dotContent) {
        showGraphWebview(context, dotContent, focusOnModule);
      } else {
        vscode.window.showErrorMessage('Failed to generate dependency graph');
      }
    } catch (error) {
      if (!token.isCancellationRequested) {
        vscode.window.showErrorMessage(`Error generating dependency graph: ${error}`);
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
  dependencies: string[];
  fan_in: number;
  fan_out: number;
  in_cycle: boolean;
  file_path: string | null;
  location: { start: number; end: number } | null;
}

interface DependencyData {
  modules: ModuleNode[];
  cycles: string[][];
  metrics: {
    total_modules: number;
    avg_fan_in: number;
    avg_fan_out: number;
    max_fan_in: number;
    max_fan_out: number;
    cyclic_modules: number;
  };
}

// Graph webview display function
function showGraphWebview(context: vscode.ExtensionContext, dotContent: string, isFocusedMode: boolean = false) {
  // Existing code...

  // Generate JSON graph data
  const jsonData = parseDotContentAsJson(dotContent);

  // Form HTML content
  const htmlContent = `
    <!DOCTYPE html>
    <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>ReScript Dependency Visualizer</title>
      <script src="https://d3js.org/d3.v7.min.js"></script>
      <script src="https://unpkg.com/@hpcc-js/wasm@1.14.1/dist/index.min.js"></script>
      <script src="https://unpkg.com/d3-graphviz@4.1.0/build/d3-graphviz.min.js"></script>
    <style>
        body, html {
          margin: 0;
          padding: 0;
          width: 100%;
          height: 100%;
          overflow: hidden;
        }
        #container {
          display: flex;
          width: 100%;
          height: 100vh;
          overflow: hidden;
        }
        #graph {
          flex: 3;
          height: 100%;
          overflow: hidden;
          position: relative;
        }
        #graph svg {
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          cursor: move; /* Indicates draggable */
        }
        #details {
          flex: 1;
          padding: 15px;
          overflow-y: auto;
          border-left: 1px solid #ccc;
          height: 100%;
          box-sizing: border-box;
        }
        
        /* Additional click-related styles */
        .module-node {
          cursor: pointer;
        }
        .module-node:hover {
          opacity: 0.8;
        }
        /* Selected node style */
        .module-node.selected .label {
          fill: #ff6600 !important;
          font-weight: bold;
        }
        .module-node.selected ellipse {
          stroke: #ff6600 !important;
          stroke-width: 2px !important;
        }
        .dependency-item {
          cursor: pointer;
          padding: 4px 8px;
          margin: 2px 0;
          border-radius: 4px;
        }
        .dependency-item:hover {
          background-color: #e9e9e9;
        }
        .file-path {
          font-size: 0.8em;
          color: #666;
          margin-left: 10px;
        }
        .status-message {
          color: #666;
          font-size: 0.9em;
          margin-top: 8px;
          font-style: italic;
        }
        /* Graph zoom controls */
        .controls {
          position: absolute;
          bottom: 20px;
          left: 20px;
          z-index: 10;
          background: rgba(255, 255, 255, 0.8);
          padding: 5px;
          border-radius: 4px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        }
        .controls button {
          width: 30px;
          height: 30px;
          margin: 0 5px;
          font-size: 18px;
          cursor: pointer;
          border: 1px solid #ccc;
          background: white;
          border-radius: 4px;
        }
        .controls button:hover {
          background: #f0f0f0;
        }
    </style>
  </head>
  <body>
      <div id="container">
        <div id="graph">
          <div class="controls">
            <button id="zoom-in">+</button>
            <button id="zoom-out">-</button>
            <button id="zoom-reset">⟳</button>
          </div>
        </div>
        <div id="details">
          <h2>Module Details</h2>
          <div id="module-name">Select a module to see details</div>
          <div id="status-message" class="status-message"></div>
          <h3>Dependencies</h3>
          <ul id="dependencies"></ul>
          <h3>Dependents</h3>
          <ul id="dependents"></ul>
        </div>
      </div>
    <script>
        // Main script for dependency visualization
        (function() {
          try {
            console.log("Initializing webview script");
            const vscode = acquireVsCodeApi();
            
            // Status message helper
            function showStatusMessage(message) {
              console.log("Status message:", message);
              const statusEl = document.getElementById('status-message');
              statusEl.textContent = message;
              
              // Clear after 5 seconds
              setTimeout(() => {
                if (statusEl.textContent === message) {
                  statusEl.textContent = '';
                }
              }, 5000);
            }
            
            // Module data
            let moduleData = ${JSON.stringify(jsonData)};
            console.log("Module data loaded:", moduleData);
            
            // Currently selected module
            let selectedModule = null;
            
            // Zoom related variables
            let currentZoom = 1.0;
            
            // Zoom control event handlers
            document.getElementById('zoom-in').addEventListener('click', function(e) {
              e.preventDefault();
              e.stopPropagation();
              currentZoom *= 1.2;
              applyZoom();
            });
            
            document.getElementById('zoom-out').addEventListener('click', function(e) {
              e.preventDefault();
              e.stopPropagation();
              currentZoom /= 1.2;
              applyZoom();
            });
            
            document.getElementById('zoom-reset').addEventListener('click', function(e) {
              e.preventDefault();
              e.stopPropagation();
              currentZoom = 1.0;
              applyZoom();
            });
            
            function applyZoom() {
              try {
                const svg = document.querySelector('#graph svg');
                if (svg && svg.__data__) {
                  svg.__data__.zoom.scaleTo(d3.select(svg), currentZoom);
                }
              } catch (e) {
                console.error("Error applying zoom:", e);
              }
            }
            
            // Function to highlight the selected module in the graph
            function highlightSelectedModule() {
              if (!selectedModule) return;
              
              // Remove selected class from all nodes
              document.querySelectorAll('.module-node').forEach(node => {
                node.classList.remove('selected');
              });
              
              // Add selected class to the selected module
              try {
                const moduleNodes = document.querySelectorAll('.node');
                moduleNodes.forEach(node => {
                  const title = node.querySelector('title');
                  if (title && title.textContent === selectedModule) {
                    node.classList.add('selected');
                  }
                });
              } catch (e) {
                console.error("Error highlighting module:", e);
              }
            }
            
            // Parse DOT content to extract module data
            function parseDotToModuleData(dotContent) {
              try {
                const nodes = new Set();
                const edges = [];
                
                // Extract node definitions
                const nodeRegex = /"([^"]+)"/g;
                let match;
                while ((match = nodeRegex.exec(dotContent)) !== null) {
                  nodes.add(match[1]);
                }
                
                // Extract edges
                const edgeRegex = /"([^"]+)"\s*->\s*"([^"]+)"/g;
                while ((match = edgeRegex.exec(dotContent)) !== null) {
                  edges.push({
                    source: match[1],
                    target: match[2]
                  });
                }
                
                // Create new module data
                const newModules = [];
                nodes.forEach(nodeName => {
                  // Find existing module info if available
                  const existingModule = moduleData.modules.find(m => m.name === nodeName);
                  
                  const dependencies = edges
                    .filter(edge => edge.source === nodeName)
                    .map(edge => edge.target);
                    
                  const dependents = edges
                    .filter(edge => edge.target === nodeName)
                    .map(edge => edge.source);
                    
                  newModules.push({
                    name: nodeName,
                    dependencies: dependencies,
                    fan_in: dependents.length,
                    fan_out: dependencies.length,
                    in_cycle: existingModule ? existingModule.in_cycle : false,
                    file_path: existingModule ? existingModule.file_path : null,
                    location: existingModule ? existingModule.location : null
                  });
                });
                
                return {
                  modules: newModules,
                  cycles: [],
                  metrics: {
                    total_modules: newModules.length,
                    avg_fan_in: 0,
                    avg_fan_out: 0,
                    max_fan_in: 0,
                    max_fan_out: 0,
                    cyclic_modules: 0
                  }
                };
              } catch (e) {
                console.error("Error parsing DOT content:", e);
                return null;
              }
            }
            
            // Store d3-graphviz object globally
            let graphvizInstance = null;
            
            // Track if this is the first render
            let isFirstRender = true;
            
            // Function to render the graph (with all interactive features)
            function renderGraph(dotContent, focusedModule = null) {
              try {
                console.log("Rendering graph...");
                
                // Update module data from DOT content
                const newData = parseDotToModuleData(dotContent);
                if (newData) {
                  moduleData = newData;
                  console.log("Updated module data from DOT content:", moduleData);
                }
                
                // Set current selected module
                if (focusedModule) {
                  selectedModule = focusedModule;
                  // Completely recreate the graph when focusing on a module to ensure center positioning
                  d3.select('#graph').html(''); // Initialize graph container
                  graphvizInstance = null; // Reset instance to create a new one
                }
                
                // Create or reuse d3-graphviz instance
                if (!graphvizInstance) {
                  // Create new graphviz instance
                  graphvizInstance = d3.select('#graph')
                    .graphviz()
                    .zoom(true)
                    .fit(true)
                    .width('100%')
                    .height('100%')
                    .scale(1.0) // Explicitly set initial scale
                    .attributer(function(d) {
                      if (d.tag === 'svg') {
                        d.attributes.width = '100%';
                        d.attributes.height = '100%';
                      }
                    });
                }
                
                // Render DOT content without animation
                graphvizInstance
                  .engine('dot') // Explicitly specify layout engine
                  .transition(() => null) // Disable transition effects
                  .renderDot(dotContent)
                  .on('end', async () => {
                    console.log("Graph rendered successfully");
                    
                    // Get SVG element
                    const svg = document.querySelector('#graph svg');
                    if (!svg) {
                      console.error("SVG element not found");
                      return;
                    }
                    
                    // Set center position and zoom behavior
                    try {
                      const width = svg.clientWidth || svg.parentElement.clientWidth;
                      const height = svg.clientHeight || svg.parentElement.clientHeight;
                      
                      // Only recalculate center position when focusing on a module
                      if (focusedModule) {
                        console.log("Centering graph for focused module:", focusedModule);
                        
                        // Get bounding box of graph content
                        const g = svg.querySelector('g');
                        if (g) {
                          const bbox = g.getBBox();
                          
                          // Calculate scale (to fit screen)
                          let scale = Math.min(
                            width / (bbox.width + 100), 
                            height / (bbox.height + 100)
                          );
                          scale = Math.min(scale, 1.0); // Limit to prevent excessive zoom
                          
                          // Calculate transform to move to center position
                          const transform = d3.zoomIdentity
                            .translate(
                              (width - bbox.width * scale) / 2 - bbox.x * scale,
                              (height - bbox.height * scale) / 2 - bbox.y * scale
                            )
                            .scale(scale);
                          
                          // Set zoom behavior and apply transform
                          const zoom = d3.zoom()
                            .scaleExtent([0.1, 10])
                            .on('zoom', (event) => {
                              g.setAttribute('transform', event.transform);
                            });
                          
                          // Apply zoom behavior and initial transform
                          d3.select(svg)
                            .call(zoom)
                            .call(zoom.transform, transform);
                            
                          console.log("Applied transform to center graph:", transform);
                        }
                      } else {
                        // Only set zoom behavior if no focus
                        const zoom = d3.zoom()
                          .scaleExtent([0.1, 10])
                          .on('zoom', (event) => {
                            const g = svg.querySelector('g');
                            if (g) g.setAttribute('transform', event.transform);
                          });
                        
                        d3.select(svg).call(zoom);
                      }
                    } catch (e) {
                      console.error("Error setting zoom behavior:", e);
                    }
                    
                    // Add click events to nodes
                    document.querySelectorAll('.node').forEach(node => {
                      node.classList.add('module-node');
                      
                      // Remove previous event listeners and add new ones
                      const newNode = node.cloneNode(true);
                      node.parentNode.replaceChild(newNode, node);
                      
                      newNode.addEventListener('click', function(event) {
                        event.stopPropagation();
                        const title = this.querySelector('title')?.textContent;
                        if (title) {
                          console.log("Node clicked:", title);
                          moduleClicked(title);
                        }
                      });
                    });
                    
                    // Highlight selected module
                    highlightSelectedModule();
                    
                    showStatusMessage("Graph ready - use mouse to zoom and drag");
                  });
                
              } catch (e) {
                console.error("Error rendering graph:", e);
                showStatusMessage("Error rendering graph: " + e.message);
              }
            }
            
            // Module click handler
            function moduleClicked(moduleName) {
              console.log("Module clicked:", moduleName);
              
              const moduleInfo = moduleData.modules.find(m => m.name === moduleName);
              if (!moduleInfo) return;
              
              // Update module selection
              selectedModule = moduleName;
              
              document.getElementById('module-name').textContent = moduleInfo.name;
              
              // Display dependencies
              const dependenciesEl = document.getElementById('dependencies');
              dependenciesEl.innerHTML = '';
              if (moduleInfo.dependencies.length === 0) {
                dependenciesEl.innerHTML = '<li>None</li>';
              } else {
                moduleInfo.dependencies.forEach(dep => {
                  const depInfo = moduleData.modules.find(m => m.name === dep);
                  const li = document.createElement('li');
                  li.className = 'dependency-item';
                  li.textContent = dep;
                  
                  // Display file path if available
                  if (depInfo && depInfo.file_path) {
                    const filePathSpan = document.createElement('span');
                    filePathSpan.className = 'file-path';
                    filePathSpan.textContent = '(' + depInfo.file_path + ')';
                    li.appendChild(filePathSpan);
                  }
                  
                  // Add click event
                  li.addEventListener('click', () => {
                    if (depInfo && depInfo.file_path) {
                      const line = depInfo.location ? depInfo.location.start : 1;
                      vscode.postMessage({
                        command: 'openFile',
                        path: depInfo.file_path,
                        line: line,
                        moduleName: dep
                      });
                    } else {
                      vscode.postMessage({
                        command: 'openFile',
                        path: null,
                        line: 1,
                        moduleName: dep
                      });
                      moduleClicked(dep);
                    }
                  });
                  
                  dependenciesEl.appendChild(li);
                });
              }
              
              // Display dependents
              const dependentsEl = document.getElementById('dependents');
              dependentsEl.innerHTML = '';
              const dependents = moduleData.modules
                .filter(m => m.dependencies.includes(moduleName))
                .map(m => m.name);
                
              if (dependents.length === 0) {
                dependentsEl.innerHTML = '<li>None</li>';
              } else {
                dependents.forEach(dep => {
                  const depInfo = moduleData.modules.find(m => m.name === dep);
                  const li = document.createElement('li');
                  li.className = 'dependency-item';
                  li.textContent = dep;
                  
                  // Display file path if available
                  if (depInfo && depInfo.file_path) {
                    const filePathSpan = document.createElement('span');
                    filePathSpan.className = 'file-path';
                    filePathSpan.textContent = '(' + depInfo.file_path + ')';
                    li.appendChild(filePathSpan);
                  }
                  
                  // Add click event
                  li.addEventListener('click', () => {
                    if (depInfo && depInfo.file_path) {
                      const line = depInfo.location ? depInfo.location.start : 1;
                      vscode.postMessage({
                        command: 'openFile',
                        path: depInfo.file_path,
                        line: line,
                        moduleName: dep
                      });
                    } else {
                      vscode.postMessage({
                        command: 'openFile',
                        path: null,
                        line: 1,
                        moduleName: dep
                      });
                      moduleClicked(dep);
                    }
                  });
                  
                  dependentsEl.appendChild(li);
                });
              }

              console.log("Module info:", {name: moduleInfo.name, path: moduleInfo.file_path, loc: moduleInfo.location});
              
              // Send request to focus on this module
              showStatusMessage("Analyzing dependencies for " + moduleName + "...");
              vscode.postMessage({
                command: 'focusModule',
                moduleName: moduleName
              });
            }
            
            // Message listener for communication with vscode
            window.addEventListener('message', event => {
              const message = event.data;
              console.log("Received message from VS Code:", message.command);
              
              switch (message.command) {
                case 'updateGraph':
                  console.log("Received new graph data");
                  
                  // Update the graph with new dot content
                  if (message.dotContent) {
                    console.log("Rendering new graph data for module:", message.focusedModule);
                    // Use integrated renderGraph function - pass dotContent and selected module
                    renderGraph(message.dotContent, message.focusedModule);
                  } else {
                    showStatusMessage("Received empty graph data");
                  }
                  break;
              }
            });
            
            // Let VS Code know the webview is ready
            console.log("Webview initialized, notifying VS Code");
            vscode.postMessage({ command: 'webviewReady' });
            
            // Initial graph rendering
            console.log("Starting initial graph rendering");
            renderGraph(\`${dotContent.replace(/\\/g, '\\\\').replace(/`/g, '\\`').replace(/\${/g, '\\${')}\`);
          } catch (e) {
            console.error("Fatal error in webview script:", e);
            document.body.innerHTML = "<h1>Error initializing dependency visualizer</h1><pre>" + e.toString() + "</pre>";
          }
        })();
      </script>
    </body>
    </html>
  `;

  // Create and display webview panel
  const panel = vscode.window.createWebviewPanel(
    'rescriptDepVisualizer',
    isFocusedMode ? 'Module Dependencies' : 'ReScript Dependencies',
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

              // Run rescriptdep with the module flag
              progress.report({ message: 'Generating graph...' });
              if (token.isCancellationRequested) return;

              const dotContent = await runRescriptDep(cliPath, [
                '--format=dot',
                '--module',
                moduleName,
                bsDir
              ]);

              if (dotContent) {
                // Send the new dot content back to the webview for update
                console.log(`Sending updated graph data for module: ${moduleName}`);
                panel.webview.postMessage({
                  command: 'updateGraph',
                  dotContent: dotContent,
                  focusedModule: moduleName
                });
              } else {
                vscode.window.showErrorMessage(`Failed to generate dependency graph for ${moduleName}`);
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

  // Ensure the webview is properly initialized before sending messages
  panel.onDidChangeViewState(e => {
    if (e.webviewPanel.visible) {
      console.log("Webview became visible");
    }
  });
}

// Convert DOT file content to JSON data (extension of existing function)
function parseDotContentAsJson(dotContent: string): DependencyData {
  // Parse nodes and edges
  const { nodes, edges } = parseDotContent(dotContent);

  // Output debugging info to console
  console.log("Parsed nodes:", nodes.length);
  console.log("Parsed edges:", edges.length);

  // Create empty base data
  const data: DependencyData = {
    modules: [],
    cycles: [],
    metrics: {
      total_modules: nodes.length,
      avg_fan_in: 0,
      avg_fan_out: 0,
      max_fan_in: 0,
      max_fan_out: 0,
      cyclic_modules: 0
    }
  };

  // Process dependencies per node
  nodes.forEach(nodeName => {
    const dependencies = edges
      .filter(edge => edge.source === nodeName)
      .map(edge => edge.target);

    const dependents = edges
      .filter(edge => edge.target === nodeName)
      .map(edge => edge.source);

    const moduleNode: ModuleNode = {
      name: nodeName,
      dependencies: dependencies,
      fan_in: dependents.length,
      fan_out: dependencies.length,
      in_cycle: false, // Default value
      file_path: null,  // Default value
      location: null    // Default value
    };

    data.modules.push(moduleNode);
  });

  // Get JSON data from actual CLI
  // Request JSON format from CLI
  try {
    // Add temporary test data if modules are empty (for testing)
    if (data.modules.length === 0) {
      // Test data
      data.modules.push({
        name: "TestModule",
        dependencies: ["DepModule1", "DepModule2"],
        fan_in: 1,
        fan_out: 2,
        in_cycle: false,
        file_path: "/path/to/file.res",
        location: { start: 1, end: 20 }
      });
    }
  } catch (error) {
    console.error("Error processing module data:", error);
  }

  return data;
}

// Helper function to parse DOT content into a simple graph structure
function parseDotContent(dotContent: string): { nodes: string[], edges: { source: string, target: string }[] } {
  const nodes = new Set<string>();
  const edges: { source: string, target: string }[] = [];

  // Extract node definitions (basic regex parsing)
  const nodeRegex = /"([^"]+)"/g;
  let match;
  while ((match = nodeRegex.exec(dotContent)) !== null) {
    nodes.add(match[1]);
  }

  // Extract edges (basic regex parsing)
  const edgeRegex = /"([^"]+)"\s*->\s*"([^"]+)"/g;
  while ((match = edgeRegex.exec(dotContent)) !== null) {
    edges.push({
      source: match[1],
      target: match[2]
    });
  }

  return {
    nodes: Array.from(nodes),
    edges: edges
  };
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
