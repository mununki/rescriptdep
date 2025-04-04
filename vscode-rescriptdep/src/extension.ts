import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

// Command IDs
const SHOW_DEPENDENCY_GRAPH = 'bibimbob.showDependencyGraph';
const FOCUS_MODULE_DEPENDENCIES = 'bibimbob.focusModuleDependencies';

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

  context.subscriptions.push(fullGraphCommand);
  context.subscriptions.push(focusModuleCommand);
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
      if (!focusOnModule) {
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

          // Log module count for debugging
          console.log(`Detected approximately ${moduleCount} modules in the project`);

          // Prompt user if module count is high
          if (moduleCount > 1000) {
            const response = await vscode.window.showWarningMessage(
              `This project contains approximately ${moduleCount} modules, which may cause performance issues or visualization errors.`,
              'Continue Anyway', 'Focus on Module', 'Cancel'
            );

            if (response === 'Focus on Module') {
              // User chose to focus on a specific module
              await vscode.commands.executeCommand(FOCUS_MODULE_DEPENDENCIES);
              return;
            } else if (response !== 'Continue Anyway') {
              // User chose to cancel
              return;
            }

            // If continuing, use the DOT output we already have
            progress.report({ message: 'Generating visualization...' });
            if (token.isCancellationRequested) { return; }

            if (dotOutput) {
              showDotGraphWebview(context, dotOutput, focusOnModule, moduleName);
              return;
            }
          }
        } catch (error) {
          // If check operation fails, continue anyway with regular flow
          console.warn('Failed to estimate project size:', error);
        }
      }

      // Run the CLI command with the determined bsDir and moduleName (if applicable)
      progress.report({ message: 'Running rescriptdep CLI...' });
      if (token.isCancellationRequested) { return; }

      // Define CLI arguments - Use DOT format instead of JSON for better performance
      const args: string[] = ['--format=dot'];

      // Add module focus if specified
      if (moduleName) {
        args.push('--module', moduleName);
      }

      // Add bsDir target
      args.push(bsDir);

      // Get DOT format data
      const dotContent = await runRescriptDep(cliPath, args, context);

      // Display webview
      progress.report({ message: 'Generating visualization...' });
      if (token.isCancellationRequested) { return; }

      if (dotContent) {
        showDotGraphWebview(context, dotContent, focusOnModule, moduleName);
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
    // Note: Windows doesn't have a simple equivalent to 'nice' via command line

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
function showDotGraphWebview(context: vscode.ExtensionContext, dotContent: string, isFocusedMode: boolean = false, centerModuleName?: string) {
  // Detect if the current theme is dark
  const isDarkTheme = vscode.window.activeColorTheme && vscode.window.activeColorTheme.kind === vscode.ColorThemeKind.Dark;

  // Set appropriate graph attributes based on theme
  const themeAttributes = isDarkTheme ?
    'bgcolor="transparent" fontcolor="#e0e0e0"' :
    'bgcolor="transparent" fontcolor="#333333"';

  // 노드 배경색 및 패딩 설정 - 더 명확한 색상과 스타일 적용
  const nodeStyle = isDarkTheme ?
    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#1e1e1e", margin="0.3,0.2", color="#aaaaaa", penwidth=1]' :
    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#f0f0f0", margin="0.3,0.2", color="#666666", penwidth=1]';

  // 라인과 화살표 스타일 설정
  const edgeStyle = isDarkTheme ?
    'edge[color="#555555", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]' :
    'edge[color="#cccccc", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]';

  // Update DOT content - node 스타일을 직접 적용
  let themedDotContent = dotContent;

  // 기본 속성 설정
  themedDotContent = themedDotContent.replace(/^(digraph\s+\w+\s*\{)/m,
    `$1\n  ${themeAttributes}\n  ${nodeStyle}\n  ${edgeStyle}\n  splines=true\n  overlap=false\n  sep="+10"`);

  // 노드 스타일을 강제로 적용 - 모든 노드에 직접 스타일 추가
  if (isDarkTheme) {
    // 다크 테마에서는 어두운 배경색 적용
    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#1e1e1e", ');
  } else {
    // 라이트 테마에서는 연한 회색 배경색 적용
    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#f0f0f0", ');
  }

  // 포커스 모드일 때 중앙 모듈 스타일 추가
  if (isFocusedMode && centerModuleName) {
    // 중앙 모듈의 노드 스타일 변경
    const centerNodePattern = new RegExp(`\\s+(${centerModuleName})\\s*\\[`);
    themedDotContent = themedDotContent.replace(centerNodePattern, ` $1 [style="filled", fillcolor="lightgreen", `);

    // 보다 강력한 엣지 색상 변경 로직 - DOT 형식에 맞춰 수정
    const lines = themedDotContent.split('\n');
    const coloredLines = lines.map(line => {
      // 더 정확한 엣지 라인 패턴 매칭 
      if (line.includes('->') && !line.includes('//')) { // 주석이 아닌 엣지 라인
        const trimmed = line.trim();

        // source -> target 패턴 찾기 (속성 있는 경우와 없는 경우 모두 처리)
        const parts = trimmed.split('->');
        if (parts.length === 2) {
          const source = parts[0].trim();
          let target = parts[1].trim();

          // 세미콜론이나 속성 제거해서 순수 타겟 이름 추출
          const targetName = target.split(/[\[\s;]/)[0].trim();

          // 이미 속성이 있는지 확인
          const hasAttributes = target.includes('[');

          // 1. dependents -> center (중앙 모듈로 들어오는 화살표)
          if (targetName === centerModuleName) {
            if (hasAttributes) {
              // 기존 속성에 색상 추가 - 다크 테마일 때 더 어두운 색상 사용
              const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
              return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
            } else {
              // 새 속성 추가 - 다크 테마일 때 더 어두운 색상 사용
              const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
              return line.replace(/;/, ` [color="${arrowColor}", penwidth=1.5];`);
            }
          }
          // 2. center -> dependencies (중앙 모듈에서 나가는 화살표)
          else if (source === centerModuleName) {
            if (hasAttributes) {
              // 기존 속성에 색상 추가 - 다크 테마일 때 더 어두운 색상 사용
              const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
              return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
            } else {
              // 새 속성 추가 - 다크 테마일 때 더 어두운 색상 사용
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
    <script src="https://unpkg.com/viz.js@2.1.2/viz.js"></script>
    <script src="https://unpkg.com/viz.js@2.1.2/full.render.js"></script>
    <style>
        /* 기본 스타일만 유지 */
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
        
        #graph-container {
            width: 100%;
            height: calc(100vh - 60px);
            overflow: hidden;
            display: flex;
            justify-content: center;
            align-items: center;
            position: relative;
            user-select: none;
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
        
        .controls {
            display: flex;
            justify-content: center;
            margin-bottom: 10px;
            gap: 10px;
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

        /* 레전드 스타일 - 라인으로 변경 */
        .legend {
            display: flex;
            justify-content: center;
            margin-bottom: 10px;
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
    </style>
</head>
<body>
    <div class="legend">
        <div class="legend-item">
            <div class="legend-line" style="background-color: var(--dependents-color, lightblue);"></div>
            <span>Dependents (uses center module)</span>
        </div>
        <div class="legend-item">
            <div class="legend-line" style="background-color: var(--dependencies-color, lightcoral);"></div>
            <span>Dependencies (used by center module)</span>
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
        // 줌 관련 변수
        let currentZoom = 1;
        const MIN_ZOOM = 0.1;
        const MAX_ZOOM = 5;
        const ZOOM_SPEED = 0.1;
        
        // 드래그 관련 변수
        let isDragging = false;
        let lastX = 0;
        let lastY = 0;
        let viewBox = { x: 0, y: 0, width: 1000, height: 1000 };
        
        // Initial data placeholders - will be populated via message
        let dotSrc = '';
        let isFocusedMode = false;
        let centerModule = null;
        
        // 테마 관련 변수
        const isDarkTheme = document.body.classList.contains('vscode-dark');
        
        // 테마에 따른 색상 설정
        document.documentElement.style.setProperty('--dependents-color', isDarkTheme ? 'steelblue' : 'lightblue');
        document.documentElement.style.setProperty('--dependencies-color', isDarkTheme ? 'indianred' : 'lightcoral');
        
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
        
        // Render the DOT data - only called after we receive data
        function renderGraph() {
            if (!dotSrc) {
                console.log('No DOT data yet');
                return;
            }
        
            const viz = new Viz();
            
            viz.renderSVGElement(dotSrc)
                .then(element => {
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
                    
                    // SVG 요소에 선택 방지 스타일 추가
                    svgElement.style.userSelect = 'none';
                    svgElement.style.webkitUserSelect = 'none';
                    svgElement.style.msUserSelect = 'none';
                    
                    // SVG 내부의 모든 요소에 선택 방지 적용
                    const allSvgElements = svgElement.querySelectorAll('*');
                    allSvgElements.forEach(el => {
                        el.style.userSelect = 'none';
                        el.style.webkitUserSelect = 'none';
                        el.style.msUserSelect = 'none';
                    });
                    
                    // 테마 감지 및 클래스 추가
                    const isDarkTheme = document.body.classList.contains('vscode-dark');
                    if (isDarkTheme) {
                        document.body.classList.add('dark-theme');
                    }
                    
                    // SVG 직접 수정 - 모든 노드 배경색 변경
                    const nodeRects = svgElement.querySelectorAll('.node rect, .node polygon');
                    const bgColor = isDarkTheme ? '#1e1e1e' : '#f0f0f0';
                    
                    nodeRects.forEach(rect => {
                        rect.setAttribute('fill', bgColor);
                        // 테두리도 확실히 설정
                        rect.setAttribute('stroke', isDarkTheme ? '#aaaaaa' : '#666666');
                        rect.setAttribute('stroke-width', '1px');
                        // 둥근 모서리 추가
                        if (rect.tagName.toLowerCase() === 'rect') {
                            rect.setAttribute('rx', '4');
                            rect.setAttribute('ry', '4');
                        }
                    });
                    
                    // 다크 모드일 때 텍스트 색상 변경
                    if (isDarkTheme) {
                        const nodeTexts = svgElement.querySelectorAll('.node text');
                        nodeTexts.forEach(text => {
                            text.setAttribute('fill', '#cccccc'); // 옅은 회색으로 변경
                        });
                    }
                    
                    // 화살표 스타일 수정 - 테두리와 배경색 동일하게 설정
                    const edgePaths = svgElement.querySelectorAll('.edge path');
                    const arrowColor = isDarkTheme ? '#555555' : '#cccccc';
                    edgePaths.forEach(path => {
                        path.setAttribute('stroke', arrowColor);
                        // fill은 화살표 헤드에만 영향을 주도록 설정
                        path.setAttribute('fill', 'none');
                        path.setAttribute('stroke-width', '1.2');
                    });
                    
                    // 화살표 헤드 스타일 조정
                    const arrowHeads = svgElement.querySelectorAll('.edge polygon');
                    arrowHeads.forEach(head => {
                        head.setAttribute('fill', arrowColor);
                        head.setAttribute('stroke', arrowColor);
                    });
                    
                    // 포커스 모드일 때 중앙 모듈 관련 엣지 색상 후처리
                    if (centerModule) {
                        // 중앙 모듈 노드 식별 (title 텍스트가 중앙 모듈명과 일치)
                        const centerNode = Array.from(svgElement.querySelectorAll('.node')).find(node => {
                            const titleEl = node.querySelector('title');
                            return titleEl && titleEl.textContent === centerModule;
                        });
                        
                        if (centerNode) {
                            // 중앙 모듈로 들어오는 화살표 처리 (Dependents)
                            const edgesTo = svgElement.querySelectorAll('.edge');
                            edgesTo.forEach(edge => {
                                const titleEl = edge.querySelector('title');
                                if (titleEl && titleEl.textContent) {
                                    const titleText = titleEl.textContent;
                                    // 화살표 타이틀은 보통 "source->target" 형식
                                    const parts = titleText.split('->');
                                    if (parts.length === 2) {
                                        const target = parts[1].trim();
                                        const source = parts[0].trim();
                                        
                                        // 1. Dependents -> Center 방향
                                        if (target === centerModule) {
                                            const paths = edge.querySelectorAll('path');
                                            const polygons = edge.querySelectorAll('polygon');
                                            
                                            // 다크 테마일 때 더 어두운 색상 사용
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
                                        // 2. Center -> Dependencies 방향
                                        else if (source === centerModule) {
                                            const paths = edge.querySelectorAll('path');
                                            const polygons = edge.querySelectorAll('polygon');
                                            
                                            // 다크 테마일 때 더 어두운 색상 사용
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
                    }
                    
                    // 초기 viewBox 설정
                    const bbox = svgElement.getBBox();
                    viewBox = {
                        x: bbox.x,
                        y: bbox.y,
                        width: bbox.width,
                        height: bbox.height
                    };
                    svgElement.setAttribute('width', '100%');
                    svgElement.setAttribute('height', '100%');
                    
                    // SVG 요소에 스타일 추가하여 전체 영역을 채우도록 함
                    svgElement.style.display = 'block';
                    svgElement.style.width = '100%';
                    svgElement.style.height = '100%';
                    svgElement.style.margin = '0';
                    svgElement.style.padding = '0';
                    
                    // preserveAspectRatio 속성 설정하여 화면에 맞게 확장
                    svgElement.setAttribute('preserveAspectRatio', 'xMidYMid meet');
                    
                    updateViewBox();
                    
                    // Add click handlers to nodes
                    setupNodeClickHandlers();
                    
                    // Setup dragging
                    setupDragHandlers();
                    
                    // 화면에 맞게 초기 뷰 조정
                    fitGraphToContainer();
                    
                    // 스크롤 줌 설정
                    setupScrollZoom();
                })
                .catch(error => {
                    showErrorMessage(error);
                });
        }
        
        function setupNodeClickHandlers() {
            if (!svgElement) return;
            
            // 모든 노드 처리
            const nodes = svgElement.querySelectorAll('.node');
            nodes.forEach(node => {
                const titleEl = node.querySelector('title');
                if (!titleEl || !titleEl.textContent) return;
                
                const moduleName = titleEl.textContent.trim();
                if (!moduleName) return;
                
                // 중앙 모듈인지 확인
                if (centerModule && moduleName === centerModule) {
                    // 중앙 모듈에는 클릭 방지 스타일만 적용
                    node.style.cursor = 'default';
                    
                    // 클릭 이벤트 무효화
                    node.addEventListener('click', (e) => {
                        e.preventDefault();
                        e.stopPropagation();
                        return false;
                    }, true);
                } else {
                    // 일반 모듈에는 클릭 이벤트 추가
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
            
            // 마우스 다운 - 드래그 시작
            svgElement.addEventListener('mousedown', (e) => {
                e.preventDefault(); // 텍스트 선택 방지
                isDragging = true;
                lastX = e.clientX;
                lastY = e.clientY;
                svgElement.style.cursor = 'grabbing';
            });
            
            // 마우스 이동 - 드래그 중
            document.addEventListener('mousemove', (e) => {
                if (!isDragging) return;
                
                const dx = e.clientX - lastX;
                const dy = e.clientY - lastY;
                lastX = e.clientX;
                lastY = e.clientY;
                
                // 고정된 비율로 이동 - SVG 좌표계로 변환
                const svgRect = svgElement.getBoundingClientRect();
                
                // SVG 스케일 계산 (1 화면 픽셀당 얼마만큼의 SVG 좌표가 표현되는지)
                const scaleX = viewBox.width / svgRect.width;
                const scaleY = viewBox.height / svgRect.height;
                
                // 동일한 이동 비율 적용
                // x, y 모두에 같은 스케일 적용을 위해 평균값 또는 최대값 사용
                const scale = Math.max(scaleX, scaleY);
                
                // viewBox 업데이트
                viewBox.x -= dx * scale;
                viewBox.y -= dy * scale;
                
                updateViewBox();
            });
            
            // 마우스 업 - 드래그 종료
            document.addEventListener('mouseup', () => {
                isDragging = false;
                svgElement.style.cursor = 'grab';
            });
            
            // 마우스가 SVG 영역 밖으로 나가도 드래그 종료
            document.addEventListener('mouseleave', () => {
                isDragging = false;
                svgElement.style.cursor = 'grab';
            });
            
            // Initial cursor style
            svgElement.style.cursor = 'grab';
        }
        
        function setupScrollZoom() {
            // 마우스 휠 이벤트
            document.getElementById('graph-container').addEventListener('wheel', (e) => {
                e.preventDefault();
                
                // 마우스 위치 가져오기
                const svgRect = svgElement.getBoundingClientRect();
                const mouseX = e.clientX - svgRect.left;
                const mouseY = e.clientY - svgRect.top;
                
                // SVG 내의 상대적 위치 계산 (0~1 범위)
                const relativeX = mouseX / svgRect.width;
                const relativeY = mouseY / svgRect.height;
                
                // viewBox 내의 실제 좌표 계산
                const pointX = viewBox.x + (viewBox.width * relativeX);
                const pointY = viewBox.y + (viewBox.height * relativeY);
                
                // 줌 인/아웃 (휠 방향에 따라)
                const zoomFactor = e.deltaY > 0 ? (1 - ZOOM_SPEED) : (1 + ZOOM_SPEED);
                zoomByFactor(zoomFactor, { x: pointX, y: pointY });
            });
            
            // 더블 클릭 시 확대
            document.getElementById('graph-container').addEventListener('dblclick', (e) => {
                e.preventDefault(); // 텍스트 선택 방지
                // 마우스 위치 가져오기
                const svgRect = svgElement.getBoundingClientRect();
                const mouseX = e.clientX - svgRect.left;
                const mouseY = e.clientY - svgRect.top;
                
                // SVG 내의 상대적 위치 계산
                const relativeX = mouseX / svgRect.width;
                const relativeY = mouseY / svgRect.height;
                
                // viewBox 내의 실제 좌표 계산
                const pointX = viewBox.x + (viewBox.width * relativeX);
                const pointY = viewBox.y + (viewBox.height * relativeY);
                
                // 줌 인
                zoomByFactor(0.7, { x: pointX, y: pointY });
            });
            
            // 우클릭 시 리셋
            document.getElementById('graph-container').addEventListener('contextmenu', (e) => {
                e.preventDefault(); // 우클릭 메뉴 방지
                resetZoom();
            });
        }
        
        function zoomByFactor(factor, point) {
            if (!svgElement) return;
            
            // 현재 줌 계산
            const oldZoom = currentZoom;
            currentZoom = Math.max(MIN_ZOOM, Math.min(MAX_ZOOM, currentZoom * factor));
            
            // 실제 적용할 비율 계산
            const realFactor = currentZoom / oldZoom;
            
            // 포인트 위치에서의 줌 계산
            const newWidth = viewBox.width / realFactor;
            const newHeight = viewBox.height / realFactor;
            
            // 마우스 포인터 위치 기준으로 새 좌표 계산
            const mouseRatioX = (point.x - viewBox.x) / viewBox.width;
            const mouseRatioY = (point.y - viewBox.y) / viewBox.height;
            
            const newX = point.x - mouseRatioX * newWidth;
            const newY = point.y - mouseRatioY * newHeight;
            
            // viewBox 업데이트
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
            
            // 그래프를 화면에 맞게 재조정
            fitGraphToContainer();
        }
        
        function fitGraphToContainer() {
            if (!svgElement) return;
            
            // 컨테이너 크기 가져오기
            const container = document.getElementById('graph-container');
            const containerRect = container.getBoundingClientRect();
            
            // SVG의 viewBox 가져오기
            const bbox = svgElement.getBBox();
            
            // 컨테이너 크기와 그래프 크기의 비율 계산
            const widthRatio = containerRect.width / bbox.width;
            const heightRatio = containerRect.height / bbox.height;
            
            // 더 작은 비율을 사용하여 그래프가 완전히 보이도록 함
            const ratio = Math.min(widthRatio, heightRatio) * 0.7; // 여백을 위해 70%만 사용
            
            // 최대 확대 비율 제한 (노드가 적은 경우 과도한 확대 방지)
            const limitedRatio = Math.min(ratio, 0.7);
            
            // 새 viewBox 계산
            const newWidth = bbox.width * 1.5; // 좌우 여백 확보를 위해 너비 확장
            const newHeight = bbox.height * 1.5; // 상하 여백 확보를 위해 높이 확장
            const centerX = bbox.x + bbox.width / 2;
            const centerY = bbox.y + bbox.height / 2;
            
            // 화면 중앙에 그래프가 오도록 viewBox 설정
            viewBox = {
                x: centerX - newWidth / 2,
                y: centerY - newHeight / 2,
                width: newWidth,
                height: newHeight
            };
            
            // 계산된 viewBox 적용
            updateViewBox();
            
            // 초기 줌 레벨 설정
            currentZoom = limitedRatio;
        }
        
        // Listen for messages from VS Code
        window.addEventListener('message', event => {
            const message = event.data;
            if (message.command === 'initGraph') {
                // Receive the graph data from the extension
                dotSrc = message.dotContent;
                isFocusedMode = message.isFocusedMode;
                centerModule = message.centerModule;
                
                // Now that we have data, render the graph
                renderGraph();
            } else if (message.command === 'updateGraph') {
                // Update graph with new data
                dotSrc = message.dotContent;
                isFocusedMode = message.isFocusedMode;
                centerModule = message.centerModule;
                
                // Re-render with new data
                renderGraph();
            } else if (message.command === 'updateTheme') {
                // Reload the page to apply new theme
                window.location.reload();
            } else if (message.command === 'showError') {
                // Display an error message without rendering graph
                showErrorMessage({ message: message.errorMessage });
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
    currentPanel.title = isFocusedMode && centerModuleName ? `Module: ${centerModuleName} Dependencies` : 'ReScript Dependencies';
    currentPanel.reveal(vscode.ViewColumn.One);

    // Send the graph data after the webview is loaded
    currentPanel.webview.onDidReceiveMessage(
      message => {
        if (message.command === 'webviewReady') {
          currentPanel?.webview.postMessage({
            command: 'initGraph',
            dotContent: themedDotContent,
            isFocusedMode: isFocusedMode,
            centerModule: centerModuleName
          });
        }
      }
    );
  } else {
    // Create a new panel
    currentPanel = vscode.window.createWebviewPanel(
      'bibimbobVisualizer',
      isFocusedMode && centerModuleName ? `Module: ${centerModuleName} Dependencies` : 'ReScript Dependencies',
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
            centerModule: centerModuleName
          });
        }
      }
    );
  }

  // Listen for theme changes
  context.subscriptions.push(
    vscode.window.onDidChangeActiveColorTheme(theme => {
      const newIsDarkTheme = theme.kind === vscode.ColorThemeKind.Dark;
      if (newIsDarkTheme !== isDarkTheme && currentPanel) {
        // Instead of recreating the webview, just send a message to update theme
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

                  // 테마 스타일 설정
                  const themeAttributes = isDarkTheme ?
                    'bgcolor="transparent" fontcolor="#e0e0e0"' :
                    'bgcolor="transparent" fontcolor="#333333"';

                  const nodeStyle = isDarkTheme ?
                    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#1e1e1e", margin="0.3,0.2", color="#aaaaaa", penwidth=1]' :
                    'node[shape=box, fontname="sans-serif", style="filled", fillcolor="#f0f0f0", margin="0.3,0.2", color="#666666", penwidth=1]';

                  const edgeStyle = isDarkTheme ?
                    'edge[color="#555555", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]' :
                    'edge[color="#cccccc", arrowsize=0.8, arrowhead=normal, penwidth=1, minlen=1]';

                  // Update DOT content - node 스타일을 직접 적용
                  let themedDotContent = dotContent;

                  // 기본 속성 설정
                  themedDotContent = themedDotContent.replace(/^(digraph\s+\w+\s*\{)/m,
                    `$1\n  ${themeAttributes}\n  ${nodeStyle}\n  ${edgeStyle}\n  splines=true\n  overlap=false\n  sep="+10"`);

                  // 노드 스타일을 강제로 적용
                  if (isDarkTheme) {
                    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#1e1e1e", ');
                  } else {
                    themedDotContent = themedDotContent.replace(/\s+(\w+)\s*\[/g, ' $1 [style="filled", fillcolor="#f0f0f0", ');
                  }

                  // 포커스 모드에서 중앙 모듈 및 엣지 스타일 처리
                  // 중앙 모듈의 노드 스타일 변경
                  const centerNodePattern = new RegExp(`\\s+(${moduleName})\\s*\\[`);
                  themedDotContent = themedDotContent.replace(centerNodePattern, ` $1 [style="filled", fillcolor="lightgreen", `);

                  // 엣지 색상 변경
                  const lines = themedDotContent.split('\n');
                  const coloredLines = lines.map(line => {
                    // 엣지 라인 패턴 매칭
                    if (line.includes('->') && !line.includes('//')) {
                      const trimmed = line.trim();

                      // source -> target 패턴 찾기
                      const parts = trimmed.split('->');
                      if (parts.length === 2) {
                        const source = parts[0].trim();
                        let target = parts[1].trim();

                        // 순수 타겟 이름 추출
                        const targetName = target.split(/[\[\s;]/)[0].trim();

                        // 속성 여부 확인
                        const hasAttributes = target.includes('[');

                        // 1. dependents -> center (중앙 모듈로 들어오는 화살표)
                        if (targetName === moduleName) {
                          if (hasAttributes) {
                            // 기존 속성에 색상 추가 - 다크 테마일 때 더 어두운 색상 사용
                            const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
                            return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
                          } else {
                            // 새 속성 추가 - 다크 테마일 때 더 어두운 색상 사용
                            const arrowColor = isDarkTheme ? 'steelblue' : 'lightblue';
                            return line.replace(/;/, ` [color="${arrowColor}", penwidth=1.5];`);
                          }
                        }
                        // 2. center -> dependencies (중앙 모듈에서 나가는 화살표)
                        else if (source === moduleName) {
                          if (hasAttributes) {
                            // 기존 속성에 색상 추가 - 다크 테마일 때 더 어두운 색상 사용
                            const arrowColor = isDarkTheme ? 'indianred' : 'lightcoral';
                            return line.replace(/\[([^\]]*)\]/, `[color="${arrowColor}", penwidth=1.5, $1]`);
                          } else {
                            // 새 속성 추가 - 다크 테마일 때 더 어두운 색상 사용
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

// Helper function to get webview URIs for local files
function getWebviewUri(webview: vscode.Webview, extensionUri: vscode.Uri, ...pathSegments: string[]): vscode.Uri {
  return webview.asWebviewUri(vscode.Uri.joinPath(extensionUri, ...pathSegments));
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
