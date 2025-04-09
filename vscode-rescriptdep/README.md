# Bibimbob

A Visual Studio Code extension for visualizing dependencies between ReScript modules in your project. This extension helps you understand the structure and relationships in your ReScript codebase.

## Features

- **Full Dependency Graph Visualization**: View the complete dependency graph of your ReScript project
- **Module-Focused Views**: Focus on a specific module and see its direct dependencies and dependents
- **Interactive Graph**: Click on modules to navigate through dependencies
- **High Performance**: Uses digest-based caching to improve performance for large projects

<div style="display: flex; justify-content: space-between;">
  <img src="https://github.com/mununki/rescriptdep/raw/main/vscode-rescriptdep/images/rescriptdep_screenshot_0.png" alt="ReScript Dependency Visualization" width="49%">
  <img src="https://github.com/mununki/rescriptdep/raw/main/vscode-rescriptdep/images/rescriptdep_screenshot_1.png" alt="Module Dependency View" width="49%">
</div>

## Platform Support

Currently, this extension is supported on:
- macOS
- Linux

**Important: Windows is not supported in the current version.**

Windows support is planned for a future release. The limitation is due to issues with the CLI binary build process.

## Commands

This extension provides several commands in the Command Palette (`Ctrl+Shift+P` or `Cmd+Shift+P`):

- **ReScript: Show Dependency Graph**: Shows the full dependency graph of your project
- **ReScript: Focus On Module Dependencies**: Focuses on a specific module and its relationships
- **ReScript: Clear Dependency Cache**: Clears the cached dependency data for improved accuracy after major changes

## Cache Management

The extension uses a caching mechanism to improve performance when analyzing module dependencies:

- Cache files are stored in the extension's global storage area:
  - macOS: `~/Library/Application Support/Code/User/globalStorage/mununki.vscode-bibimbob/cache`
  - Linux: `~/.config/Code/User/globalStorage/mununki.vscode-bibimbob/cache`
- Each workspace gets its own cache file based on the workspace name
- The cache is automatically invalidated when module files change (using digest comparison)
- You can manually clear the cache using the "ReScript: Clear Dependency Cache" command

## Requirements

- A ReScript project (with either `bsconfig.json` or `rescript.json` configuration file)
- Compiled output in the `lib/bs` directory
- **Platform**: macOS or Linux (Windows is not supported in the current version)

## Installation

1. Install the extension from the VS Code Marketplace
2. Open a ReScript project in VS Code
3. Use the Command Palette to run the dependency visualization commands

## Usage Tips

1. **First Analysis**: The first analysis might take longer as it builds the initial cache
2. **After Major Changes**: Clear the cache using the command if you've made significant changes to your codebase
3. **Module Navigation**: Click on any module in the visualization to focus on that module

## Extension Settings

This extension doesn't require any specific settings.

## Known Issues

- Windows platform is not supported in the current version
- Please report any issues on the GitHub repository

## Release Notes

### 0.6.0

- Improved dependency graph rendering with optimized layout algorithms
- Added support for filtering modules by namespace
- Improved error handling for invalid project configurations

### 0.5.0

- Added monorepo support: The extension now detects and works with monorepo projects containing multiple ReScript packages
- Improved visualization with better node positioning and relationship indicators
- Enhanced module focus mode with clearer dependency direction highlighting

### 0.4.0

- Added AST-based dependency analysis for more accurate module dependency detection
- Significantly improved performance with optimized caching and parallel processing
- Reduced memory usage through efficient data structures and lazy loading

### 0.3.0

- Added dark theme support: The dependency graph visualization now automatically adapts to your VS Code theme

### 0.2.0

- Added external module visualization: Now properly represents external dependencies in the dependency graph

### 0.1.1

- Fixed compatibility issue by downgrading the VS Code engine requirement to support older VS Code versions

### 0.1.0

- Initial release of Bibimbob
- Dependency graph visualization
- Module-focused views
- Caching for improved performance

---

## For more information

* [ReScript Language](https://rescript-lang.org/)
* [GitHub Repository](https://github.com/mununki/rescriptdep)

**Enjoy!**
