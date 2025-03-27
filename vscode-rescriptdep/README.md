# Bibimbob

A Visual Studio Code extension for visualizing dependencies between ReScript modules in your project. This extension helps you understand the structure and relationships in your ReScript codebase.

## Features

- **Full Dependency Graph Visualization**: View the complete dependency graph of your ReScript project
- **Module-Focused Views**: Focus on a specific module and see its direct dependencies and dependents
- **Interactive Graph**: Click on modules to navigate through dependencies
- **High Performance**: Uses digest-based caching to improve performance for large projects

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
  - macOS: `~/Library/Application Support/Code/User/globalStorage/yourpublisher.vscode-rescriptdep/cache`
  - Linux: `~/.config/Code/User/globalStorage/yourpublisher.vscode-rescriptdep/cache`
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
* [GitHub Repository](https://github.com/yourname/vscode-rescriptdep)

**Enjoy!**
