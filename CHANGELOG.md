# Changelog

All notable changes to the ReScriptDep project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Fixed an issue where standard modules were not filtered when the `-m` flag was not used

### Added
- Static linking support for Linux environments
- Performance benchmarking mode with `--benchmark` flag
- Improved cache management with `--clear-cache` option

### Changed
- Enhanced error handling for workspace analysis
- Improved progress reporting in VS Code extension

## [0.1.0] - 2024-06-19

### Added
- Initial release of ReScriptDep
- Dependency analyzer for ReScript modules
- CMT file parsing and module information extraction
- Module dependency tracking and visualization support
- Caching system for improved performance
- Parallel processing of module files
- VS Code extension for dependency visualization
  - Full dependency graph visualization
  - Module-focused views
  - Interactive graph navigation
  - Digest-based caching 