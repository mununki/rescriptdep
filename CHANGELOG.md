# Changelog

All notable changes to the ReScriptDep project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1] - 2026-04-18
### Fixed
- Fixed an issue where standard modules were not filtered when the `-m` flag was not used
- Fixed focused module dependency analysis for projects using `namespace` in `rescript.json`
- Improved CI resilience against transient opam dependency fetch failures

### Added
- Static linking support for Linux environments
- Performance benchmarking mode with `--benchmark` flag
- Added a Makefile-based test workflow for local and CI runs
- Added namespace-focused regression coverage

### Changed
- Removed file-based cache persistence in favor of in-memory caching only
- Removed `--cache-file` option completely
- Enhanced error handling for workspace analysis
- Improved progress reporting in VS Code extension
- Removed outdated Rewatch-specific test fixtures and compatibility logic, keeping analysis focused on compiler-generated `.cmt` and `.ast` files

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
