# Changelog

All notable changes to the ReScriptDep project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Added regression coverage for dependency graph dependent-index updates
- Added ReScript 12 support for value usage analysis (`-vb`) by decoding typed trees with the layout of the compiler that wrote each `.cmt` file
- Added ReScript 12 fixture projects (standard and namespaced) with dependency graph and value usage regression tests
- Added ReScript 13 (13.0.0-alpha.6) support for dependency analysis; value usage analysis reports `-6` for modules built by compilers it cannot decode yet
- Added ReScript 13 fixture projects and self-contained compiler identification tests
- Added a supported ReScript versions table to the README

### Changed
- Organized typed tree decoding into one backend per compiler release (`Compiler_v11`, `Compiler_v12`) selected by the CMT magic number and `cmt_infos` layout, with decoders skipping constructors added by later releases instead of misreading them
- `.cmt` files written by unknown compilers now stop the analysis with an error instead of silently producing an empty graph
- Improved dependency graph performance by maintaining a reverse dependent index instead of scanning the full graph for each dependent lookup
- Reduced VS Code inline value usage analysis work with debounced requests, short-lived result caching, and JSON output parsing

### Fixed
- Prevented stale VS Code inline value usage decorations from appearing after source edits or outdated CLI responses
- Improved inline value usage cache invalidation when ReScript build artifacts change
- Fixed a segmentation fault when counting value usage (`-vb`) in projects built with ReScript 12
- Fixed namespaced projects so imports are filtered by each module's `.ast` file again; ReScript 12 runtime modules (`Stdlib_*`, `Primitive_*`) and unused imports no longer appear as dependencies, and the generated namespace module is no longer shown as a node
- Fixed value usage analysis (`-vb`) in namespaced projects
- Read the ReScript 12 `dependencies` and `dev-dependencies` fields of `rescript.json` when resolving external module paths
- Read `.ast` headers by their byte length so data from the binary AST is never taken as module references
- Aligned the vendored ReScript 11 typedtree with the compiler's constructor layout

## [0.1.2] - 2026-04-22
### Fixed
- Fixed the generated opam metadata tags so opam lint no longer reports the default example `topics` tag
- Fixed the opam test suite by keeping `@runtest` self-contained instead of depending on locally built ReScript fixture outputs
- Improved fixture path resolution for local integration tests executed from build directories

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
