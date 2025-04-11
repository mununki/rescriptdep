# ReScriptDep

A dependency analyzer for ReScript modules.

## Introduction

ReScriptDep is a tool for analyzing dependencies between ReScript modules. It uses cmt files to identify module dependencies and can visualize them or output them in JSON format.

## Installation

```bash
# Install via opam
opam install rescriptdep
```

### Building with Static Linking (Linux)

To build rescriptdep with static linking on Linux, which creates a standalone executable without external dependencies:

```bash
# Build the project using the static profile
dune build --profile static

# This will generate a statically linked executable for Linux environments
```

## Usage

```bash
# Basic usage
rescriptdep [options] files_or_dirs

# Options
# -o, --output <file>     : Output file (default: stdout)
# -f, --format <format>   : Output format (dot, json)
# -m, --module <name>     : Focus on specific module and its dependencies
# -v, --verbose           : Enable verbose output
# -b, --benchmark         : Enable performance benchmarking
# --no-cache              : Skip using cache
# --clear-cache           : Clear the cache before analyzing
# -nd, --no-dependents    : Find modules that have no dependents (not imported by any other modules)
```

## Development

For detailed information about building the project, running tests, and contributing, please see [CONTRIBUTING.md](CONTRIBUTING.md).

### Quick Start for Development

```bash
# Build the project
dune build

# Run all tests
dune runtest
```

## License

MIT