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
# -o, --output <file>         : Output file (default: stdout)
# -f, --format <format>       : Output format (dot, json)
# -m, --module <name>         : Focus on specific module and its dependencies
# -v, --verbose               : Enable verbose output
# -b, --benchmark             : Enable performance benchmarking
# --no-cache                  : Skip using cache
# --clear-cache               : Clear the cache before analyzing
# -nd, --no-dependents        : Find modules that have no dependents (not imported by any other modules)
# -vb, --value-binding <name> : Count usage of a value binding (by name) across the project (in the module itself and all dependents; requires -m)
# -vl, --value-line <line>    : Line number of the value binding (1-based, for disambiguation when multiple bindings have the same name; use with -vb)
```

`files_or_dirs` are compiled `.cmt` files or directories that contain them, usually the project's `lib/bs`.

With `-vb`, each module reports its usage count, or a negative code when it could not be analyzed:
`-2` no implementation, `-3` unreadable `.cmt` file, `-4` no `.cmt` file, `-5` no file path,
`-6` the compiler that built the module is not supported by value usage analysis.

## Supported ReScript Versions

| ReScript | Dependency graph | Value usage (`-vb`) |
|---|---|---|
| 11.0.0 - 11.1.4 | Yes | Yes |
| 12.0.0 - 12.3.1 | Yes | Yes |
| 13.0.0-alpha.1 - alpha.3, alpha.5, alpha.6 | Yes | Yes |
| 13.0.0-alpha.4 | Yes | No (`-6`) |

rescriptdep identifies the compiler from each `.cmt` file and, for ReScript 12 and later, from the
version rewatch records in `lib/bs/compiler-info.json`. Files in formats no supported compiler
writes are rejected with an error instead of being misread. Other versions of a supported format
(for example a later 13.0 prerelease) get the dependency graph, and `-6` from `-vb` until their
typed tree has been checked.

## Development

For detailed information about building the project, running tests, and contributing, please see [CONTRIBUTING.md](CONTRIBUTING.md).

### Quick Start for Development

```bash
# Build the project
make build

# Run the CI-equivalent test suite (requires pnpm 10.x)
make test
```

## License

MIT
