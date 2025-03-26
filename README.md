# ReScriptDep

A dependency analyzer for ReScript modules.

## Introduction

ReScriptDep is a tool for analyzing dependencies between ReScript modules. It uses cmt files to identify module dependencies and can visualize them or output them in JSON format.

## Installation

```bash
# Install via opam
opam install rescriptdep
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
# --cache-file <file>     : Specify cache file location
# --clear-cache           : Clear the cache before analyzing
```

## Development

### Building the Project

```bash
# Build the project
dune build

# Build a specific executable
dune build bin/main.exe
```

### Running Tests

You can run tests using the following methods:

```bash
# Run all tests
dune runtest

# Run tests in a specific directory
dune runtest test/

# Run specific tests
dune runtest test/test_cmt_imports.exe
dune runtest test/test_cmt_values.exe

# Run tests directly
dune exec test/test_cmt_imports.exe
```

Test code is located in the `test/` directory and includes the following tests:

- `test_cmt_imports.ml`: Tests the import information from cmt files.
- `test_cmt_values.ml`: Tests the value dependency information from cmt files.

When modifying test code, check the configuration in the `test/dune` file. Fixture files used by tests are located in the `test/fixtures/` directory.

## License

MIT 