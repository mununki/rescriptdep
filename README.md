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

# Run specific tests:
dune exec test/test_cmt_values.exe   # Directly execute the test
dune exec test/test_cmt_imports.exe  # Directly execute the test

# Run benchmark
dune build @bench                    # Build and run the benchmark
dune exec test/benchmark.exe         # Directly execute the benchmark
```

Test code is located in the `test/` directory and includes the following tests:

- `test_cmt_imports.ml`: Tests the import information from cmt files.
- `test_cmt_values.ml`: Tests the value dependency information from cmt files.
- `test_rescriptdep.ml`: Basic smoke test for the rescriptdep functionality.
- `benchmark.ml`: Performance benchmark for the parser.

#### Running the Benchmark

The benchmark is designed to be run separately from the regular tests and requires a ReScript project with compiled .cmt files to analyze:

```bash
# Run the benchmark with a specific ReScript project
RESCRIPTDEP_BENCHMARK_PATH=/path/to/rescript/project/lib/bs/src dune exec test/benchmark.exe

# Example with environment variables for detailed output
RESCRIPTDEP_BENCHMARK=1 RESCRIPTDEP_VERBOSE=1 RESCRIPTDEP_BENCHMARK_PATH=/path/to/project/lib/bs/src dune exec test/benchmark.exe
```

When running as part of `dune runtest`, the benchmark will be skipped automatically.

#### Test Fixtures

Fixture files used by tests are located in the `test/fixtures/` directory. These files include:

- `app.cmt`: Example application module.
- `math.cmt`: Example math utility module.
- `logger.cmt`: Example logging module.
- `utils.cmt`: Example utilities module.

Tests automatically locate these fixture files, so they should work correctly regardless of your current working directory.

## License

MIT 