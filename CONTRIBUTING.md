# Contributing to ReScriptDep

Thank you for your interest in contributing to ReScriptDep! This document provides guidelines and instructions for contributing to the project.

## Development Setup

```bash
# Build the project
dune build

# Build a specific executable
dune build bin/main.exe

# Build with static linking (Linux)
dune build --profile static
```

## Running Tests

You can run tests using the following methods:

```bash
# Run all tests
dune runtest

# Run specific tests:
dune exec test/test_cmt_values.exe   # Test value dependency information
dune exec test/test_cmt_imports.exe  # Test import information
dune exec test/test_rescriptdep.exe  # Test dependency analysis on sample projects
```

### Test Structure

The test code is located in the `test/` directory and includes:

- `test_cmt_imports.ml`: Tests the import information from cmt files
- `test_cmt_values.ml`: Tests the value dependency information from cmt files
- `test_rescriptdep.ml`: Tests the dependency analysis on sample ReScript projects
- `benchmark.ml`: Performance benchmark for the parser

### Test Fixtures

The test system uses two types of fixtures:

#### Dependency Analysis Fixtures

The dependency analysis tests use JSON fixtures to ensure consistent behavior:

- `test/fixtures/rescript.json`: Reference output for the rescript test project
- `test/fixtures/rewatch.json`: Reference output for the rewatch test project

These fixtures are automatically generated on first run and serve as reference points for future test runs. If the dependency analysis output differs from these fixtures, the test will fail and show a diff of the changes.

#### Test Projects

The test directory contains sample ReScript projects used for testing the dependency analyzer:

- `test/rescript/`: Sample ReScript project with standard ReScript setup
- `test/rewatch/`: Sample ReWatch project with similar structure

Each test project contains:
- `src/`: Source code files (.res, .resi)
- `lib/`: Compiled output including .cmt files used for analysis
- `package.json`, `rescript.json`: Project configuration

These projects are analyzed during tests to generate dependency information which is then compared against the reference fixtures.

### Managing Test Fixtures

When making changes that affect dependency analysis:

1. Review the test output diff carefully to ensure changes are expected
2. If the changes are intentional:
   - Delete the existing fixture files: `rm test/fixtures/*.json`
   - Run the tests again to generate new fixtures: `dune exec test/test_rescriptdep.exe`
   - Commit both the code changes and the updated fixtures

### Running the Benchmark

The benchmark requires a ReScript project with compiled .cmt files to analyze:

```bash
# Run the benchmark with a specific ReScript project
RESCRIPTDEP_BENCHMARK_PATH=/path/to/rescript/project/lib/bs/src dune exec test/benchmark.exe

# For detailed output
RESCRIPTDEP_BENCHMARK=1 RESCRIPTDEP_VERBOSE=1 RESCRIPTDEP_BENCHMARK_PATH=/path/to/project/lib/bs/src dune exec test/benchmark.exe
```

Note: The benchmark is skipped during normal test runs (`dune runtest`).

## Pull Request Guidelines

1. Create a branch for your changes
2. Ensure all tests pass: `dune runtest`
3. Add or update tests for new functionality
4. Update documentation if needed
5. Submit a pull request with a clear description of the changes

## Code Style

- Follow OCaml coding conventions
- Use meaningful variable names
- Add comments for complex logic
- Keep functions focused and modular

## License

By contributing to ReScriptDep, you agree that your contributions will be licensed under the MIT License. 