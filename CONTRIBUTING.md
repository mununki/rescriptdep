# Contributing to ReScriptDep

Thank you for your interest in contributing to ReScriptDep! This document provides guidelines and instructions for contributing to the project.

## Development Setup

```bash
# Build the project
make build

# Build a specific executable
dune build bin/main.exe

# Build with static linking (Linux)
dune build --profile static
```

## Running Tests

You can run tests using the following methods:

```bash
# Run the CI-equivalent test suite (requires pnpm 10.x)
make test

# Run specific tests:
dune exec test/test_cmt_values.exe   # Test value dependency information
dune exec test/test_cmt_imports.exe  # Test import information
dune exec test/test_ast.exe          # Test reading module references from .ast files
dune exec test/test_rescriptdep.exe  # Test dependency analysis on sample projects
dune exec test/test_value_usage.exe  # Test value usage counts (-vb) on sample projects
dune exec test/test_namespace.exe    # Test namespace handling (self-contained)
```

### Test Structure

The test code is located in the `test/` directory and includes:

- `test_cmt_imports.ml`: Tests the import information from cmt files
- `test_cmt_values.ml`: Tests the value dependency information from cmt files
- `test_ast.ml`: Tests reading module references from compiler-generated .ast files
- `test_rescriptdep.ml`: Tests the dependency analysis on sample ReScript projects
- `test_value_usage.ml`: Tests value usage counts on ReScript 11 and 12 projects
- `test_namespace.ml`: Tests namespace handling without compiled fixtures
- `test_compilers.ml`: Tests compiler identification and typed tree decoder guards without compiled fixtures
- `benchmark.ml`: Performance benchmark for the parser

### Test Fixtures

The test system uses two types of fixtures:

#### Dependency Analysis Fixtures

The dependency analysis tests use JSON fixtures to ensure consistent behavior:

- `test/fixtures/rescript.json`: Reference output for the ReScript 11 test project
- `test/fixtures/rescript12_basic.json`: Reference output for the ReScript 12 test project
- `test/fixtures/rescript12_namespaced.json`: Reference output for the namespaced ReScript 12 test project
- `test/fixtures/rescript13_basic.json`, `test/fixtures/rescript13_namespaced.json`: The same for ReScript 13

These fixtures are automatically generated on first run and serve as reference points for future test runs. If the dependency analysis output differs from these fixtures, the test will fail and show a diff of the changes.

#### Test Projects

The test directory contains sample ReScript projects used for testing the dependency analyzer:

- `test/rescript/`: Sample ReScript 11 project with standard ReScript setup
- `test/rescript12/`: pnpm workspace with ReScript 12 projects built from the same sources:
  `basic/` (standard setup) and `namespaced/` (`"namespace": true`). `math.res` is
  named `mathUtil.res` there because ReScript 12 resolves `Math` to `Stdlib.Math`.
- `test/rescript13/`: The same workspace for ReScript 13, with `Belt` calls replaced by
  `Stdlib` ones since ReScript 13 no longer ships `Belt`, plus `loops.res`, which uses syntax
  only ReScript 13 has (`for...of`, `continue`, template literals, object literals).

The packages of a workspace share their compiled dependencies, so `res:build` builds them
one at a time (`--workspace-concurrency=1`); parallel builds corrupt the shared `.cmi` files.

Each test project contains:
- `src/`: Source code files (.res, .resi)
- `lib/`: Compiled output including .cmt files used for analysis
- `package.json`, `rescript.json`: Project configuration

These projects are analyzed during tests to generate dependency information which is then compared against the reference fixtures.

### Managing Test Fixtures

When making changes that affect dependency analysis:

1. Review the test output diff carefully to ensure changes are expected
2. If the changes are intentional:
   - Delete the existing fixture file, e.g. `rm test/fixtures/rescript.json`
   - Run the tests again to generate new fixtures: `dune exec test/test_rescriptdep.exe`
   - Commit both the code changes and the updated fixtures

### Supporting a New ReScript Release

`.cmt` files are marshalled OCaml values without type information, so they must be read with
type definitions whose constructor order and arity match the compiler exactly. Only the typed
tree used by value usage analysis (`-vb`) differs between the supported releases; the fields of
`cmt_infos` used by the dependency graph and the `.ast` header format are shared.

`lib/compilers.ml` picks the decoder from the CMT magic number (in `compiler/ext/config.ml` of
the ReScript repository), the number of fields of the `cmt_infos` record and, where several
releases write the same CMT format, the version in `lib/bs/compiler-info.json`. For example,
ReScript 12 and 13.0.0-alpha.1 to alpha.5 all write `Caml1999T022` with 14 fields, but alpha.4
changed the typed tree in place.

To add a release:

1. Diff its `compiler/ml/typedtree.mli` against the release a decoder already reads. If only
   constructors were appended, or constant constructors added, extend that decoder's vendored
   typedtree and list the version in its entry in `lib/compilers.ml`.
2. Otherwise vendor the parts of its typedtree that are decoded as `vendor/typedtree_vNN.ml`,
   keeping every constructor in order, add a `lib/compiler_vNN.ml` decoder with its constructor
   counts, and add an entry to `all` in `lib/compilers.ml`. `test/test_compilers.ml` checks that
   the counts match the vendored definitions.
3. Add a fixture workspace (`test/rescriptNN/`) or update the existing one, its reference JSON
   fixtures, and the project to `test/test_rescriptdep.ml`, `test/test_value_usage.ml` and the
   `Makefile`.

### Running the Benchmark

The benchmark requires a ReScript project with compiled .cmt files to analyze:

```bash
# Run the benchmark with a specific ReScript project
RESCRIPTDEP_BENCHMARK_PATH=/path/to/rescript/project/lib/bs/src dune exec test/benchmark.exe

# For detailed output
RESCRIPTDEP_BENCHMARK=1 RESCRIPTDEP_VERBOSE=1 RESCRIPTDEP_BENCHMARK_PATH=/path/to/project/lib/bs/src dune exec test/benchmark.exe
```

Note: The benchmark is skipped during normal CI-style test runs
(`make test`).

## Pull Request Guidelines

1. Create a branch for your changes
2. Ensure all tests pass: `make test`
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
