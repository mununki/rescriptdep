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
dune exec test/test_output_formats.exe  # Test DOT/JSON/-nd/-vb output against golden files
dune exec test/test_value_usage.exe  # Test value usage counts (-vb) on sample projects
dune exec test/test_scan_robustness.exe  # Test that an unreadable lib/bs entry is skipped on its own
dune exec test/test_namespace.exe    # Test namespace handling (self-contained)
dune exec test/test_compilers.exe    # Test compiler identification (self-contained)
dune exec test/test_formatter_escape.exe  # Test escaping in the DOT and JSON writers (self-contained)
```

### Test Structure

The test code is located in the `test/` directory and includes:

- `test_cmt_imports.ml`: Tests the import information from cmt files
- `test_cmt_values.ml`: Tests the value dependency information from cmt files
- `test_ast.ml`: Tests reading module references from compiler-generated .ast files
- `test_rescriptdep.ml`: Tests the dependency analysis (JSON output) on sample ReScript projects
- `test_output_formats.ml`: Compares the CLI's DOT graph, focused graph (`-m`), modules without
  dependents (`-nd`) and value usage (`-vb`) output with golden files, and checks that `-vb`
  reports `-6` for a compiler version it does not support
- `test_value_usage.ml`: Tests value usage counts on ReScript 11, 12 and 13 projects
- `test_scan_robustness.ml`: Tests that a dangling symlink in `lib/bs` skips only that entry
- `test_namespace.ml`: Tests namespace handling without compiled fixtures
- `test_compilers.ml`: Tests compiler identification and typed tree decoder guards without compiled fixtures
- `test_formatter_escape.ml`: Tests that paths with `"` and `\` give valid DOT and JSON, without compiled fixtures
- `test_support.ml`: Shared helpers (fixture lookup, path normalization, fixture comparison)
- `benchmark.ml`: Performance benchmark for the parser

### Test Fixtures

The test system uses two types of fixtures:

#### Dependency Analysis Fixtures

The dependency analysis tests use JSON fixtures to ensure consistent behavior:

- `test/fixtures/rescript.json`: Reference output for the ReScript 11 test project
- `test/fixtures/rescript12_basic.json`: Reference output for the ReScript 12 test project
- `test/fixtures/rescript12_namespaced.json`: Reference output for the namespaced ReScript 12 test project
- `test/fixtures/rescript13_basic.json`, `test/fixtures/rescript13_namespaced.json`: The same for ReScript 13
- `test/fixtures/<project>.dot`, `<project>_focus.dot`, `<project>_focus.json`, `<project>_nd.dot`,
  `<project>_nd.json`, `<project>_vb.json`: Golden output of `test_output_formats.ml` for the same
  five projects

These checked-in fixtures serve as reference points for future test runs. Missing references fail the tests; create or update them only with `RESCRIPTDEP_UPDATE_FIXTURES=1` as described below. Absolute paths are compared as repository-relative `test/...` paths. If the output differs from these fixtures, the test will fail and show a diff of the changes.

#### Test Projects

The test directory contains sample ReScript projects used for testing the dependency analyzer:

- `test/rescript/`: Sample ReScript 11 project with standard ReScript setup. Its `users.res`
  shares its name with an entry the standard library filter used to have, and checks that a
  module with its own `.cmt` file is never filtered out.
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

1. Run the tests and review output differences. Missing reference files fail the tests.
2. If the changes are intentional, explicitly regenerate the references:
   `RESCRIPTDEP_UPDATE_FIXTURES=1 dune exec test/test_rescriptdep.exe`
   (or `RESCRIPTDEP_UPDATE_FIXTURES=1 dune exec test/test_output_formats.exe`
   for the golden files). This creates missing files and overwrites existing references.
3. Review the resulting fixture diff, rerun tests without the environment variable,
   and commit both the code changes and every new or updated fixture.

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
   fixtures and golden files, and the project to `test/test_rescriptdep.ml`,
   `test/test_output_formats.ml`, `test/test_value_usage.ml` and the `Makefile`.

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

## Releasing

The CLI and VS Code extension have separate versions and publishing steps. A CLI
release is complete when its `ocaml/opam-repository` PR has merged and the version
is available through the opam index. An extension release is complete when the
new version is published to both VS Code Marketplace and Open VSX.

### CLI: publish to opam

Use **`opam publish`** to generate the repository metadata, checksums, and PR.
Keep subsequent updates on the PR managed by that command. Do not construct an
opam-repository PR manually.

Prerequisites: the project's OCaml dependencies, pnpm 10.x for fixture tests,
GitHub release access, and `opam-publish` (`opam install opam-publish`). The 0.3.0
submission used opam-publish 3.0.1. Let the tool use its configured GitHub
authentication or authentication prompt; keep credentials out of commands and
commits.

1. Update the package version in `dune-project` and the release entry in
   `CHANGELOG.md`. Run `dune build` to regenerate `rescriptdep.opam`; edit its
   source stanza in `dune-project`, rather than the generated file.
2. Run `make test`, `dune runtest`, and `opam lint rescriptdep.opam`. Commit the
   version, generated metadata, changelog, and any intended fixture changes.
   Publish from the reviewed commit on `main`, after CI passes.
3. Create and push the CLI tag from that commit. The versions below illustrate
   the 0.3.0 release; choose a new, unreleased version for future releases.

   ```bash
   CLI_VERSION=0.3.0
   git tag -a "cli-v${CLI_VERSION}" -m "Release CLI ${CLI_VERSION}"
   git push origin "cli-v${CLI_VERSION}"
   ```

4. Create a static **source** archive from the tag. The archive must contain
   `dune-project`, `rescriptdep.opam`, and the sources. Binary archives and VSIX
   packages are separate downloads.

   ```bash
   RELEASE_DIR=$(mktemp -d)
   SOURCE_ARCHIVE="${RELEASE_DIR}/rescriptdep-${CLI_VERSION}.tar.gz"
   git archive --format=tar \
     --prefix="rescriptdep-${CLI_VERSION}/" \
     --output="${RELEASE_DIR}/rescriptdep-${CLI_VERSION}.tar" \
     "cli-v${CLI_VERSION}"
   gzip -n "${RELEASE_DIR}/rescriptdep-${CLI_VERSION}.tar"
   ```

   Build and test the unpacked archive in a clean directory, using the same
   targets as the opam package. Install documentation dependencies first if
   needed (`opam install . --deps-only --with-test --with-doc`).

   ```bash
   tar -xzf "$SOURCE_ARCHIVE" -C "$RELEASE_DIR"
   (
     cd "${RELEASE_DIR}/rescriptdep-${CLI_VERSION}"
     opam exec -- dune build -p rescriptdep @install @runtest @doc
     opam exec -- dune install --prefix="${RELEASE_DIR}/install" rescriptdep
   )
   "${RELEASE_DIR}/install/bin/rescriptdep" --help
   ```

5. Create a **public GitHub release** for `cli-v${CLI_VERSION}` with the reviewed
   CLI release notes, then attach the source archive:

   ```bash
   gh release upload "cli-v${CLI_VERSION}" "$SOURCE_ARCHIVE"
   SOURCE_URL="https://github.com/mununki/rescriptdep/releases/download/cli-v${CLI_VERSION}/rescriptdep-${CLI_VERSION}.tar.gz"
   ```

   Verify the public URL downloads the same bytes. Keep published source
   archives and tags immutable; source fixes require a new version. The Build
   CLI workflow uploads binary artifacts to Actions, so creating the public
   release and source asset is a separate step. If attaching binary downloads,
   verify their architecture with `file`: the current workflow's macOS artifact
   names contain the legacy `darwin-x64` label even when the binary is arm64.

6. Review the patch generated by the publishing tool, then submit it:

   ```bash
   opam publish --dry-run --no-browser -v "$CLI_VERSION" "$SOURCE_URL"
   opam publish --no-browser -v "$CLI_VERSION" "$SOURCE_URL"
   ```

   Check the version, source URL, dependencies, and generated patch at the
   prompts. Specify the version and CLI source URL explicitly: this repository
   also has extension releases, and the latest GitHub release may be an
   extension release. Save the returned PR URL. Rerun the command to update its
   existing open PR instead of opening duplicate submissions. Follow the
   [opam-repository contribution guide](https://github.com/ocaml/opam-repository/blob/master/CONTRIBUTING.md)
   when addressing CI or maintainer feedback.

7. Follow CI and maintainer review through **merge**, then verify index
   propagation on [opam.ocaml.org](https://opam.ocaml.org/packages/rescriptdep/).
   In an environment without a local pin for `rescriptdep`, verify:

   ```bash
   opam update
   opam show rescriptdep --field=all-versions
   opam install "rescriptdep.${CLI_VERSION}"
   ```

   Confirm the intended version is listed and installable. Report the current
   phase explicitly: GitHub release published, opam PR submitted, CI passed,
   PR merged, or opam index updated. Report CLI deployment as complete only
   after the version is available through opam.

### VS Code extension: publish Bibimbob

The extension bundles the CLI built from its tagged source commit. Publishing
the extension does not submit that CLI version to opam; complete the CLI
procedure separately when releasing CLI changes.

1. Update `vscode-rescriptdep/package.json` and its README release notes. Refresh
   screenshots when the UI changes; label illustrative mockups accurately.
2. Validate the extension from the repository root:

   ```bash
   pnpm --dir vscode-rescriptdep install --frozen-lockfile
   pnpm --dir vscode-rescriptdep run test:unit
   pnpm --dir vscode-rescriptdep run package
   ```

   `package` runs type checking, lint, and the production build. Smoke-test
   graph navigation, native context menus, search, pan/zoom, narrow panels,
   themes, and source opening in the VS Code development host.
3. Commit the version and release notes, then create the tag from the reviewed
   commit on `main`. Replace the example version for future releases:

   ```bash
   EXTENSION_VERSION=0.10.0
   git tag -a "extension-v${EXTENSION_VERSION}" \
     -m "Release Bibimbob ${EXTENSION_VERSION}"
   git push origin "extension-v${EXTENSION_VERSION}"
   ```

4. Follow the **VSCode Extension** Actions workflow. It builds the CLI binaries,
   checks and packages the extension, then publishes the VSIX to Marketplace
   and Open VSX. The repository needs `VSCE_PAT` and `OVSX_PAT` secrets for the
   publishing accounts; the Open VSX account must have completed its publisher
   agreement. Keep secret values out of source and logs.
5. Verify **both publishing steps** and the version shown by each registry.
   Registry listings may take time to refresh. If only one registry succeeds,
   report partial publication and retry the failed destination without
   republishing the successful version.
6. Download and inspect the VSIX artifact: check the manifest version, bundled
   CLI binaries, webview JS/CSS, and `ThirdPartyNotices.txt`. Attach it and the
   release notes to a public GitHub release for `extension-v${EXTENSION_VERSION}`.
   Confirm the release is public (`isDraft: false`) and the asset is downloadable.
   Actions artifacts alone expire after the workflow's retention period.

Announce a release after its intended distribution channels are verified. Use
the published version, public download links, current screenshots, and accurate
platform requirements in the announcement.

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
