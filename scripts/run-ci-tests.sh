#!/usr/bin/env sh

set -eu

pnpm_major="$(pnpm --version | cut -d. -f1)"
if [ "$pnpm_major" != "8" ]; then
  echo "run-ci-tests.sh requires pnpm 8.x for the checked-in lockfiles" >&2
  exit 1
fi

pnpm --dir test/rescript install --frozen-lockfile
pnpm --dir test/rewatch install --frozen-lockfile

pnpm --dir test/rescript run res:build
pnpm --dir test/rewatch run res:build

dune exec ./test/test_cmt_imports.exe
dune exec ./test/test_cmt_values.exe
dune exec ./test/test_ast.exe
dune exec ./test/test_rescriptdep.exe
dune exec ./test/test_namespace.exe
