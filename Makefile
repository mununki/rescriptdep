.PHONY: build test check-pnpm install-test-deps build-test-projects run-ocaml-tests

build:
	dune build

test: check-pnpm install-test-deps build-test-projects run-ocaml-tests

check-pnpm:
	@pnpm_major="$$(pnpm --version 2>/dev/null | cut -d. -f1 || true)"; \
	if [ "$$pnpm_major" != "8" ]; then \
		echo "make test requires pnpm 8.x for the checked-in lockfiles" >&2; \
		exit 1; \
	fi

install-test-deps:
	pnpm --dir test/rescript install --frozen-lockfile
	pnpm --dir test/rewatch install --frozen-lockfile

build-test-projects:
	pnpm --dir test/rescript run res:build
	pnpm --dir test/rewatch run res:build

run-ocaml-tests:
	dune exec ./test/test_cmt_imports.exe
	dune exec ./test/test_cmt_values.exe
	dune exec ./test/test_ast.exe
	dune exec ./test/test_rescriptdep.exe
	dune exec ./test/test_namespace.exe
	dune exec ./test/test_rewatch_paths.exe
