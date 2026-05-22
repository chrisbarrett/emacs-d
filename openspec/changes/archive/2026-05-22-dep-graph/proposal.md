## Why

`scripts/affected.sh` and `scripts/affected-tests.sh` compute the
transitive-dependent set of staged Elisp files by grepping the source
with regex. Two near-duplicate bash scripts, ~300 lines combined, that
parse Lisp with a regex parser:

- Fragile under macros, commented-out forms, and string literals (any
  `(require 'foo)` in a docstring matches).
- Quietly silent on `^L` form-feed pages and other Emacs syntactic
  conventions — the kind of surprise the recent migrate-prek-toml
  change had to clean up after.
- Locked to `git diff` for input — can't be called from interactive
  Emacs, can't be unit-tested without a real repo.

Moving the graph into Emacs (reader-based, no eval) gives us a parser
that respects Lisp syntax, a pure core that's unit-testable in
isolation, and a single source of truth that prek / Makefile / scripts
all consume through a small CLI.

This change is the keystone for the rest of the pipeline tightening
trajectory (flip local hooks to `pass_filenames=true`, collapse the
three gate scripts, slim the Makefile) — none of which can land
cleanly while the bash regex parser is alive.

## What Changes

- New library `lisp/dep-graph/dep-graph.el` with a pure core
  (form-list → graph, graph → dependents) and a thin IO shell
  (file → forms, root → graph) that uses the Emacs reader, not regex.
- New `scripts/dep-graph` CLI wrapper exposing `affected`,
  `affected-tests`, and `print` subcommands. Boots Emacs once per
  invocation; no caching (recompute each call — sub-second on this
  repo).
- Delete `scripts/affected.sh` and `scripts/affected-tests.sh`.
- Update callers (`scripts/byte-compile.sh`, `scripts/checkdoc.sh`,
  `scripts/run-tests.sh`, `Makefile`) to invoke the new CLI.
- Tests in `lisp/dep-graph/dep-graph-tests.el` cover the pure core
  exhaustively (direct/transitive dependents, cycles, multiple
  providers, `:after` handling, optional `require`s) plus a thin
  fixture-driven integration test for the IO shell.

## Capabilities

### New Capabilities

- `dep-graph`: library at `lisp/dep-graph/` that builds and queries an
  Elisp file-dependency graph from form-level scans of the source
  tree.

### Modified Capabilities

- `contributor-internals`: the pre-commit pipeline scope-detection now
  goes through `scripts/dep-graph` instead of `scripts/affected.sh`
  and `scripts/affected-tests.sh`. The `--affected` invocation contract
  of the gate scripts is unchanged — only the underlying implementation
  swaps.

## Impact

- `lisp/dep-graph/dep-graph.el`, `lisp/dep-graph/dep-graph-tests.el` — new.
- `scripts/dep-graph` — new CLI.
- `scripts/affected.sh`, `scripts/affected-tests.sh` — deleted.
- `scripts/byte-compile.sh`, `scripts/checkdoc.sh`,
  `scripts/run-tests.sh` — replace `./scripts/affected.sh` calls with
  `./scripts/dep-graph`.
- `Makefile` — `build-affected` / `test-affected` / `test-quick` /
  `lint-affected` targets shift to the new CLI.
- `openspec/specs/contributor-internals/spec.md` — the "Local hooks
  invoke `--affected`" requirement gets a clarifying note that scope
  expansion now lives in elisp; the requirement itself is unchanged.
- No change to `prek.toml` (hooks still invoke `<script> --affected`).
