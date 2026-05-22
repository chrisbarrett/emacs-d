## 1. Library skeleton + TDD harness

- [x] 1.1 Create `lisp/dep-graph/dep-graph.el` with the package
      preamble, `(provide 'dep-graph)`, and `;;; Code:` scaffold.
- [x] 1.2 Create `lisp/dep-graph/dep-graph-tests.el` with the
      `require 'dep-graph` and `require 'ert` boilerplate plus an
      always-passing sanity test so the runner picks the file up.
- [x] 1.3 Run `./scripts/run-tests.sh lisp/dep-graph/dep-graph-tests.el`
      to confirm the harness wires up.

## 2. Pure core — `dep-graph-build`

- [x] 2.1 RED: test "empty input yields empty graph".
- [x] 2.2 GREEN: implement minimal `dep-graph-build`.
- [x] 2.3 RED: test "direct dependent maps feature → file list".
- [x] 2.4 GREEN: extend implementation; refactor.
- [x] 2.5 RED: test "duplicate provider raises user-error mentioning
      both paths".
- [x] 2.6 GREEN: add the duplicate-provider check; refactor.

## 3. Pure core — `dep-graph-dependents`

- [x] 3.1 RED: test "single file with no dependents returns just
      itself".
- [x] 3.2 GREEN: implement.
- [x] 3.3 RED: test "two-hop transitive chain returns all three files".
- [x] 3.4 GREEN.
- [x] 3.5 RED: test "cycle terminates and includes both files".
- [x] 3.6 GREEN: add visited-set traversal.

## 4. Pure core — `dep-graph-affected`

- [x] 4.1 RED: test "union of dependents across multiple starting
      files".
- [x] 4.2 GREEN.
- [x] 4.3 RED: test "filter predicate selects matching paths only".
- [x] 4.4 GREEN.

## 5. IO shell — `dep-graph-scan-file`

- [x] 5.1 RED: test using a temp file with one `provide` + one
      `require` returns the expected `:provides` / `:requires`
      summary.
- [x] 5.2 GREEN.
- [x] 5.3 RED: test "docstring `require` is not extracted" (defun with
      `(require 'foo)` inside docstring; expect empty `:requires`).
- [x] 5.4 GREEN.
- [x] 5.5 RED: test "use-package :after (a b) lists both as
      dependencies".
- [x] 5.6 GREEN.
- [x] 5.7 RED: test "optional `(require 'foo nil t)` is still a
      dependency".
- [x] 5.8 GREEN.
- [x] 5.9 RED: test "`(load \"path/to/foo\")` records `foo` as
      dependency".
- [x] 5.10 GREEN.

## 6. IO shell — `dep-graph-scan-root`

- [x] 6.1 RED: integration test using a temp directory with
      `lisp/foo.el`, `lib/bar/baz.el`, `elpaca/builds/excluded.el`.
      Assert excluded path absent.
- [x] 6.2 GREEN.

## 7. CLI — `scripts/dep-graph`

- [x] 7.1 Write `scripts/dep-graph` shell wrapper (executable shebang,
      `nix develop --command` not needed at this layer — scripts run
      from inside the gate hooks which already wrap).
- [x] 7.2 Implement `dep-graph-main` in the library: parse subcommand
      + args, dispatch to `affected` / `affected-tests` / `print`.
- [x] 7.3 RED: ert test invoking `dep-graph-main` with synthetic
      `command-line-args-left` for `affected` against a fixture root.
- [x] 7.4 GREEN.
- [x] 7.5 Add `--from-git` flag: read staged + unstaged elisp paths
      via `git diff` for callers that don't pass explicit files. Match
      the file-set computation from the deleted `affected.sh`.
- [x] 7.6 Ensure sentinels `all` (no input changed files) and `none`
      (input present, no affected output) match the bash contract.

## 8. Parity sweep

- [x] 8.1 Run `./scripts/affected.sh` and
      `./scripts/dep-graph affected --from-git` with the same staged
      input; diff sorted outputs. Repeat for `affected-tests`.
- [x] 8.2 Run both on the full repo (no staged changes) — expect
      both to emit `all`.
- [x] 8.3 Stage a representative cross-section of changes (a leaf
      file, a mid-tree file, a test-only file, a load-only consumer)
      and diff. Document any intentional divergence (e.g.,
      `:after (a b)` correctness fix).
      **Result**: identical for top-level `lisp/*.el` and `lib/*/*.el`
      paths. New CLI additionally surfaces files under
      `lisp/SUBDIR/*.el` (e.g. `lisp/dune-mode/dune-mode.el` and its
      sibling test) that the bash version dropped silently because
      its `feature_to_file` / `find_test_file` lookups never searched
      `lisp/*/`. This is the intended correctness fix flagged in
      design.md ("Hidden bash bugs become spec changes").

## 9. Switch callers

- [x] 9.1 `scripts/byte-compile.sh`: replace `./scripts/affected.sh`
      invocation with `./scripts/dep-graph affected --from-git`.
- [x] 9.2 `scripts/checkdoc.sh`: same.
- [x] 9.3 `scripts/run-tests.sh`: replace
      `./scripts/affected-tests.sh` with
      `./scripts/dep-graph affected-tests --from-git`.
- [x] 9.4 `Makefile`: update `build-affected`, `test-affected`,
      `test-quick`, and `lint-affected` targets to call the new CLI.
      (`lint-affected` derives its own file list from `git diff`
      with the same regex the old `affected.sh` used; nothing to
      change there since the target never went through
      `./scripts/affected.sh`.)

## 10. Delete legacy

- [x] 10.1 `git rm scripts/affected.sh`.
- [x] 10.2 `git rm scripts/affected-tests.sh`.

## 11. Verification

- [x] 11.1 `make test` — passes.
- [x] 11.2 `make pre-commit` — passes.
- [x] 11.3 `make test-quick` — passes (touch a file, confirm only
      its dependents' tests run).
- [x] 11.4 `make lint-affected` — passes.
- [x] 11.5 `nix develop --command prek run --all-files` — every hook
      green.
- [x] 11.6 `nix develop --command prek run byte-compile` after
      staging an isolated change — affected set matches expectation.

## 12. Archive

- [x] 12.1 Sync delta specs:
      `openspec/specs/dep-graph/spec.md` created;
      `openspec/specs/contributor-internals/spec.md` updated per
      MODIFIED block.
- [x] 12.2 Archive via `/opsx:archive`.
