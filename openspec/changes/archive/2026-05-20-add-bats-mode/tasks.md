## 1. Scaffold

- [x] 1.1 Create `lisp/bats-mode/` directory.
- [x] 1.2 Add empty `lisp/bats-mode/bats-mode.el` with package header
      (`;;; bats-mode.el --- Major mode for Bats test files -*- lexical-binding: t; -*-`),
      Commentary, and `provide` footer.
- [x] 1.3 Add empty `lisp/bats-mode/bats-mode-tests.el` with header and
      `(require 'ert)` / `(require 'bats-mode)`.
- [x] 1.4 Confirm `lisp/bats-mode/` is picked up by the autoloads scan
      (mirror `lisp/gfm/` precedent); add a `(require 'bats-mode)` from
      `modules/lang-shscript/init.el` only if needed.

## 2. Activation (red → green)

- [x] 2.1 Write failing `bats-mode/activates-on-bats-extension` test in
      `bats-mode-tests.el`.
- [x] 2.2 Write failing `bats-mode/activates-on-bats-interpreter` test.
- [x] 2.3 In `bats-mode.el`, `define-derived-mode bats-mode bash-ts-mode`
      with `;;;###autoload`.
- [x] 2.4 Add `;;;###autoload` `add-to-list 'auto-mode-alist`
      entry for `\\.bats\\'`.
- [x] 2.5 Add `;;;###autoload` `add-to-list 'interpreter-mode-alist`
      entry for `"bats"`.
- [x] 2.6 Verify both activation tests pass via `make test-quick` filtered
      to the bats-mode suite.

## 3. Directive font-lock (red → green)

- [x] 3.1 Write failing `bats-mode/fontifies-test-directive` test.
- [x] 3.2 Write failing tests for `setup`, `teardown`, `setup_file`,
      `teardown_file`, `setup_suite`, `teardown_suite`, `load`,
      `bats_load_library`, `run`, `skip`, `bats::on_failure`.
- [x] 3.3 Define `bats-directive-face` (use `defface`; inherit from
      `font-lock-keyword-face` by default).
- [x] 3.4 Build the directive keyword list with `rx` and install via
      `font-lock-add-keywords 'bats-mode …` at top level (not inside
      mode hook).
- [x] 3.5 Verify directive tests pass.

## 4. Profile detection + assertion font-lock (red → green)

- [x] 4.1 Write failing test: buffer with `bats_load_library 'bats-assert'`
      → `assert_equal` carries `bats-assertion-face`.
- [x] 4.2 Write failing test: buffer with `load 'test_helper'` + sibling
      `test_helper.bash` containing `bats_load_library 'bats-assert'`
      → `assert_equal` carries `bats-assertion-face`. Use a temp dir.
- [x] 4.3 Write failing test: buffer with no load lines → `assert_equal`
      does not carry `bats-assertion-face`.
- [x] 4.4 Write failing test: profile cache refreshes after save.
- [x] 4.5 Define `defcustom bats-mode-profile-keywords` alist with
      `:core`, `:bats-assert`, `:bats-support`, `:bats-file` entries.
      Populate the assertion-name lists per design.md.
- [x] 4.6 Define `bats-assertion-face` (inherit from
      `font-lock-function-name-face` by default).
- [x] 4.7 Implement `bats-mode--detect-profiles` (buffer-local cache,
      one level of `load`/`bats_load_library` follow-through).
- [x] 4.8 Install per-buffer font-lock keywords for active profiles
      using `font-lock-add-keywords nil …` from `bats-mode` body, after
      profile detection.
- [x] 4.9 Add `after-save-hook` (buffer-local) to invalidate the cache
      and refontify.
- [x] 4.10 Verify all profile tests pass.

## 5. `$BATS_*` variable font-lock (red → green)

- [x] 5.1 Write failing test: documented bats var carries
      `bats-variable-face`.
- [x] 5.2 Write failing test: user-coined `BATS_*` does not match.
- [x] 5.3 Define `bats-variable-face` (inherit from
      `font-lock-variable-name-face` by default).
- [x] 5.4 Build exact-match `rx` regex from the documented variable list
      (anchored to `$` and word boundary) and add via
      `font-lock-add-keywords 'bats-mode …`.
- [x] 5.5 Verify variable tests pass.

## 6. Imenu (red → green)

- [x] 6.1 Write failing test: `@test` entries appear under `Tests`.
- [x] 6.2 Write failing test: fixture functions appear under `Fixtures`.
- [x] 6.3 Write failing test: plain bash helper appears under
      `Functions` (inherited from bash-ts-mode).
- [x] 6.4 Implement `bats-mode--imenu-create-index`:
      - regex scan for `^[[:space:]]*@test[[:space:]]+\"(.*?)\"`,
        collect into `Tests`;
      - regex scan for the six fixture function definitions, collect
        into `Fixtures`;
      - call `treesit-simple-imenu` (or whatever bash-ts-mode binds
        `imenu-create-index-function` to in its body) and graft under
        `Functions`.
- [x] 6.5 Set `imenu-create-index-function` from `bats-mode` body.
- [x] 6.6 Verify imenu tests pass.

## 7. Hygiene

- [x] 7.1 Audit `bats-mode.el` for unused variables / forms; tidy
      `defvar` / `defcustom` docstrings.
- [x] 7.2 Ensure all faces have `:group 'bats-mode` and that a
      `defgroup bats-mode` exists.
- [x] 7.3 Add `;;;###autoload` cookies where activation depends on them.
- [x] 7.4 Run `make test` and confirm green across the full suite.
- [x] 7.5 Run `nix develop --command bash -c 'pre-commit run --all-files'`
      (or the project's prek invocation) and resolve any flagged issues.

## 8. Spec-conventions catch-up

- [x] 8.1 During archive, the `## MODIFIED Requirements` block in
      `openspec/changes/add-bats-mode/specs/spec-conventions/spec.md`
      replaces the existing `### Requirement: One spec per axis; spec
      name matches lib or module name` body in
      `openspec/specs/spec-conventions/spec.md` so the recognised-axes
      list gains `bats-mode (lib)`. No additional manual edits needed.
