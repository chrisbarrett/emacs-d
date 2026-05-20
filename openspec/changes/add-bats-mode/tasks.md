## 1. Scaffold

- [ ] 1.1 Create `lisp/bats-mode/` directory.
- [ ] 1.2 Add empty `lisp/bats-mode/bats-mode.el` with package header
      (`;;; bats-mode.el --- Major mode for Bats test files -*- lexical-binding: t; -*-`),
      Commentary, and `provide` footer.
- [ ] 1.3 Add empty `lisp/bats-mode/bats-mode-tests.el` with header and
      `(require 'ert)` / `(require 'bats-mode)`.
- [ ] 1.4 Confirm `lisp/bats-mode/` is picked up by the autoloads scan
      (mirror `lisp/gfm/` precedent); add a `(require 'bats-mode)` from
      `modules/lang-shscript/init.el` only if needed.

## 2. Activation (red → green)

- [ ] 2.1 Write failing `bats-mode/activates-on-bats-extension` test in
      `bats-mode-tests.el`.
- [ ] 2.2 Write failing `bats-mode/activates-on-bats-interpreter` test.
- [ ] 2.3 In `bats-mode.el`, `define-derived-mode bats-mode bash-ts-mode`
      with `;;;###autoload`.
- [ ] 2.4 Add `;;;###autoload` `add-to-list 'auto-mode-alist`
      entry for `\\.bats\\'`.
- [ ] 2.5 Add `;;;###autoload` `add-to-list 'interpreter-mode-alist`
      entry for `"bats"`.
- [ ] 2.6 Verify both activation tests pass via `make test-quick` filtered
      to the bats-mode suite.

## 3. Directive font-lock (red → green)

- [ ] 3.1 Write failing `bats-mode/fontifies-test-directive` test.
- [ ] 3.2 Write failing tests for `setup`, `teardown`, `setup_file`,
      `teardown_file`, `setup_suite`, `teardown_suite`, `load`,
      `bats_load_library`, `run`, `skip`, `bats::on_failure`.
- [ ] 3.3 Define `bats-directive-face` (use `defface`; inherit from
      `font-lock-keyword-face` by default).
- [ ] 3.4 Build the directive keyword list with `rx` and install via
      `font-lock-add-keywords 'bats-mode …` at top level (not inside
      mode hook).
- [ ] 3.5 Verify directive tests pass.

## 4. Profile detection + assertion font-lock (red → green)

- [ ] 4.1 Write failing test: buffer with `bats_load_library 'bats-assert'`
      → `assert_equal` carries `bats-assertion-face`.
- [ ] 4.2 Write failing test: buffer with `load 'test_helper'` + sibling
      `test_helper.bash` containing `bats_load_library 'bats-assert'`
      → `assert_equal` carries `bats-assertion-face`. Use a temp dir.
- [ ] 4.3 Write failing test: buffer with no load lines → `assert_equal`
      does not carry `bats-assertion-face`.
- [ ] 4.4 Write failing test: profile cache refreshes after save.
- [ ] 4.5 Define `defcustom bats-mode-profile-keywords` alist with
      `:core`, `:bats-assert`, `:bats-support`, `:bats-file` entries.
      Populate the assertion-name lists per design.md.
- [ ] 4.6 Define `bats-assertion-face` (inherit from
      `font-lock-function-name-face` by default).
- [ ] 4.7 Implement `bats-mode--detect-profiles` (buffer-local cache,
      one level of `load`/`bats_load_library` follow-through).
- [ ] 4.8 Install per-buffer font-lock keywords for active profiles
      using `font-lock-add-keywords nil …` from `bats-mode` body, after
      profile detection.
- [ ] 4.9 Add `after-save-hook` (buffer-local) to invalidate the cache
      and refontify.
- [ ] 4.10 Verify all profile tests pass.

## 5. `$BATS_*` variable font-lock (red → green)

- [ ] 5.1 Write failing test: documented bats var carries
      `bats-variable-face`.
- [ ] 5.2 Write failing test: user-coined `BATS_*` does not match.
- [ ] 5.3 Define `bats-variable-face` (inherit from
      `font-lock-variable-name-face` by default).
- [ ] 5.4 Build exact-match `rx` regex from the documented variable list
      (anchored to `$` and word boundary) and add via
      `font-lock-add-keywords 'bats-mode …`.
- [ ] 5.5 Verify variable tests pass.

## 6. Imenu (red → green)

- [ ] 6.1 Write failing test: `@test` entries appear under `Tests`.
- [ ] 6.2 Write failing test: fixture functions appear under `Fixtures`.
- [ ] 6.3 Write failing test: plain bash helper appears under
      `Functions` (inherited from bash-ts-mode).
- [ ] 6.4 Implement `bats-mode--imenu-create-index`:
      - regex scan for `^[[:space:]]*@test[[:space:]]+\"(.*?)\"`,
        collect into `Tests`;
      - regex scan for the six fixture function definitions, collect
        into `Fixtures`;
      - call `treesit-simple-imenu` (or whatever bash-ts-mode binds
        `imenu-create-index-function` to in its body) and graft under
        `Functions`.
- [ ] 6.5 Set `imenu-create-index-function` from `bats-mode` body.
- [ ] 6.6 Verify imenu tests pass.

## 7. Hygiene

- [ ] 7.1 Audit `bats-mode.el` for unused variables / forms; tidy
      `defvar` / `defcustom` docstrings.
- [ ] 7.2 Ensure all faces have `:group 'bats-mode` and that a
      `defgroup bats-mode` exists.
- [ ] 7.3 Add `;;;###autoload` cookies where activation depends on them.
- [ ] 7.4 Run `make test` and confirm green across the full suite.
- [ ] 7.5 Run `nix develop --command bash -c 'pre-commit run --all-files'`
      (or the project's prek invocation) and resolve any flagged issues.

## 8. Spec-conventions catch-up

- [ ] 8.1 During archive, the `## MODIFIED Requirements` block in
      `openspec/changes/add-bats-mode/specs/spec-conventions/spec.md`
      replaces the existing `### Requirement: One spec per axis; spec
      name matches lib or module name` body in
      `openspec/specs/spec-conventions/spec.md` so the recognised-axes
      list gains `bats-mode (lib)`. No additional manual edits needed.
