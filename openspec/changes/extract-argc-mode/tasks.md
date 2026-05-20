## 1. Scaffold

- [ ] 1.1 Create `lisp/argc-mode/` directory.
- [ ] 1.2 Create empty `lisp/argc-mode/argc-mode.el` with package
      header
      (`;;; argc-mode.el --- Fontify argc CLI directives in shell scripts -*- lexical-binding: t; -*-`),
      Commentary block paraphrasing the existing `+argc.el` commentary,
      `(provide 'argc-mode)`, and footer.
- [ ] 1.3 Create empty `lisp/argc-mode/argc-mode-tests.el` with header
      and `(require 'ert)` / `(require 'argc-mode)`.
- [ ] 1.4 Confirm `lisp/argc-mode/` is picked up by the autoloads
      scan (mirror `lisp/bats-mode/` precedent).

## 2. Relocate library (red → green)

- [ ] 2.1 Write failing test
      `argc-mode/autoloaded-without-require` in
      `argc-mode-tests.el`: assert `(fboundp 'argc-mode)` is non-nil
      after only loading `lisp/+autoloads.el` (no explicit
      `require`).
- [ ] 2.2 Write failing test
      `argc-mode/feature-provided-after-require` asserting
      `(require 'argc-mode)` returns non-nil and
      `(featurep 'argc-mode)` is non-nil.
- [ ] 2.3 Copy the entire body of
      `modules/lang-shscript/lib/+argc.el` into
      `lisp/argc-mode/argc-mode.el` (faces, defconsts, defvars, all
      `argc--*` and `argc-` defuns, the `;;;###autoload`
      `define-minor-mode argc-mode`).
- [ ] 2.4 Replace `(provide '+argc)` with `(provide 'argc-mode)`.
- [ ] 2.5 Delete `modules/lang-shscript/lib/+argc.el`.
- [ ] 2.6 Regenerate `lisp/+autoloads.el` (run the harvester). Verify
      line 275 now references `lisp/argc-mode/argc-mode.el` instead
      of `modules/lang-shscript/lib/+argc.el`. If the harvester is
      not part of `make test`, update the entry by hand.
- [ ] 2.7 Verify the two relocation tests pass via `make test-quick`
      filtered to the argc-mode suite.

## 3. Migrate library tests

- [ ] 3.1 Copy `argc-test-has-face-p` helper from
      `modules/lang-shscript/tests.el:81-87` into
      `lisp/argc-mode/argc-mode-tests.el`.
- [ ] 3.2 Move each library-level test from
      `modules/lang-shscript/tests.el` into
      `lisp/argc-mode/argc-mode-tests.el`:
      `argc-test-minor-mode-exists`,
      `argc-test-creates-overlays-on-enable`,
      `argc-test-removes-overlays-on-disable`,
      `argc-test-fontify-cmd`,
      `argc-test-fontify-describe`,
      `argc-test-fontify-arg-name`,
      `argc-test-fontify-arg-modifier`,
      `argc-test-fontify-option-flags`,
      `argc-test-fontify-flag`,
      `argc-test-fontify-env-name`,
      `argc-test-fontify-meta-key`,
      `argc-test-fontify-alias`,
      `argc-test-fontify-notation`,
      `argc-test-fontify-choices`,
      `argc-test-no-false-positive`,
      `argc-test-fontify-option-long-only`,
      `argc-test-all-directives`,
      `argc-test-fontify-arg-modifiers`,
      `argc-test-fontify-cmd-description`,
      `argc-test-fontify-arg-description`,
      `argc-test-fontify-option-description`,
      `argc-test-find-blocks-single`,
      `argc-test-find-blocks-contiguous`,
      `argc-test-find-blocks-separated`,
      `argc-test-function-after-basic`,
      `argc-test-function-after-keyword`,
      `argc-test-function-after-none`,
      `argc-test-function-after-hyphenated`,
      `argc-test-overlays-created`,
      `argc-test-overlays-removed`,
      `argc-test-box-header-has-func-name`,
      `argc-test-box-min-width-80`,
      `argc-test-box-expands-for-long-lines`,
      `argc-test-indirect-buffer-skip`,
      `argc-test-spell-fu-advice-skips-directives`,
      `argc-test-spell-fu-advice-allows-non-directives`,
      `argc-test-mode-enable-idempotent`,
      `argc-test-bottom-border-on-last-line`,
      `argc-test-no-box-on-regular-comments`,
      `argc-test-find-blocks-comment-continuation`,
      `argc-test-no-zero-width-box-overlay`.
- [ ] 3.3 Leave the `+argc-maybe-enable` tests in
      `modules/lang-shscript/tests.el`:
      `argc-test-maybe-enable-no-double`,
      `argc-test-maybe-enable-no-directives`,
      `argc-test-maybe-enable-beyond-50-lines`,
      `argc-test-maybe-enable-within-50-lines`,
      `argc-test-maybe-enable-skip-indirect`.
- [ ] 3.4 In `modules/lang-shscript/tests.el`, delete the
      `(load (expand-file-name "lib/+argc.el" …))` line at line 79
      and replace with `(require 'argc-mode)` so the kept
      `+argc-maybe-enable` tests can still call `argc-mode`.
- [ ] 3.5 Run `make test` and confirm the full suite is green.

## 4. Smoke check

- [ ] 4.1 Open a real `.sh` file with argc directives, confirm the
      minor mode auto-enables via `+argc-maybe-enable`, faces render,
      and box overlays draw.
- [ ] 4.2 Toggle the mode off, confirm overlays vanish.

## 5. spec-conventions catch-up

- [ ] 5.1 During archive, the `## MODIFIED Requirements` block in
      `openspec/changes/extract-argc-mode/specs/spec-conventions/spec.md`
      replaces the existing `### Requirement: One spec per axis;
      spec name matches lib or module name` body in
      `openspec/specs/spec-conventions/spec.md` so the recognised-
      axes list gains `argc-mode (lib)`.
- [ ] 5.2 The argc delta is authored assuming `extract-may-i-mode`
      archives first (it includes both `may-i` and `argc-mode` in
      the axes list). If `extract-argc-mode` archives first, rebase
      the spec-conventions delta to remove the `may-i` bullet
      before archive; if `extract-may-i-mode` lands and its delta
      no longer carries the `argc-mode` bullet, this change must
      add it back. Verify before archive by diffing against current
      `openspec/specs/spec-conventions/spec.md`.

## 6. Hygiene

- [ ] 6.1 Audit `lisp/argc-mode/argc-mode.el` for stale
      `+argc`-prefixed references; tidy docstrings on public faces
      and functions.
- [ ] 6.2 Confirm `;;;###autoload` cookie survives harvest by
      grepping `lisp/+autoloads.el` for `argc-mode` after the
      regeneration.
- [ ] 6.3 Run `make test` and confirm green across the full suite.
- [ ] 6.4 Run `nix develop --command bash -c 'pre-commit run --all-files'`
      (or the project's prek invocation) and resolve any flagged
      issues.
