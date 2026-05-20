## 1. Scaffold

- [ ] 1.1 Create `lisp/dune-mode/` and `lisp/opam-mode/` directories.
- [ ] 1.2 Create empty `lisp/dune-mode/dune-mode.el` with header
      (`;;; dune-mode.el --- Major mode for Dune build config files -*- lexical-binding: t; -*-`),
      Commentary, `(provide 'dune-mode)`, and footer.
- [ ] 1.3 Create empty `lisp/dune-mode/dune-mode-tests.el` with
      header and `(require 'ert)` / `(require 'dune-mode)`.
- [ ] 1.4 Create empty `lisp/opam-mode/opam-mode.el` with header
      (`;;; opam-mode.el --- Major mode for OPAM package config files -*- lexical-binding: t; -*-`),
      Commentary, `(provide 'opam-mode)`, and footer.
- [ ] 1.5 Create empty `lisp/opam-mode/opam-mode-tests.el` with
      header and `(require 'ert)` / `(require 'opam-mode)`.
- [ ] 1.6 Confirm `lisp/dune-mode/` and `lisp/opam-mode/` are picked
      up by the autoloads scan.

## 2. `dune-mode` extraction (red → green)

- [ ] 2.1 Write failing test `dune-mode/feature-provided` in
      `dune-mode-tests.el`.
- [ ] 2.2 Write failing test `dune-mode/parent-is-lisp-data-mode`.
- [ ] 2.3 Write failing test
      `dune-mode/dune-files-open-in-dune-mode` covering `dune`,
      `dune-workspace`, `dune-project` via
      `string-match-p` against the `auto-mode-alist` regex (mirror
      the existing test in `lang-ocaml/tests.el:42-49`).
- [ ] 2.4 Write failing test `dune-mode/dune-txt-does-not-match`.
- [ ] 2.5 Write failing test `dune-mode/comment-add-is-zero` that
      activates the mode in a temp buffer and asserts
      `(local-variable-p 'comment-add)` and `comment-add` is 0.
- [ ] 2.6 In `lisp/dune-mode/dune-mode.el`, define `dune-mode` via
      `define-derived-mode dune-mode lisp-data-mode "Dune Config"`
      with `(setq-local comment-add 0)` in the body and a
      `;;;###autoload` cookie.
- [ ] 2.7 Add `;;;###autoload` `add-to-list 'auto-mode-alist` for
      `(rx "/dune" (? "-" (or "workspace" "project")) eos)`
      pointing at `#'dune-mode`.
- [ ] 2.8 Verify all dune tests pass.

## 3. `opam-mode` extraction (red → green)

- [ ] 3.1 Write failing test `opam-mode/feature-provided` in
      `opam-mode-tests.el`.
- [ ] 3.2 Write failing test `opam-mode/parent-is-conf-colon-mode`.
- [ ] 3.3 Write failing test
      `opam-mode/opam-files-open-in-opam-mode` covering
      `foo.opam` and asserting `foo.opamignore` does not match.
- [ ] 3.4 Write failing test `opam-mode/hook-variable-exists`
      asserting `(boundp 'opam-mode-hook)` after load.
- [ ] 3.5 In `lisp/opam-mode/opam-mode.el`, define `opam-mode` via
      `define-derived-mode opam-mode conf-colon-mode "OPAM Config"`
      with a `;;;###autoload` cookie.
- [ ] 3.6 Add `;;;###autoload` `add-to-list 'auto-mode-alist` for
      `(rx ".opam" eos)` pointing at `'opam-mode`.
- [ ] 3.7 Verify all opam tests pass.

## 4. Delete legacy definitions

- [ ] 4.1 Delete the `dune-config-mode` `define-derived-mode` block
      and its `auto-mode-alist` push from
      `modules/lang-ocaml/lib.el`.
- [ ] 4.2 Delete the `opam-config-mode` `define-derived-mode` block
      and its `auto-mode-alist` push from
      `modules/lang-ocaml/lib.el`. Keep the tempel helpers
      (`+ocaml--tempel-in-expr-p`,
      `+ocaml--point-in-node-field-p`,
      `+ocaml--treesit-in-expr-context-p`,
      `+ocaml-capture-let-context`, `+ocaml-maybe-in`).
- [ ] 4.3 Move the dune mode-tests
      (`lang-ocaml-test-dune-files-open-in-dune-config-mode`,
      `lang-ocaml-test-dune-config-mode-defined`,
      `lang-ocaml-test-dune-config-mode-parent`) out of
      `modules/lang-ocaml/tests.el`. The replacements in
      `dune-mode-tests.el` cover their assertions.
- [ ] 4.4 Move the opam mode-test (around line 77 in
      `modules/lang-ocaml/tests.el`) out. The replacement in
      `opam-mode-tests.el` covers it.
- [ ] 4.5 Keep all `+ocaml-*` / `+ocaml--*` tests in
      `modules/lang-ocaml/tests.el`.

## 5. Hook rename

- [ ] 5.1 Write failing test in `modules/lang-ocaml/tests.el`
      asserting that visiting an `.opam` file whose first 500 bytes
      contain `# This file is generated` ends up in
      `read-only-mode`.
- [ ] 5.2 In `modules/lang-ocaml/init.el:57`, change the
      `add-hook!` form from `'opam-config-mode-hook` to
      `'opam-mode-hook`. Body unchanged.
- [ ] 5.3 Verify the regression test passes.

## 6. Autoloads regen

- [ ] 6.1 Regenerate `lisp/+autoloads.el`. Confirm:
      - `dune-mode` and `opam-mode` autoload entries appear with
        paths under `lisp/dune-mode/` and `lisp/opam-mode/`.
      - No `dune-config-mode` or `opam-config-mode` entries
        remain.
      - The `+ocaml-*` entries still reference
        `modules/lang-ocaml/lib.el`.

## 7. spec-conventions catch-up

- [ ] 7.1 During archive, the `## MODIFIED Requirements` block in
      `openspec/changes/extract-ocaml-config-modes/specs/spec-conventions/spec.md`
      replaces the existing `### Requirement: One spec per axis;
      spec name matches lib or module name` body in
      `openspec/specs/spec-conventions/spec.md` so the recognised-
      axes list gains `dune-mode (lib)` and `opam-mode (lib)`.
- [ ] 7.2 This change's spec-conventions delta assumes
      `extract-may-i-mode` and `extract-argc-mode` both archive
      first. If they archive in a different order, rebase the
      delta against current main before archive — diff against
      `openspec/specs/spec-conventions/spec.md` and confirm every
      already-present axis bullet survives in the new body.

## 8. Hygiene

- [ ] 8.1 Audit `modules/lang-ocaml/lib.el` for any stray
      references to the renamed mode symbols.
- [ ] 8.2 Audit `modules/lang-ocaml/tests.el` for stray references
      to the moved tests.
- [ ] 8.3 Run `make test` and confirm green across the full suite.
- [ ] 8.4 Run `nix develop --command bash -c 'pre-commit run --all-files'`
      (or the project's prek invocation) and resolve any flagged
      issues.
