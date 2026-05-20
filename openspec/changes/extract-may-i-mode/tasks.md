## 1. Scaffold

- [ ] 1.1 Create `lisp/may-i/` directory.
- [ ] 1.2 Create empty `lisp/may-i/may-i.el` with package header
      (`;;; may-i.el --- Major mode for may-i configuration files -*- lexical-binding: t; -*-`),
      Commentary, `(provide 'may-i)`, and footer.
- [ ] 1.3 Create empty `lisp/may-i/may-i-tests.el` with header and
      `(require 'ert)` / `(require 'may-i)`.
- [ ] 1.4 Confirm `lisp/may-i/` is picked up by the autoloads scan
      (mirror `lisp/bats-mode/` precedent).

## 2. Relocate code (red → green)

- [ ] 2.1 Write failing test `may-i/activates-on-may-i-dir` that
      visits a temp file matching `/may-i/foo.lisp` and asserts
      `major-mode` is `may-i-config-mode`.
- [ ] 2.2 Write failing test `may-i/activates-on-dotted-config` for
      `.may-i.lisp` and `.may-i.local.lisp`.
- [ ] 2.3 Write failing test `may-i/derived-from-lisp-data-mode`.
- [ ] 2.4 Copy the entire body of
      `modules/lang-lisp/lib/+may-i.el` into `lisp/may-i/may-i.el`
      (faces, font-lock defconsts, helper functions,
      `font-lock-keywords`, `define-derived-mode`, `auto-mode-alist`
      pushes, apheleia wiring, indent puts).
- [ ] 2.5 Add `defgroup may-i` (`:group 'languages`) and ensure each
      `defface` declares `:group 'may-i`.
- [ ] 2.6 Move the `defcontext` and `with-facts` indent puts from
      `modules/lang-lisp/init.el:30-31` into `lisp/may-i/may-i.el`
      alongside the existing `put` calls. Delete the originals from
      `lang-lisp/init.el`.
- [ ] 2.7 Delete `modules/lang-lisp/lib/+may-i.el`.
- [ ] 2.8 Regenerate `lisp/+autoloads.el` (run the harvester) so the
      `may-i-config-mode` entries point at the new path. If the
      harvester is not part of `make test`, update lines 306-308 by
      hand.
- [ ] 2.9 Verify the three activation tests pass via `make test-quick`
      filtered to the may-i suite.

## 3. Apheleia wiring (red → green)

- [ ] 3.1 Write failing test
      `may-i/apheleia-registers-formatter-after-load`: stub apheleia
      vars, `(require 'may-i)`, `(provide 'apheleia)` + trigger
      `with-eval-after-load`, assert both alists contain the may-i
      entry.
- [ ] 3.2 Verify the existing `with-eval-after-load 'apheleia` form
      satisfies the test (no code change expected; if the test fails
      adjust the form, not the test).

## 4. Indent rules (red → green)

- [ ] 4.1 Write failing test that asserts every required head has the
      expected `lisp-indent-function` value after the library loads:
      `define` → 1, `define-arg-style` → 1, `parser` → 1, `rule` → 1,
      `when` → 1, `unless` → 1, `with-facts` → 1, `cond` → 0,
      `defcontext` → 1.
- [ ] 4.2 Write failing test asserting `modules/lang-lisp/init.el`
      contains no `with-facts` or `defcontext` text (use
      `with-temp-buffer` + `insert-file-contents` +
      `re-search-forward`).
- [ ] 4.3 Verify both tests pass.

## 5. Imenu (red → green)

- [ ] 5.1 Write failing test `may-i/imenu-rules-named` covering
      `(rule "foo" …)`.
- [ ] 5.2 Write failing test `may-i/imenu-rules-or` covering
      `(rule (or "a" "b") …)` — expect both entries pointing at the
      same `(rule` position.
- [ ] 5.3 Write failing tests for `(parser …)`, `(define-arg-style
      …)`, `(define …)`, asserting they appear under
      `Parsers`/`Arg styles`/`Definitions`.
- [ ] 5.4 Write failing test `may-i/imenu-check-anonymous` for
      `(check (allow "x"))` expecting `"check (1)"`.
- [ ] 5.5 Write failing tests for `(load "shared.lisp")` and
      `(safe-env-vars …)` — `Loads` and `Safe env vars` sections.
- [ ] 5.6 Write failing test `may-i/imenu-skips-strings-and-comments`
      where the only `(rule …)` content lives inside a string literal
      and a `;;` comment — expect empty index.
- [ ] 5.7 Implement `may-i--imenu-index-function`:
      - Walk the buffer with `forward-sexp` from `point-min`.
      - At each open paren, use `syntax-ppss` to skip inside string
        or comment.
      - Read the head symbol via `(read (current-buffer))` after
        advancing past `(`; dispatch by head into the appropriate
        collector.
      - For `rule`, read the first argument; if it's a string,
        single entry; if it's an `(or …)` list, walk and emit a
        string-keyed entry per literal child; otherwise skip.
      - Return a nested alist `((Section . ((name . pos) …)) …)` in
        the fixed order from the spec.
- [ ] 5.8 Set `imenu-create-index-function` from
      `may-i-config-mode` body to the new function.
- [ ] 5.9 Verify all imenu tests pass.

## 6. spec-conventions catch-up

- [ ] 6.1 During archive, the `## MODIFIED Requirements` block in
      `openspec/changes/extract-may-i-mode/specs/spec-conventions/spec.md`
      replaces the existing `### Requirement: One spec per axis;
      spec name matches lib or module name` body in
      `openspec/specs/spec-conventions/spec.md` so the recognised-
      axes list gains `may-i (lib)`. No additional manual edits.

## 7. Hygiene + verification

- [ ] 7.1 Audit `lisp/may-i/may-i.el` for dead code and unused
      defconsts; tidy docstrings on every public face and function.
- [ ] 7.2 Confirm `;;;###autoload` cookies survive harvest by
      grepping `lisp/+autoloads.el` for `may-i-config-mode` after the
      regeneration.
- [ ] 7.3 Run `make test` and confirm green across the full suite.
- [ ] 7.4 Run `nix develop --command bash -c 'pre-commit run --all-files'`
      (or the project's prek invocation) and resolve any flagged
      issues.
