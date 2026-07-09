# Tasks: add-lang-setup-helper

## 1. Helper

- [x] 1.1 Write failing tests for `+lang-declare`: declaration loads
      neither eglot nor apheleia; LSP declaration adds `eglot-ensure`
      to `<mode>-local-vars-hook`; formatter pair registers definition
      + association after apheleia loads; formatter symbol adds
      association only
- [x] 1.2 Create `lisp/+lang.el` (shared library, `require`d by
      consumers) with `+lang-declare`; tests green
- [x] 1.3 Checkdoc + byte-compile clean; docstring documents the exact
      expansion (hook choice rationale included)

## 2. Migrate language modules

- [x] 2.1 Migrate `lang-rust` (eglot ×2 + toml formatter pair); tests
      green
- [x] 2.2 Migrate `lang-ocaml` (formatter associations); tests green
- [x] 2.3 Migrate `lang-conf` (eglot ×2, formatter); tests green
      — conf init wires no formatter, only the two eglot hooks
- [x] 2.4 Migrate `lang-elixir`, `lang-js`, `lang-nix`, `lang-swift`;
      tests green after each
- [x] 2.5 Migrate `lang-latex` and `lang-markdown` formatter wiring;
      resolve the latexindent open question (helper shape or documented
      hand-rolled exception)
      — resolved via the helper's define-only (nil MODE) form; no
      exception needed

## 3. Enforce and close out

- [x] 3.1 Add a repo test (`lisp/+lang-tests.el`) asserting no
      `(<mode>-local-vars-hook . eglot-ensure)` pairs or direct
      apheleia alist mutation remain in `modules/lang-*/init.el`
      (honouring any documented exception from 2.5)
      — `lang-terraform` exempted (formatter chain, genuine variation)
- [x] 3.2 Full `make test` green (1100/1100, byte-compile + checkdoc
      clean); integration smoke loads every migrated module through the
      autoload path and confirms each eglot hook + formatter
      association is wired (live-server attach is environment-dependent)
