# Tasks: add-lang-setup-helper

## 1. Helper

- [ ] 1.1 Write failing tests for `+lang-declare`: declaration loads
      neither eglot nor apheleia; LSP declaration adds `eglot-ensure`
      to `<mode>-local-vars-hook`; formatter pair registers definition
      + association after apheleia loads; formatter symbol adds
      association only
- [ ] 1.2 Create `modules/lang-support/lib.el` with autoloaded
      `+lang-declare`; tests green
- [ ] 1.3 Checkdoc + byte-compile clean; docstring documents the exact
      expansion (hook choice rationale included)

## 2. Migrate language modules

- [ ] 2.1 Migrate `lang-rust` (eglot ×2 + toml formatter pair); tests
      green
- [ ] 2.2 Migrate `lang-ocaml` (formatter associations); tests green
- [ ] 2.3 Migrate `lang-conf` (eglot ×2, formatter); tests green
- [ ] 2.4 Migrate `lang-elixir`, `lang-js`, `lang-nix`, `lang-swift`;
      tests green after each
- [ ] 2.5 Migrate `lang-latex` and `lang-markdown` formatter wiring;
      resolve the latexindent open question (helper shape or documented
      hand-rolled exception)

## 3. Enforce and close out

- [ ] 3.1 Add a repo test asserting no
      `(<mode>-local-vars-hook . eglot-ensure)` pairs or direct
      apheleia alist mutation remain in `modules/lang-*/init.el`
      outside lang-support (honouring any documented exception from
      2.5)
- [ ] 3.2 Full `make test`; manual smoke: open one rust and one nix
      buffer, confirm eglot attaches and format-on-save runs
