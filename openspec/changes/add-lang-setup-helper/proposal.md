## Why

Seven `modules/lang-*/init.el` files hand-wire eglot activation
(`:hook (<mode>-local-vars-hook . eglot-ensure)`) and eight hand-wire
apheleia formatter registration (`add-to-list`/`alist-set!` on
`apheleia-formatters` plus `apheleia-mode-alist`) inside per-package
`use-package` blocks. Every language module author must re-derive the
same subtleties — that eglot must attach to the `local-vars-hook`
variant (so dir-locals apply first), and that apheleia needs paired
formatter-definition and mode-association entries. The wiring interface
is as large as its implementation: a shallow pattern repeated per
module.

## What Changes

- **New `lang-support` module**: `modules/lang-support/lib.el` provides
  one autoloaded declarative helper that, given a major mode, wires
  LSP activation on the mode's `local-vars-hook` and registers an
  apheleia formatter (definition + mode association) lazily.
- **lang modules migrate**: existing hand-rolled `eglot-ensure` hook
  entries and apheleia alist surgery across `modules/lang-*/init.el`
  are replaced by helper declarations. Behaviour identical.
- **Subtleties live once**: the `local-vars-hook`-not-`mode-hook`
  choice and the deferred apheleia registration idiom are encoded in
  the helper, not repeated in every language module.

## Capabilities

### New Capabilities

- `lang-support`: the declarative language-wiring helper — LSP
  activation and formatter registration for language modules. New axis
  (decision recorded in design.md); module directory
  `modules/lang-support/`.

### Modified Capabilities

<!-- none — lang-markdown/lang-jq spec requirements describe observable
behaviour (which formatter, which modes), not the wiring mechanism, so
migrating the mechanism produces no delta -->

## Impact

- New `modules/lang-support/` (lib.el, tests.el).
- `modules/lang-*/init.el` for rust, ocaml, elixir, conf, js, nix,
  swift, latex, markdown (where applicable) — wiring replaced by
  declarations.
- No new packages; eglot and apheleia remain the underlying machinery.
- No behaviour change: same servers, same formatters, same hooks fire.
