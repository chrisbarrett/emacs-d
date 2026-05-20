## Why

`may-i-config-mode` is a self-contained major mode whose code currently
lives in `modules/lang-lisp/lib/+may-i.el` — a `+`-prefixed module helper
file conventionally reserved for `lang-lisp`-internal glue. The mode has
no semantic relationship to `lang-lisp` beyond also being lisp-shaped,
and its footprint (faces, font-lock, formatter wiring, indent setup,
auto-mode registration) matches the `lisp/<lib>/` standalone-library
shape used by `bats-mode` and `gfm-pretty`. Extraction makes the mode
self-locating and unblocks adding navigation features (imenu) without
inflating an unrelated module.

The mode also currently has no imenu support, so `M-x imenu` /
`consult-imenu` cannot jump to rule definitions, parser definitions,
arg-style definitions, or top-level `check`/`load`/`safe-env-vars`
forms — the structural anchors a may-i config is organised around.

## What Changes

- Relocate the major mode to a standalone library
  `lisp/may-i/may-i.el`. Provide `may-i` (matching the file name; the
  mode entry point remains `may-i-config-mode`). Move the autoloaded
  `auto-mode-alist` entries, faces, font-lock keywords, font-lock
  matchers, the `apheleia` formatter wiring, and the may-i-specific
  `lisp-indent-function` puts (`define`, `define-arg-style`, `parser`,
  `rule`, `with-facts`) into the new file.
- Delete `modules/lang-lisp/lib/+may-i.el` and the now-redundant
  `defcontext` / `with-facts` indent puts at
  `modules/lang-lisp/init.el:30-31`.
- Refresh `lisp/+autoloads.el` so the harvested autoload entries for
  `may-i-config-mode` point at the new path (existing entries at lines
  306-308 reference the old file).
- Tests: add `lisp/may-i/may-i-tests.el` with at minimum imenu coverage
  (rules, parsers, arg-style definitions, check / load / safe-env-vars
  forms) and a smoke test asserting `may-i-config-mode` activates on
  the existing `auto-mode-alist` patterns.
- Add `imenu-create-index-function` to `may-i-config-mode` so navigation
  surfaces structural elements:
  - **Rules** — `(rule "name" …)` and `(rule (or "a" "b" …) …)` heads;
    each named alternative becomes its own entry.
  - **Parsers** — `(parser <name> …)` heads.
  - **Arg styles** — `(define-arg-style <name> …)` heads.
  - **Definitions** — `(define <name> …)` heads.
  - **Checks** — `(check …)` heads (positional, indexed when unnamed).
  - **Loads** — `(load <path>)` heads.
  - **Safe env vars** — `(safe-env-vars …)` heads.
- New spec axis `may-i` documenting activation, faces, font-lock layering,
  and imenu composition.
- `spec-conventions` recognised-axes list gains a `may-i (lib)` entry.

## Capabilities

### New Capabilities
- `may-i`: behaviour-facing spec for the `may-i` library — activation,
  faces, font-lock keywords (def heads, top-level forms, decision verbs,
  reason strings, parser attributes, style names, fact keywords),
  formatter registration, indent rules, and the imenu index composing
  rules / parsers / arg-styles / definitions / checks / loads /
  safe-env-vars.

### Modified Capabilities
- `spec-conventions`: recognised-axes list gains `may-i (lib)`.

## Impact

- New files: `lisp/may-i/may-i.el`, `lisp/may-i/may-i-tests.el`.
- Deleted file: `modules/lang-lisp/lib/+may-i.el`.
- Edited files:
  - `modules/lang-lisp/init.el` — drop may-i-specific indent puts.
  - `lisp/+autoloads.el` — regenerated entries for the new path.
- No external package additions.
- Activation behaviour for users is unchanged: the same
  `auto-mode-alist` patterns continue to select `may-i-config-mode`.
