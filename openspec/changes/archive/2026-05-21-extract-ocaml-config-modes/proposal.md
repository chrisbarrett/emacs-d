## Why

`modules/lang-ocaml/lib.el` mixes three unrelated concerns in one
~71 LOC file:

1. **Tempel template helpers** (`+ocaml-capture-let-context`,
   `+ocaml-maybe-in`, the buffer-local `+ocaml--tempel-in-expr-p`
   var, the tree-sitter helpers) — used from
   `templates/ocaml.eld` to decide whether a `let` binding inserted
   from a snippet needs an ` in` suffix. These are genuine
   `lang-ocaml` glue.

2. **`dune-config-mode`** — a major mode for Dune build files
   (`dune`, `dune-workspace`, `dune-project`). Eight lines including
   the `auto-mode-alist` push.

3. **`opam-config-mode`** — a major mode for OPAM package files
   (`*.opam`). Six lines including the `auto-mode-alist` push.

The two config modes have nothing to do with `neocaml-mode` /
ocamlformat / ocamllsp / Tempel — they're standalone derived modes
that target different file types. They fit the `lisp/<lib>/` shape
exactly the way `bats-mode` and `argc-mode` do: a derived mode plus
an `auto-mode-alist` registration, no composition concerns of their
own. The hook in `modules/lang-ocaml/init.el:57-59` that makes
generated `*.opam` files read-only is composition (a `lang-ocaml`
policy about generated files) and stays in the module.

Extraction also lets each mode own its spec axis instead of being
buried in an undocumented module library.

The modes get renamed during extraction: `dune-config-mode` →
`dune-mode` and `opam-config-mode` → `opam-mode`. Reasons:

- Matches the file/dir/provide-symbol/mode-name = same-name
  convention used by `bats-mode` and (in the in-flight extraction)
  `argc-mode`.
- The "-config-" infix is redundant — both modes only ever target
  config files; the brevity wins.
- Personal-config blast radius is tiny: three test names, one
  hook name, three autoload entries, two `auto-mode-alist` entries.

## What Changes

- Create `lisp/dune-mode/dune-mode.el`:
  - `dune-mode` (renamed from `dune-config-mode`), derived from
    `lisp-data-mode`, mode-line `"Dune Config"`.
  - `;;;###autoload` `auto-mode-alist` entry for
    `/dune(?:-workspace|-project)?` end-of-string.
  - `comment-add` set to 0 (current behaviour).
- Create `lisp/opam-mode/opam-mode.el`:
  - `opam-mode` (renamed from `opam-config-mode`), derived from
    `conf-colon-mode`, mode-line `"OPAM Config"`.
  - `;;;###autoload` `auto-mode-alist` entry for `\.opam\'`.
- Move related tests:
  - `lang-ocaml-test-dune-files-open-in-dune-config-mode` (renamed
    to drop the `-config-`),
    `lang-ocaml-test-dune-config-mode-defined`,
    `lang-ocaml-test-dune-config-mode-parent` →
    `lisp/dune-mode/dune-mode-tests.el` (renamed accordingly).
  - The opam-config-mode test (line 77 in
    `modules/lang-ocaml/tests.el`) →
    `lisp/opam-mode/opam-mode-tests.el`.
- Delete `dune-config-mode` and `opam-config-mode` blocks from
  `modules/lang-ocaml/lib.el`. Keep the tempel helpers
  (`+ocaml--tempel-in-expr-p`,
  `+ocaml--point-in-node-field-p`,
  `+ocaml--treesit-in-expr-context-p`,
  `+ocaml-capture-let-context`,
  `+ocaml-maybe-in`) in `lib.el`.
- Update `modules/lang-ocaml/init.el:57-59` to reference
  `opam-mode-hook` instead of `opam-config-mode-hook`. Keep the
  read-only-on-generated-files behaviour exactly as-is — that's
  composition policy and stays in the module.
- Refresh `lisp/+autoloads.el` so the four harvested entries for
  the two modes point at the new paths and the new symbol names.
- Two new spec axes:
  - `dune-mode`: behaviour-facing spec for the Dune config mode.
  - `opam-mode`: behaviour-facing spec for the OPAM config mode.
- `spec-conventions` recognised-axes list gains both
  `dune-mode (lib)` and `opam-mode (lib)`.

## Capabilities

### New Capabilities
- `dune-mode`: behaviour-facing spec for `lisp/dune-mode/` —
  derivation from `lisp-data-mode`, mode-line label, `comment-add`,
  and the file-association regex covering `dune`, `dune-workspace`,
  `dune-project` (with no extension).
- `opam-mode`: behaviour-facing spec for `lisp/opam-mode/` —
  derivation from `conf-colon-mode`, mode-line label, the
  `\.opam\'` file association.

### Modified Capabilities
- `spec-conventions`: recognised-axes list gains
  `dune-mode (lib)` and `opam-mode (lib)`.

## Impact

- New files: `lisp/dune-mode/dune-mode.el`,
  `lisp/dune-mode/dune-mode-tests.el`,
  `lisp/opam-mode/opam-mode.el`,
  `lisp/opam-mode/opam-mode-tests.el`.
- Edited files:
  - `modules/lang-ocaml/lib.el` — two mode blocks deleted; tempel
    helpers kept.
  - `modules/lang-ocaml/init.el` — `opam-config-mode-hook` →
    `opam-mode-hook` in the read-only-on-generated-files hook.
  - `modules/lang-ocaml/tests.el` — the four mode tests are
    moved out; tempel tests remain.
  - `lisp/+autoloads.el` — regenerated entries for the two modes
    point at new paths and new names; `+ocaml-*` entries continue
    to point at `modules/lang-ocaml/lib.el`.
- **Breaking-within-the-config rename**: `dune-config-mode` →
  `dune-mode`, `opam-config-mode` → `opam-mode`. No external
  callers (this is a personal config); test names updated; the
  autoload-harvester reflects the new symbol.
- No external package additions.
