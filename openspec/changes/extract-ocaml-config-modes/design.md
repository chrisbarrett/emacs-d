## Context

`modules/lang-ocaml/lib.el` (~71 LOC) currently defines:

- Tree-sitter Tempel helpers for OCaml `let` insertion
  (`+ocaml--tempel-in-expr-p` var, three helpers, two autoloaded
  entry points). Used from `templates/ocaml.eld:11-12`.
- `dune-config-mode`: derived from `lisp-data-mode`, mode-line
  `"Dune Config"`, sets `comment-add` to 0, autoloaded
  `auto-mode-alist` push for the regex
  `(rx "/dune" (? "-" (or "workspace" "project")) eos)`.
- `opam-config-mode`: derived from `conf-colon-mode`, mode-line
  `"OPAM Config"`, autoloaded `auto-mode-alist` push for
  `(rx ".opam" eos)`.

`modules/lang-ocaml/init.el:57-59` installs a hook on
`opam-config-mode-hook` that turns on `read-only-mode` when the
buffer's first 500 bytes contain `# This file is generated`. That
hook is composition — a policy choice that *this module*'s users
want generated opam files read-only — and is unrelated to the mode
itself.

Tests for the two modes occupy ~50 of the ~85 lines in
`modules/lang-ocaml/tests.el` (mode-defined, parent, file
association). The remaining tempel tests stay with the module.

The two modes target different file families (Dune build configs
vs OPAM package configs) with different parent modes and
different `auto-mode-alist` regexes. They share nothing semantic
beyond also being "OCaml-ecosystem config files".

## Goals / Non-Goals

**Goals:**

- Move each mode to its own self-contained library with matching
  file/dir/provide/mode name (matching the `bats-mode` precedent):
  `lisp/dune-mode/dune-mode.el` provides `dune-mode`;
  `lisp/opam-mode/opam-mode.el` provides `opam-mode`.
- Rename `dune-config-mode` → `dune-mode` and
  `opam-config-mode` → `opam-mode` so the symbol matches the
  file/dir/provide name. Update the one remaining caller
  (`opam-config-mode-hook` in `lang-ocaml/init.el:57`) and the
  test names.
- Keep Tempel helpers and the read-only-on-generated-files hook
  in `modules/lang-ocaml/` — both are composition.
- File two new behaviour-facing specs (`dune-mode`, `opam-mode`)
  and add both to the recognised-axes list.

**Non-Goals:**

- No change to mode behaviour beyond the rename: same parent
  mode, same `comment-add`, same `auto-mode-alist` regex (just
  pointing at the renamed symbol).
- No font-lock, imenu, or formatter enhancements (those are
  separate proposals).
- No removal of the read-only-on-generated-files hook. Whether
  that policy belongs in this config at all is a separate
  question.

## Decisions

### Decision: two axes, not one combined `lang-ocaml-config`

The two modes inherit from different parents (`lisp-data-mode` vs
`conf-colon-mode`), target different file shapes (path-pattern
matching vs extension), and have no shared helpers. Their joint
"ocaml ecosystem" framing is editorial, not technical.
`spec-conventions` says "every spec covers exactly one
configuration axis"; pretending these are one axis would create a
spec that fans out into two unrelated halves. Two axes — even
small ones — fits the rule.

Alternative considered: one combined `lang-ocaml-config` axis,
`lisp/lang-ocaml-config/{dune,opam}.el`. Rejected: violates the
one-axis rule for editorial convenience.

Alternative considered: a new `lang-ocaml` module spec that
covers both modes plus the Tempel helpers plus the
`use-package neocaml` wiring. Rejected: the modes live in
`lisp/`, not in `modules/lang-ocaml/`, so they don't belong in a
module spec; a module spec is still worth filing later but is out
of scope here.

### Decision: rename `*-config-mode` → `*-mode`

Following `bats-mode` and (in flight) `argc-mode`: file = dir =
provide-symbol = mode-name. The `-config-` infix adds no
information (both modes only target config files) and breaks the
name symmetry.

Surface area of the rename inside this config:

- `modules/lang-ocaml/lib.el`: the two `define-derived-mode` heads
  and the two `auto-mode-alist` entries → moved to the new libs.
- `modules/lang-ocaml/init.el:57`:
  `opam-config-mode-hook` → `opam-mode-hook`.
- `modules/lang-ocaml/tests.el`: three test names and assertions
  → moved + renamed.
- `lisp/+autoloads.el`: four entries, regenerated from the new
  source paths and symbols.

No callers outside this repo and no other files reference the old
names (verified via grep on `dune-config-mode|opam-config-mode`).

Alternative considered: keep the existing symbol names and use
`lisp/dune-config-mode/`, `lisp/opam-config-mode/`. Rejected: the
name is uglier with no upside; the per-symbol-rename cost is
small (one production hook reference + three test renames + four
autoload regenerations).

### Decision: tempel helpers stay in `modules/lang-ocaml/lib.el`

`+ocaml-capture-let-context` and `+ocaml-maybe-in` are template-
specific glue tied to `templates/ocaml.eld`. They use the
buffer-local `+ocaml--tempel-in-expr-p` flag and three private
tree-sitter helpers. Together they are a `lang-ocaml`
implementation detail of the `let`/`let rec` tempel templates —
that's classic composition glue, not a standalone library.
Extracting them would create a single-consumer "library" with no
independent test surface beyond what already exists.

Alternative considered: move them to
`lisp/ocaml-tempel/ocaml-tempel.el`. Rejected: violates the
"self-contained library, no composition concerns" rule from
`spec-conventions` — these helpers ARE composition glue and the
caller is the module's template file.

### Decision: `opam-config-mode-hook` → `opam-mode-hook` is a careful edit

The hook installed at `lang-ocaml/init.el:57` runs the generated-
file check at mode-init. After the rename, the hook variable's
name changes (`define-derived-mode` synthesises the hook from the
mode symbol). The edit replaces `'opam-config-mode-hook` with
`'opam-mode-hook` in the `add-hook!` call. Behaviour is
preserved; only the symbol name changes.

A regression test in `lisp/opam-mode/opam-mode-tests.el` asserts
that the hook variable exists post-rename
(`(boundp 'opam-mode-hook)` is non-nil after loading the
library). The lang-ocaml-tests retain a check that the read-only
behaviour fires for buffers containing `# This file is generated`
in the first 500 bytes.

### Decision: tests live at `lisp/{dune,opam}-mode/{dune,opam}-mode-tests.el`

Mirror `lisp/bats-mode/bats-mode-tests.el` and the
`find-sibling-rules` setup in `modules/lang-lisp/init.el:12-18`.
Each test file `(require)`s its sibling library and `(require
'ert)`.

Test moves (with renames):

| Source (lang-ocaml/tests.el)                              | Destination                              | New name                                       |
| :-------------------------------------------------------- | :--------------------------------------- | :--------------------------------------------- |
| `lang-ocaml-test-dune-files-open-in-dune-config-mode`     | `dune-mode-tests.el`                     | `dune-mode/dune-files-open-in-dune-mode`       |
| `lang-ocaml-test-dune-config-mode-defined`                | `dune-mode-tests.el`                     | `dune-mode/mode-defined`                       |
| `lang-ocaml-test-dune-config-mode-parent`                 | `dune-mode-tests.el`                     | `dune-mode/parent-is-lisp-data-mode`           |
| (opam test at line ~77)                                   | `opam-mode-tests.el`                     | `opam-mode/opam-files-open-in-opam-mode`       |

A new `opam-mode-tests.el` test
`opam-mode/parent-is-conf-colon-mode` covers the parent assertion
the source file lacks for opam.

## Risks / Trade-offs

- [Risk] Renaming a public-ish symbol (`dune-config-mode`) could
  break user keybindings, dir-locals, or muscle memory.
  → Mitigation: this is a personal config; the rename is announced
  here, the autoload regen handles the binding rewire, and the
  test suite covers activation. Worst case the user runs
  `M-x dune-mode` once and notices the new name.

- [Risk] Autoload harvester ordering — old entries for
  `dune-config-mode` / `opam-config-mode` could linger in
  `lisp/+autoloads.el` after the harvester runs from the new
  locations, causing duplicate-definition warnings.
  → Mitigation: tasks.md regenerates `lisp/+autoloads.el` from
  scratch (delete + harvest) rather than in-place edit.

- [Risk] The opam read-only hook runs at mode-init; if the rename
  isn't applied to all three referents (mode symbol, hook
  variable name in init.el, test references) at once, the hook
  silently stops firing.
  → Mitigation: the regression test for the hook still asserts
  read-only fires on generated buffers; failure here is loud.

- [Trade-off] Two tiny spec files (each ~30 LOC) instead of one
  combined ~60 LOC file. Slightly more bookkeeping; cleaner axis
  ownership. Matches existing precedent.

- [Trade-off] The rename creates churn that this change wouldn't
  strictly need. Doing it as part of the extraction means a
  single coherent commit instead of two passes (extract first,
  rename later). The "name should equal file/dir" rule is best
  enforced at the moment of extraction.
