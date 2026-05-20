## Context

`may-i-config-mode` is a small `lisp-data-mode`-derived major mode for
the `may-i` config DSL. Today it lives at
`modules/lang-lisp/lib/+may-i.el` (~197 LOC) and is registered via
`;;;###autoload` cookies harvested into `lisp/+autoloads.el`. There is
also a pair of `lisp-indent-function` puts at
`modules/lang-lisp/init.el:30-31` (`defcontext`, `with-facts`) that
exist only for may-i.

The mode has no shared state with `lang-lisp`. It is a near-twin of
`bats-mode` (a major mode derived from another major mode, with its
own faces, font-lock, formatter wiring) which already lives at
`lisp/bats-mode/bats-mode.el`. The `lisp/<family>/<lib>.el` convention
exists precisely to host such self-contained libraries, and the
recognised-axes list in `openspec/specs/spec-conventions/spec.md`
treats each as its own axis (`bats-mode (lib)`, `gfm-pretty (lib)`,
`gfm-present (lib)`).

Parallel to extraction, the mode currently exposes no imenu function,
so structural navigation falls back to `lisp-data-mode`'s symbol
scanner — which doesn't know about may-i's `(rule …)`,
`(parser …)`, `(define-arg-style …)`, `(define …)`, `(check …)`,
`(load …)`, `(safe-env-vars …)` heads. The same change adds
`imenu-create-index-function` to lift those into a grouped index.

## Goals / Non-Goals

**Goals:**

- Move `may-i-config-mode` to a self-locating library at
  `lisp/may-i/may-i.el`, mirroring `lisp/bats-mode/bats-mode.el`.
- Preserve activation behaviour exactly: the same `auto-mode-alist`
  patterns continue to select `may-i-config-mode`, and the apheleia
  formatter hook continues to fire when apheleia is loaded.
- Add a `may-i-config-mode` imenu index covering all structural
  anchors: rules (named, including each alternative in `(rule (or
  …))`), parsers, arg-style definitions, plain `define` forms,
  checks, loads, and `safe-env-vars` forms.
- File a new behaviour-facing spec at `openspec/specs/may-i/spec.md`
  and add `may-i (lib)` to the recognised-axes list in
  `openspec/specs/spec-conventions/spec.md`.

**Non-Goals:**

- No change to the font-lock keyword set, faces, indent rules, or
  formatter command. The extraction is a relocation, not a redesign.
- No tree-sitter rewrite. The existing regex-based font-lock and a
  new regex-based imenu scanner stay aligned with the codebase's
  `lisp-data-mode`-derived approach.
- No removal of the `defcontext` indent put — it sits at
  `modules/lang-lisp/init.el:30` and never had a matching keyword in
  `may-i-font-lock-keywords`. Move it into `may-i.el` alongside the
  others rather than deleting it; if it's truly dead the dead-code
  pass picks it up later.

## Decisions

### Decision: new axis `may-i`

The mode is a standalone library; no existing axis covers it. Per
`spec-conventions` §"One spec per axis", a new lib axis is the right
bucket. Spec name `may-i` matches the library directory name
`lisp/may-i/` and the library file `lisp/may-i/may-i.el`.

Alternative considered: extend `lang-lisp` to cover may-i. Rejected:
`lang-lisp` is a module-shaped axis covering Emacs-Lisp / Common-Lisp
editor tooling; a may-i config is neither, and folding the spec there
would conflate two unrelated concerns and mismatch the source layout.

### Decision: file path `lisp/may-i/may-i.el`, provide name `may-i`

Match the `bats-mode` precedent at `lisp/bats-mode/bats-mode.el`
(`provide 'bats-mode`). The provide symbol is the file base name; the
mode entry point (`may-i-config-mode`) is unchanged so external
references and the harvested autoload entries keep working after a
single rewrite of paths in `lisp/+autoloads.el`.

Alternative considered: provide `may-i-config-mode`. Rejected: the
existing convention is "lib name = file name = provide name", and
splitting the provide symbol from the file base would be the only
exception across `lisp/`.

### Decision: imenu uses a regex scanner, not `lisp-imenu-generic-expression`

`lisp-data-mode` ships an `imenu-generic-expression` that recognises
`defun`/`defvar`/etc. — none of which appear in may-i configs.
Overriding it with a regex scanner is simpler than authoring a custom
`imenu-generic-expression`, because:

1. Rules can be named via either `(rule "name" …)` or
   `(rule (or "a" "b" …) …)` — the second shape produces multiple
   entries from one head, which generic expressions can't express.
2. Checks may be unnamed; the index should still surface them with a
   stable label (`check (N)` for the Nth check).
3. Top-level keywords (`load`, `safe-env-vars`) have argument shapes
   worth surfacing (the loaded path, the variable list head) which
   `imenu-generic-expression` can't compose.

A custom `imenu-create-index-function` walks the buffer once via
`syntax-ppss` to ignore strings/comments and collects entries into a
nested alist keyed by section name. See
`lisp/bats-mode/bats-mode.el` for the same pattern.

Alternative considered: tree-sitter (`treesit-simple-imenu`).
Rejected: there is no may-i grammar; the mode derives from
`lisp-data-mode`, not a TS mode.

### Decision: imenu sections

| Section          | Form head                | Label source                                    |
| :--------------- | :----------------------- | :---------------------------------------------- |
| `Rules`          | `(rule …)`               | First string literal, or each element of `(or "a" "b" …)` |
| `Parsers`        | `(parser <name> …)`      | Symbol after `parser`                            |
| `Arg styles`     | `(define-arg-style …)`   | Symbol after `define-arg-style`                  |
| `Definitions`    | `(define <name> …)`      | Symbol after `define`                            |
| `Checks`         | `(check …)`              | First string literal, else `check (N)`           |
| `Loads`          | `(load <path>)`          | The string-literal path                          |
| `Safe env vars`  | `(safe-env-vars …)`      | The literal head text (single entry; positional)|

`safe-env-vars` collapses to a single entry per form because its
contents are not user-named anchors.

### Decision: keep regex-based head matching consistent with font-lock

The existing `may-i--head-rx` helper in `+may-i.el` produces an `rx`
matcher for "head of a call form". The imenu scanner re-uses it
(passed via `funcall` to avoid duplication) rather than re-inventing
the pattern. Both font-lock and imenu therefore agree on what counts
as a head.

### Decision: tests live at `lisp/may-i/may-i-tests.el`

Mirror the `lisp/bats-mode/bats-mode-tests.el` precedent and the
`find-sibling-rules` setup in `modules/lang-lisp/init.el:12-18`
(`<base>-tests.el` ↔ `<base>.el`). The test file requires
`'may-i` and `'ert`, and asserts:

1. `may-i-config-mode` activates on `/may-i/foo.lisp`.
2. `may-i-config-mode` activates on `/.may-i.lisp` and `/.may-i.local.lisp`.
3. `imenu--make-index-alist` on a representative buffer yields the
   expected nested alist for rules / parsers / arg-styles /
   definitions / checks / loads / safe-env-vars.
4. `(rule (or "a" "b") …)` produces one entry per alternative.
5. Strings inside comments / nested strings do not produce false
   imenu entries.

### Decision: `defcontext` indent put

`modules/lang-lisp/init.el:30` sets
`(put 'defcontext 'lisp-indent-function 1)` with the comment
`;; may-i config`. There is no `defcontext` head in
`may-i-font-lock-keywords` and no documented use in the codebase,
but the comment marks it as may-i's. Move both that put and the
`with-facts` put into `may-i.el` alongside the existing `define` /
`define-arg-style` / `parser` / `rule` / `when` / `unless` /
`with-facts` puts; deleting the originals from `lang-lisp/init.el`
keeps may-i's surface area in one file. Do not delete `defcontext`
even if unused — that's a separate hygiene call.

## Risks / Trade-offs

- [Risk] The autoload harvester at `lisp/+modules.el` rewrites
  `lisp/+autoloads.el` from the current file paths. After moving the
  source file, the harvester must be re-run (or the manual entries at
  `lisp/+autoloads.el:306-308` updated by hand) or activation breaks.
  → Mitigation: tasks.md requires regenerating autoloads and adds a
  smoke test that opens a `.may-i.lisp` buffer and asserts
  `major-mode` is `may-i-config-mode`.

- [Risk] Imenu false positives from string content that mimics a head
  (e.g. a reason string containing literal `"(rule \"foo\" …)"`).
  → Mitigation: scanner uses `syntax-ppss` to skip strings/comments
  before considering a `(` open paren as a form head.

- [Risk] `(rule (or "a" "b") …)` parsing depends on the `or` form
  being literal and not a macro expansion. → Mitigation: the
  scanner reads forward with `read` on the first argument; if it's
  a list whose `car` is `or`, each string element becomes an entry;
  otherwise the first string literal (if any) becomes the single
  entry; otherwise the entry is skipped. The behaviour is documented
  in the spec scenarios.

- [Trade-off] The imenu scanner reads sexps with `read`, which is
  slower than a pure regex walk but correct under nesting. Buffer
  sizes for may-i configs are small (configs, not application code),
  so the cost is negligible.
