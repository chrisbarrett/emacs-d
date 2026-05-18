## Why

Every overlay-based visual enhancement for `gfm-mode` is currently five
near-identical minor modes (`gfm-callouts-mode`, `gfm-code-fences-mode`,
`gfm-tables-mode`, `gfm-hrule-mode`, `gfm-links-mode`) plus a sixth font-lock
fontifier, each maintaining its own scheduler, reconciler, reveal handler,
dirty-region tracker, and window-state snapshot, sharing primitives through
the private `+gfm-block-borders.el` lib but otherwise drifting. External
consumers (`modules/leader/init.el`) already reach into `gfm-tables--`
internal symbols because no public block-introspection API exists. There is
no unified extension point for adding a new decorator, and the spec for the
whole stack is a 2343-line catalogue mixed in with non-visual markdown-mode
concerns.

Unifying them into one library with a single umbrella minor mode and a
small public extension protocol replaces five copies of the lifecycle
wiring with one, gives external consumers a stable API, and lets every
future visual tweak for markdown plug into the same engine and test suite.

The visual-decoration code and the presentation-mode code are both
substantial, self-contained pieces of elisp; they belong in `lisp/`
where the existing infrastructure libraries already live, not in
`modules/` (which this config reserves for composition). This change
also formalises that placement convention so the rationale is captured.

## What Changes

- **NEW** library `lisp/gfm/gfm-pretty.el` (plus impl files
  `gfm-pretty-borders.el`, `gfm-pretty-callouts.el`,
  `gfm-pretty-fences.el`, `gfm-pretty-tables.el`, `gfm-pretty-hrule.el`,
  `gfm-pretty-links.el`) exposing umbrella `gfm-pretty-mode`, an
  extension protocol (`gfm-pretty-define-decorator`), a small set of
  rendering primitives (borders, wrap simulation, width helpers, overlay
  factories), and public block introspection
  (`gfm-pretty-block-at-point`, `gfm-pretty-edit-block-at-point`).
- **NEW** library `lisp/gfm/gfm-present.el` with `gfm-present-mode`
  replacing `+presentation-mode`. Declares load-order dep on
  `gfm-pretty`; no code-level coupling yet.
- **REMOVED** module `modules/presentation/` (its `init.el` is an
  11-line `(require '+autoloads)` shim that becomes redundant once the
  lib autoloads from `lisp/gfm/`).
- Five existing minor modes collapse into registered decorators inside
  `gfm-pretty`: callouts, fences, tables, hrule, links. The umbrella mode
  is the only user-facing toggle; per-decorator toggling routes through
  `gfm-pretty-toggle-decorator`.
- Font-lock callout fontifier (`+markdown-fontify-gfm-callouts`), callout
  faces, body-face theme refresh, header-face weight styling, and
  `markdown-blockquote-face` neutralisation move from `lang-markdown` into
  the callouts decorator inside `gfm-pretty`.
- `modules/lang-markdown/init.el` retains composition concerns —
  file associations, `major-mode-remap-alist`,
  `markdown-code-lang-modes`, `apheleia` wiring, the `gfm-mode-hook`
  that loads `gfm-pretty` and enables `gfm-pretty-mode`, and local-leader
  bindings — and `modules/lang-markdown/lib.el` retains `+markdown-tab-dwim`,
  lang-mode memoise advice, and the syntax-propertize clamp advice.
- **BREAKING** for any external caller — `gfm-callouts-mode`,
  `gfm-code-fences-mode`, `gfm-tables-mode`, `gfm-hrule-mode`,
  `gfm-links-mode`, and all `gfm-<name>--*` private symbols are gone. The
  config's only internal consumer (`modules/leader/init.el`) switches to
  `gfm-pretty-block-at-point` / `gfm-pretty-edit-block-at-point`.
- Engine owns one of each: `after-change-functions` handler,
  `window-configuration-change-hook` handler, `post-command-hook` reveal,
  idle rebuild timer, dirty-region tracker, window-state snapshot.
  Decorators register `:collect`, `:range`, `:apply-anchors`,
  `:apply-display`, optional `:font-lock`, `:revealable-p`,
  `:block-at-point`, `:edit-at-point`, `:on-enable`, `:on-disable`.
- `lang-markdown/spec.md` shrinks from 2343 lines to file associations,
  `gfm-mode` hookup, formatters, `+markdown-tab-dwim`, lang-mode memoise
  advice, and `markdown-syntax-propertize-extend-region` clamp advice.
  Visual-behaviour requirements relocate to `gfm-pretty/spec.md`.
- `spec-conventions/spec.md` Requirement "One spec per axis…" reworded
  so an axis MAY correspond to either a `lisp/<lib>/` library OR a
  `modules/<name>/` composition unit. Recognised-axes list updated:
  `presentation` removed; `gfm-pretty` and `gfm-present` added as
  library axes; existing module axes retained.
- All existing narrowing-regression tests relocate to
  `lisp/gfm/gfm-pretty-tests.el` and continue to pass; presentation
  tests relocate to `lisp/gfm/gfm-present-tests.el`.

## Capabilities

### New Capabilities

- `gfm-pretty`: Umbrella minor mode and extension protocol for overlay-
  based and font-lock-based visual enhancement of GFM/markdown buffers.
  Owns engine (registry, reconciler, scheduler), rendering primitives,
  and the five built-in decorators (callouts, fences, tables, hrule,
  links). New library axis under `lisp/gfm/`; rationale captured in
  design.md.
- `gfm-present`: Buffer-local slide-walkthrough mode over markdown
  documents, with heading-narrowed slides, navigation commands,
  source-range and diff-range link previews, and click actions. Content
  inherited from the dissolved `presentation` capability. New library
  axis under `lisp/gfm/`; rationale captured in design.md.

### Modified Capabilities

- `lang-markdown`: Removes every visual-behaviour requirement (callout
  rendering, code-fence rendering, table rendering, link rendering, HR
  rendering, callout font-lock fontification, header-face weights,
  blockquote-face neutralisation). Retains `gfm-mode` file association,
  `major-mode-remap-alist` entry, `markdown-code-lang-modes` table,
  `apheleia` formatter wiring, `+markdown-tab-dwim`, lang-mode memoise
  advice, and `markdown-syntax-propertize-extend-region` clamp advice.
  The `gfm-mode-hook` enabling `gfm-pretty-mode` lives here too (this
  module composes the `gfm-pretty` library into the buffer lifecycle).
  Cross-references `gfm-pretty` for visual concerns.
- `presentation`: Capability dissolved. All requirements move to the new
  `gfm-present` capability as part of the library relocation. The
  `openspec/specs/presentation/` directory is removed; the
  `modules/presentation/` directory is deleted.
- `contributor-internals`: Requirement "One spec per axis; spec name
  matches module name" reworded — an axis SHALL correspond to either a
  `lisp/<lib>/` library OR a `modules/<name>/` composition unit. The
  recognised-axes list is updated accordingly. Existing module axes are
  unchanged in placement; this change introduces the library-axis
  category and assigns `gfm-pretty` and `gfm-present` to it.

## Impact

- **Code**: `modules/lang-markdown/lib/+gfm-*.el` (6 files, ~5900 lines)
  relocate and rename into `lisp/gfm/gfm-pretty*.el`.
  `modules/presentation/lib.el` (724 lines) relocates and renames into
  `lisp/gfm/gfm-present.el`. `modules/presentation/` directory is
  deleted (its 11-line `init.el` becomes a redundant shim).
  `modules/lang-markdown/lib.el` shrinks from 333 lines to ~80.
  `modules/lang-markdown/init.el` removes the five `gfm-mode-hook`
  decorator wirings and the `markdown-blockquote-face` neutralisation
  block; adds a `gfm-mode-hook` that `(require 'gfm-pretty)` then
  enables `gfm-pretty-mode`. External call site in
  `modules/leader/init.el:85-87` migrates to the public block API.
- **Public API**: every existing `gfm-<name>-mode` symbol disappears.
  `gfm-pretty-mode`, `gfm-pretty-define-decorator`,
  `gfm-pretty-toggle-decorator`, `gfm-pretty-block-at-point`,
  `gfm-pretty-edit-block-at-point`, and a handful of `gfm-pretty-*`
  primitives replace them. `+presentation-mode` renames to
  `gfm-present-mode`; all `+present-*` commands rename to
  `gfm-present-*`.
- **Tests**: `modules/lang-markdown/tests.el` narrowing-regression
  suite, per-decorator suites, and all related fixtures relocate into
  `lisp/gfm/gfm-pretty-tests.el`. Presentation tests move to
  `lisp/gfm/gfm-present-tests.el`. Test tags
  (`:tags '(narrowing-regression)`) preserved verbatim. The makefile
  test target picks up `lisp/gfm/*-tests.el` (verify the existing
  glob already covers `lisp/`, otherwise extend it).
- **Specs**: `openspec/specs/lang-markdown/spec.md` reduced; new
  `openspec/specs/gfm-pretty/spec.md` and
  `openspec/specs/gfm-present/spec.md` authored;
  `openspec/specs/presentation/spec.md` deleted;
  `openspec/specs/spec-conventions/spec.md` axis rule reworded and
  axes list updated.
- **Load path**: `lisp/gfm/` added to `load-path`. The existing
  `lisp/` is already on the load-path (per `+core-paths.el` and
  `+modules.el`); confirm the loader descends into sub-directories or
  add a one-line `(add-to-list 'load-path …)` for `lisp/gfm/`.
- **Autoloads**: `+autoloads.el` (or equivalent autoload generator)
  must scan `lisp/gfm/` so `gfm-pretty-mode`, `gfm-present-mode`,
  `gfm-present-markdown`, etc. autoload without an explicit `require`.
- **Packaging**: No `packages.eld` in `lisp/gfm/` (the lib has no
  Elpaca-managed deps; `markdown-mode` is pulled by
  `modules/lang-markdown/packages.eld`, `nerd-icons` is soft-required
  via `(require 'nerd-icons nil t)`).
- **No behaviour change** for end users beyond the public-API symbol
  rename: every overlay, every face, every reveal, every scoped
  rebuild behaves identically. The change is structural.
