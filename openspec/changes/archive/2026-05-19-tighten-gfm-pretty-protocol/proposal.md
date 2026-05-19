## Why

The `gfm-pretty` engine/decorator seam has accreted several shallow slots and
split-state arrangements that survived the recent `deepen-gfm-pretty-engine`
work. The 17-keyword `gfm-pretty-define-decorator` protocol now contains slots
used by exactly one decorator (`:block-at-point-fn`, `:edit-at-point-fn`,
`:reconcile-windows-fn` — all `tables`-only; `:revealable-p-fn` — unused),
overlay tag names are declared in two places, per-decorator overlay lists live
in module-local `defvar-local`s while the engine's state plist documents them
as if it owned them, and the anchor/display dichotomy is enforced as a
protocol obligation even though only two decorators (`callouts`, `fences`)
actually obey it. Tightening the protocol now is cheaper than carrying the
ambiguity forward each time a decorator is added or edited.

## What Changes

- Collapse single-adapter / unused protocol slots on `gfm-pretty--decorator`:
  - **BREAKING**: drop `:block-at-point-fn`, `:edit-at-point-fn`,
    `:reconcile-windows-fn`, and `:revealable-p-fn` from the public
    `gfm-pretty-define-decorator` keyword list.
  - The behaviours those slots provided continue to work, but their
    implementations move to: `gfm-pretty-tables.el` (block-at-point /
    edit-at-point / reconcile-windows hooked directly), and the engine's
    default revealable predicate (presence of `:revealable-prop`).
- Source overlay-tag property names from the registry struct only:
  - **BREAKING**: drop `:revealable-prop` and `:saved-display-prop` from
    `gfm-pretty-define-decorator`. The engine reads them from the decorator's
    `gfm-pretty--registry` slot (`registry-revealable` /
    `registry-saved-display`), which already derives the symbol names from
    the registry's `tag`.
- Move per-decorator overlay state into `gfm-pretty--state`:
  - **BREAKING**: drop the registry's `overlays-symbol` and
    `hidden-ovs-symbol` slots and the corresponding module-local
    `defvar-local *--overlays` / `*--hidden-ovs` symbols across every
    decorator file. Overlays + hidden overlays live in
    `(gfm-pretty--state-get NAME 'overlays)` /
    `(gfm-pretty--state-get NAME 'hidden-ovs)` instead.
  - Update the `gfm-pretty--state` docstring to match (it already lists
    `overlays` / `hidden-ovs` but the slots were never populated there).
- Demote the anchor/display split from protocol to internal convention:
  - **BREAKING**: replace `:apply-anchors-fn` + `:apply-display-fn` with a
    single `:apply-block-fn (block window)` on the protocol. Decorators that
    want the anchor-once / display-per-window split (callouts, fences) use
    a shared helper `gfm-pretty-borders--apply-anchor-and-display` exported
    by `gfm-pretty-borders.el`.
  - `hrule`, `links`, `tables` (and any future decorator that doesn't need
    the split) implement `:apply-block-fn` directly.
- Merge structural-line and edit-adjacency hooks into one predicate:
  - **BREAKING**: replace `:structural-line-ranges-fn` +
    `:edit-adjacency-fn` with a single
    `:full-rebuild-required-p (dirty)` predicate. Decorators compose their
    own answer; the engine still exposes `gfm-pretty--in-ranges-p` /
    `gfm-pretty--region-overlaps-p` as helpers.

All `BREAKING` items are internal to the `gfm-pretty` library and its
five built-in decorators; there are no external callers of the
registration protocol outside this repository.

## Capabilities

### New Capabilities

(none — no new axis introduced)

### Modified Capabilities

- `gfm-pretty`: the registration protocol shape narrows, per-decorator
  overlay state moves into the engine's state plist, and the structural /
  adjacency predicates collapse to one. Behaviour-facing requirements (what
  the decorators visually do, what `gfm-pretty-mode` toggles) are
  unchanged; the internals-facing requirement *Decorator registration via
  `gfm-pretty-define-decorator`* is rewritten, and the requirements naming
  the dropped protocol slots / split-state arrangement are revised in
  place.

## Impact

- **Affected library files**: `lisp/gfm/gfm-pretty.el`,
  `lisp/gfm/gfm-pretty-engine.el`, `lisp/gfm/gfm-pretty-borders.el`,
  `lisp/gfm/gfm-pretty-callouts.el`, `lisp/gfm/gfm-pretty-fences.el`,
  `lisp/gfm/gfm-pretty-tables.el`, `lisp/gfm/gfm-pretty-hrule.el`,
  `lisp/gfm/gfm-pretty-links.el`.
- **Affected tests**: `lisp/gfm/gfm-pretty-tests.el` (3205 LOC — currently
  tightly coupled to the protocol; many assertions reference the dropped
  slots and module-local overlay lists).
- **External callers**: none. `gfm-pretty-define-decorator` and the
  per-decorator `*--overlays` / `*--hidden-ovs` symbols are private to
  `lisp/gfm/`.
- **gfm-present**: depends on `gfm-pretty-mode` lifecycle but not on the
  internal protocol slots; no changes expected.
- **Risk**: regressions in scoped rebuild routing for callouts / fences if
  the merged `:full-rebuild-required-p` predicate composes its inputs
  differently than the previous OR-of-two-hooks. The narrowing-regression
  suite under `modules/lang-markdown/tests.el` covers the overlay
  convergence cases.
