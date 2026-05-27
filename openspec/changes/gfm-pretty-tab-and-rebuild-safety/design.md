## Context

`gfm-pretty-mode` decorates markdown buffers with display overlays
owned by per-decorator registries. The engine's incremental rebuild
path
([`lisp/gfm/gfm-pretty-engine.el:656`](../../../lisp/gfm/gfm-pretty-engine.el))
detects which decorator block(s) overlap an `after-change` dirty
region and rebuilds only those.

A user-visible failure was observed: pressing `TAB` at BOL of a
callout marker line (`> [!NOTE]`) silently inserts 4 spaces via
`markdown-cycle` → `indent-for-tab-command` →
`markdown-indent-line` → `indent-line-to`
([`elpaca/builds/markdown-mode/markdown-mode.el:5169`](../../../elpaca/builds/markdown-mode/markdown-mode.el),
[7339](../../../elpaca/builds/markdown-mode/markdown-mode.el)). The
indented marker no longer parses as a callout. The engine then
collects blocks from the new source, finds none overlapping the dirty
region, hits the `(null matching)` branch
([engine.el:668](../../../lisp/gfm/gfm-pretty-engine.el)), returns
`nil` without cleanup. Stale overlays from the previous (now-defunct)
callout block remain, producing visible decoration corruption.

`evaporate t` does not save us: the overlays' ranges shifted but did
not empty, so Emacs keeps them alive.

## Goals / Non-Goals

**Goals:**

- Engine survives source edits that destroy a tracked block: no stale
  overlays remain for any decorator.
- `TAB` in `gfm-pretty-mode` never silently mutates source in
  contexts where indent is meaningless (callouts, fences,
  paragraphs, hrules, block-level non-list content).
- `TAB` still indents list items when the user is actually editing a
  list-item prefix.

**Non-Goals:**

- Restructuring the dirty-region/idle-rebuild scheduler. Only the
  null-match branch changes.
- Replacing `markdown-cycle` globally; the new binding is
  buffer-local to `gfm-pretty-mode` and inactive when the mode is
  off.
- Supporting indent-cycling through multiple positions à la
  `markdown-indent-line`. The wrapper either indents one step or
  does nothing.

## Decisions

### Engine: null-match in `rebuild-scoped-by-block` escalates to full rebuild

When the post-edit block collection contains no block overlapping
the dirty region, run `gfm-pretty--rebuild`
([engine.el:614](../../../lisp/gfm/gfm-pretty-engine.el)) — the full
rebuild path. It already widens the registry's overlay sweep
(`gfm-pretty--remove-overlays` with no bounds,
[engine.el:257](../../../lisp/gfm/gfm-pretty-engine.el)) and rebuilds
from scratch.

**Alternatives considered:**

- *Targeted cleanup only* — sweep decorator-tagged overlays
  intersecting an expanded dirty range, then return. Rejected:
  introduces a third partial-cleanup code path that has to know how
  to expand the dirty range. Full rebuild covers all decorators
  uniformly and matches what the `t` branch already does for the
  multi-block-overlap case.
- *Defer to next idle rebuild* — leave `nil`, rely on a later full
  rebuild from somewhere else. Rejected: there is no scheduled full
  rebuild after this point; the dirty region was already consumed
  by `gfm-pretty--scheduled-rebuild`
  ([engine.el:725](../../../lisp/gfm/gfm-pretty-engine.el)).

### TAB wrapper lives in `gfm-pretty-mode-map`

Add `:keymap gfm-pretty-mode-map` to the `define-minor-mode`
declaration in `lisp/gfm/gfm-pretty.el` and bind `TAB` to a new
command `gfm-pretty-tab-dwim`. The command dispatches:

| Context                                       | Action                                |
| :-------------------------------------------- | :------------------------------------ |
| `markdown-on-heading-p`                       | call `markdown-cycle`                 |
| `markdown-table-at-point-p`                   | call `markdown-table-forward-cell`    |
| list-item prefix slot + evil-insert state     | call `markdown-indent-line` once      |
| otherwise                                     | no-op                                 |

List-item prefix slot predicate: line matches
`^\s-*\(?:[-*+]\|[0-9]+[.)]\)\s-+` and `(point) ≤ (match-end 0)`.
Item-content-start may equal EOL for an empty marker (`- `) which
is fine — the inequality includes EOL.

Evil gating uses `(and (bound-and-true-p evil-mode)
(evil-insert-state-p))` so the wrapper degrades to no-op-for-list
when evil is disabled, rather than always indenting. Acceptable
because this is a personal config with evil always on; tests stub
`evil-insert-state-p`.

**Alternatives considered:**

- *Bind TAB globally in `gfm-mode-map`* — broader blast radius and
  affects users (the author) running gfm without pretty mode.
  Rejected: keep the change scoped to where the bug manifests.
- *Override `markdown-cycle` via `advice-add`* — opaque, breaks
  inspection of `TAB`'s binding. Rejected.

### Calling `markdown-cycle` directly for headings keeps semantics intact

The heading branch of `markdown-cycle` (visibility cycling, state
machine through `markdown-cycle-subtree-status`) is intricate and
state-bearing. The wrapper invokes `markdown-cycle` interactively in
the heading case so that machinery — including `last-command`
checks at
[markdown-mode.el:7401](../../../elpaca/builds/markdown-mode/markdown-mode.el)
— continues to work.

### Tests cover the engine path and the wrapper

Two new ERT blocks in `lisp/gfm/gfm-pretty-tests.el`:

1. **Engine null-match repro.** Build a buffer with a callout,
   enable `gfm-pretty-mode`, capture overlay count and
   callouts-registry overlays in the callout range. Insert a space
   at BOL of the marker line. Run the rebuild synchronously
   (calling `gfm-pretty--scheduled-rebuild` rather than waiting on
   the idle timer). Assert: no overlays tagged
   `gfm-pretty-callouts` remain in the old callout's character
   range.

2. **TAB wrapper dispatch.** Five mini-buffers covering heading,
   table, list-item-allowed, list-item-disallowed, callout-marker.
   Stub `evil-insert-state-p` to return `t` for the list cases.
   Assert behaviour by side effect (point movement, buffer
   modification, `last-command` value), not by mock counts.

## Risks / Trade-offs

- [Risk] Full rebuild on every block-destroying edit is more
  expensive than a scoped rebuild. → Mitigation: this branch only
  fires when the dirty region overlaps no block, which is the
  uncommon "edit broke a block" case. The common case (edit inside
  an existing block) keeps the scoped path.
- [Risk] `gfm-pretty-tab-dwim` shadowing `markdown-cycle` could
  surprise muscle memory if a user expects TAB-as-indent in a
  paragraph. → Mitigation: documented in
  `gfm-pretty-tab-dwim`'s docstring; matches the existing
  intentional disuse of `indent-for-tab-command` in markdown prose.
- [Risk] Evil-state gating ties pretty mode's UX to evil. →
  Mitigation: when evil is absent the wrapper still dispatches
  correctly for heading / table / non-list contexts. The "indent
  list item" branch is the only one gated on evil insert state,
  and only because the user's mental model for it is an
  insert-state action.

## Migration Plan

Pure additive: the engine branch was a no-op, the new keymap entry
is buffer-local to `gfm-pretty-mode`. Disabling the mode restores
the previous TAB binding (`markdown-cycle`). No rollback hook
required.
