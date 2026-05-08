## Why

`gfm-callouts-mode` currently produces a single set of buffer-wide overlays
sized purely from content (`(max 80 (+ max-col 2))`).  When a buffer is
shown in two windows of different widths, or in a narrow window, the box
either overflows the window edge or leaves dead space.  Long body lines
also blow past the right border because there is no wrap simulation.
`gfm-code-fences-mode` already solved this with a per-window
anchor/display overlay split, wrap-aware right-edge padding, and a
window-state reconciler.  Callouts should adopt the same model so the
two markdown decorators behave consistently across splits and resizes.

The two modes already overlap heavily on box-drawing primitives,
wrap simulation, the overlay registry, and the rebuild scheduler.
Porting the per-window machinery to callouts is the right moment to
extract a shared `gfm-block-borders` library so the duplication does
not double.

## What Changes

- Per-window display overlays for callouts: top border, bottom border,
  and per-body-line right edges sized to each window currently
  showing the buffer.
- Anchor overlays carry the width-independent props: tinted background
  face, `> ` → `│ ` substitution, wrap-prefix.
- Box width clamps to `min(text-width, max(80, max-content + 4))` per
  window, where `text-width` comes from `window-max-chars-per-line`.
- Long body lines wrap natively via `wrap-prefix`, with right-edge
  padding computed by simulating wrap (mirroring fences).
- Window-configuration-change-hook drives a per-window reconcile:
  added/resized windows render fresh display overlays; removed
  windows have their display overlays deleted; unchanged windows
  keep theirs.
- Visible-window blocks render first; off-screen blocks defer to
  the next idle tick.
- Post-edit rebuild is scoped: edits inside one callout body rebuild
  only that callout; edits on the marker line trigger a full rebuild.
- Reveal becomes selected-window aware so cursor in one window does
  not expose source in another.
- New shared library `+gfm-block-borders.el` hosts the wrap simulator,
  border-string builders, anchor/display overlay constructors,
  rebuild scheduler primitives, and window-state tracking.
  `+gfm-code-fences.el` and `+gfm-callouts.el` both consume it.

## Capabilities

### New Capabilities

- `gfm-callout-rendering`: Per-window decoration of GFM callout
  blockquotes — box discovery, bordered rendering, body wrapping,
  cursor reveal, scoped post-edit rebuild, window-state reconcile,
  and performance instrumentation.

### Modified Capabilities

<!-- None. `gfm-code-fence-rendering` requirements are unchanged; the
shared-lib extraction is an internal refactor that preserves
existing behaviour. -->

## Impact

- New file: `modules/lang-markdown/lib/+gfm-block-borders.el`.
- Modified: `modules/lang-markdown/lib/+gfm-code-fences.el` — consumes
  shared lib instead of in-file primitives.
- Modified: `modules/lang-markdown/lib/+gfm-callouts.el` — rewritten
  to anchor/display split; consumes shared lib.
- New tests under `modules/lang-markdown/` covering callout
  per-window behaviour and the shared lib's wrap/width helpers.
- No dependency or API changes outside the markdown module.
