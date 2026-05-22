## Why

Plain GFM blockquotes currently render the left rail (`▌ `) flush
with the buffer's left margin.  An inset gutter makes the rail read
more clearly as a structural marker for set-aside content rather
than "lines that happen to start with a bar", and it visually
distances the blockquote from the surrounding flowing prose.

(An earlier iteration of this change added a tinted background
across the full rectangle.  It was removed after the user found
that Emacs' `:extend t` paints past EOL only on the visual row
containing EOL — word-wrapped blockquotes ended up with a stepped
right edge on continuation rows.  See design.md "Iteration history"
for the full story.  The remaining change is the inset gutter +
rail, theme-static.)

## What Changes

- Plain blockquote lines render their rail (`▌ `) at a configurable
  column inset (default `tab-width`) instead of at column 0.
- A new defcustom `gfm-pretty-blockquotes-inset-cols` (default the
  symbol `tab-width`) controls the gutter width.  Integer values
  pin a fixed width independent of `tab-width`.
- The decorator now follows the callouts anchor / per-window
  display split: anchors carry the `before-string` inset gutter and
  the inset + rail `wrap-prefix`; per-window display overlays carry
  the `> ` → `▌<space>` (or `>` → `▌`) swap with masked / bare
  variants for the engine's reveal walker.
- The anchor's `before-string` MUST persist through reveal so
  point-on-line exposes the raw `> ` source at the SAME column the
  masked rail occupies (no horizontal jitter of body text).

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: the blockquotes decorator gains an inset-cols
  defcustom and emits an anchor `before-string` for the gutter; the
  rendering contract changes from "rail at col 0" to "rail at col
  `inset-cols`, gutter to the left in default face".

## Impact

- `lisp/gfm/gfm-pretty-blockquotes.el`: rewrite of
  `--apply-block-anchors` and `--apply-block-display`; new
  defcustom and helper.
- `lisp/gfm/gfm-pretty-tests.el`: existing blockquote display
  assertions updated for the inset; new tests for the defcustom,
  the before-string gutter, the wrap-prefix shape, and the
  reveal-preserves-inset invariant.
- `openspec/specs/gfm-pretty/spec.md`: delta updates the existing
  "Blockquote left-rail rendering" and "Blockquote source reveal"
  requirements and adds the inset-cols defcustom requirement.
- No engine changes — existing seams in `gfm-pretty-borders` and
  `gfm-pretty-engine` already support the anchor / per-window
  display split and the masked/bare reveal pattern.
