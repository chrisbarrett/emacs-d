## Why

Plain GFM blockquotes currently render as a bare left-rail glyph
(`▌ `) with no background and no inset, so they read as "lines that
happen to start with a bar" rather than as a distinct paragraph-set-
aside region.  Callouts already prove that a tinted rectangle reads
unambiguously as a set-aside block; lifting that visual treatment
(minus the type-coloured borders) to plain blockquotes gives the two
block kinds a consistent grammar — set-aside content always lives in
a tinted rectangle with a left rail — while keeping callouts'
chromatic typing as the distinguishing axis.

## What Changes

- Plain blockquotes render as a tinted rectangle with a left rail at
  a configurable column inset (default `tab-width`).
- The rectangle has no top, right, or bottom border — only the
  existing left rail (`▌ `).  The right edge is a column-defined
  stop where the tinted background terminates and a `(space :align-to
  right)` in the default face suppresses `:extend` leak past it.
- A new defcustom `gfm-pretty-blockquotes-inset-cols` (default
  `tab-width`) controls the gutter width between the buffer's left
  margin and the rail glyph.
- A new face `gfm-pretty-blockquotes-bg-face` (independent copy of
  `gfm-pretty-tables-row-alt-face` with `:extend t`) provides the
  rectangle's tinted background.
- The decorator now follows the callouts anchor / per-window display
  split: anchors carry the bg face and the inset-aware
  `wrap-prefix`; per-window display overlays carry the prefix swap
  and a right-edge after-string.
- Reveal mirrors callouts' masked↔bare swap so point/region on a
  blockquote line exposes the raw `> ` source.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: the blockquotes decorator's rendering contract
  changes from "left-rail glyph only" to "tinted rectangle with left
  rail, inset gutter, and reveal-aware masking", and gains a new
  customisation knob plus a new face.

## Impact

- `lisp/gfm/gfm-pretty-blockquotes.el`: rewrite of `--apply-block-
  anchors` and `--apply-block-display` to add the bg face, inset
  gutter, and right-edge after-string.  New defcustom and defface.
- `lisp/gfm/gfm-pretty-tests.el`: existing blockquote display-string
  assertions updated for the new inset + rectangle shape; new tests
  for bg face, inset defcustom, and rhs after-string.
- `openspec/specs/gfm-pretty/spec.md`: delta updates the existing
  "Blockquote left-rail rendering", "Blockquote rail face", and
  "Blockquote source reveal" requirements and adds requirements for
  the inset defcustom, bg face, and rectangle right-edge.
- No engine changes — existing seams in `gfm-pretty-borders` and
  `gfm-pretty-engine` already support the anchor / per-window display
  split and the masked/bare reveal pattern.
