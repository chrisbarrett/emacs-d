## Context

The GFM block decorators (`+gfm-callouts.el`, `+gfm-code-fences.el`,
sharing `+gfm-block-borders.el`) draw boxes by decorating *live buffer
text* with overlays: a `before-string` left edge, an `after-string`
right edge, and top/bottom border overlays.

Emacs' `:extend` face attribute: when the face covering a newline
character has `:extend t`, the background fills from end-of-line to the
window edge.

The trigger is `markdown-fontify-code-blocks-natively` — it runs
fenced-block content through the language major mode and copies the
resulting `face` text properties back onto the buffer, newline included.
`diff-mode` defines `diff-added` / `diff-removed` with `:extend t`.
Verified live: on a `+ source` line in a ` ```diff ` block the text
property run `[7948,8042)` is `(diff-added markdown-code-face)` and
covers the newline at 8041; `diff-added` background is `#c3ebc1` with
`:extend` t. The result is the green fill leaking past the right
border `│` to the window edge.

The same leak occurs for `:extend t` faces carried by *overlays* rather
than text properties — `hl-line` (`:extend t`) and `region` — on any
callout or fence body line.

The callout decorator's own per-line `bg-face` is already
`(:inherit default :background TINT :extend t)`
(`+gfm-callouts.el:317-319`), but its anchor spans `[lbeg, lend)` and so
does not cover the newline — that `:extend t` is currently inert.

## Goals / Non-Goals

**Goals:**

- A body line's `:extend t` background never extends past the box's
  right border, for fenced, YAML-helmet, indent, and callout blocks.
- The clip works whether `:extend` is carried as a text property (the
  diff case) or as an overlay face (`hl-line`, `region`).
- The clip survives the narrow → rebuild → widen cycle that the
  existing narrowing-regression suite enforces.

**Non-Goals:**

- Filling the gap between end-of-text and the right border with the
  line's own background — that is the sibling change
  `add-gfm-fence-bg-fill`.
- `+gfm-tables.el`. Table rows are rendered as a composed `display`
  string that replaces the buffer line, so buffer-text `:extend` faces
  never render. The dead `:extend t` on `gfm-tables-active-cell-face`
  is left for a separate hygiene change.
- Changing how borders are sized or composed.

## Decisions

### Decision: clip via a named `defface` `gfm-block-borders-extend-clip-face`

The clip face specifies only `:extend nil`; every other attribute
stays unspecified and merges from below, so the background colour on
the actual text is untouched — only the past-EOL fill is suppressed.
An overlay carrying that face over the block body, covering each
interior newline, forces the merged `:extend` to nil at every
body-line end.

The face MUST be a `defface`. The originally-considered anonymous
attribute plist `(:extend nil)` was tried first and **silently
fails**: Emacs's face-spec parser rejects a plist whose first key is
`:extend` with `Invalid face: :extend`, the display engine drops the
spec, and the leak persists.  `face-attribute-merged-with` raises the
same error, so the named-face form also makes the merge testable
without a custom helper.

Alternatives considered:

- Buffer-local `face-remap-add-relative` on `diff-added` /
  `diff-removed` → `(:extend nil)`. Trivial and robust, but
  diff-specific and buffer-global; does nothing for `hl-line` /
  `region`. Rejected as the primary mechanism — the overlay
  generalises.
- Stripping `:extend` from the copied text properties after native
  fontification. Fights jit-lock, which re-applies properties on every
  refontification. Rejected.
- Anonymous attribute plist `(:extend nil)` on the overlay. Rejected
  per above — does not clip.

### Decision: one clip anchor per block, spanning the whole body

`:extend` is consulted only at newline positions, so a block-spanning
overlay with `(:extend nil)` is a no-op on every non-EOL character and
clips every interior newline at once — one overlay per block instead of
one per line. It is width-independent, so it is an **anchor** overlay
(shared across windows), not a per-window display overlay. It is
created in the existing anchor pass
(`gfm-code-fences--apply-*-anchors`, `gfm-callouts--apply-block-anchors`)
and torn down by the existing registry range-removal.

### Decision: explicit high `priority` on the clip overlay

Overlay faces always beat text-property faces regardless of priority, so
the diff (text-property) case needs no priority. But `hl-line` (overlay
priority -50; `hl-line.el`, `hl-line-highlight`) and `region` are
*overlay* faces; to outrank them the clip overlay needs an explicit
numeric `priority`. Use a named constant
(`gfm-block-borders--extend-clip-priority`, value 100) so the intent is
documented. Without the priority the clip still fixes the reported diff
case but not `hl-line` / `region`.

### Decision: shared helper in `+gfm-block-borders.el`

A `gfm-block-borders--make-extend-clip (registry beg end)` helper builds
the anchor through the registry's `--make-anchor`, tagged so existing
teardown handles it. Both decorators call it from their anchor pass.
This keeps the mechanism in one place, mirroring how the border
primitives already live in the shared lib.

## Risks / Trade-offs

- [The clip also confines `hl-line` / `region` to the box interior] →
  This is a behaviour change beyond the reported diff bug, but it is
  consistent with "fit within the box" and is arguably an improvement.
  If it proves undesirable the priority constant can be lowered below
  -50 to clip only text-property faces. Captured in a spec scenario.
- [Overlay priority interactions] → priority 100 is high; a hypothetical
  other mode that legitimately paints over the box body would lose. No
  such mode is used in this configuration. Acceptable.
- [Empty body lines] → a blank line inside the box still has a newline
  carrying the `:extend` face; the block-spanning anchor covers it, so
  no special-casing is needed.
- [Narrowing] → the anchor is created in the existing anchor pass, which
  already widens; teardown is range-based. The narrowing-regression
  suite covers convergence.

**Migration Plan:** none — internal overlay change, no configuration or
data involved.

**Open Questions:** none.
