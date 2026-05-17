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

### Decision: fill the visual line to the window edge in the per-line `after-string`

Each body line's existing right-edge `after-string` is extended with a
trailing `(space :align-to right)` glyph painted in the `default`
face.  This physically paints the visual line from `│` to the
window's right edge with the default background, so when Emacs
renders the line break there is no past-EOL region left to fill —
`:extend` becomes irrelevant.  No new overlays; the existing per-line,
per-window after-strings are simply lengthened.

The shared `gfm-block-borders--right-after` and
`gfm-block-borders--right-after-overflow` append the tail for fenced /
indent body lines; `gfm-callouts--right-after` and
`gfm-callouts--right-after-overflow` do the same for callout body
lines.

The originally-shipped approach — a high-priority overlay carrying
`face '(:extend nil)'` (anonymous plist or a `defface`) over each
block body's newlines — **does not work**, for a reason internal to
Emacs's face engine:

- `face_at_buffer_position` (`xfaces.c`) is called with
  `attr_filter = LFACE_EXTEND_INDEX` when picking the past-EOL fill
  face (`extend_face_to_end_of_line` in `xdisp.c`).
- With that filter, `merge_face_ref` **skips** any face that has
  `:extend nil`, whether explicit or unspecified — the face opts
  out of the merge instead of suppressing siblings.
- An overlay with `:extend nil` therefore can't clip a text-property
  / overlay neighbour with `:extend t`: both are visited
  independently, and the opted-out face contributes nothing while
  the opted-in face's `:background` still paints.

Sources: `src/xdisp.c` `extend_face_to_end_of_line`, `src/xfaces.c`
`face_at_buffer_position` and `merge_face_ref`; manual entry for
`:extend` in *Face Attributes*.

Alternatives considered:

- Buffer-local `face-remap-add-relative` on `diff-added` /
  `diff-removed` → `(:extend nil)`. Trivial and robust for the diff
  case, but diff-specific and buffer-global; does nothing for
  `hl-line` / `region`. Rejected.
- A high-priority `:extend t` overlay carrying the box's background
  over each body-line newline. Works for the *background* leak but
  forces a single fixed bg colour; tints, gradients, and per-line
  fix-2 fills become harder to compose. The after-string tail is the
  more local mechanism.
- Stripping `:extend` from copied text properties after native
  fontification. Fights jit-lock, which re-applies properties on
  every refontification. Rejected.
- Overlay face `(:extend nil)` — the previously-shipped approach.
  Rejected; cannot clip per the engine semantics above.

### Decision: per-line, per-window — co-located with the existing after-string

The tail piggybacks on the same per-window display overlay that
already carries the `│` right border.  The window-reconciler that
rebuilds per-window display overlays on resize / split also rebuilds
the tail at the new width, so `:align-to right` is always evaluated
against the current window edge.  No new anchor overlays, no new
teardown.

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
