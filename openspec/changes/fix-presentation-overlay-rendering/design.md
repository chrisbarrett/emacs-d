## Context

Presentation slides today decorate buffers with two kinds of overlay:

1. **Annotations** — line-anchored commentary attached to file/diff/
   narrative slides.  Specified per-slide as
   `[{ line, text, position }, ...]`.  Currently rendered with
   either `before-string` or `after-string` on a point-at-bol
   overlay.  In practice both look the same (annotation appears
   between BOL and the line content) because `after-string` on a
   BOL-anchored overlay still inserts before the line text.
2. **Focus** — a multi-line block highlight applied to a
   sub-range of a `file` slide using the built-in `region` face
   with `:extend t`, which paints to the window edge.

Neither has a defined visual design.  Meanwhile the repo already
ships a solid overlay design language in
`modules/lang-markdown/lib/+gfm-callouts.el` (per-type faces,
`┌─...─┐` box drawing, theme-aware colour blending) and
`+gfm-tables.el` (per-row stripe faces, half-block edge caps,
`align-to` space tricks).  The presentation module should align with
those patterns rather than invent its own.

Constraints:
- Stay within Emacs' overlay/`display` property mechanism — no new
  rendering primitive.
- Theme-aware: light/dark backgrounds via `defface` `display`
  conditions, just like `+gfm-tables.el`.
- Backwards compatible: existing slide payloads without `kind` /
  `severity` keep working.
- Keep validation pure (`+presentation--validate-slide`) — still
  one function, no host-side state.

## Goals / Non-Goals

**Goals:**
- Three annotation kinds (`inline` / `callout` / `margin`) covering
  the spectrum from "tag this single token" to "block-level note"
  to "marginalia that doesn't disrupt source flow".
- Severity drives colour without forcing the agent to think about
  faces.
- Focus highlight that paints only over text glyphs, never window
  width.
- Visual consistency with `+gfm-callouts.el`: same box characters
  (`┌─┐│└┘`), same blend-toward-fg trick for muted edges, same
  `align-to` approach for right-edge alignment.

**Non-Goals:**
- Replacing the existing GFM callout implementation; this change
  may *reuse* primitives but does not refactor `+gfm-callouts.el`.
- Multi-line `inline` annotations.  Inline stays line-bounded; if
  the agent wants block content, that's what `callout` is for.
- Per-annotation custom colours.  Severity is the only knob.
- New slide kinds.

## Decisions

### Annotation `kind` taxonomy

`"inline"` (default) — short text appended to a single line,
honouring `position: "before" | "after"`.  Maps to `before-string` /
`after-string` on overlays anchored at the appropriate line edge
(BOL for `before`, EOL for `after`).  Inline annotations stay on the
*same visual line* as their target whenever possible (Emacs may wrap
if the overlay text is wider than window width — accepted).

`"callout"` — block-level note rendered between the target line and
the next.  Uses the `+gfm-callouts.el` box-drawing visual: per-
severity border face, optional label, body text wrapped/padded to a
fixed box width.  Implemented as a single overlay at point-at-eol
of the target line with a multi-line `after-string` containing the
box.  Position is implicit (always after the anchor line); a
`position: "before"` callout simply anchors to the previous line's
EOL or BOL accordingly — but the spec restricts callouts to
`position: "after"` for the first cut.

`"margin"` — short text in the window margin (LHS or RHS) on the
target line.  Implemented via an overlay at the target line with a
`before-string` carrying a `(margin <side> "text")` `display`
property, the same mechanism Emacs uses for `linum`/`display-line-
numbers-mode`.  `position` selects `"left-margin"` or
`"right-margin"`; default RHS.  Requires the buffer to have margins
allocated — the renderer ensures `left-margin-width` /
`right-margin-width` is at least 12 columns when any margin
annotation is present.

Why not subsume callouts under inline?  Inline implies "fits on the
line"; callouts are fundamentally block content.  Conflating them
forces wrapping logic into a path that should stay simple.

Why margin separately from inline?  Margin overlays don't push body
text around — the source column geometry stays exactly the same.
That's qualitatively different from an `after-string` that extends
the line.  Margin is the right kind for "structural" notes (say,
"line N is the bug"); inline is right for "explain this token".

### Severity → face mapping

```
note    → +presentation-annotation-note-face    (inherits +markdown-gfm-callout-note-face)
tip     → +presentation-annotation-tip-face     (inherits +markdown-gfm-callout-tip-face)
warning → +presentation-annotation-warning-face (inherits +markdown-gfm-callout-warning-face)
```

Inheriting from the markdown callout faces means the user's existing
theme tweaks for "what does an IMPORTANT block look like in
markdown" carry over to "what does a warning annotation look like
in a presentation slide".  Less surface to theme.

The `note`/`tip`/`warning` choice (rather than the full GFM range
of NOTE/TIP/IMPORTANT/WARNING/CAUTION/CRITICAL) reflects the
expressive needs of slide commentary: "neutral", "approval",
"watch out".  Adding more later is purely additive — pick a name,
add a face.

### Position semantics per kind

- `inline`: `"before"` (BOL anchor + `before-string`), `"after"`
  (EOL anchor + `after-string`).  Default `"after"`.
- `callout`: `position` is rejected at validation; callouts always
  anchor between the target line and its successor.
- `margin`: `position` is `"left"` / `"right"` (default `"right"`),
  mapped to `left-margin` / `right-margin` on the `display`
  property.  The strings `"before"` / `"after"` are accepted as
  aliases for `"right"` to keep payloads written for inline kind
  parseable, but normalised internally to `"right"`.

### Focus highlight reworked

Replace the single multi-line overlay (which extends to window
width) with one overlay per line in the focus range, each spanning
exactly `(line-beginning-position) .. (line-end-position)`.  Use a
new `+presentation-focus-face` (no `:extend t`) with a muted
background that blends 15% toward the default foreground (same
trick used in `gfm-callouts--quote-face`).  The face only paints
over real glyphs; whitespace and the rest of the window stay
default-bg.

Trade-off: a focus range with empty lines in it has visual gaps
where the empty lines sit.  Acceptable — those lines have no
content to highlight.

### Reusing GFM-callouts primitives

The `gfm-callouts--make-border` helper, the
`gfm-callouts--quote-face` colour-blending helper, and the
`align-to` right-edge trick are all small and self-contained.
First cut: copy the relevant 5-10 lines into the presentation
module rather than refactoring the markdown module.  If a third
caller appears later, extract to a shared `+overlay-utils.el`.
Premature extraction with two callers loses more than it gains.

### Validation surface

`+presentation--validate-annotation` extends to:
- `:kind` (when present) is one of `"inline"` / `"callout"` /
  `"margin"`.
- `:severity` (when present) is one of `"note"` / `"tip"` /
  `"warning"`.
- `:position` accepted values depend on `:kind`:
  - `inline`: `"before"` / `"after"`.
  - `callout`: must be absent (or signal a clear error).
  - `margin`: `"left"` / `"right"` / `"before"` / `"after"`.
- Errors signal `user-error` with messages naming the offending
  field, matching the existing validation style.

## Risks / Trade-offs

- **Margin width allocation.** Setting `left-margin-width` /
  `right-margin-width` modifies the buffer.  → Stash the prior
  values on the slide's `:render-state` `:restorers` list so
  `+presentation--cleanup-render-state` resets them on slide leave.
  Reuses the existing restorer pattern in the file slide renderer.

- **Callout box width assumption.** Hard-coded `max(80, content+2)`
  may overflow narrow windows.  → Match the GFM-callouts behaviour
  exactly (which already has this limit and works fine in practice).
  Window-width-aware sizing is a follow-up.

- **Severity faces depend on theme.** If the user has no theme
  loaded, the inherited markdown callout faces fall back to bland
  defaults.  → Acceptable; the markdown faces themselves degrade
  the same way.  No worse than today.

- **`before-string` on a wrapping line.** If a wide inline
  annotation wraps, the visual relationship to its target token
  may be unclear.  → Document; recommend short inline annotations,
  reach for `callout` or `margin` kinds when content is long.

- **Backwards-compat default.** Old payloads with
  `position: "after"` and no `kind` now actually render at EOL
  instead of BOL.  Visually different from before, but matches the
  documented intent.  → Pre-announce in the change description; no
  consumer code outside the agent depends on the placement.
