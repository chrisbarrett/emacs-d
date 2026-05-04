## Why

Dogfooding the slide-ops MCP surface uncovered three concrete
rendering bugs and a deeper gap.  The bugs:

1. **`position: "after"` annotations render at start-of-line.** The
   overlay anchors at point-at-bol with an `after-string`, which
   places the annotation between BOL and the line content —
   indistinguishable from `position: "before"`.
2. **Focus highlight paints full window width.** The `region` face
   on a multi-line overlay extends past the actual text glyphs to the
   window edge, producing wallpaper-like grey blocks instead of a
   highlight.
3. **Annotation styling competes with the focus background.** Italic
   arrow text rendered on top of the grey focus block is hard to
   read; there's no clear visual separation between LLM commentary
   and source content.

The deeper gap: there is no *design language* for presentation
overlays.  Each renderer reaches for whatever face is handy.
Meanwhile this repo has already invested in a coherent overlay design
system for GFM callouts and tables in
`modules/lang-markdown/lib/+gfm-callouts.el` and
`+gfm-tables.el` — box-drawn borders with per-type faces, theme-aware
colour blending, half-block edge caps, `align-to` for right-edge
alignment.  Presentation annotations should reuse that idiom so the
agent can speak with consistent visual semantics across the system.

## What Changes

- **Annotation kinds.** Slide annotations gain a `kind` field with
  values `"inline"` (default), `"callout"`, and `"margin"`.
  - `"inline"` keeps something close to today's behaviour but with
    correct `before`/`after` placement and a face that doesn't
    collide with focus highlights.
  - `"callout"` renders a box-drawn block in the body anchored to
    the target line, modelled on `+gfm-callouts.el`: per-severity
    border face, label header, body wrapped to a fixed column.
  - `"margin"` renders the annotation in the LHS or RHS margin via
    a `display` property on a marker overlay, leaving the body text
    column untouched.
- **Annotation severity.** Annotations gain an optional `severity`
  field with values `"note"` / `"tip"` / `"warning"` (default
  `"note"`).  Severity drives face selection (border + label
  colour) so the agent can express different intent without writing
  a face by hand.
- **Position semantics fixed.** `"after"` annotations anchor at
  point-at-eol with `after-string`; `"before"` anchors at
  point-at-bol with `before-string`.  Each kind documents which
  position values are meaningful (callouts are always block-level
  and ignore `position`; margin annotations always sit beside their
  target line; inline respects `before`/`after`).
- **Focus highlight reworked.** The `region` face is replaced with
  a dedicated `+presentation-focus-face` (theme-aware, muted
  background colour blended toward the default fg).  The overlay
  ranges per-line from BOL to EOL *without* `extend`, so the face
  paints only over real text glyphs.  Lines with shorter content
  no longer get full-width wallpaper.
- **Faces, not hard-coded colours.** All overlay colours go
  through `defface`s in `modules/presentation/lib.el`,
  inheriting from existing markdown callout faces where applicable
  (`+markdown-gfm-callout-note-face`, etc.) so users' theme tweaks
  carry through.
- **Backwards compatibility.** Annotations without a `kind` field
  are treated as `kind: "inline"`.  Existing slide payloads keep
  working; only the visual rendering changes.

Out of scope here:

- New slide kinds.  Annotations are decorations on existing
  buffer-bearing slides only.
- Configurable border characters or columns (callout box width
  follows the GFM-callouts pattern: max(80, content+2)).

## Capabilities

### New Capabilities

<!-- none — extends the existing presentation capability -->

### Modified Capabilities

- `presentation`: gains an annotation rendering subsystem with
  `inline` / `callout` / `margin` kinds plus `note` / `tip` /
  `warning` severity, and replaces the focus highlight implementation
  to paint only over real text glyphs.

## Impact

- Modified files:
  - `modules/presentation/lib.el` — `defface`s for focus and per-
    severity callouts/margins; new `+presentation--render-annotation`
    dispatcher; rewrite of `+presentation--apply-annotations` and the
    file-slide focus overlay; validation extended to cover `:kind` /
    `:severity`.
  - `modules/presentation/init.el` — slide-spec coercion accepts
    `kind` / `severity`; tool descriptions updated to mention the
    new annotation knobs.
  - `modules/presentation/spec.md` — document annotation kinds,
    severities, position semantics per kind.
  - `modules/presentation/tests.el` — overlay placement tests
    asserting `before`/`after` end up at BOL/EOL respectively;
    callout box geometry tests; margin-overlay tests; focus-overlay
    width-bounded test.
- Reuses existing patterns from
  `modules/lang-markdown/lib/+gfm-callouts.el` and `+gfm-tables.el`.
  No copy-paste — extracts the small helpers (`make-border`,
  blend-toward-foreground colour) into a shared utility if needed,
  but the first cut may simply mirror the technique with
  presentation-specific variants.
- No new external packages.
- No breaking changes: annotations without `kind`/`severity` keep
  working with sensible defaults (`inline` / `note`).
- Lands independently of `add-presentation-layout-and-nav`.  Either
  order is fine; merge conflicts are unlikely (different functions).
