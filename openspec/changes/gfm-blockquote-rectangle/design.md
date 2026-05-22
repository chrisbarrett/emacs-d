## Context

The blockquotes decorator (`lisp/gfm/gfm-pretty-blockquotes.el`)
previously emitted the rail (`▌ `) flush with the buffer's left
margin via a per-window display overlay swapping `> ` for `▌ `,
plus a `wrap-prefix` anchor for soft-wrap continuation rows.  This
change moves the rail to a configurable inset column (default
`tab-width`) so the rail visually separates from surrounding
content.

The callouts decorator's anchor / per-window display split is the
engine seam every block decorator follows.  Anchors carry width-
independent props (face, `wrap-prefix`, here also a `before-string`
for the gutter).  Per-window displays carry the prefix swap.
Reveal flips `display`/`after-string` between paired
`gfm-pretty-display-masked` / `-bare` values when point or
selection overlaps an overlay carrying the revealable property.

## Goals / Non-Goals

**Goals:**

- Plain blockquote rail at a configurable column inset (default
  `tab-width`).
- Reveal exposes the raw `> ` source when point or selection
  overlaps a blockquote line, mirroring callouts' masked↔bare
  swap.  The inset gutter MUST persist through reveal so body text
  doesn't shift horizontally.
- No engine changes; reuse `gfm-pretty-borders` / `gfm-pretty-
  engine` seams verbatim.

**Non-Goals:**

- Top, right, or bottom border glyphs.  Future-extension only.
- Type-coloured blockquotes.  Callouts own the chromatic axis.
- A tinted background spanning the blockquote rectangle.  An
  earlier iteration tried this; see "Iteration history" below for
  why it was removed.
- A separate file for the rail logic.  Stays in
  `gfm-pretty-blockquotes.el`.

## Decisions

### D1. Inset gutter via `gfm-pretty-blockquotes-inset-cols` defcustom

A buffer-local-friendly defcustom whose default is the symbol
`tab-width` (evaluated at render time).  Reading from a defcustom
rather than directly from `tab-width` lets users pin the visual
inset independent of code-indent settings — a markdown buffer with
`tab-width 8` would otherwise produce a comically wide gutter; a
`tab-width 2` buffer would barely show the inset.

The defcustom value is consulted once per `--apply-block-anchors`
pass and threaded into the `before-string` and `wrap-prefix`
strings.

### D2. Anchor layout: before-string gutter + wrap-prefix per line

```elisp
(before-string <inset-spaces>                       in default face)
(wrap-prefix   <inset-spaces> + ▌ + " "             rail face on ▌)
```

The `before-string` hangs `<inset-spaces>` (default face) off the
line start so the gutter is a property of the **anchor**, not the
per-window prefix display.  This matters at reveal time: when point
sits on a blockquote line the engine's reveal nils the per-window
display overlay's `display` to expose the raw `> ` source.  If the
inset lived inside the display string, nilling it would also drop
the gutter and body text would jitter left by `inset-cols` columns.
With the gutter on the anchor's before-string the inset persists
through reveal — the raw `> ` just appears at column `inset-cols`
instead of being replaced by `▌ ` at the same column.

Continuation visual rows of a soft-wrapped line have no buffer
char at column 0 to host the anchor's `before-string`, so the
inset is baked into the `wrap-prefix` itself: inset spaces
(default face) + `▌` (rail face) + trailing space (default face).

### D3. Display layout: per-window prefix swap

Per-window display overlay covers the source `> ` (or bare `>`).
The masked display string is `▌<space>` (or `▌` for the 1-char
bare-`>` form), with the rail in `gfm-pretty-blockquotes-rail-
face` and the trailing space unfaced.  The inset is NOT part of
this string — see D2 for why the gutter lives on the anchor's
`before-string` instead.

Carries `gfm-pretty-display-masked` (this string) and
`gfm-pretty-display-bare` (the source with `gfm-pretty--str-with-
region-bg`) so the variant walker flips it on selection.  Point-
on-line is handled by the engine's reveal walker, which nils
`display` to expose the raw `> ` source — the anchor's gutter
before-string keeps the body alignment stable.

### D4. File layout: extend `gfm-pretty-blockquotes.el` in place

Adding the defcustom, the gutter `before-string`, and the rail
prefix string keeps the file well under callouts' size.

## Iteration history

The change was originally scoped as "tinted rectangle with inset
rail" — every blockquote line would have a tinted background plus
the inset rail, and a per-line rhs after-string would terminate the
bg at a column-defined right edge to give the blockquote a
defined-shape rectangle.

Three problems surfaced during implementation:

1. **Theme-static baked colours.**  First implementation called
   `(face-background 'gfm-pretty-blockquotes-bg-face nil t)` per
   render and inlined the hex string into overlay face specs.
   Theme changes left the rendered overlays with stale tints.
   Fixed by switching to `:inherit FACE` references.  See
   `[[feedback-face-baking-vs-inherit]]`.

2. **Stretch-glyph rhs ineffective under word-wrap.**  First
   attempt at a window-relative right edge used `(space :align-to
   (- right 1))`.  It collapsed to zero width on word-wrapped
   continuation rows — last visual row still rendered ragged.
   Reverted to a literal-space pad sized by `gfm-pretty--simulate-
   wrap`.

3. **`:extend t` does not paint past soft-wrap on continuation
   rows.**  This was the killer.  Even with a correctly-padded
   last visual row, intermediate continuation rows of a word-
   wrapped long blockquote line left default-bg cells between
   their content and the intended right edge — Emacs' `:extend t`
   only paints past EOL on the row containing EOL, and there's no
   overlay mechanism to inject padding into the END of an
   intermediate continuation row.  See `[[feedback-extend-t-
   continuation-rows]]`.

   Three fixes considered:
   - Heavy body display: replace body content with a multi-line
     `display` string containing manual wrap newlines and per-row
     padding.  Complex (font-lock face merging, reveal interactions
     with editing) and out of scope for the visual polish gained.
   - Disable `word-wrap` buffer-locally: char-wrap fills each row
     but breaks words mid-word — readability hit across the whole
     buffer.
   - **Drop the rectangle.**  Keep the inset + rail, abandon the
     tint and rhs after-string.  No ragged edges because no edges.
     User chose this.

The final change is the modest inset + rail; the bg face, anchor
face, and rhs after-string code from the rectangle iteration have
been removed.

## Risks / Trade-offs

- **Buffer-local `tab-width` mid-block changes** → if a user
  changes `tab-width` while a blockquote is on screen and the
  defcustom default resolves at render time, the inset changes on
  the next rebuild but the wrap-prefix on already-laid anchors
  doesn't.  Mitigation: `gfm-pretty-borders--apply-with-anchors`
  re-runs anchors on every full rebuild; `tab-width`-changing edits
  pass through `--full-rebuild-required-p`.  Acceptable lag if the
  user only changes `tab-width` without editing.
