## Context

The callouts decorator (`lisp/gfm/gfm-pretty-callouts.el`) renders
typed blockquote panels as bordered rectangles with a tinted
background.  Its anchor / per-window display split is the engine
seam every block decorator follows: anchors carry width-independent
props (`:background`, `wrap-prefix`), per-window displays carry
width-dependent props (border strings, prefix swap, right-edge after-
strings).  Reveal flips `display`/`after-string` between paired
`gfm-pretty-display-masked` / `gfm-pretty-display-bare` values when
point or selection overlaps an overlay carrying the engine's
revealable property.

The blockquotes decorator (`lisp/gfm/gfm-pretty-blockquotes.el`) uses
the same seams today but only emits the minimal `> ` → `▌ ` prefix
swap and a `wrap-prefix` anchor — no background, no inset, no right
edge.  This change lifts the callouts pattern to plain blockquotes,
dropping three of four borders so the only border glyph remains the
existing left rail.

Tables' alt-row stripe face (`gfm-pretty-tables-row-alt-face`)
provides the visual reference for the new blockquote background; the
user explicitly wants a face copy, not an inheriting alias, so each
decorator can re-theme independently.

## Goals / Non-Goals

**Goals:**

- Plain blockquotes render as a tinted rectangle with a left rail,
  indistinguishable in *grammar* from callouts (rectangle + rail)
  but distinguishable in *chrome* (no type colour, no border glyphs
  on three sides).
- The rectangle's left edge sits at a configurable inset column
  (default `tab-width`).
- The rectangle's right edge is a column-defined stop derived from
  the callouts box-width formula minus the inset.
- Reveal exposes the raw `> ` source when point or selection
  overlaps a blockquote line, mirroring callouts' masked↔bare swap.
- No engine changes; reuse `gfm-pretty-borders` /
  `gfm-pretty-engine` seams verbatim.

**Non-Goals:**

- Top, right, or bottom border glyphs.  Future-extension only.
- Type-coloured blockquotes.  Callouts own the chromatic axis.
- A separate file for the rectangle logic.  Stays in
  `gfm-pretty-blockquotes.el`.
- Foreground-derived blockquote tint.  Tables-style theme-mode
  variants only (light/dark hex pair).
- Changes to the partition between callouts and plain blockquotes.

## Decisions

### D1. Fixed-width rectangle, no visible right border

Mirror callouts' `(min text-width (max 80 (+ max-content 4)))` shape,
applied to the post-inset available width:

```
box-width = (min (- text-width inset)
                 (max 80 (+ max-content 4)))
```

`text-width` from `gfm-pretty--available-width`; `max-content` from
`gfm-pretty--max-line-width` with prefix length 2 (the `> `).  The
rectangle spans columns `[inset, inset + box-width)`.

**Alternative considered:** full-width slab with `:extend t` and no
right cap.  Rejected — the rectangle reads as a defined shape, not a
horizontal stripe, so a column-stop terminator is worth the rhs
after-string and wrap-simulation cost.  Holding to the callouts
shape also leaves room for a future right-border toggle without
restructuring.

### D2. Inset gutter via `gfm-pretty-blockquotes-inset-cols`
defcustom

A buffer-local-friendly defcustom whose default is the symbol
`tab-width` (evaluated at render time).  Reading from a defcustom
rather than directly from `tab-width` lets users pin the visual
inset independent of code-indent settings — a markdown buffer with
`tab-width 8` would otherwise produce a comically wide gutter; a
`tab-width 2` buffer would barely show the inset.

The defcustom value is consulted once per `--apply-block-anchors` /
`--apply-block-display` pass and threaded into the display strings,
wrap-prefix, and rhs after-string padding.

**Alternative considered:** read `tab-width` directly at render
time.  Rejected — couples visual concern to mode-specific indent
state and leaves no escape hatch for users who want a different
inset.

### D3. Bg face is an independent copy of
`gfm-pretty-tables-row-alt-face`

```elisp
(defface gfm-pretty-blockquotes-bg-face
  '((((background light)) :background "#f6f1e6" :extend t)
    (((background dark))  :background "#262637" :extend t))
  "Tinted background for the plain blockquote rectangle."
  :group 'gfm-pretty-blockquotes)
```

**Alternative considered:** `:inherit gfm-pretty-tables-row-alt-face
:extend t`.  Rejected — couples the two faces permanently; user
explicitly wants a copy so re-theming one does not drag the other.

### D4. Anchor layout: bg face on every line, wrap-prefix on every line

Mirror callouts' anchor:

```elisp
(face       (:background <bg-of-bg-face> :extend t))
(wrap-prefix <inset-spaces> + ▌ + " "  propertized for rail+bg)
```

The anchor face spec extracts only `:background` from
`gfm-pretty-blockquotes-bg-face` so emphasis faces (bold, italic,
links, inline code) on buffer text merge through without being
clobbered by the face's other potential attributes.  `:extend t`
ensures the bg paints past EOL until the rhs after-string's
terminator suppresses it.

Wrap-prefix is propertized in three segments:

| Segment | Face |
|---|---|
| `<inset-spaces>` (length = inset-cols) | `default` (no bg) |
| `▌` | `gfm-pretty-blockquotes-rail-face` + bg face's bg |
| ` ` | bg face's bg |

The leading spaces deliberately render with default bg so the inset
*is* visible as an empty gutter.

### D5. Display layout: per-window prefix swap + per-line rhs after-string

Per-window display overlay covers the source `> ` (or bare `>`).  The
display string is:

```
<inset-spaces>▌<space>
```

with the same per-segment face propertisation as the wrap-prefix.
Carries `gfm-pretty-display-masked` (this string) and
`gfm-pretty-display-bare` (the source with `gfm-pretty--str-with-
region-bg`) so reveal flips it on selection / point-on-line.

Per-window rhs after-string at line-end:

```
(propertize " "
            'display `(space :align-to ,(- inset-cols + box-width - 2))
            'face <bg-with-extend>)
(propertize "  " 'face <bg-with-extend>)
(propertize " "
            'display '(space :align-to right)
            'face 'default)
```

This is callouts' `--right-after` minus the `│` glyph.  The default-
face terminator at the end is load-bearing: it suppresses `:extend
t` leak from the anchor bg + foreign overlays (`hl-line`, `region`)
that would otherwise paint past the rectangle's right edge.

Wrapped lines (`gfm-pretty--simulate-wrap` says the line overflows
`box-width - 2`) use a per-window after-string whose pad length is
computed from the simulated wrap last-visual-col, identical in shape
to `gfm-pretty-callouts--right-after-overflow` but with the prefix
width passed as `(+ inset-cols 2)`.

### D6. Bare `>` continuation lines

Display string for a bare `>` becomes `<inset-spaces>▌` (no trailing
space — matches the 1-char source).  The anchor bg + rhs after-
string still fire, so a bare line renders as a tinted rectangle row
with rail glyph but no body content.

### D7. Reveal: extend masked↔bare to cover inset

The display overlay's `gfm-pretty-display-bare` value is `> ` (or
`>`) propertized with `gfm-pretty--str-with-region-bg` so selecting
across the line drops the inset and rail, exposes the raw source,
and paints region bg through.  The bare variant remains source-width
(2 or 1 chars) — only the *masked* display is wider than the source.

The rhs after-string carries paired `gfm-pretty-after-masked` /
`gfm-pretty-after-bare`; bare variant wraps the pad with
`gfm-pretty--str-with-region-bg` and appends
`gfm-pretty--region-tail` so the rhs follows selection state.

### D8. File layout: extend `gfm-pretty-blockquotes.el` in place

Adding ~120 lines (defcustom, bg face, rewritten anchors/display,
helpers for the rhs after-string + overflow variant) keeps the file
well under callouts' ~900-line size.  Splitting would force a new
shared header, duplicate registry-context wiring, and a new
`require` for no compression win.

## Risks / Trade-offs

- **[Wrap-simulation prefix width is now inset-dependent]** →
  callouts' `gfm-pretty--simulate-wrap` and `--last-visual-col` take
  a prefix-width argument; thread `(+ inset-cols 2)` through every
  call site.  Mitigation: add a unit test asserting that a body line
  whose visible width equals `box-width - 2` does NOT wrap, and one
  asserting that width `(+ box-width - 1)` does wrap on the
  simulated visual width.

- **[`:extend t` leak past the rhs terminator on the wrapped
  visual row]** → callouts hit the same problem; their fix is the
  default-face `(space :align-to right)` after the pipe.  We reuse
  that fix.  Mitigation: regression test asserts the rhs after-
  string ends in a default-face span aligned to `right`.

- **[Existing test churn]** → every assertion in `gfm-pretty-
  tests.el` on the blockquote display string (~6 sites) now expects
  `<inset-spaces>▌<space>` instead of `▌ `.  Mitigation: update
  asserts in lockstep with the impl; the test file already pins the
  rail glyph so the diff is mechanical.

- **[Buffer-local `tab-width` mid-block changes]** → if a user
  changes `tab-width` while a blockquote is on screen and the
  defcustom default resolves at render time, the inset changes on
  the next rebuild but the wrap-prefix on already-laid anchors
  doesn't.  Mitigation: `gfm-pretty-borders--apply-with-anchors`
  re-runs anchors on every full rebuild; a `tab-width`-changing edit
  passes through `--full-rebuild-required-p` because it's an
  arbitrary buffer edit (not a structural-line predicate).  If the
  user *only* changes `tab-width` without editing, they'll see
  stale anchors until the next rebuild — acceptable.

- **[Nested fenced code blocks inside blockquotes]** → fences
  decorator only matches lines starting with the fence marker (no
  `>` prefix), so fenced blocks inside `> `-prefixed lines stay
  owned by the blockquote decorator and render as tinted-bg source
  text.  Tinted code inside a blockquote rectangle is intentional —
  a code fence inside a blockquote is just blockquote content.

## Open Questions

None — all design questions resolved in the explore + grill session
preceding this change.
