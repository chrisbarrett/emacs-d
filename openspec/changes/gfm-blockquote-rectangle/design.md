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

### D1. Window-margin-terminated rectangle, no visible right border

The rectangle's right edge sits one column before the window margin
on every blockquote line:

```
rhs pad: (space :align-to (- right 1))   in bg face
rhs tail: (space :align-to right)        in default face
```

`text-width` from `gfm-pretty--available-width` is consulted for the
wrap-prefix width and for masked/bare reveal accounting, but NOT for
clamping the rectangle's width — the right edge is window-margin-
relative, period.

**Iteration history.** An earlier draft (and an initial
implementation) clamped the rectangle to
`(min (- text-width inset) (max 80 (+ max-content 4)))`, mirroring
callouts.  In practice this produced a stepped right edge under
word-wrap: Emacs' `:extend t` paints past EOL only on the visual
row containing EOL, so continuation rows of a long word-wrapped
blockquote line leave default-bg cells past the wrap point.  With a
clamped right edge, those default cells sit BETWEEN the last
content char and the clamp column → visible "notch" per
continuation row.  Window-margin termination concedes the defined-
shape rectangle but makes the right edge predictable on every line,
which the user prioritised after seeing the ragged shape on a long
blockquote in a presentations file.

**Alternative considered:** keep the clamp + simulate wrap to pad
every visual row.  Rejected — Emacs has no per-visual-row overlay
hook, so emulating per-row padding would require re-rendering the
body text via a multi-line `display` string (tables-style heavy
machinery).  Out of scope for this change.

**Alternative considered:** full-width slab without the default-face
terminator.  Rejected — `:extend t` past EOL is preempted by other
overlays with `:extend t` (`hl-line`, `region`), so without a
default-face terminator at the margin, point-on-line would paint
`hl-line` to the margin and obliterate the bg face.  The 2-cell
after-string (bg pad to `right-1`, default at `right`) is the
minimum needed to keep the right edge stable under foreign-overlay
interaction.

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

### D4. Anchor layout: bg face + before-string gutter + wrap-prefix per line

```elisp
(face         (:inherit gfm-pretty-blockquotes-bg-face :extend t))
(before-string <inset-spaces>                       in default face)
(wrap-prefix   <inset-spaces> + ▌ + " "             propertized via face refs)
```

The anchor face references `gfm-pretty-blockquotes-bg-face' by name
(via `:inherit`) rather than baking `(face-background ...)` into the
spec as a `:background "#xxxxxx"' colour string.  Two reasons:

1. **Theme reactivity.**  A baked colour string is frozen at render
   time; a theme change updates the face's attributes but the
   already-rendered overlays keep their stale colour.  With
   `:inherit` Emacs re-resolves the face at every redisplay, so
   theme changes propagate automatically.
2. **No emphasis clobber.**  The bg face's defface specifies only
   `:background` and `:extend t' — no `:foreground`, `:slant`,
   `:weight`, etc. — so inheriting from it brings ONLY those two
   attributes through.  Emphasis faces on buffer text (bold, italic,
   link, inline code) merge through unclobbered.

`:extend t` is set explicitly on the spec so the bg paints past EOL
until the rhs after-string's default-face terminator suppresses it.

The `before-string` hangs `<inset-spaces>` (default face — no bg) off
the line start so the untinted gutter is a property of the
**anchor**, not the per-window prefix display.  This matters at
reveal time: when point sits on a blockquote line the engine's
reveal nils the per-window display overlay's `display` to expose the
raw `> ` source.  If the inset lived inside the display string,
nilling it would also drop the gutter and body text would jitter
left by `inset-cols` columns.  With the gutter on the anchor's
before-string the inset persists through reveal — the raw `> ` just
appears at column `inset-cols` instead of being replaced by `▌ ` at
the same column.

Wrap-prefix is propertized in three segments:

| Segment | Face |
|---|---|
| `<inset-spaces>` (length = inset-cols) | `default` (no bg) |
| `▌` | `gfm-pretty-blockquotes-rail-face` + bg face's bg |
| ` ` | bg face's bg |

The leading spaces deliberately render with default bg so the inset
*is* visible as an empty gutter on continuation visual rows too.
Continuation rows have no buffer char at column 0 to host the
anchor's before-string, so the inset is baked into the wrap-prefix
itself.

### D5. Display layout: per-window prefix swap + per-line rhs after-string

Per-window display overlay covers the source `> ` (or bare `>`).  The
masked display string is `▌<space>` (or `▌` for the 1-char bare-`>'
form).  Face propertization via `:inherit` (same theme-reactivity
rationale as D4):

| Cell | Face spec |
|---|---|
| `▌` | `(:inherit (gfm-pretty-blockquotes-rail-face gfm-pretty-blockquotes-bg-face))` |
| ` ` | `(:inherit gfm-pretty-blockquotes-bg-face)` |

The inset is **not** part of this string — see D4 for why the
gutter lives on the anchor's `before-string` instead.

Carries `gfm-pretty-display-masked` (this string) and
`gfm-pretty-display-bare` (the source with `gfm-pretty--str-with-
region-bg`) so the variant walker flips it on selection.  Point-on-
line is handled by the engine's reveal walker, which nils `display`
to expose the raw `> ` source — the anchor's gutter before-string
keeps the body alignment stable.

The per-line rhs after-string is identical on every line:

```elisp
(propertize " "
            'display '(space :align-to (- right 1))
            'face '(:inherit gfm-pretty-blockquotes-bg-face :extend t))
(propertize " "
            'display '(space :align-to right)
            'face 'default)
```

No box-width-derived absolute column; no overflow variant; no wrap
simulation.  Termination is always `right - 1` for bg, `right` for
the default-face stopper.  See D1 for the iteration history.

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
