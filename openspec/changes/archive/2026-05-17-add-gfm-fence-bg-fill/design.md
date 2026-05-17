## Context

`gfm-block-borders--right-after` (`+gfm-block-borders.el:143-153`) builds
the fence right-edge after-string: `space :align-to` padding, a
separator, and `│`, all painted with
`(gfm-block-borders--normalised-border-face FACE)`. For code fences
`FACE` is `+markdown-overlay-border-face`, whose background is nil — so
the gap between end-of-text and the border renders default-coloured.
`gfm-block-borders--right-after-overflow` (`:203-223`) is the
wrapped-line variant.

Callouts' `gfm-callouts--right-after` (`+gfm-callouts.el:264-277`)
already takes a `bg` argument and paints its padding with the callout
tint, so callout box interiors are already filled. This change is
code-fences-only.

After the sibling change `fix-gfm-extend-leak`, a `:extend t` background
(for example `diff-added`) is clipped at the right border but still ends
at end-of-text; the gap is default-coloured, so the highlight reads as a
stub rather than a band. Verified live: on a `+ source` diff line the
text-property run `[7948,8042)` is `(diff-added markdown-code-face)` and
`diff-added`'s background is `#c3ebc1`; the body display overlay is
`[7947,8041]` with `after-string` = padding + separator + `│`.

## Goals / Non-Goals

**Goals:**

- A fenced or indent block body line carrying an `:extend t` background
  has its right-edge after-string padding painted with that background,
  so the highlight band reaches the right border.
- No regression when a line carries no `:extend t` background — the gap
  renders exactly as it does today.

**Non-Goals:**

- Callouts — already filled via the tint `bg` argument.
- YAML helmets — the decorator re-fontifies the helmet body itself
  (`gfm-code-fences--fontify-yaml-body`) and that produces no `:extend`
  backgrounds; out of scope.
- Clipping the leak past the border — that is `fix-gfm-extend-leak`.
- Owning fontification or disabling
  `markdown-fontify-code-blocks-natively`.

## Decisions

### Decision: thread an optional `bg` through the right-edge builders

Add `&optional bg` to `gfm-block-borders--right-after` and
`gfm-block-borders--right-after-overflow`. When non-nil, the padding
face gains `:background bg`; nil preserves current behaviour. This
mirrors the `bg` parameter `gfm-callouts--right-after` already carries,
so the shared lib stays consistent.

Alternative considered — a separate filler overlay covering the gap:
there is no buffer text in the gap to hang a `display` space on; the gap
exists only inside the after-string. Rejected.

### Decision: resolve the line background from `face` text properties

In `gfm-code-fences--apply-bordered-display` and the indent display
path, for each body line scan the `face` text properties on the line for
a face that specifies both `:background` and `:extend t`, resolve that
background, and pass it as `bg`. The scan reads what native
fontification has already applied.

Alternative considered — structural diff knowledge (`+` → `diff-added`):
no jit-lock race, but diff-specific and does not generalise. The
text-property scan covers any language whose native fontification yields
an `:extend` background. Rejected as primary mechanism.

### Decision: only fill from `:extend t` faces

A line may carry a `:background` from a non-extending face. Only a
background whose face also has `:extend t` should fill the gap — that is
the background that *would have* leaked past the border, and the one the
reader expects to see span the box. A non-extending background naturally
stops at end-of-text and is left alone.

### Decision: best-effort, degrade gracefully

The text-property scan depends on native fontification having run for
the block. For off-screen or just-edited blocks it may find nothing, in
which case `bg` is nil and the gap stays default-coloured — exactly as
today. This is acceptable because `fix-gfm-extend-leak` already prevents
the *ugly* case (the leak past the border); the fill is pure polish. No
new hooks into jit-lock are added; the existing visible-first reconciler
repaints once fontification settles.

## Risks / Trade-offs

- [Race with jit-lock] → off-screen or just-edited blocks may read no
  face, leaving the gap default-coloured. Mitigation: graceful
  degradation; the sibling clip change already handles correctness, and
  a later rebuild repaints once fontification settles.
- [Per-line face scan cost] → one text-property walk per body line per
  window per rebuild. Body lines are typically few and the walk is over
  a single line; the rebuild is already debounced and visible-first.
  Acceptable.
- [Multiple `:extend` backgrounds on one line] → pick the first face on
  the line specifying `:background` + `:extend t`. Edge case; diff lines
  are uniform.
- [Wrapped diff lines] → `gfm-block-borders--right-after-overflow`
  receives the same `bg`, so continuation rows' padding fills too. Diff
  lines are usually short, so this path is rare but covered.

**Migration Plan:** none — internal overlay change, no configuration or
data involved.

**Open Questions:** none.
