## Why

With the extend leak clipped (`fix-gfm-extend-leak`), a body line's
`:extend t` background — e.g. `diff-added` in a ` ```diff ` block —
still ends at end-of-text, leaving a default-coloured gap between the
text and the right border `│`. The highlight reads as a stub, not a
band. Painting that gap with the line's own background makes the
highlight fill the box interior and stop cleanly at the border, which is
the appearance the box decoration is meant to convey.

## What Changes

- `+gfm-block-borders.el`: `gfm-block-borders--right-after` and
  `gfm-block-borders--right-after-overflow` gain an optional `bg`
  parameter; when non-nil the right-edge padding is painted with that
  background instead of the border face.
- `+gfm-code-fences.el`: when composing a body line's display overlay,
  resolve the line's `:extend`-carrying background from its `face` text
  properties and pass it as `bg`. When no such background is present,
  behaviour is unchanged (the gap stays default-coloured).
- Add coverage: a ` ```diff ` block's `+` / `-` lines render the
  right-edge padding with the `diff-added` / `diff-removed` background.
- Depends on `fix-gfm-extend-leak` — without the clip, filling the gap
  would still leave the leak past the border.
- Callouts already pass their tint as the `bg` argument, so callout gaps
  are already filled; this change is code-fences-only.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `lang-markdown`: adds a code-fence body background-fill requirement — a
  fenced or indent block body line carrying an `:extend t` background
  has its right-edge padding painted with that background so the
  highlight band reaches the right border.

## Impact

- `modules/lang-markdown/lib/+gfm-block-borders.el` — `right-after` /
  `right-after-overflow` signatures gain an optional `bg` argument
- `modules/lang-markdown/lib/+gfm-code-fences.el` — per-line background
  resolution in the display pass
- `modules/lang-markdown/tests.el` — new coverage
- No new dependencies. Affects only overlay rendering. Best-effort:
  depends on native fontification having run; degrades gracefully to
  unchanged behaviour when faces are absent.
