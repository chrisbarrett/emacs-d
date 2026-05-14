## Why

Inside a bordered GFM block, a face carrying `:extend t` paints its
background past the box's right border all the way to the window edge.
The concrete trigger is a ` ```diff ` fenced block:
`markdown-fontify-code-blocks-natively` copies `diff-mode`'s `diff-added`
/ `diff-removed` faces (both `:extend t`) onto the buffer text —
including the trailing newline — so the highlight leaks past the `│`.
The same leak hits any `:extend t` overlay face (`hl-line`, `region`) on
a callout or fence body line. The decoration is meant to read as a
contained box; the leak breaks that.

## What Changes

- Add a shared extend-clip primitive in `+gfm-block-borders.el`: a
  per-block anchor overlay whose `face` is `(:extend nil)`, spanning the
  block body, at a priority high enough to outrank `hl-line` and
  `region`.
- `+gfm-code-fences.el`: apply the clip over fenced, YAML-helmet, and
  indent block bodies.
- `+gfm-callouts.el`: apply the clip over callout bodies; this also
  neutralises the latent `:extend t` on the callout's own tint
  `bg-face`.
- Add regression coverage asserting `:extend` resolves to nil at
  body-line newlines and that the clip survives a
  narrow → rebuild → widen cycle.
- Non-goal: `+gfm-tables.el`. Tables replace each row with a `display`
  string, so buffer-text `:extend` faces never render; the dead
  `:extend t` on `gfm-tables-active-cell-face` is left for a separate
  hygiene change.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `lang-markdown`: the callout and code-fence bordered-block rendering
  requirements gain an extend-clip clause — a body line's `:extend t`
  background is confined to the box interior and never extends past the
  right border.

## Impact

- `modules/lang-markdown/lib/+gfm-block-borders.el` — new shared helper
- `modules/lang-markdown/lib/+gfm-code-fences.el` — clip applied in the
  anchor pass
- `modules/lang-markdown/lib/+gfm-callouts.el` — clip applied in the
  anchor pass
- `modules/lang-markdown/tests.el` — new regression coverage
- No new dependencies, no user-facing configuration. Affects only
  overlay rendering.
