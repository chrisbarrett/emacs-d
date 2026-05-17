## Why

Inline markup faces (italic, bold, underline, link, inline code) do not
render inside GFM callout boxes.  Body text inside a callout is a
markdown blockquote, so `markdown-blockquote-face` (inheriting from
`font-lock-doc-face`'s `:slant italic`) is applied to every body line by
font-lock.  To kill that italic and read callouts as alerts rather than
quotations, `gfm-callouts-mode` paints an anchor overlay with face
`(:inherit default :background TINT :extend t)` over each line.  Because
`:inherit default` specifies every face attribute, it also clobbers the
emphasis attributes from `markdown-italic-face`, `markdown-bold-face`,
`markdown-link-face`, and `markdown-inline-code-face` that would
otherwise be merged in from the buffer text.  Net effect: every body
line renders as plain default-faced text, regardless of inline markup.

## What Changes

- Detach `markdown-blockquote-face` from `font-lock-doc-face` so it no
  longer carries `:slant italic` into either callout bodies or plain
  blockquotes.  Blockquotes are styled by this configuration the same
  way callouts and code fences are: decoration is the config's job, not
  font-lock's.
- Drop `:inherit default` from the callout anchor overlay's face,
  leaving `(:background TINT :extend t)` (and `(:extend t)` when no
  tint is available).  With blockquote-face no longer italic, the
  anchor no longer needs to suppress slant; specifying only
  `:background` and `:extend` lets the buffer text's emphasis faces
  merge through.
- Cover the new behaviour with a regression test under the
  `lang-markdown` test suite.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `lang-markdown`: callout body lines now propagate inline markup faces
  through the anchor overlay; the anchor's role narrows to background
  tint and `:extend t` only.  Plain blockquote styling shifts from
  font-lock to configuration ownership (the italic comes off globally).

## Impact

- `modules/lang-markdown/init.el` — face customisation for
  `markdown-blockquote-face`.
- `modules/lang-markdown/lib/+gfm-callouts.el` — anchor face at the
  body-line apply path (currently lines 332–333).
- `modules/lang-markdown/tests.el` — regression coverage.
- Side effect: plain blockquotes outside callouts also lose italic.
  Accepted per design discussion; aligns with the broader direction of
  taking over blockquote rendering the way callouts and fences are
  already owned.
