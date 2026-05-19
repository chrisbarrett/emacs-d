## 1. Wiring

- [x] 1.1 Add `adaptive-wrap` to `modules/lang-markdown/packages.eld`.
- [x] 1.2 Add a `use-package adaptive-wrap` block to `modules/lang-markdown/init.el` declaring `adaptive-wrap-prefix-mode` as a deferred autoload.
- [x] 1.3 Add `(gfm-mode-hook . adaptive-wrap-prefix-mode)` to the existing `markdown-mode` `:hook` list in `modules/lang-markdown/init.el`.

## 2. Verification

- [x] 2.1 Open a real GFM buffer (e.g. a project README) with nested list items, blockquotes, paragraphs, callouts, fenced code blocks, and a table. Confirm visual-line continuation rows in lists / blockquotes / paragraphs inherit the source indent.
- [x] 2.2 In the same buffer, confirm callout left-bar wrap, fence-border wrap, and table-border wrap render unchanged (overlay `wrap-prefix` precedence).
- [x] 2.3 Toggle `M-x adaptive-wrap-prefix-mode` off; confirm non-decorator continuation rows fall back to column 0 and decorator wrap-prefix is unaffected.
- [x] 2.4 Run `make test` and confirm no regressions.
