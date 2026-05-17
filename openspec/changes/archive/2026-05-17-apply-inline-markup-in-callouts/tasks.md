## 1. Face customisation

- [x] 1.1 In `modules/lang-markdown/init.el`'s `(use-package markdown-mode … :config …)`, neutralise `markdown-blockquote-face` by setting every face attribute (`:family`, `:foundry`, `:width`, `:height`, `:weight`, `:slant`, `:underline`, `:overline`, `:strike-through`, `:box`, `:inverse-video`, `:foreground`, `:background`, `:stipple`, `:extend`, `:inherit`) to `'unspecified`.  Themes like catppuccin set `:foreground`/`:background`/`:slant` directly on the face, so an `:inherit`-only override leaves the foreground (and italic) intact.

## 2. Anchor face

- [x] 2.1 In `modules/lang-markdown/lib/+gfm-callouts.el` at `gfm-callouts--apply-block-anchors`, drop `:inherit default` from the body-line anchor face.  Replace `(:inherit default :background ,tint :extend t)` with `(:background ,tint :extend t)`, and replace `(:inherit default :extend t)` (no-tint branch) with `(:extend t)`.
- [x] 2.2 Re-read the surrounding docstring / comments in `gfm-callouts--apply-block-anchors` and update any line that still claims the anchor strips blockquote face (the suppression now happens at the face layer).

## 3. Regression tests

- [x] 3.1 In `modules/lang-markdown/tests.el`, add a test under `gfm-callouts` that constructs a buffer with a callout body line containing `**bold**`, `*italic*`, `[label](url)`, and `` `code` ``, enables `gfm-mode` + `gfm-callouts-mode`, and asserts the `face` text property at each markup span carries the expected markdown face (`markdown-bold-face`, `markdown-italic-face`, `markdown-link-face`, `markdown-inline-code-face`).  Match the existing test patterns at `tests.el:1478-1483` and `:1652`.
- [x] 3.2 Add a test that asserts the body-line anchor overlay's `face` property does NOT specify `:slant`, `:weight`, `:underline`, `:strike-through`, or `:foreground` (only `:background` and/or `:extend`).
- [x] 3.3 Add a test that asserts `markdown-blockquote-face` resolves to a face spec without `:slant italic` after the module is loaded.

## 4. Verification

- [x] 4.1 Run `make test-quick` and confirm the new tests pass alongside the existing callout suite.
- [x] 4.2 Run `make test` to confirm no regressions across narrowing, per-window rendering, or reveal flows.
- [x] 4.3 Open a real `.md` file containing a callout with inline markup; visually confirm bold/italic/link/code render inside the box at both a graphical frame and (if available) a terminal frame.
