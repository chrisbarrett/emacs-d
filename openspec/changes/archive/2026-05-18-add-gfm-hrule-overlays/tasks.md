## 1. Module scaffold

- [x] 1.1 Create `modules/lang-markdown/lib/+gfm-hrule.el` with the standard module preamble (`;;;` commentary mirroring `+gfm-callouts.el`'s, `(require '+gfm-block-borders)`, `defgroup gfm-hrule`).
- [x] 1.2 Define `+markdown-gfm-hrule-face` (or `gfm-hrule-bar-face`) inheriting `shadow` by default.  Add a docstring noting it is the face used for the unicode bar.

## 2. Discovery

- [x] 2.1 Implement `gfm-hrule--find-blocks` that walks the buffer for ranges where the `markdown-hr` text property is set and the underlying character at the start of the range is `-`.  Skip ranges whose first non-whitespace character is `>`.  Use `widen` for the duration of the scan as in `gfm-callouts--find-blocks-1`.
- [x] 2.2 Implement a `gfm-hrule--blocks-cache` memoised by `buffer-chars-modified-tick` so repeat calls without an edit reuse the cached scan (mirror `gfm-callouts--find-blocks`).

## 3. Rendering

- [x] 3.1 Implement `gfm-hrule--apply-block-display block window` that creates a per-window display overlay over the HR line.  The overlay's `display` is `(make-string (gfm-block-borders--available-width window) ?─)` propertized with `+markdown-gfm-hrule-face`.  Tag the overlay with `gfm-hrule-revealable t` and `evaporate t`.
- [x] 3.2 Implement `gfm-hrule--apply-overlays` that iterates discovered blocks and applies a display overlay per (window × block).  Use `gfm-block-borders--display-windows` as in `+gfm-callouts.el`.
- [x] 3.3 Wire the overlay registry through `gfm-block-borders-registry-for` with mode symbol `gfm-hrule`.

## 4. Lifecycle: scheduler, reconciler, reveal

- [x] 4.1 Wire `gfm-hrule--rebuild`, `gfm-hrule--rebuild-block`, and `gfm-hrule--rebuild-blocks` mirroring the callouts module.  Use `gfm-block-borders-make-reconciler` to build the reconciler context.
- [x] 4.2 Implement `gfm-hrule--schedule-rebuild` and `gfm-hrule--schedule-full-rebuild`, hooked into `after-change-functions` and `window-configuration-change-hook` respectively via the mode toggle.
- [x] 4.3 Implement `gfm-hrule--reveal` on `post-command-hook`, scoped to the selected window via the `window` overlay property check (mirror `gfm-callouts--reveal`).

## 5. Mode definition

- [x] 5.1 Define `gfm-hrule-mode` as an autoloaded `define-minor-mode`.  On enable: register hooks and run an initial rebuild.  On disable: remove hooks, cancel any pending timer, and call the registry's bulk-cleanup.
- [x] 5.2 In `modules/lang-markdown/init.el`'s `markdown-mode` use-package form, add `(gfm-mode-hook . gfm-hrule-mode)` to the `:hook` block alongside the existing `gfm-callouts-mode` / `gfm-code-fences-mode` / `gfm-tables-mode` entries.

## 6. Regression tests

- [x] 6.1 In `modules/lang-markdown/tests.el`, add a `gfm-hrule` test group.  First test: a buffer containing `\nfoo\n\n---\n\nbar\n` enables `gfm-mode`, and after a font-lock-ensure the HR line has a display overlay whose `display` property is a string of `─` repeated to the test window's `window-max-chars-per-line`.
- [x] 6.2 Test that an asterisk-form HR (`\n***\n`) produces no display overlay.
- [x] 6.3 Test that a setext-2 underline (`Heading\n---\n`) produces no display overlay.
- [x] 6.4 Test that an HR inside a fenced code block produces no display overlay.
- [x] 6.5 Test that a `> ---` line inside a blockquote produces no display overlay.
- [x] 6.6 Test reveal: with point on the HR line, the overlay's `display` is nil; moving off the line restores it.
- [x] 6.7 Add a narrowing-regression test under the `narrowing-regression` tag: enable the mode, narrow to a sub-region containing some but not all HR lines, call `gfm-hrule--rebuild`, and assert the overlay set converges across `narrow → rebuild → widen → rebuild` versus a clean widened rebuild.

## 7. Verification

- [x] 7.1 Run `make test-quick` and confirm the new tests pass.
- [x] 7.2 Run `make test` and confirm no regressions across the existing GFM decorator suites.
- [ ] 7.3 Open a real markdown document with multiple HR separators in both a wide and a narrow window split; visually confirm full-width rendering, reveal-on-cursor, and that asterisk-form / setext / fenced-code dashes remain undecorated.
