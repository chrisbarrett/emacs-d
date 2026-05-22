## 1. Replace alist lookup with auto-mode-alist resolver

- [ ] 1.1 In `lisp/gfm/gfm-present.el`, add helper
  `gfm-present--major-mode-for-path PATH` that returns the
  major-mode symbol via
  `(assoc-default path auto-mode-alist #'string-match-p)` or
  `#'fundamental-mode` on miss.
- [ ] 1.2 Remove `gfm-present--ext-to-lang` and
  `gfm-present--language-from-extension`.

## 2. Fontify body via temp buffer

- [ ] 2.1 Add helper `gfm-present--fontify-source PATH LINES` that:
  - opens a temp buffer
  - sets `buffer-file-name` to PATH (transient, never written)
  - resolves and calls the major-mode via the helper above
  - inserts the joined LINES
  - calls `font-lock-ensure`
  - returns `(buffer-substring (point-min) (point-max))` (keeps
    text properties)
- [ ] 2.2 Rewrite `gfm-present--source-preview-fence` (rename to
  `gfm-present--source-preview-display`) to:
  - build the header line propertised with `markdown-code-face`
  - call the fontify helper for the body (or substitute sentinel
    for file-not-found / invalid-range)
  - append a `shadow`-propertised footer when EXTRA > 0
  - concatenate header + body + optional footer (newline-joined)
- [ ] 2.3 Update the single call site in
  `gfm-present--render-link-previews` to use the renamed helper.

## 3. Tests

- [ ] 3.1 In `lisp/gfm/gfm-present-tests.el`, retire any test
  asserting language-tag in the fence string.
- [ ] 3.2 Add tests covering every scenario in the delta spec:
  - small range body carries `face` property from major-mode
  - `.yml` extension fontifies via the user's resolved yaml mode
  - unknown extension produces unfontified body without error
  - oversized range produces the truncation footer
  - missing file produces the sentinel
  - buffer-substring saved to disk equals the original markdown link

## 4. Verify

- [ ] 4.1 Run `make test`.
- [ ] 4.2 In `/private/tmp/presentations/2026-05-22T06-33-tg-native-drift-filters.md`,
  enter present mode, navigate to a slide containing a
  source-range link to `drift.core.yml#L13-L22`, confirm the
  preview body shows YAML syntax highlighting and no literal
  `\`\`\`` characters.
- [ ] 4.3 Repeat the check for a link pointing at an `.el` file —
  confirm Lisp fontification.
- [ ] 4.4 Run `openspec validate fontify-source-preview-body
  --strict`.
