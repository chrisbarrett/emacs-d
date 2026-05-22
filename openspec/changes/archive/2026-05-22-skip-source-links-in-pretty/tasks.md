## 1. Implement skip predicate and filter

- [x] 1.1 In `lisp/gfm/gfm-pretty-links.el`, define two `rx`
  constants: `gfm-pretty-links--source-range-url-rx` matching
  `<path>#L<digits>[-L<digits>]` and
  `gfm-pretty-links--diff-url-rx` matching
  `diff:<base>...<head>[#<path>]`.
- [x] 1.2 Define `gfm-pretty-links--skip-url-p URL` returning non-nil
  when URL matches either regex.
- [x] 1.3 Extend the filter pass in
  `gfm-pretty-links--blocks-in-range` to drop records whose
  `(gfm-pretty-links--link-url record)` satisfies the skip
  predicate. Place alongside the existing
  `gfm-pretty-links--in-code-p` check.

## 2. Tests

- [x] 2.1 Add ERT tests in `lisp/gfm/gfm-pretty-links-tests.el`
  covering every scenario from the delta spec:
  - inline source-range (range + single line)
  - inline diff (with + without `#path` suffix)
  - reference link resolving to source-range URL
  - plain file link still decorates
  - plain anchor link still decorates

## 3. Verify

- [x] 3.1 Run `make test` and confirm green.
- [x] 3.2 Live-verify in
  `/private/tmp/presentations/2026-05-22T06-33-tg-native-drift-filters.md`:
  with both `gfm-present-mode` and `gfm-pretty-mode` active, inspect
  overlays at a source-range link. Confirm exactly one overlay
  carries `gfm-present` = t and zero overlays carry
  `gfm-pretty-links-class`.
- [x] 3.3 Confirm RET on a source-range link now opens the file
  narrowed to the line range with the focus overlay
  (gfm-present-follow-link path takes over).
- [x] 3.4 Run `openspec validate skip-source-links-in-pretty
  --strict` and confirm zero errors.
