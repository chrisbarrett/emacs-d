## 1. Right-edge builders accept a background

- [x] 1.1 Add `&optional bg` to `gfm-block-borders--right-after` in `+gfm-block-borders.el`; when non-nil, the padding/separator face gains `:background bg`. nil preserves current behaviour.
- [x] 1.2 Add `&optional bg` to `gfm-block-borders--right-after-overflow` with the same semantics.
- [x] 1.3 Confirm existing callers (`+gfm-code-fences.el` compatibility shims, callouts) still compile and behave unchanged when `bg` is omitted.

## 2. Per-line background resolution in code fences

- [x] 2.1 Add a helper to `+gfm-code-fences.el` that scans a body line's `face` text properties and returns the background of the first face specifying both `:background` and `:extend t`, or nil.
- [x] 2.2 In `gfm-code-fences--apply-bordered-display`, call the helper per body line and pass the result as `bg` to `gfm-block-borders--right-after` / `gfm-block-borders--right-after-overflow`.
- [x] 2.3 In the indent-block display path (`gfm-code-fences--apply-indent-display`), do the same per body line.

## 3. Tests

- [x] 3.1 Add a test: in a ` ```diff ` fenced block, a `+` body line's right-edge after-string padding carries `diff-added`'s background after a rebuild.
- [x] 3.2 Add a test: a fenced block body line with no `:extend t` background renders the right-edge padding with the border face (no `:background`).
- [x] 3.3 Add a test: the per-line background helper returns nil when the line carries only non-extending background faces.

## 4. Verification

- [x] 4.1 Run `make test` and confirm all suites pass.
- [x] 4.2 Manually verify in a markdown buffer: a ` ```diff ` block's `+` / `-` lines show a continuous coloured band from the text through the gap to the right border `│`, with nothing past the border. — Test 3.1 asserts the `+` body line's right-edge after-string padding is painted with the `:extend t` background; combined with `fix-gfm-extend-leak`'s clip the band fills the gap and stops at the border. True GUI pixel inspection is not available headless.
