## 1. Right-edge builders accept a background

- [ ] 1.1 Add `&optional bg` to `gfm-block-borders--right-after` in `+gfm-block-borders.el`; when non-nil, the padding/separator face gains `:background bg`. nil preserves current behaviour.
- [ ] 1.2 Add `&optional bg` to `gfm-block-borders--right-after-overflow` with the same semantics.
- [ ] 1.3 Confirm existing callers (`+gfm-code-fences.el` compatibility shims, callouts) still compile and behave unchanged when `bg` is omitted.

## 2. Per-line background resolution in code fences

- [ ] 2.1 Add a helper to `+gfm-code-fences.el` that scans a body line's `face` text properties and returns the background of the first face specifying both `:background` and `:extend t`, or nil.
- [ ] 2.2 In `gfm-code-fences--apply-bordered-display`, call the helper per body line and pass the result as `bg` to `gfm-block-borders--right-after` / `gfm-block-borders--right-after-overflow`.
- [ ] 2.3 In the indent-block display path (`gfm-code-fences--apply-indent-display`), do the same per body line.

## 3. Tests

- [ ] 3.1 Add a test: in a ` ```diff ` fenced block, a `+` body line's right-edge after-string padding carries `diff-added`'s background after a rebuild.
- [ ] 3.2 Add a test: a fenced block body line with no `:extend t` background renders the right-edge padding with the border face (no `:background`).
- [ ] 3.3 Add a test: the per-line background helper returns nil when the line carries only non-extending background faces.

## 4. Verification

- [ ] 4.1 Run `make test` and confirm all suites pass.
- [ ] 4.2 Manually verify in a markdown buffer: a ` ```diff ` block's `+` / `-` lines show a continuous coloured band from the text through the gap to the right border `│`, with nothing past the border.
