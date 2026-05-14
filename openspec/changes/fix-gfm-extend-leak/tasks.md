## 1. Shared clip primitive

- [ ] 1.1 Add `gfm-block-borders--extend-clip-priority` constant (value 100) to `+gfm-block-borders.el`, with a docstring noting it must outrank `hl-line` (-50) and `region`.
- [ ] 1.2 Add `gfm-block-borders--make-extend-clip (registry beg end)` to `+gfm-block-borders.el`: builds a registry-tagged anchor overlay over `[beg, end)` with `face '(:extend nil)` and `priority` set to the constant. Reuse `gfm-block-borders--make-anchor` so existing teardown applies.

## 2. Apply the clip in the decorators

- [ ] 2.1 In `+gfm-code-fences.el`, call `gfm-block-borders--make-extend-clip` over the body range in the anchor pass for fenced blocks, YAML helmets, and indent blocks (covering every interior newline).
- [ ] 2.2 In `+gfm-callouts.el`, call `gfm-block-borders--make-extend-clip` over the callout body range in `gfm-callouts--apply-block-anchors`.
- [ ] 2.3 Confirm the clip anchor is removed by the existing registry range-removal on scoped and full rebuilds (no new teardown code needed; verify by reading the teardown paths).

## 3. Tests

- [ ] 3.1 Add a test: in a ` ```diff ` fenced block, the merged `:extend` attribute at a `+` body line's newline resolves to nil after a rebuild.
- [ ] 3.2 Add a test: in a callout body line carrying an `:extend t` overlay face, the merged `:extend` at the line's newline resolves to nil.
- [ ] 3.3 Add a narrowing-regression test (tag `narrowing-regression`): the clip anchor set converges across `narrow → rebuild → widen → rebuild` versus a clean widened rebuild, for both fences and callouts.

## 4. Verification

- [ ] 4.1 Run `make test` and confirm all suites pass, including the narrowing-regression tag.
- [ ] 4.2 Manually verify in a markdown buffer: a ` ```diff ` block's `diff-added` / `diff-removed` background stops at the right border; `hl-line` on a callout body line stops at the border.
