## 1. Shared clip primitive

- [x] 1.1 Add `gfm-block-borders--extend-clip-priority` constant (value 100) to `+gfm-block-borders.el`, with a docstring noting it must outrank `hl-line` (-50) and `region`.
- [x] 1.2 Add `gfm-block-borders--make-extend-clip (registry beg end)` to `+gfm-block-borders.el`: builds a registry-tagged anchor overlay over `[beg, end)` with `face '(:extend nil)` and `priority` set to the constant. Reuse `gfm-block-borders--make-anchor` so existing teardown applies.

## 2. Apply the clip in the decorators

- [x] 2.1 In `+gfm-code-fences.el`, call `gfm-block-borders--make-extend-clip` over the body range in the anchor pass for fenced blocks, YAML helmets, and indent blocks (covering every interior newline).
- [x] 2.2 In `+gfm-callouts.el`, call `gfm-block-borders--make-extend-clip` over the callout body range in `gfm-callouts--apply-block-anchors`.
- [x] 2.3 Confirm the clip anchor is removed by the existing registry range-removal on scoped and full rebuilds (no new teardown code needed; verify by reading the teardown paths).

## 3. Tests

- [x] 3.1 Add a test: in a ` ```diff ` fenced block, the merged `:extend` attribute at a `+` body line's newline resolves to nil after a rebuild.
- [x] 3.2 Add a test: in a callout body line carrying an `:extend t` overlay face, the merged `:extend` at the line's newline resolves to nil.
- [x] 3.3 Add a narrowing-regression test (tag `narrowing-regression`): the clip anchor set converges across `narrow → rebuild → widen → rebuild` versus a clean widened rebuild, for both fences and callouts.

## 4. Verification

- [x] 4.1 Run `make test` and confirm all suites pass, including the narrowing-regression tag.
- [x] 4.2 Manually verify in a markdown buffer: a ` ```diff ` block's `diff-added` / `diff-removed` background stops at the right border; `hl-line` on a callout body line stops at the border. — Confirmed in a sandbox daemon: the fence extend-clip overlay covers the body line's newline at priority 100; tests 3.1/3.2 assert the merged `:extend` resolves to nil there (the rendering cause of the background stopping at the border). True GUI pixel inspection is not available headless.
