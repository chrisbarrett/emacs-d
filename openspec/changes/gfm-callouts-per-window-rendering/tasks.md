## 1. Extract shared lib `+gfm-block-borders.el`

- [ ] 1.1 Create `modules/lang-markdown/lib/+gfm-block-borders.el` with `provide '+gfm-block-borders` and a `gfm-block-borders` defgroup.
- [ ] 1.2 Move `gfm-code-fences--simulate-wrap`, `--last-visual-col`, `--wrap-prefix`, `--wrap-prefix-w`, `--normalised-border-face` into the lib under `gfm-block-borders-` prefix.  Keep behaviour identical.
- [ ] 1.3 Move `gfm-code-fences--top-strings`, `--bottom-strings`, `--right-after`, `--right-after-overflow`, `--available-width`, `--text-width`, `--max-line-width`, `--display-windows`, `--in-ranges-p`, `--region-overlaps-p` into the lib.
- [ ] 1.4 Move overlay registry primitives (`--make-anchor`, `--make-display`, `--register`, `--remove-overlays`, `--remove-display-overlays-in-range`, `--remove-display-overlays-for-window`, `--prune-dead-overlays`) into the lib parameterised by a buffer-local registry symbol.
- [ ] 1.5 Move scheduler primitives (`--arm-rebuild-timer`, `--extend-dirty-region`, debounce helpers) into the lib parameterised by callback + buffer-local timer symbol.
- [ ] 1.6 Move window-state tracking (`--window-state`, `--reconcile-windows`, `--rebuild-window-prioritised`, `--pace-window-rebuild`, `--block-visible-p`, `--visible-window-ranges`) into the lib as generic helpers parameterised by a `collect-blocks` callback and a `render-block-for-window` callback.
- [ ] 1.7 Add unit tests for the wrap simulator (zero-width, prefix-larger-than-width edge cases) and width-clamp formula in `modules/lang-markdown/tests.el` (or a new sibling).

## 2. Migrate `+gfm-code-fences.el` to consume the shared lib

- [ ] 2.1 Replace internal calls with `gfm-block-borders-*` equivalents; delete the moved definitions from `+gfm-code-fences.el`.
- [ ] 2.2 Add `(require '+gfm-block-borders)` to `+gfm-code-fences.el`.
- [ ] 2.3 Plumb the fences-specific overlay-registry symbol and timer symbol through the shared scheduler.
- [ ] 2.4 Run `make test` and verify the fences spec tests still pass bit-identically.
- [ ] 2.5 Manual smoke: open `openspec/changes/gfm-callouts-per-window-rendering/proposal.md`, split window 3 ways, resize, edit; confirm fences still render correctly.

## 3. Rewrite `+gfm-callouts.el` core decoration on the shared lib

- [ ] 3.1 Add cached `gfm-callouts--find-blocks` keyed on `buffer-chars-modified-tick`.  Write a failing test for cache identity.  Make it pass.
- [ ] 3.2 Replace ad-hoc `gfm-callouts--block-max-col` with the shared `gfm-block-borders--max-line-width`.
- [ ] 3.3 Adopt the box-width formula `min(text-width, max(80, max-content + 4))`.  Write a failing test that a 60-col window clamps a 100-col-content callout to 60.  Make it pass.
- [ ] 3.4 Refactor overlay creation into anchors (bg face, `> ` → `│ ` substitution, wrap-prefix) and displays (top-leading, top-trailing, per-body-line right-edge, bottom border).
- [ ] 3.5 Use `gfm-block-borders--top-strings` / `--bottom-strings` for the box edges, passing the per-callout `marker-buf-w` as `buffer-width`.
- [ ] 3.6 Use `gfm-block-borders--right-after` for body lines whose visible width is within the content budget; use `--right-after-overflow` when it exceeds, mirroring fences.
- [ ] 3.7 Wire `wrap-prefix` `│ ` on each body line so wrapped content stays inside the box.  Write a failing test that a 200-col body line wraps and the right-edge `│` lands on the wrapped row at column `box-width - 1`.  Make it pass.
- [ ] 3.8 Special-case body-less callouts: bottom border attaches to the marker line's trailing after-string (preserve current behaviour).

## 4. Per-window restriction

- [ ] 4.1 Replace each display overlay's plain `make-overlay` with `gfm-block-borders--make-display` so the `window` property is set.
- [ ] 4.2 Loop `dolist` over `(gfm-block-borders--display-windows)` (or `(list nil)` fallback) when applying display overlays.
- [ ] 4.3 Write a failing test: split-buffer rendering produces N display overlay sets where N = number of windows showing the buffer.  Make it pass.

## 5. Window-configuration reconciler

- [ ] 5.1 Add buffer-local `gfm-callouts--last-window-state`.
- [ ] 5.2 Add `gfm-callouts--schedule-full-rebuild` calling the shared `--arm-rebuild-timer` with a callouts-specific reconcile callback.
- [ ] 5.3 Wire `window-configuration-change-hook` in the minor-mode body.
- [ ] 5.4 Implement reconcile callback using `gfm-block-borders--reconcile-windows` parameterised by `gfm-callouts--collect-blocks` and `gfm-callouts--apply-block-display`.
- [ ] 5.5 Write a failing test: resizing one of two windows replaces only the resized window's display overlays.  Make it pass.

## 6. Visible-first prioritised rebuild

- [ ] 6.1 Wire the shared `--rebuild-prioritised` / `--rebuild-window-prioritised` paths via callouts callbacks.
- [ ] 6.2 Write a failing test: rebuild produces overlays for visible callouts synchronously and defers off-screen ones to the next idle tick.  Make it pass.

## 7. Scoped post-edit rebuild

- [ ] 7.1 Add `gfm-callouts--dirty-region` and `gfm-callouts--extend-dirty-region` (or use the shared lib).
- [ ] 7.2 Add `gfm-callouts--marker-line-ranges` returning per-line ranges for every `> [!TYPE]` line.
- [ ] 7.3 Add `gfm-callouts--region-overlaps-marker-line-p` and `gfm-callouts--region-adjacent-to-callout-p`.
- [ ] 7.4 Implement `gfm-callouts--rebuild-scoped` with the four-way fork from the spec.
- [ ] 7.5 Write failing tests: edit-in-body rebuilds one callout, edit-on-marker rebuilds full, edit-adjacent rebuilds full, edit-outside rebuilds nothing.  Make them pass.

## 8. Per-window cursor reveal

- [ ] 8.1 Update `gfm-callouts--reveal` to match `gfm-code-fences--reveal`: only suppress display when overlay's `window` prop is nil or `eq` to `(selected-window)`.
- [ ] 8.2 Write a failing test: cursor in window A does not reveal source in window B for the same buffer.  Make it pass.

## 9. Documentation & polish

- [ ] 9.1 Update the commentary block at the top of `+gfm-callouts.el` to describe the anchor/display split and the per-window model.
- [ ] 9.2 Update `+gfm-code-fences.el` commentary if any phrasing referred to in-file primitives that have moved.
- [ ] 9.3 Run `make test` end-to-end; address any byte-compile warnings introduced by the new lib.
