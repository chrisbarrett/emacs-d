## 1. Tint face scaffolding

- [ ] 1.1 Add five `defface` blocks for the per-type tint faces (`note`, `tip`, `important`, `warning`, `caution`) with empty default specs `'((t))'` and a docstring explaining the `:background`-set-dynamically contract.
- [ ] 1.2 Add a `gfm-pretty-callouts--type-tint-face-alist` mapping each type label (including CRITICAL → caution) to its tint face symbol, mirroring `--type-face-alist` / `--type-body-face-alist`.
- [ ] 1.3 Write a failing ERT asserting all five tint faces are defined and their default `:background` is `unspecified`.

## 2. Refresh helper

- [ ] 2.1 Extend `gfm-pretty-callouts--refresh-body-faces` (consider renaming to `--refresh-faces` and add a `defalias` for back-compat) so each iteration computes the blend once per type and `set-face-attribute`s `:background` on both the body face AND the tint face.
- [ ] 2.2 Write a failing ERT asserting that after a synthetic `+theme-changed-hook` fire (or direct call), each tint face's `:background` equals the corresponding body face's `:background` and equals `(gfm-pretty-callouts--font-lock-tint-bg <header-face>)`.

## 3. Overlay face spec rewrite (RED → GREEN)

- [ ] 3.1 Write failing ERT scanning every callout-tagged overlay in a rendered NOTE+TIP+IMPORTANT+WARNING+CAUTION sample buffer: assert no overlay's `face` plist contains a `:background` key whose value is a string.
- [ ] 3.2 Write failing ERT asserting each anchor overlay's `face` carries `:inherit` containing the corresponding tint face symbol.
- [ ] 3.3 Rewrite `gfm-pretty-callouts--apply-block-anchors`: drop the `(tint ...)` `let` binding; switch `bg-face` to `(:inherit <tint-face> :extend t)` and `wrap` to `(:inherit (<tint-face> <border-face>) :slant normal :weight light)`.
- [ ] 3.4 Rewrite `gfm-pretty-callouts--apply-block-display`: replace every `(gfm-pretty-callouts--upright STR FACE TINT &optional WEIGHT)` call site so the `tint` argument is removed and `--upright`'s face spec uses `:inherit (<tint-face> FACE)` instead of `(:inherit FACE :background TINT)`.
- [ ] 3.5 Update `gfm-pretty-callouts--upright`'s signature: drop the `bg` parameter (or accept it but treat as deprecated no-op).  Update all internal callers in callouts to pass the tint face by name and `:inherit` it.
- [ ] 3.6 Update `gfm-pretty-callouts--right-after` and `--right-after-overflow` similarly: face spec uses `:inherit (<tint-face> FACE)`, no baked `:background`.
- [ ] 3.7 Confirm the RED tests in 3.1 and 3.2 turn GREEN.

## 4. Drop apply-path callers of `--tinted-bg`

- [ ] 4.1 Grep `gfm-pretty-callouts.el` for `gfm-pretty-callouts--tinted-bg` and confirm the only remaining caller is the refresh helper (the blend computation).
- [ ] 4.2 If any apply-path caller remains, rewrite it to consult the tint face.

## 5. Existing test churn

- [ ] 5.1 Update every existing callouts test that asserts the shape of an overlay face spec containing a `:background` colour string: rewrite the expectation to look for `:inherit <tint-face>` instead.
- [ ] 5.2 Add a regression test asserting theme switch propagates without `gfm-pretty-mode` toggle: render a callout, mutate `gfm-pretty-callouts-note-tint-face`'s `:background` via `set-face-attribute`, query the overlay face spec, and assert it still references the face by `:inherit` (i.e. the spec did not bake the old value).

## 6. Visual verification

- [ ] 6.1 In a running Emacs (or sandboxed daemon), open a buffer with one callout of each type, enable `gfm-pretty-mode`, then switch theme via the user's normal theme-switch command.  Confirm every callout's tint updates without toggling the mode.
- [ ] 6.2 Confirm body-text tint (font-lock-applied body face) and panel tint (overlay-applied tint face) stay visually identical post-switch — no drift between them.

## 7. Specs + checks

- [ ] 7.1 Run `openspec validate callouts-theme-reactivity --strict`.
- [ ] 7.2 Run `nix develop --command make test` and confirm zero failures (the `gfm-pretty-callouts-*` test block bears most of the churn).
- [ ] 7.3 Run the narrowing regression tag to confirm the refactor did not regress overlay teardown / rebuild.

## 8. Memory + docs

- [ ] 8.1 If any decision proves surprising (e.g. `:inherit` ordering subtleties between tint face and border face), capture a feedback memory entry before archiving — link to `[[feedback-face-baking-vs-inherit]]`.
