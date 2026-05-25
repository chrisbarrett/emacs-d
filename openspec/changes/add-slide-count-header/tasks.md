## 1. Header-line counter (TDD)

- [ ] 1.1 Write failing test: enabling mode on slide 2 of 5 sets `header-line-format` to `"2/5"`.
- [ ] 1.2 Write failing test: doc with no H1s leaves `header-line-format` nil on enable.
- [ ] 1.3 Add buffer-local `gfm-present--refresh-header` helper computing `<n>/<m>` from `gfm-present--all-h1-positions` + `cl-position`; sets `header-line-format` to the string or nil.
- [ ] 1.4 Call helper from mode-enable branch (gfm-present.el:328-341).
- [ ] 1.5 Clear `header-line-format` on mode-disable branch.
- [ ] 1.6 Tests 1.1 and 1.2 pass.

## 2. Refresh on transitions (TDD)

- [ ] 2.1 Write failing test: `next-slide` updates `2/5` → `3/5`.
- [ ] 2.2 Write failing test: `previous-slide` updates `2/5` → `1/5`.
- [ ] 2.3 Write failing test: `follow-link` to slug of slide 4 updates header to `4/5`.
- [ ] 2.4 Call helper at end of `gfm-present-next-slide`.
- [ ] 2.5 Call helper at end of `gfm-present-previous-slide`.
- [ ] 2.6 Call helper after the slug-branch re-narrow in `gfm-present-follow-link`.
- [ ] 2.7 Call helper from `gfm-present--after-anchor-jump`.
- [ ] 2.8 Call helper from `gfm-present--restore-position`.
- [ ] 2.9 Tests 2.1–2.3 pass.

## 3. Widen remap (TDD)

- [ ] 3.1 Write failing test: `(call-interactively (key-binding (kbd "C-x n w")))` in a `gfm-present-mode` buffer disables the mode and widens.
- [ ] 3.2 Write failing test (with evil normal state active): same gesture disables the mode and widens.
- [ ] 3.3 Add `gfm-present--exit` command that calls `(gfm-present-mode -1)`.
- [ ] 3.4 Add `[remap widen] #'gfm-present--exit` to `gfm-present-mode-map`.
- [ ] 3.5 Confirm the evil `evil-make-overriding-map`/`evil-define-key*` block does not need an explicit `(kbd "C-x n w")` entry (remap flows through overriding map).
- [ ] 3.6 Tests 3.1 and 3.2 pass.

## 4. Verification

- [ ] 4.1 Run `make test-quick` — all tests green.
- [ ] 4.2 Run `make test` — full suite green.
- [ ] 4.3 Manually: open a markdown file with ≥3 H1s, `M-x gfm-present-mode`, step through with `C-n`/`C-p`, follow an in-doc heading link, type `C-x n w`. Verify header counter updates at each step and widen exits the mode.
- [ ] 4.4 `openspec validate add-slide-count-header --strict` passes.
