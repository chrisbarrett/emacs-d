## 1. Pretty-links narrowing safety + hook

- [ ] 1.1 In `lisp/gfm/gfm-pretty-links.el`, declare public defvar
  `gfm-pretty-links-after-anchor-jump-functions` (abnormal hook, one
  argument: target buffer position).
- [ ] 1.2 Modify `gfm-pretty-links--jump-to-anchor` so the heading
  search runs under `(save-restriction (widen) ...)`. On a successful
  match, widen the buffer, `push-mark` the click site, `goto-char`
  the target, then `run-hook-with-args` the new hook with the target
  position. On miss, signal `user-error` as before and do not run
  the hook.
- [ ] 1.3 Add ERT tests in `lisp/gfm/gfm-pretty-links-tests.el`:
  - Anchor jump under narrowing finds the off-region heading and
    widens.
  - Hook fires once with the target position on successful jump.
  - Hook does not fire on miss.

## 2. Present-mode subscribes to the hook

- [ ] 2.1 In `lisp/gfm/gfm-present.el`, define
  `gfm-present--after-anchor-jump` taking one arg `target-pos`: call
  `gfm-present--narrow-to-heading-at` with that pos, then
  `gfm-present--render-link-previews`.
- [ ] 2.2 In `gfm-present-mode`'s enable branch, add
  `gfm-present--after-anchor-jump` to
  `gfm-pretty-links-after-anchor-jump-functions` with LOCAL=t.
- [ ] 2.3 In `gfm-present-mode`'s disable branch, remove the same
  function from the buffer-local hook.
- [ ] 2.4 Add `(declare-function gfm-pretty-links--jump-to-anchor ...)`
  / hook-variable forward declaration to satisfy the byte-compiler
  if pretty-links is not loaded at compile time. Confirm load order
  via `modules/lang-markdown/init.el`.
- [ ] 2.5 Add ERT tests in `lisp/gfm/gfm-present-tests.el`:
  - With both modes active in a narrowed buffer, RET on a TOC link
    lands point on the target heading narrowed to that H1's region.
  - Hook function is registered on mode enable and removed on
    disable (use `gfm-pretty-links-after-anchor-jump-functions`
    inspection).

## 3. Verify and integrate

- [ ] 3.1 Run `make test` and confirm green.
- [ ] 3.2 Live-verify in `/private/tmp/presentations/2026-05-22T06-33-tg-native-drift-filters.md`:
  open in presentation mode, narrowed to slide 1, position point on
  each TOC link in turn, press `RET`, confirm point lands on the
  target heading narrowed to its slide region. Test the miss path
  by editing a TOC link to a bogus slug.
- [ ] 3.3 Sanity-check non-present-mode paths: in a plain markdown
  buffer with `gfm-pretty-mode` on, `M-x narrow-to-region` to a
  small region, press `RET` on an anchor link — buffer widens and
  point lands at heading.
- [ ] 3.4 Run `openspec validate fix-anchor-jump-under-narrowing
  --strict` and confirm zero errors.
