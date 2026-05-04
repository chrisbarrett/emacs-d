## 1. Tests (red)

- [ ] 1.1 Add ERT test asserting `+presentation-mode-map` binds `C-c q` to `+presentation-quit`.
- [ ] 1.2 Add ERT test: `+presentation-quit` invoked in a buffer whose `+presentation--session-key` names a registered session calls `+presentation-end` with that key.
- [ ] 1.3 Add ERT test: `+presentation-quit` no-ops without error when buffer-local key is `nil`.
- [ ] 1.4 Add ERT test: `+presentation-quit` no-ops without error when buffer-local key references a key absent from `+presentation--sessions`.

## 2. Implementation (green)

- [ ] 2.1 Add `(define-key map (kbd "C-c q") #'+presentation-quit)` to `+presentation-mode-map` in `modules/presentation/lib.el`.
- [ ] 2.2 Define autoloaded interactive command `+presentation-quit` next to the navigation commands; resolve the session via buffer-local `+presentation--session-key`, guard with `gethash` against `+presentation--sessions`, call `+presentation-end` when present.
- [ ] 2.3 Run `make test-quick` and confirm the new tests pass.

## 3. Module spec sync

- [ ] 3.1 Add `C-c q` row to the keybinding table in `modules/presentation/spec.md` (under the user-navigation section).
- [ ] 3.2 Add `+presentation-quit` row to the function table in `modules/presentation/spec.md`.
- [ ] 3.3 Append testable property covering the new binding + command behaviour.

## 4. Verification

- [ ] 4.1 `make test` passes.
- [ ] 4.2 Manual smoke: start a presentation session via MCP, press `C-c q` from a slide buffer in evil normal state, confirm the frame is torn down and the session entry is gone.
