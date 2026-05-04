## Why

Presentation sessions can only be ended via the `end_presentation` MCP tool today. If the agent crashes, disconnects, or simply forgets, the user has no in-Emacs escape hatch — they must manually `tmux kill-pane` or restore the window-config from register `?P`. A direct keybinding lets the user reclaim the frame without leaving Emacs.

## What Changes

- Add `C-c q` binding to `+presentation-mode-map` that ends the current session via `+presentation-end`.
- Introduce a new interactive command `+presentation-quit` that resolves the session from buffer-local `+presentation--session-key` and tears it down. No-op when the key is absent or already cleared from the session table.
- Binding takes precedence over evil states (existing `evil-make-overriding-map` already covers the whole keymap).

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `presentation`: extends the `+presentation-mode` minor-mode requirement with a force-quit binding requirement.

## Impact

- `modules/presentation/lib.el`: new `+presentation-quit` command + `C-c q` entry in `+presentation-mode-map`.
- `modules/presentation/spec.md`: keybinding table + function table + testable property.
- `modules/presentation/tests.el`: assertion that the keymap binds `C-c q` to `+presentation-quit` and that the command invokes `+presentation-end` with the buffer-local key.
- No external API change; MCP tool surface unchanged.
