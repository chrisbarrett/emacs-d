## Context

`+presentation-mode` is a buffer-local minor mode enabled on every rendered slide buffer (narrative, file, diff, layout panes, splash). Its keymap currently binds `C-n`/`C-f` and `C-p`/`C-b` for navigation. `evil-make-overriding-map` is applied to the entire keymap so bindings work across evil states.

Session teardown lives in `+presentation-end` (`lib.el`), exposed via the `end_presentation` MCP tool. There is no in-Emacs path to call it: if the agent disconnects, the user is stranded with a live presentation frame.

Buffer-local `+presentation--session-key` already resolves a buffer back to its session — the same pattern used by the navigation commands.

## Goals / Non-Goals

**Goals:**

- One keybinding (`C-c q`) inside a presentation buffer ends that session.
- Works in any evil state (normal, insert, visual, …).
- Safe when the buffer outlives its session (frame torn down underneath).

**Non-Goals:**

- Global binding active outside presentation buffers. The minor mode is buffer-local; staying buffer-local keeps the surface tight.
- Confirmation prompt. `C-c q` is a deliberate chord, not a fat-finger key.
- Changing or replacing the `end_presentation` MCP tool. The agent path is unchanged.

## Decisions

### Bind in `+presentation-mode-map`, not globally

`+presentation-mode-map` is already wired through `evil-make-overriding-map` and is enabled on exactly the buffers we care about. A new binding inherits both properties for free. A global binding would require a separate "is any session live?" predicate and would be active in unrelated buffers.

Alternative considered: bind on the session frame via `frame-parameter` rather than buffer. Rejected — every render path already enables the mode on the produced buffer; the buffer-local hook is the simpler and existing extension point.

### `C-c q` (not single-key `q`)

`C-c <letter>` is the conventional user-reserved space in Emacs minor-mode keymaps. Single-key `q` would clash with the major modes underneath (e.g. `diff-mode` binds `q` to `quit-window`).

### New named command `+presentation-quit`

A named, autoloaded `interactive` command is discoverable via `M-x` and stable for tests to assert on. Inline lambda would be cheaper but opaque.

The command resolves the session key via the same buffer-local variable the navigation commands use (`+presentation--session-key`), keeping the resolution pattern uniform.

### No-op on stale key

`+presentation-end` calls `+presentation--get-session` which signals `user-error` for unknown keys. The buffer can outlive its session (frame teardown races, manual `kill-buffer`, etc.). `+presentation-quit` SHALL `gethash` the key first and silently return when absent — typing `C-c q` in a stale buffer is meaningfully equivalent to "session is already gone".

Alternative considered: let `user-error` propagate. Rejected — surfacing a noisy error for an idempotent operation is worse UX than silence.

## Risks / Trade-offs

- **Risk**: User invokes `C-c q` from inside a buffer that is *about* to be torn down (`'created` origin → `tmux kill-pane` deletes the frame). → Mitigation: the `interactive` command frame finishes before the kill-pane effect runs; `+presentation-end` is already proven re-entrant safe via the `delete-frame-functions` hook.
- **Risk**: Major modes that bind `C-c q` (none common in render targets — `markdown-view-mode`, `diff-mode`, `fundamental-mode` do not). → Mitigation: `evil-make-overriding-map` and minor-mode keymap precedence already shadow major-mode bindings for keys in `+presentation-mode-map`.
- **Trade-off**: Buffer-local scope means the binding is unavailable if the user has navigated away from every presentation buffer in the frame. Acceptable — that workflow is already covered by the agent's `end_presentation` path.
