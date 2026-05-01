## Why

Reviewing agent work through chat alone is high-friction: the agent describes
code in prose, the user has no anchored visual context, and steering becomes
guesswork on both sides. claude-code-ide already provides an MCP bridge
between agent and Emacs; this change extends that bridge with a *presentation*
surface so an agent can drive a structured, paged narrative inside an Emacs
frame while the user continues to chat normally.

## What Changes

- New module `modules/presentation/` providing per-frame presentation sessions
  keyed by a generated session key.
- Two MCP tools exposed via `claude-code-ide-make-tool`:
  - `start_presentation` — accepts `worktree`, `tmux_session`, `tmux_window`,
    optional `split` (`"horizontal"` default | `"vertical"`), and optional
    `initial_slide`. Returns a session key string.
  - `end_presentation` — accepts the session key. Restores or tears down.
- Effect-interpreter for tmux interaction: pure functions emit shell command
  specifications as data; a single thin runner executes them. Tmux behaviour
  is unit-tested by asserting on emitted commands; no integration test against
  a live tmux.
- Minimum viable slide kind: `narrative` markdown text rendered in a dedicated
  `*presentation: <key>*` buffer. The first change ships this one slide kind
  so the start/end loop is end-to-end useful. Agent ops for managing the
  slide deck (`push_slide` / `replace_slide` / `truncate_after`) are
  explicitly deferred to a follow-up change.
- Frame lifecycle:
  - Discover existing emacsclient frames in the target tmux window by joining
    `tmux list-panes` output (`pane_tty`) against `(frame-parameter f 'tty)`.
    Match → reuse and stash the window-configuration.
  - No match → `tmux split-window` in the target window, command
    `emacsclient -t -s SOCK`. Wait for the new frame via
    `after-make-frame-functions`. Tag origin as `'created`.
  - On `end_presentation`: `'reused` → restore window-config, drop frame
    parameters; `'created` → `tmux kill-pane` on the recorded pane id.
- Robustness:
  - `delete-frame-functions` hook clears state if the user closes a
    presentation frame mid-session.
  - Stashed window-configurations are also pushed to register `?P` as a
    manual escape hatch.
- display-buffer protection: extend the centralised `display-buffer-alist`
  in `modules/ui/init.el` with a predicate that suppresses auto-popups in
  any frame carrying the `presentation-key` frame parameter.

## Capabilities

### New Capabilities

- `presentation`: agent-driven review sessions inside an Emacs frame —
  frame discovery, reuse-vs-create lifecycle, state book-keeping, MCP tool
  surface, splash buffer, and minimal narrative slide rendering.

### Modified Capabilities

<!-- none — claude-code-ide tool registration happens inside the new
     module's `init.el`, not by editing the existing claude module. -->

## Impact

- New files:
  - `modules/presentation/init.el`
  - `modules/presentation/lib.el`
  - `modules/presentation/tests.el`
  - `modules/presentation/packages.eld` (likely empty / no external deps)
  - `modules/presentation/spec.md`
- Modified file: `modules/ui/init.el` — adds one entry to
  `display-buffer-alist` keyed off the `presentation-key` frame parameter.
- Modified file: `init.el` (or wherever modules are loaded) — register the
  new module.
- No external dependencies. Reuses `claude-code-ide-mcp-server` and
  `claude-code-ide-make-tool` from the existing claude module.
