## 1. Slide validation extension

- [x] 1.1 Add failing tests for `pane_layout` validation: accepts
      `"tall"` / `"wide"`; rejects other strings, non-strings, and
      empty strings; absent field is permitted on every kind
- [x] 1.2 Extend `+presentation--validate-slide` to enforce
      `:pane-layout` rules; tests green
- [x] 1.3 Add coercion test confirming snake_case → kebab-case
      `pane_layout` → `:pane-layout` on the MCP boundary

## 2. Tmux geometry effect planner

- [x] 2.1 Add failing tests for `+presentation--pane-layout-effects`:
      `'tall` returns the `select-layout main-horizontal` +
      `set-window-option main-pane-height 25%` argv pair targeted at
      the session's tmux window; `'wide` returns the analogous
      `main-vertical` + `main-pane-width 33%` pair
- [x] 2.2 Implement `+presentation--pane-layout-effects`; tests green
- [x] 2.3 Add failing test that the planner emits no effects when the
      requested layout matches the session's `:pane-layout` slot

## 3. Render integration & idempotence

- [x] 3.1 Add failing test that `+presentation--render-slide` runs
      the layout effects (via the existing runner) when the slide's
      `:pane-layout` differs from the session slot, then writes the
      new value into `:pane-layout` after success
- [x] 3.2 Wire layout effect emission into the render dispatch path,
      before slide-kind dispatch; tests green
- [x] 3.3 Add failing test that a sequence of three slides all with
      `pane_layout: "tall"` invokes tmux exactly once
- [x] 3.4 Add failing test that a slide without `:pane-layout`
      leaves the session slot and tmux untouched

## 4. Save and restore tmux window layout

- [x] 4.1 Add failing test that the spawn-path planner emits a
      `display-message -p '#{window_layout}'` effect early and that
      its output is captured into the session plist as
      `:tmux-saved-layout`
- [x] 4.2 Add failing test for the reuse-path planner emitting the
      same capture (so reused sessions also restore on teardown)
- [x] 4.3 Wire layout capture into `+presentation--plan-spawn` and
      `+presentation--plan-reuse`; tests green
- [x] 4.4 Add failing test that `+presentation-end` emits
      `tmux select-layout -t <window> <saved-layout>` before the
      kill-pane (`'created`) or restore-window-config (`'reused`)
      effect
- [x] 4.5 Add failing test that an empty / nil `:tmux-saved-layout`
      causes the restore step to be skipped without error
- [x] 4.6 Implement the teardown wiring; tests green

## 5. +presentation-mode minor mode

- [x] 5.1 Add failing tests for `+presentation-mode` keymap: `C-n`
      and `C-f` bound to `+presentation-next-slide`; `C-p` and `C-b`
      bound to `+presentation-previous-slide`
- [x] 5.2 Add failing tests for the navigation commands using a
      live session: advance and retreat the deck via
      `+presentation--deck-goto`; no-op at deck ends without signal
- [x] 5.3 Add failing test that `+presentation--session-key` is
      buffer-local and read by the navigation commands
- [x] 5.4 Implement `+presentation-mode` (define-minor-mode), the
      keymap, and the two navigation commands; tests green

## 6. Renderer hookup of the minor mode

- [x] 6.1 Add failing tests asserting that each renderer
      (`narrative`, `file`, `diff`) sets `+presentation--session-key`
      and enables `+presentation-mode` on the produced buffer
- [x] 6.2 Add failing test that `+presentation--render-layout`
      enables the minor mode on both pane buffers
- [x] 6.3 Wire renderers to set the buffer-local key and enable the
      mode before the buffer is returned; tests green
- [x] 6.4 Add failing test that the splash buffer (deck empty) also
      carries the minor mode so the user can navigate once slides
      arrive

## 7. Documentation & polish

- [x] 7.1 Update `modules/presentation/spec.md`: document the
      `pane_layout` field, the layout save/restore behaviour, the
      `+presentation-mode` minor mode, and its key bindings
- [x] 7.2 Update tool descriptions in `modules/presentation/init.el`
      to mention `pane_layout` on slide specs (acceptable values,
      default behaviour)
- [x] 7.3 Run `make test`; fix any byte-compile warnings
- [x] 7.4 `openspec validate add-presentation-layout-and-nav --strict`
