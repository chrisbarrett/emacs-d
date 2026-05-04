## Why

Dogfooding the slide-ops MCP surface surfaced two friction points the
slide model itself can't fix:

1. **Pane geometry is wrong for 2-up content.** The presentation frame
   shares horizontal space with the claude-code pane, so a `layout`
   slide splitting horizontally inside Emacs ends up too narrow to
   read. The right move is for the agent to flip tmux to a vertical
   stack (claude-code on top, presentation filling the rest) when the
   slide warrants the extra width.
2. **The user has no way to walk the deck.** Today only the agent can
   move slides via `goto_slide`. If the user wants to flip back to
   re-read an earlier slide while we're chatting, they have to ask.
   That breaks flow during a presentation.

Both problems are about *adapting the surface to what the slide
contains* — geometry follows content, and the user gets local control
to move within the delivered deck.

## What Changes

- Slide spec gains optional `pane_layout` field with values `"tall"`
  or `"wide"` (default: leave geometry alone).  Carried on every
  slide kind.  The hint travels with the slide, so any arrival path
  (`push_slide`, `goto_slide`, `replace_slide`, user-side nav) lands
  in the right geometry.
- On render, the presentation module reconfigures the tmux pane
  geometry to match the hint.  Session-level state tracks the current
  layout so tmux is only invoked on change.
  - `"tall"` — claude-code on top at ~25% screen height,
    presentation below filling the rest.
  - `"wide"` — claude-code on the left at ~33% screen width,
    presentation on the right at ~66%.  This is the default for an
    active session (Emacs gets the lion's share of horizontal space).
- `start_presentation` captures the tmux window's current
  `window_layout` string and stashes it on the session plist as
  `:tmux-saved-layout`.  `end_presentation` restores that layout via
  `tmux select-layout` before its existing teardown step.  This means
  whatever pane shape the user had before the agent reshaped things
  comes back when the session ends.
- New minor mode `+presentation-mode` enabled on every rendered
  buffer (narrative splash, file buffers, diff buffers, both layout
  panes).  Carries buffer-local `+presentation--session-key` so the
  mode can resolve back to the session from any rendered buffer.
- `+presentation-mode` binds:
  - `C-n` / `C-f` → next slide
  - `C-p` / `C-b` → previous slide
  Out-of-range navigation no-ops at deck ends (no wraparound).  The
  bindings are local to the minor mode; they do not shadow defaults
  in non-presentation buffers.
- Mutation ops (`push_slide`, `replace_slide`, `truncate_after`)
  remain agent-only.  User-side keys only navigate; no new MCP tool
  surface.

## Capabilities

### New Capabilities

<!-- none — extends the existing presentation capability -->

### Modified Capabilities

- `presentation`: adds `pane_layout` slide hint with tmux-driven
  geometry switching, and a `+presentation-mode` minor mode that gives
  the user keyboard navigation across the deck.

## Impact

- Modified files:
  - `modules/presentation/init.el` — extend slide spec coercion to
    pass `pane_layout` through; tool descriptions updated.
  - `modules/presentation/lib.el` — `+presentation-mode` minor mode
    and keymap; renderer hook to enable it on every produced buffer;
    tmux geometry effect builder; session `:pane-layout` slot.
  - `modules/presentation/spec.md` — document the new field, the
    minor mode, and key bindings.
  - `modules/presentation/tests.el` — tests for the geometry effect
    planner, idempotence on repeated renders with the same hint, and
    the minor mode's navigation commands at deck boundaries.
- Depends on `add-presentation-slide-ops` being implemented (it is)
  but does not require it to be archived first.
- No new external packages.  Tmux geometry uses the existing effect
  runner (`+presentation-effect-shell`).
- No breaking changes: `pane_layout` is optional; absent → no
  geometry change.  `+presentation-mode` is opt-in via render path
  and only binds keys inside its own keymap.
