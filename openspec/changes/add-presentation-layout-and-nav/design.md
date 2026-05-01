## Context

The presentation surface today lives in a tmux pane next to the
claude-code pane.  In the common horizontal layout the presentation
pane occupies roughly half the screen width, which is fine for a
single buffer but cramped for `layout` slides that split horizontally
inside Emacs (each Emacs sub-pane gets ~25% of screen width).

User navigation is also one-sided: only the agent can move slides via
`goto_slide`.  If the user wants to glance back at an earlier slide
while we're discussing, they have to ask.  That breaks flow.

Both problems live at the renderer/host boundary: geometry wants to
follow content, and the user wants local control over slide position.

Constraints:
- Stay inside the existing effect runner (`+presentation-effect-shell`)
  for tmux work — no new dependencies, no new transport.
- Don't break the existing `display-buffer-no-window` protection on
  the presentation frame.
- Don't bind keys in random file buffers; the navigation surface MUST
  be scoped to a minor mode that the renderer turns on per buffer.
- Keep mutation server-side: the user's keys navigate but never push,
  replace, or truncate.

## Goals / Non-Goals

**Goals:**
- One optional `pane_layout` field on slide specs that the renderer
  uses to drive tmux geometry.
- Idempotent geometry switching: rendering the same hint twice in a
  row hits tmux exactly once (the first time).
- Save the tmux window's pre-session layout on `start_presentation`
  and restore it on `end_presentation`, so the agent's geometry
  reshaping is fully reversible.
- A `+presentation-mode` minor mode the renderer enables on every
  buffer it produces, providing `C-n` / `C-f` / `C-p` / `C-b`
  navigation that resolves back to the session via a buffer-local
  `+presentation--session-key`.
- Navigation keys no-op at deck ends; no wraparound.

**Non-Goals:**
- New MCP tools.  The agent has `goto_slide`; the user has the minor
  mode.  No third path.
- Configurable layout names beyond `"tall"` and `"wide"`.
- User-driven mutation (push/replace/truncate from keys).
- Auto-revert on slide leave.  Geometry changes when the next slide
  asks for a different layout, otherwise stays put.
- Persisting layout state across sessions.

## Decisions

### Layout names: `"tall"` / `"wide"` (not `"vertical"` / `"horizontal"`)

The slide spec already uses `split: "horizontal"` / `"vertical"` for
the *Emacs* split inside a `layout` slide.  Reusing those names for
the *tmux* split would collide semantically (tmux's "horizontal" is
side-by-side, Emacs' is stacked).  Naming the hint after the
*resulting shape of the presentation pane* avoids the collision:

- `"wide"` — presentation pane is wide (side-by-side; claude-code
  ~33% width on the left, presentation ~66% on the right).
- `"tall"` — presentation pane is tall (stacked; claude-code ~25%
  height on top, presentation below).

Alternative considered: `claude_pane_position: "left"` / `"top"`.
Rejected — the slide author thinks about *their* pane shape, not
about claude-code's position.

### Geometry implemented via `tmux resize-pane` + targeted swap

`"tall"` layout requires:
1. Claude-code pane on top, presentation on bottom.
2. Claude-code at ~25% of the window's height.

If the panes are currently side-by-side, simply resizing won't move
them — we need to change the split direction.  Tmux doesn't have a
single command for "rotate the split"; the standard recipe is
`select-layout` with `even-vertical` / `even-horizontal` or
`main-horizontal` / `main-vertical`.

Tmux's `main-horizontal` and `main-vertical` preset layouts are the
right primitives: each stacks/splits panes around a "main" pane whose
size is controlled by a window option.

- `"tall"` → `select-layout main-horizontal` +
  `set-window-option main-pane-height 25%`.  Claude-code (the main
  pane) sits on top at 25% height; presentation fills the rest.
- `"wide"` → `select-layout main-vertical` +
  `set-window-option main-pane-width 33%`.  Claude-code (the main
  pane) sits on the left at 33% width; presentation gets ~66% on
  the right.

This assumes claude-code is the *first* pane in the window (tmux's
"main" pane).  That matches the current convention where the user
launches claude-code in the original pane and the presentation is
spawned as a second pane.

Effect: the renderer emits two shell effects per layout change — a
`select-layout` followed by a `set-window-option`.  A pure planner
builds both argv lists; the runner executes them in order.

Alternative considered: manual `swap-pane` + `resize-pane` sequence.
Rejected — fragile under N>2 panes, and the user already arranges
their tmux to match this assumption (claude-code + presentation =
two-pane window).  We document the assumption and degrade gracefully
(layout commands no-op if there's only one pane).

### Save and restore tmux window layout around the session

`start_presentation` already runs effects against tmux (list-panes,
split-window).  We extend the spawn-path *and* the reuse-path planners
to first capture the window's layout string via:

```
tmux display-message -p -t <window> '#{window_layout}'
```

The returned opaque string is stashed on the session plist as
`:tmux-saved-layout`.  `end_presentation` consumes it: before the
existing teardown step (kill-pane on `'created`, restore-window-config
on `'reused`), it emits:

```
tmux select-layout -t <window> <saved-layout>
```

…which restores both the pane sizes and the split topology to whatever
the user had pre-session.

This applies regardless of `pane_layout` hints — if the agent never
pushed a `pane_layout` slide, the saved-layout string is still applied
on teardown.  That's intentional: even today's behaviour spawns a new
pane via `split-window` which leaves the window in a different shape;
restoring the layout cleans that up too.

Failure mode: if `display-message` returns empty (window has been
destroyed mid-session), we skip the restore step.  The teardown
proceeds normally.

### Idempotence via session-level `:pane-layout`

Each session plist gains a `:pane-layout` slot tracking the most
recently applied layout (`'tall` / `'wide` / `nil`).  The renderer
compares the slide's hint to the session slot before emitting the
effect; equal → no-op.  This keeps render cost low when an agent
pushes a sequence of slides all marked `"tall"`.

The slot is updated *after* the effect runs successfully.  If the
shell effect errors, the slot stays at its prior value so the next
render retries.

### `+presentation-mode` is a minor mode, not a major mode

A major mode would clobber `diff-mode` / file major modes.  A minor
mode layers cleanly: `M-x diff-mode` is preserved, our keymap binds
`C-n` etc. on top.  The minor mode keymap takes precedence over the
major mode keymap (Emacs' standard ordering), so navigation works in
file and diff buffers without special handling.

Buffer-local `+presentation--session-key` (string) lets each command
resolve back to the session.  Set by the renderer immediately before
enabling the mode, on every buffer it produces — narrative splash,
file buffer, diff buffer, and both layout pane buffers.

### `C-n` / `C-f` (and `C-p` / `C-b`) shadow defaults inside presentation buffers

`C-n` is `next-line` by default; binding it to `next-slide` means
inside a presentation file/diff buffer the user can no longer move
the cursor down with `C-n`.  Acceptable trade-off: presentations are
read-only contexts, line motion via arrow keys / `C-v` / `M-v`
remains.  `C-f` / `C-b` similarly lose forward-char / backward-char.

Alternative considered: `M-n` / `M-p` only.  Rejected — user
specifically asked for the `C-` bindings, and the muscle memory for
"next thing" in narrative contexts is `C-n`.

If this turns out to be too aggressive in practice we can demote to
`M-n` / `M-p` in a follow-up; the change is local to the keymap.

### Navigation no-ops at deck ends, no wraparound

Wraparound surprises (jumping from slide 0 to the last slide on
`C-p` is jarring during a talk).  No-op is quiet and predictable.
We don't beep — Emacs' default `ding` on no-op is enough signal.

## Risks / Trade-offs

- **Tmux assumes 2-pane window.** `main-horizontal` on a window with
  N>2 panes will rearrange unrelated panes.  → Document the
  assumption in the slide spec and degrade gracefully (the user can
  still `goto_slide` past offending slides; no irreversible
  damage).

- **`select-layout main-horizontal` resets pane proportions.**
  Setting `main-pane-height 25%` immediately after fixes the main
  (top) pane, but the bottom pane's internal proportions reset.  →
  Acceptable: presentation pane has no internal split at this layer.

- **Minor mode key shadowing.** `C-n` shadowing `next-line` may
  confuse users who reflexively scroll within a long file slide.  →
  Trade-off accepted (decision above).  Easy to relax later.

- **`+presentation--session-key` staleness.** If a buffer survives
  past `end_presentation` (e.g. a file buffer the user kept open),
  the buffer-local key points at a dead session.  → Navigation
  commands look up the session via the existing
  `+presentation--get-session` which signals `user-error` on unknown
  keys; the user sees a message rather than a crash, and the minor
  mode can disable itself on that signal.

- **Saved layout string is opaque.** Tmux's `window_layout` format
  is not documented as stable across major versions.  → In practice
  it has been stable for years and `select-layout` accepts it
  losslessly.  If a future tmux changes the format, the restore
  becomes a no-op or errors; either way the user's terminal still
  works, just with the post-session pane shape.
