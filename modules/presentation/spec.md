# Presentation Sessions

Agent-driven presentation surface inside Emacs. Eight MCP tools let an external
agent spawn or reuse an emacsclient frame in a target tmux pane and walk the
user through a deck of slides. Slide kinds: `narrative` (markdown), `file`
(buffer narrowed to a range), `diff` (`git diff` in `diff-mode`), `layout`
(two-pane split). Slides may carry inline overlay `annotations` tied to
specific lines.

## Files

| File          | Purpose                                                 |
| :------------ | :------------------------------------------------------ |
| `init.el`     | MCP tool registration, hook wiring, defcustoms          |
| `lib.el`      | Pure planning, parsers, command builders, runner, state |
| `tests.el`    | ERT tests covering planners, parsers, state, lifecycle  |
| `packages.eld`| Empty (no external deps)                                |

## External Packages

None. Reuses `claude-code-ide-make-tool` from the claude module.

## Architecture

Tmux interaction is modelled as data. Pure planning functions return lists
of effect records (`+presentation-effect-shell` and
`+presentation-effect-elisp`); a single runner (`+presentation--run-effects`)
executes them. Tests assert on emitted effects rather than running tmux.

State lives in `+presentation--sessions`, a hash keyed by session-key string.
Each entry is a plist with `:frame :origin :saved-config :tmux-pane :worktree
:started-at`. The presentation frame additionally carries `presentation-key`
and `presentation-origin` parameters so the `delete-frame-functions` hook and
`display-buffer-alist` predicates can resolve frame → key cheaply.

## API

### MCP Tools

| Tool                 | Purpose                                                  |
| :------------------- | :------------------------------------------------------- |
| `start_presentation` | Spawn or reuse a frame; return key                       |
| `get_presentation`   | Inspect a session: origin, frame_live, deck position, …  |
| `end_presentation`   | Tear down a session by key                               |
| `push_slide`         | Append a slide; render only with `set_current: true`     |
| `replace_slide`      | Replace deck[i]; re-render only when i is current        |
| `truncate_after`     | Drop deck[> i]; -1 clears the deck                       |
| `goto_slide`         | Re-render deck[i] without mutating the deck              |
| `get_deck`           | Return key, current_slide_index, slides[]                |

### Slide kinds

| Kind        | Required          | Optional                                                            |
| :---------- | :---------------- | :------------------------------------------------------------------ |
| `narrative` | `markdown`        | `annotations`, `pane_layout`                                        |
| `file`      | `path`            | `start_line`, `end_line`, `focus`, `annotations`, `pane_layout`     |
| `diff`      | —                 | `path`, (`base`+`head`), `annotations`, `pane_layout`               |
| `layout`    | `split`, `panes`  | `pane_layout`                                                       |

`annotations` is `[{ line, text, position? }, …]`; `position` is `"before"` /
`"after"` (default `"after"`). Layout `split` is `"horizontal"` / `"vertical"`;
`panes` must be exactly two non-layout slides.

`pane_layout` is `"tall"` or `"wide"`. When set, the renderer reshapes the
target tmux window before rendering: `"tall"` selects `main-horizontal` with
`main-pane-height 25%` (claude-code on top, presentation below);
`"wide"` selects `main-vertical` with `main-pane-width 33%` (claude-code on
the left, presentation on the right). Geometry switching is idempotent —
consecutive slides with the same hint hit tmux exactly once. Slides without
`pane_layout` leave geometry untouched.

### Functions

| Function                                | Purpose                                       |
| :-------------------------------------- | :-------------------------------------------- |
| `+presentation-start`                   | Entry point invoked by `start_presentation`   |
| `+presentation-info`                    | Entry point invoked by `get_presentation`     |
| `+presentation-end`                     | Entry point invoked by `end_presentation`     |
| `+presentation-deck-info`               | Entry point invoked by `get_deck`             |
| `+presentation--coerce-slide`           | Deep alist→plist conversion for MCP payloads  |
| `+presentation--validate-slide`         | Validate a slide spec; signal `user-error`    |
| `+presentation--deck-push`              | Append slide; render iff `:set-current`       |
| `+presentation--emit-nav-channel`       | Send a `claude/channel` notification          |
| `+presentation--inject-channel-capability` | Filter-return advice payload (capability)  |
| `+presentation--register-channel-capability` | Install the capability advice            |
| `+presentation--deck-replace`           | Replace deck[i]; re-render iff current        |
| `+presentation--deck-truncate`          | Drop deck[> i]; -1 clears                     |
| `+presentation--deck-goto`              | Re-render deck[i]; set current index          |
| `+presentation--render-slide`           | Cleanup + dispatch + display                  |
| `+presentation--render-current`         | Render slide at current index, or splash      |
| `+presentation--render-narrative`       | Renderer for `narrative` slides               |
| `+presentation--render-file`            | Renderer for `file` slides                    |
| `+presentation--render-diff`            | Renderer for `diff` slides                    |
| `+presentation--render-layout`          | Renderer for `layout` slides                  |
| `+presentation--apply-annotations`      | Place annotation overlays on a buffer         |
| `+presentation--cleanup-render-state`   | Delete overlays; run restorers                |
| `+presentation--diff-argv`              | Pure planner for `git diff` argv              |
| `+presentation--cmd-list-panes`         | Build `tmux list-panes` shell effect          |
| `+presentation--cmd-split-window`       | Build `tmux split-window` shell effect        |
| `+presentation--cmd-kill-pane`          | Build `tmux kill-pane` shell effect           |
| `+presentation--parse-list-panes-output`| Parse `pane_id\tpane_tty` lines               |
| `+presentation--diff-panes`             | New panes = after \\ before                   |
| `+presentation--run-effects`            | Execute effect list                           |
| `+presentation--plan-reuse`             | Plan for the reuse path                       |
| `+presentation--plan-spawn`             | Plan for the spawn path                       |
| `+presentation--register-session`       | Insert plist + tag frame                      |
| `+presentation--get-session`            | Hash lookup or `user-error`                   |
| `+presentation--make-key`               | Generate fresh session key                    |
| `+presentation--find-frame-by-tty`      | Frame whose `'tty` parameter matches          |
| `+presentation--find-existing-frame`    | First frame matching any pane in the window   |
| `+presentation--make-splash-buffer`     | Build the `*presentation: KEY*` splash buffer |
| `+presentation--frame-deleted-h`        | Hook: clear hash entry on frame deletion      |
| `+presentation--pane-layout-effects`    | Pure planner for tmux geometry effects        |
| `+presentation--apply-pane-layout`      | Run pane-layout effects + update session slot |
| `+presentation--cmd-window-layout`      | Build `tmux display-message #{window_layout}` |
| `+presentation--cmd-select-layout`      | Build `tmux select-layout` shell effect       |
| `+presentation--enable-mode-in`         | Set session key + enable `+presentation-mode' |
| `+presentation-next-slide`              | User command: advance the deck                |
| `+presentation-previous-slide`          | User command: retreat the deck                |

## Deck model

Each session plist carries `:deck` (vector of slide plists),
`:current-slide-index` (integer or nil), and `:render-state` (per-slide
bookkeeping: overlays, narrowing/read-only restorers). Mutations are
expressed via `+presentation--deck-*` helpers; each calls
`+presentation--render-current` to refresh the frame.

## Render dispatch

`+presentation--render-slide` cleans up the prior slide's render-state, runs
`+presentation--dispatch-slide` (a `pcase` on `:kind`), then applies any
annotations and switches the session frame to the produced buffer. Layout
slides bypass `display-buffer`: the renderer calls `delete-other-windows` +
`split-window` + `set-window-buffer` directly inside the session frame.

## Behaviors

### Frame discovery

Joins parsed `tmux list-panes` output (`pane_id`, `pane_tty`) against
daemon frames filtered on `(frame-parameter f 'tty)`. Match → reuse;
no match → spawn.

### Reuse path

1. Capture `current-window-configuration` on the matching frame.
2. Push it to register `?P` for manual recovery (`C-x r j P`).
3. Tag the frame with `presentation-key` / `presentation-origin 'reused`.
4. Display the splash buffer.

### Spawn path

1. `tmux list-panes` (before).
2. `tmux split-window -h|-v -- emacsclient -t -s SOCKET`.
3. `tmux list-panes` (after); diff against before to discover new pane.
4. Resolve frame by tty (bounded retry).
5. Tag frame; create + display the splash buffer.

### Tear-down

Both origins first run `tmux select-layout -t <window> <saved-layout>` when
the session carries a non-empty `:tmux-saved-layout` (captured by
`start_presentation` via `tmux display-message -p '#{window_layout}'`).
This reverses any geometry reshaping the agent did during the session.
Restore is skipped when the saved layout is `nil` or empty.

`'reused` → restore window-config, drop frame parameters, drop hash entry.
`'created` → emit `tmux kill-pane`; the resulting frame deletion fires
`delete-frame-functions`, which clears the hash entry.

### User navigation

The renderer enables `+presentation-mode` on every produced buffer
(narrative, file, diff, layout pane buffers, and the splash buffer) and
sets the buffer-local `+presentation--session-key` so navigation commands
can resolve back to the session.

| Key                | Command                          |
| :----------------- | :------------------------------- |
| `C-n` / `C-f`      | `+presentation-next-slide`       |
| `C-p` / `C-b`      | `+presentation-previous-slide`   |

Navigation is bounded: `next-slide` at the last index and
`previous-slide` at index 0 are silent no-ops. Mutation (push, replace,
truncate) remains agent-only.

### User-paced flow

`push_slide` defaults to "append, don't disrupt": the slide is added
to the deck but the user's currently rendered slide is unchanged. The
opt-in `set_current: true` field is reserved for "look here now"
moments, where the agent wants to drag the user's view to a freshly
pushed slide. `start_presentation`'s `initial_slide` always renders so
the user sees something on session start.

User-driven navigation (`C-n` / `C-p`) emits a
`notifications/claude/channel` event so the agent learns where the
user is without polling. Agent-driven moves (`goto_slide`,
`push_slide` with `set_current: true`, `replace_slide` of the current
index) do NOT emit — the agent already knows.

Notification payload:

```
method: "notifications/claude/channel"
params.source:           "presentation"
params.content:          "User advanced to slide 3 of 7."
params.meta.key:         "presentation-1981"
params.meta.current_slide: "3"
params.meta.prior_slide:   "2"
params.meta.kind:        "file"
params.meta.title:       "Deck mutation helpers"
```

`meta` values are always strings; `title` is omitted when the slide
has no `:title`. Emission is best-effort: when the MCP server is
disconnected, when channels are not supported, or when the underlying
sender errors, navigation still succeeds and no error is signalled.

### Research-preview caveats

Channel notifications require:

- Claude Code v2.1.80 or later.
- claude.ai login (API-key auth not supported).
- Launching claude with
  `--dangerously-load-development-channels server:claude-code-ide-mcp`
  (the registered server name).

Without these, navigation still works; channel notifications are
silently dropped.

The MCP server's initialize-response capability
`experimental.claude/channel: {}` is registered via local `:filter-
return` advice on `claude-code-ide-mcp--handle-initialize`. An
upstream defcustom-based extension point is the long-term durable
solution; the advice is the v0 implementation and degrades silently
when the upstream symbol is unbound.

### display-buffer protection

`modules/ui/init.el` includes a presentation-frame predicate that maps to
`display-buffer-no-window` so external `display-buffer` calls cannot pop
into a presentation frame.

## Testable Properties

1. Command builders emit exact `argv` lists.
2. Parser turns `pane_id\tpane_tty` lines into `((pane-id . tty) ...)`.
3. Diff returns panes in `after` not in `before`.
4. Runner calls `process-file` for shell effects and the thunk for elisp.
5. Make-key returns a fresh string each call.
6. Register-session puts plist into the hash and tags the frame.
7. Get-session signals `user-error` on unknown key.
8. Find-frame-by-tty matches on `(frame-parameter f 'tty)`.
9. Plan-spawn emits, in order: list-panes, split-window, poll-elisp,
   tag-elisp.
10. Plan-reuse emits an elisp effect that captures and stashes the
    window-configuration.
11. End-reused emits `set-window-configuration` and drops state.
12. End-created emits `kill-pane` for the recorded pane id.
13. End on unknown key signals `user-error`.
14. Frame-deleted hook clears matching entries by `:frame` identity;
    no-op for frames not tracked by any session.
15. MCP tools `start_presentation`, `get_presentation`,
    `end_presentation`, `push_slide`, `replace_slide`, `truncate_after`,
    `goto_slide`, `get_deck` are registered.
16. `+presentation--validate-slide` rejects unknown kinds, missing
    required fields, nested layout, half-specified diff range, and
    non-positive annotation lines.
17. `+presentation--deck-push|replace|truncate|goto` mutate the deck
    in place; out-of-range indexes signal `user-error` with no
    mutation.
18. `+presentation--render-slide` deletes prior overlays + runs
    restorers before dispatching the next slide.
19. `+presentation--render-file` narrows to `start_line..end_line`,
    overlays the `focus` range with the `region` face, and toggles
    `buffer-read-only` with restore-on-leave.
20. `+presentation--diff-argv` produces correct argv for working-tree,
    range, and path-scoped diffs; rejects half-specified ranges.
21. `+presentation--pane-layout-effects` returns the
    `select-layout`+`set-window-option` argv pair for `'tall`/`'wide`
    targeted at the session's tmux window, and emits no effects when
    the requested layout matches the session slot.
22. `+presentation--render-slide` runs the layout effects when the
    slide's `:pane-layout` differs from the session slot, then writes
    the new value into `:pane-layout` after success; slides without
    `:pane-layout` leave both slot and tmux untouched.
23. `+presentation--plan-spawn` and `+presentation--plan-reuse` capture
    `#{window_layout}` into the session plist as `:tmux-saved-layout`.
24. `+presentation-end` runs `tmux select-layout -t <window>
    <saved-layout>` before its existing teardown step; an empty / nil
    saved layout skips the restore without error.
25. `+presentation-mode` keymap binds `C-n`/`C-f` to
    `+presentation-next-slide` and `C-p`/`C-b` to
    `+presentation-previous-slide`; commands no-op at deck ends.
26. Renderers (`narrative`, `file`, `diff`, layout pane buffers, splash)
    set buffer-local `+presentation--session-key` and enable
    `+presentation-mode` on the produced buffer.
27. `+presentation--deck-push` defaults to non-disrupting: appends the
    slide but leaves `:current-slide-index` and the rendered buffer
    untouched. `:set-current t` advances and renders.
28. `push_slide` MCP tool coerces snake_case `set_current` to the
    `:set-current` keyword arg.
29. `+presentation--emit-nav-channel` composes a forward / backward
    `content` string and a string-valued `meta` record (`key`,
    `current_slide`, `prior_slide`, `kind`, optional `title`) and
    sends a `notifications/claude/channel` event via the MCP
    transport. Emission is best-effort: a missing or signalling
    `claude-code-ide-mcp--send-notification` returns nil without
    re-signalling.
30. `+presentation-next-slide` / `+presentation-previous-slide` call
    the emitter post-render; agent-driven mutation
    (`+presentation--deck-goto`, `+presentation--deck-push` with
    `:set-current`, `+presentation--deck-replace`) does not.
31. `+presentation--inject-channel-capability` splices
    `experimental.claude/channel: :json-empty' into the MCP
    initialize-response without clobbering existing experimental
    entries.
32. `+presentation--register-channel-capability` no-ops when the
    upstream MCP handler symbol is unbound.
