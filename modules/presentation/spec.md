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
| `push_slide`         | Append a slide to the deck and render it                 |
| `replace_slide`      | Replace deck[i]; re-render only when i is current        |
| `truncate_after`     | Drop deck[> i]; -1 clears the deck                       |
| `goto_slide`         | Re-render deck[i] without mutating the deck              |
| `get_deck`           | Return key, current_slide_index, slides[]                |

### Slide kinds

| Kind        | Required          | Optional                                                            |
| :---------- | :---------------- | :------------------------------------------------------------------ |
| `narrative` | `markdown`        | `annotations`                                                       |
| `file`      | `path`            | `start_line`, `end_line`, `focus`, `annotations`                    |
| `diff`      | —                 | `path`, (`base`+`head`), `annotations`                              |
| `layout`    | `split`, `panes`  | —                                                                   |

`annotations` is `[{ line, text, position? }, …]`; `position` is `"before"` /
`"after"` (default `"after"`). Layout `split` is `"horizontal"` / `"vertical"`;
`panes` must be exactly two non-layout slides.

### Functions

| Function                                | Purpose                                       |
| :-------------------------------------- | :-------------------------------------------- |
| `+presentation-start`                   | Entry point invoked by `start_presentation`   |
| `+presentation-info`                    | Entry point invoked by `get_presentation`     |
| `+presentation-end`                     | Entry point invoked by `end_presentation`     |
| `+presentation-deck-info`               | Entry point invoked by `get_deck`             |
| `+presentation--coerce-slide`           | Deep alist→plist conversion for MCP payloads  |
| `+presentation--validate-slide`         | Validate a slide spec; signal `user-error`    |
| `+presentation--deck-push`              | Append slide; render; return new index        |
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

`'reused` → restore window-config, drop frame parameters, drop hash entry.
`'created` → emit `tmux kill-pane`; the resulting frame deletion fires
`delete-frame-functions`, which clears the hash entry.

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
