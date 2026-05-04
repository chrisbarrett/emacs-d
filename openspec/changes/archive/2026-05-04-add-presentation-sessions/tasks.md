## 1. Module scaffold

- [x] 1.1 Create `modules/presentation/` with `init.el`, `lib.el`,
  `tests.el`, `packages.eld`, `spec.md` headers and `provide` forms
- [x] 1.2 Register the module in the main module loader
- [x] 1.3 Add a `defgroup +presentation` and the `+presentation--sessions`
  hash defvar

## 2. Effect data types (TDD)

- [x] 2.1 Add failing test that the planner returns a list of
  `+presentation-effect-shell` and `+presentation-effect-elisp` records
  for a representative spawn-path input
- [x] 2.2 Define `cl-defstruct +presentation-effect-shell` (with `argv`
  field) and `+presentation-effect-elisp` (with `thunk` field)
- [x] 2.3 Add failing test that `+presentation--run-effects` shells out
  via `process-file` for shell effects and calls the thunk for elisp
  effects, returning collected results in order — fake `process-file`
  in the test
- [x] 2.4 Implement `+presentation--run-effects`

## 3. Tmux command builders (TDD, pure)

- [x] 3.1 Add failing test that `+presentation--cmd-list-panes` emits the
  exact `tmux list-panes -t SESS:WIN -F #{pane_id}\t#{pane_tty}` argv
- [x] 3.2 Implement the builder
- [x] 3.3 Add failing tests for `+presentation--cmd-split-window`
  covering both horizontal and vertical splits and the
  `emacsclient -t -s SOCKET` tail
- [x] 3.4 Implement the builder
- [x] 3.5 Add failing test for `+presentation--cmd-kill-pane`
- [x] 3.6 Implement the builder
- [x] 3.7 Add failing test for `+presentation--parse-list-panes-output`
  given a multi-line `pane_id\tpane_tty` string; assert it returns
  an alist `((pane-id . tty) ...)`
- [x] 3.8 Implement the parser

## 4. State store + key generation

- [x] 4.1 Add failing test that `+presentation--make-key` returns a
  fresh, unique string each call
- [x] 4.2 Implement using `gensym` or equivalent
- [x] 4.3 Add failing test that `+presentation--register-session`
  inserts a plist into the hash and tags the frame with
  `presentation-key` and `presentation-origin`
- [x] 4.4 Implement
- [x] 4.5 Add failing test that `+presentation--get-session` returns
  the plist for a valid key and signals a user-error for an unknown key
- [x] 4.6 Implement

## 5. Frame discovery (TDD)

- [x] 5.1 Add failing test that `+presentation--find-frame-by-tty`
  given a tty string returns the matching frame from a faked
  `frame-list`/`frame-parameter` environment
- [x] 5.2 Implement
- [x] 5.3 Add failing test that `+presentation--find-existing-frame`
  joins parsed `list-panes` output against the daemon's frames and
  returns the first match (or nil)
- [x] 5.4 Implement

## 6. Splash buffer

- [x] 6.1 Add failing test that `+presentation--make-splash-buffer`
  returns a buffer named `*presentation: <key>*` containing the key,
  worktree, and a placeholder line
- [x] 6.2 Implement

## 7. Narrative slide rendering (v1)

- [x] 7.1 Add failing test that `+presentation--render-slide`
  with `(:kind "narrative" :markdown "# Hello")` replaces the buffer
  contents and enables `markdown-mode` (or the configured markdown
  major mode)
- [x] 7.2 Implement using `with-current-buffer` + `erase-buffer` +
  `insert` + `(funcall +presentation--narrative-major-mode)`

## 8. Reused-frame path orchestration (TDD)

- [x] 8.1 Add failing test that `+presentation--plan-reuse` given an
  existing-frame discovery result emits an elisp effect that captures
  `current-window-configuration` (faked) and stores it on the session
  plist with `:origin` `'reused`
- [x] 8.2 Implement; ensure the plan also emits an effect to push the
  config to register `?P`

## 9. Created-frame path orchestration (TDD)

- [x] 9.1 Add failing test that `+presentation--plan-spawn` emits, in
  order: a `list-panes` shell effect, a `split-window` shell effect,
  a poll-for-new-pane elisp effect, and a frame-tagging elisp effect
- [x] 9.2 Implement
- [x] 9.3 Add failing test that the post-split pane discovery picks
  the pane that's in `after` but not in `before`
- [x] 9.4 Implement `+presentation--diff-panes`

## 10. start_presentation entry point (TDD)

- [x] 10.1 Add failing test that `+presentation-start` with a payload
  whose tmux discovery yields an existing frame takes the reuse path
  and returns a session key
- [x] 10.2 Add failing test that with no existing frame it takes the
  spawn path and returns a session key
- [x] 10.3 Add failing test that an `initial_slide` in the payload is
  rendered into the splash buffer as part of the same orchestration
  (no intermediate "Awaiting first slide…" flash visible to caller)
- [x] 10.4 Implement `+presentation-start`

## 11. end_presentation entry point (TDD)

- [x] 11.1 Add failing test that `+presentation-end` on a `'reused`
  session emits the `set-window-configuration` effect, drops frame
  params, and removes the hash entry
- [x] 11.2 Add failing test that `+presentation-end` on a `'created`
  session emits a `kill-pane` effect for the recorded pane id
- [x] 11.3 Add failing test that `+presentation-end` on an unknown key
  signals a user-error
- [x] 11.4 Implement

## 12. Frame-deletion cleanup hook

- [x] 12.1 Add failing test that `+presentation--frame-deleted-h`
  removes the matching hash entry when called with a frame carrying
  a known `presentation-key`
- [x] 12.2 Add failing test that the hook is a no-op for frames with
  no `presentation-key` parameter
- [x] 12.3 Implement and wire into `delete-frame-functions` from
  `init.el`

## 13. display-buffer protection

- [x] 13.1 Add failing test that with a faked
  `(frame-parameter (selected-frame) 'presentation-key)` returning
  a non-nil value, `display-buffer-alist`'s presentation predicate
  matches and selects `display-buffer-no-window`
- [x] 13.2 Add the entry to `modules/ui/init.el`'s
  `display-buffer-alist` block, before the side-window rules

## 14. MCP tool registration

- [x] 14.1 In `modules/presentation/init.el`, register both tools via
  `claude-code-ide-make-tool` with names `start_presentation` and
  `end_presentation`, full `:description` text, and `:args` matching
  the spec
- [x] 14.2 Verify with `M-x claude-code-ide-mcp-server-get-tool-names`
  that all three names appear
- [x] 14.3 Add `get_presentation` MCP tool: takes a session key, returns
  an alist `(key origin frame_live tmux_pane worktree started_at)` for
  agent-side diagnostics
- [x] 14.4 Fix `+presentation--frame-deleted-h` to match by `:frame`
  identity rather than `frame-parameter`; tty-client disconnects fire
  the hook after frame params are wiped, so the parameter-based lookup
  silently no-ops

## 15. End-to-end smoke (manual)

- [x] 15.1 From a worktree's tmux window with no existing emacs pane,
  call `start_presentation` with a narrative initial slide; confirm
  the window splits horizontally and the slide renders
- [x] 15.2 Call `end_presentation`; confirm the spawned pane closes
- [x] 15.3 Repeat in a window that already has an emacs pane; confirm
  reuse, that the prior window-configuration is restored on
  `end_presentation`, and that register `?P` holds the saved config
- [x] 15.4 Close the presentation frame manually mid-session; confirm
  the hash entry is cleared and a subsequent `end_presentation` with
  the now-stale key signals a user-error
