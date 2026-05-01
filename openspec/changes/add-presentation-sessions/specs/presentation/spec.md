## ADDED Requirements

### Requirement: Session lifecycle MCP tools

The system SHALL register two MCP tools via
`claude-code-ide-make-tool`: `start_presentation` and `end_presentation`.

`start_presentation` SHALL accept the following arguments:

| Name            | Type   | Optional | Notes                              |
| :-------------- | :----- | :------- | :--------------------------------- |
| `worktree`      | string | no       | Absolute path                      |
| `tmux_session`  | string | no       | tmux session name                  |
| `tmux_window`   | string | no       | tmux window id or index            |
| `split`         | string | yes      | `"horizontal"` (default), `"vertical"` |
| `initial_slide` | object | yes      | Slide spec, `{ kind, ... }`        |

`start_presentation` SHALL return a string session key.

`end_presentation` SHALL accept a single `key` string argument and
return a status indicator.

#### Scenario: Tools are registered at module init

- **WHEN** the `presentation` module is initialised
- **THEN** `claude-code-ide-mcp-server-tools` contains entries whose
  `:name` fields are `"start_presentation"` and `"end_presentation"`

#### Scenario: end_presentation with unknown key

- **WHEN** `end_presentation` is called with a key not present in the
  session store
- **THEN** the system signals a user-error and does not mutate any state

### Requirement: Frame reuse via tty match

The system SHALL determine whether to reuse or create a presentation
frame by joining `tmux list-panes` output (`pane_tty` field) against
the daemon's frames (filtered on `(frame-parameter f 'tty)`).

#### Scenario: Existing emacsclient frame in target window

- **WHEN** the target tmux window contains a pane whose `pane_tty`
  matches the `'tty` parameter of an existing daemon frame
- **THEN** the system reuses that frame, captures
  `current-window-configuration` into the session state with
  `:origin` `'reused`, and additionally pushes the configuration into
  register `?P`

#### Scenario: No existing emacsclient frame

- **WHEN** no daemon frame's `'tty` parameter matches any pane in the
  target window
- **THEN** the system spawns a new pane via
  `tmux split-window -t SESS:WIN [-h|-v] -- emacsclient -t -s SOCK`,
  identifies the resulting pane and tty by diffing `list-panes`
  before-and-after, locates the corresponding new frame, and tags
  it with `presentation-key` and `presentation-origin 'created`

### Requirement: Effect-interpreter for tmux interaction

The system SHALL model all tmux interaction as data via
`+presentation-effect-shell` and `+presentation-effect-elisp`
records, with a single runner (`+presentation--run-effects`) responsible
for execution.

#### Scenario: Planner emits commands as data

- **WHEN** the spawn path is planned for given session/window/split inputs
- **THEN** the planner returns a list of effect records whose `argv`
  fields equal the expected tmux invocations, without executing them

#### Scenario: Tests assert on argv

- **WHEN** unit tests exercise the planner
- **THEN** they assert directly on emitted `argv` lists; no live `tmux`
  process is invoked

### Requirement: Session state book-keeping

The system SHALL maintain a hash table `+presentation--sessions`
mapping each session key to a plist with at least
`:frame :origin :saved-config :tmux-pane :worktree :started-at`.

The presentation frame SHALL also carry frame parameters
`presentation-key` and `presentation-origin`.

#### Scenario: Created session has tmux-pane and no saved-config

- **WHEN** a session is created via the spawn path
- **THEN** its plist has `:origin 'created`, a non-nil `:tmux-pane`,
  and a nil `:saved-config`

#### Scenario: Reused session has saved-config and no tmux-pane

- **WHEN** a session is created via the reuse path
- **THEN** its plist has `:origin 'reused`, a non-nil `:saved-config`,
  and a nil `:tmux-pane`

### Requirement: Tear-down by origin

The system SHALL tear down sessions according to their origin.

#### Scenario: Reused-origin tear-down

- **WHEN** `end_presentation` is called on a `'reused` session
- **THEN** the system calls `set-window-configuration` with the
  saved configuration on the session's frame, removes the
  `presentation-key` and `presentation-origin` frame parameters,
  and removes the hash entry; the frame remains alive

#### Scenario: Created-origin tear-down

- **WHEN** `end_presentation` is called on a `'created` session
- **THEN** the system emits a `tmux kill-pane -t PANE_ID` effect for
  the recorded pane id, the resulting frame deletion fires
  `delete-frame-functions`, and the hash entry is removed by the hook

### Requirement: Mid-session frame deletion cleanup

The system SHALL clear session state when a presentation frame is
deleted by any means via a `delete-frame-functions` hook.

#### Scenario: User closes the frame manually

- **WHEN** the user deletes a frame carrying a `presentation-key`
  parameter
- **THEN** the matching hash entry is removed
- **AND** a subsequent `end_presentation` call with that key signals
  a user-error

### Requirement: Splash buffer

The system SHALL display a buffer named `*presentation: <key>*` in
the presentation frame, containing at least the session key, the
worktree path, and a placeholder line indicating no slide has yet
been pushed.

#### Scenario: No initial slide

- **WHEN** `start_presentation` is called without `initial_slide`
- **THEN** the splash buffer contains the placeholder line

#### Scenario: With initial slide

- **WHEN** `start_presentation` is called with an `initial_slide`
  of kind `"narrative"` and a `markdown` field
- **THEN** the splash buffer is created with the rendered slide
  contents in place of the placeholder, before being shown to the
  user

### Requirement: Narrative slide rendering

The system SHALL render slides whose `kind` is `"narrative"` by
inserting the slide's `markdown` string into the splash buffer
and enabling the configured markdown major mode for fontification.

#### Scenario: Render replaces buffer contents atomically

- **WHEN** a narrative slide is rendered into the splash buffer
- **THEN** the buffer's prior contents are replaced and the major
  mode is set to the configured markdown mode

### Requirement: display-buffer protection for presentation frames

The `display-buffer-alist` in `modules/ui/init.el` SHALL include a
predicate that matches whenever the selected frame carries a
non-nil `presentation-key` parameter, mapped to a
`display-buffer-no-window` action with `(allow-no-window . t)`.

#### Scenario: Pop-up suppressed in presentation frame

- **WHEN** any `display-buffer` call originates from a frame with a
  `presentation-key` parameter
- **THEN** the call resolves to no window and does not disturb the
  presentation layout
