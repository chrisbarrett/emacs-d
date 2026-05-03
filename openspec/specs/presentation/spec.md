# presentation Specification

## Purpose

The `presentation` capability enables a Claude Code agent (acting as
presenter) to drive a live, slide-paced demonstration in the user's
running Emacs daemon over MCP.  A session attaches to a tmux window,
either reusing an existing emacsclient frame or spawning a fresh one,
maintains an ordered deck of slides of several kinds (narrative, file,
diff, layout), supports per-slide annotation overlays, and lets the
user step through slides at their own pace while the agent observes
their progress via channel notifications.

## Requirements

### Requirement: Session lifecycle MCP tools

The system SHALL register three MCP tools via
`claude-code-ide-make-tool`: `start_presentation`, `get_presentation`,
and `end_presentation`.

`start_presentation` SHALL accept the following arguments:

| Name            | Type   | Optional | Notes                              |
| :-------------- | :----- | :------- | :--------------------------------- |
| `worktree`      | string | no       | Absolute path                      |
| `tmux_session`  | string | no       | tmux session name                  |
| `tmux_window`   | string | no       | tmux window id or index            |
| `split`         | string | yes      | `"horizontal"` (default), `"vertical"` |
| `initial_slide` | object | yes      | Slide spec, `{ kind, ... }`        |

`start_presentation` SHALL return a string session key.  When
`initial_slide` is provided, it SHALL be stored as deck entry 0 and
rendered as the current slide; otherwise the deck SHALL begin empty
with current slide index `nil`.

`end_presentation` SHALL accept a single `key` string argument and
return a status indicator.  Tear-down SHALL include deletion of all
overlays attached to slides in the deck.

`get_presentation` SHALL accept a single `key` string argument and
return an alist with `key`, `origin` (string), `frame_live` (boolean),
`tmux_pane` (string or nil), `worktree` (string), `started_at`
(float seconds-since-epoch), `slide_count` (integer), and
`current_slide_index` (integer or nil).  Unknown keys SHALL signal a
user-error.

#### Scenario: Tools are registered at module init

- **WHEN** the `presentation` module is initialised
- **THEN** `claude-code-ide-mcp-server-tools` contains entries whose
  `:name` fields are `"start_presentation"`, `"get_presentation"`, and
  `"end_presentation"`

#### Scenario: end_presentation with unknown key

- **WHEN** `end_presentation` is called with a key not present in the
  session store
- **THEN** the system signals a user-error and does not mutate any state

#### Scenario: get_presentation reports deck state

- **WHEN** `get_presentation` is called on a session that has had two
  slides pushed
- **THEN** the returned alist's `slide_count` is 2
- **AND** `current_slide_index` is 1

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
deleted by any means via a `delete-frame-functions` hook.  The hook
SHALL match sessions by `:frame` identity (not by `frame-parameter`),
because tty-client disconnects fire `delete-frame-functions` after the
frame's parameters have already been wiped.

#### Scenario: User closes the frame manually

- **WHEN** the user deletes a frame whose object identity matches the
  `:frame` value of a session in `+presentation--sessions`
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

### Requirement: Deck mutation MCP tools

The system SHALL register four additional MCP tools via
`claude-code-ide-make-tool`: `push_slide`, `replace_slide`,
`truncate_after`, and `goto_slide`.  Each tool SHALL operate against
the session identified by a required `key` argument and SHALL signal a
user-error for unknown keys without mutating any state.

`push_slide` SHALL accept `key` and `slide` (a slide spec object),
append the slide to the session's deck, and return the new slide's
integer index.  Its render and current-slide behaviour is governed by
the `push_slide tool semantics` requirement.

`replace_slide` SHALL accept `key`, `index` (integer), and `slide`,
replace the slide at `index` in place, re-render only when `index`
equals the current slide index, and signal a user-error when `index`
is out of range or when the deck is empty.

`truncate_after` SHALL accept `key` and `index` (integer), drop all
slides whose position is greater than `index`, and — when the prior
current slide index was greater than `index` — set the current slide
index to `index` and re-render that slide.  `index` of `-1` SHALL be
permitted as "drop the entire deck".

`goto_slide` SHALL accept `key` and `index` (integer), re-render the
slide at `index`, set the current slide index, and signal a
user-error when `index` is out of range.

#### Scenario: replace_slide on current index re-renders

- **WHEN** `replace_slide` is invoked with an index equal to the
  session's current slide index
- **THEN** the deck entry at that index is replaced
- **AND** the slide is re-rendered into the presentation frame

#### Scenario: replace_slide on non-current index does not re-render

- **WHEN** `replace_slide` is invoked with an index different from the
  session's current slide index
- **THEN** the deck entry is replaced
- **AND** the presentation frame's displayed slide is unchanged

#### Scenario: truncate_after drops trailing slides

- **WHEN** `truncate_after` is invoked with an index `i` on a deck of
  length `N > i + 1`
- **THEN** the deck length becomes `i + 1`
- **AND** if the prior current slide index was greater than `i`, the
  current slide index becomes `i` and that slide is re-rendered

#### Scenario: goto_slide re-renders without mutating the deck

- **WHEN** `goto_slide` is invoked with a valid index
- **THEN** the deck contents are unchanged
- **AND** the slide at that index is rendered as the current slide
- **AND** the session's current slide index is updated

#### Scenario: Out-of-range index signals user-error

- **WHEN** any deck mutation tool is invoked with an `index` outside
  `[0, slide_count - 1]` (or `[-1, slide_count - 1]` for
  `truncate_after`)
- **THEN** the system signals a user-error
- **AND** the deck and current slide index are unchanged

### Requirement: push_slide tool semantics

`push_slide` SHALL accept `key` (string), `slide` (slide spec
object), and an optional `set_current` (boolean, default `false`).
The tool SHALL append the slide to the session's deck and return
the new slide's integer index.

When `set_current` is `false` or absent, the session's current
slide index and rendered frame SHALL be unchanged.  When
`set_current` is `true`, the session's current slide index SHALL be
set to the new slide's index and the slide SHALL be rendered as the
current view.

`push_slide` SHALL signal a `user-error` for unknown keys without
mutating any state, and SHALL run the same validation as
`+presentation--validate-slide` against the `slide` argument.

#### Scenario: push_slide returns appended index

- **WHEN** `push_slide` is invoked on a session whose deck has
  length N
- **THEN** the slide is stored at index N
- **AND** the tool returns the integer N

#### Scenario: default push leaves view unchanged

- **WHEN** `push_slide` is invoked without `set_current` while the
  user is viewing slide `i`
- **THEN** the user continues to see slide `i` after the call
  returns
- **AND** no render is performed

#### Scenario: set_current true advances and renders

- **WHEN** `push_slide` is invoked with `set_current: true` on a
  session whose deck has length `N`
- **THEN** the slide is appended at index `N`
- **AND** the session's current slide index becomes `N`
- **AND** the slide at index `N` is rendered

### Requirement: Deck recovery via get_deck

The system SHALL register an MCP tool `get_deck` that accepts a
session `key` and returns an object containing `key`,
`current_slide_index` (integer or null), and `slides`: an ordered
array of `{ index, kind, title }` records, one per deck entry.
`title` SHALL be the slide spec's `title` field when present, else
null.  Slide bodies (markdown, file content, annotations) SHALL NOT be
echoed back.

#### Scenario: Empty deck returns empty slides array

- **WHEN** `get_deck` is called on a session whose deck is empty
- **THEN** the returned `slides` array is empty
- **AND** `current_slide_index` is null

#### Scenario: Non-empty deck returns one record per slide

- **WHEN** `get_deck` is called on a session with N slides
- **THEN** the returned `slides` array has N records
- **AND** each record has fields `index`, `kind`, and `title`
- **AND** indexes are contiguous starting at 0 in array order

### Requirement: file slide kind

The system SHALL render slides whose `kind` is `"file"` by opening the
slide's `path` (resolved against the session's worktree when
relative) via `find-file-noselect`, narrowing the visible region to
`[start_line, end_line]` when both are provided, and highlighting the
sub-range given by `focus` (a two-element `[start, end]` line range)
when provided.  The buffer SHALL be set read-only for the duration of
the slide and restored to its prior read-only state when the slide is
left.

#### Scenario: file slide narrows to range

- **WHEN** a `file` slide with `start_line` and `end_line` is rendered
- **THEN** the displayed buffer is narrowed to the inclusive line range
- **AND** the major mode chosen by `auto-mode-alist` is active

#### Scenario: file slide focus highlights sub-range

- **WHEN** a `file` slide with a `focus` field is rendered
- **THEN** point is placed at the start of the focus range
- **AND** an overlay with the `region` face spans the focus range

### Requirement: diff slide kind

The system SHALL render slides whose `kind` is `"diff"` by invoking
`git -C WORKTREE diff [BASE..HEAD] [-- PATH]` via the effect runner
and inserting the output into a per-session diff buffer named
`*presentation-diff: KEY*` in `diff-mode`.  When `base` is provided
without `head`, or `head` without `base`, the system SHALL signal a
user-error.  When neither is provided, the working-tree diff SHALL be
shown.

#### Scenario: working-tree diff

- **WHEN** a `diff` slide is rendered with neither `base` nor `head`
- **THEN** the effect runner is invoked with argv
  `("git" "-C" WORKTREE "diff")`
- **AND** the output is shown in `*presentation-diff: KEY*` in
  `diff-mode`

#### Scenario: range diff with path scope

- **WHEN** a `diff` slide is rendered with `base=B`, `head=H`, and
  `path=P`
- **THEN** the effect runner is invoked with argv
  `("git" "-C" WORKTREE "diff" "B..H" "--" "P")`

#### Scenario: half-specified range is rejected

- **WHEN** a `diff` slide is rendered with exactly one of `base` and
  `head`
- **THEN** the system signals a user-error
- **AND** no git process is spawned

### Requirement: layout slide kind

The system SHALL render slides whose `kind` is `"layout"` by rendering
each child slide in `panes` to its target buffer and then composing
them in the presentation frame via `split-window`, with `split` of
`"horizontal"` producing a side-by-side layout and `"vertical"` a
stacked layout.  `panes` SHALL contain exactly two slide specs; layout
slides SHALL NOT contain layout slides as children.

#### Scenario: horizontal split places panes side-by-side

- **WHEN** a `layout` slide with `split="horizontal"` and two panes is
  rendered
- **THEN** the presentation frame contains two windows positioned
  side-by-side, each displaying the buffer for one pane

#### Scenario: nested layout is rejected

- **WHEN** a `layout` slide whose `panes` array contains a slide of
  `kind="layout"` is pushed
- **THEN** the system signals a user-error
- **AND** the deck is unchanged

#### Scenario: wrong pane count is rejected

- **WHEN** a `layout` slide whose `panes` array does not have length 2
  is pushed
- **THEN** the system signals a user-error
- **AND** the deck is unchanged

### Requirement: Per-slide annotation overlays

Slides of kind `narrative`, `file`, and `diff` SHALL accept an
optional `annotations` array of records `{ line, text, position }`
where `line` is a positive integer line number into the slide's
displayed buffer, `text` is the annotation string, and `position` is
either `"before"` or `"after"` (default `"after"`).  The system SHALL
render annotations as Emacs overlays on the slide's buffer using
`before-string` or `after-string` accordingly, and SHALL delete those
overlays when the slide is left (a different slide becomes current,
the slide is replaced via `replace_slide`, the deck is truncated past
this slide, or the session ends).

#### Scenario: annotation overlay attached at given line

- **WHEN** a slide with an annotation `{ line: 5, text: "T", position:
  "after" }` is rendered
- **THEN** an overlay exists on the slide's buffer at line 5 with an
  `after-string` containing `"T"`

#### Scenario: overlays cleared on slide change

- **WHEN** a slide with annotations is the current slide
- **AND** any of `goto_slide`, `push_slide`, or
  `replace_slide` (when the replacement targets the current slide)
  is invoked
- **THEN** every overlay created by the prior slide's annotations is
  deleted before the next slide is rendered

#### Scenario: invalid line number rejected at validation

- **WHEN** a slide with an annotation whose `line` is not a positive
  integer is pushed
- **THEN** the system signals a user-error
- **AND** the deck is unchanged

### Requirement: Slide validation

`+presentation--validate-slide` SHALL signal `user-error` for each of
the following invalid inputs without mutating any state:

- Unknown `:kind` value (anything outside `narrative`, `file`,
  `diff`, `layout`).
- A `narrative` slide missing `:markdown` or whose `:markdown` is not
  a string.
- A `file` slide missing `:path` or whose `:path` is not a string.
  When `:start-line` and `:end-line` are both present, both SHALL be
  positive integers with `start <= end`.  When `:focus` is present
  it SHALL be a 2-element list of positive integers.
- A `diff` slide where exactly one of `:base` and `:head` is
  supplied.
- A `layout` slide whose `:split` is not `"horizontal"` or
  `"vertical"`, whose `:panes` is not a 2-element sequence, or whose
  panes contain a nested `layout`.
- An annotation whose `:line` is not a positive integer or whose
  `:position` (when present) is not `"before"` / `"after"`.
- A `:pane-layout` field whose value is not `"tall"` or `"wide"`
  (case-sensitive string compare after the standard alist→plist
  coercion).

#### Scenario: unknown kind rejected

- **WHEN** `+presentation--validate-slide` is called with `:kind` of
  `frobnicator`
- **THEN** a `user-error` is signalled whose message names the
  unknown kind

#### Scenario: nested layout rejected

- **WHEN** a `layout` slide's `:panes` contains another `layout` slide
- **THEN** a `user-error` is signalled

#### Scenario: half-specified diff range rejected

- **WHEN** a `diff` slide has `:base` but no `:head` (or vice versa)
- **THEN** a `user-error` is signalled

#### Scenario: invalid pane_layout rejected

- **WHEN** a slide carries `:pane-layout` of `huge`
- **THEN** a `user-error` is signalled whose message names the
  offending value

### Requirement: Slide pane_layout hint

Every slide spec SHALL accept an optional `pane_layout` field.  When
present, its value SHALL be either `"tall"` or `"wide"`; any other
value SHALL be rejected at validation time with a `user-error`.

When a slide is rendered (via `push_slide`, `goto_slide`,
`replace_slide` whose index equals current, or user-driven
navigation), the renderer SHALL compare the slide's `pane_layout` to
the session's currently-applied layout.  When they differ, the
renderer SHALL emit tmux effects that reshape the window:

- `"tall"`: claude-code pane on top at ~25% height, presentation pane
  below filling the remainder.
- `"wide"`: claude-code pane on the left at ~33% width, presentation
  pane on the right at ~66% width.

After the effects run successfully, the session plist's
`:pane-layout` slot SHALL be updated to the new value.  When the
hint matches the current session layout, the renderer SHALL NOT
invoke tmux.

When a slide has no `pane_layout` field, the renderer SHALL leave the
tmux geometry unchanged.

#### Scenario: tall hint reshapes a side-by-side window

- **WHEN** a slide with `pane_layout: "tall"` is rendered
- **AND** the session's `:pane-layout` is `'wide` or `nil`
- **THEN** tmux receives `select-layout main-horizontal` followed by
  `set-window-option main-pane-height 25%`
- **AND** the session's `:pane-layout` becomes `'tall`

#### Scenario: wide hint reshapes a stacked window

- **WHEN** a slide with `pane_layout: "wide"` is rendered
- **AND** the session's `:pane-layout` is `'tall` or `nil`
- **THEN** tmux receives `select-layout main-vertical` followed by
  `set-window-option main-pane-width 33%`
- **AND** the session's `:pane-layout` becomes `'wide`

#### Scenario: matching hint is idempotent

- **WHEN** a slide with `pane_layout: "tall"` is rendered
- **AND** the session's `:pane-layout` is already `'tall`
- **THEN** no tmux effect is emitted
- **AND** the session's `:pane-layout` remains `'tall`

#### Scenario: missing hint leaves geometry alone

- **WHEN** a slide without a `pane_layout` field is rendered
- **THEN** no tmux effect is emitted regardless of the session's
  current `:pane-layout`
- **AND** the session's `:pane-layout` is unchanged

#### Scenario: invalid value rejected at validation

- **WHEN** a slide spec carries `pane_layout: "huge"` is pushed
- **THEN** `+presentation--validate-slide` signals a `user-error`
  whose message names the offending value
- **AND** the deck is not mutated

### Requirement: Tmux window-layout save and restore around session lifetime

`start_presentation` SHALL capture the target tmux window's layout
string (`#{window_layout}`) before splitting any new panes and SHALL
stash it on the session plist as `:tmux-saved-layout`.

`end_presentation` SHALL apply that saved layout via
`tmux select-layout` before its existing teardown step (kill-pane for
`'created` origin, restore-window-config for `'reused` origin).  When
`:tmux-saved-layout` is absent or empty, the restore step SHALL be
skipped without error.

#### Scenario: saved layout restored on end_presentation

- **WHEN** `start_presentation` runs against a window with layout
  string `L0`
- **AND** the agent later pushes slides with mixed `pane_layout`
  hints
- **AND** `end_presentation` is called
- **THEN** tmux receives `select-layout L0` before the kill-pane (or
  restore-window-config) effect

#### Scenario: missing saved layout is tolerated

- **WHEN** `end_presentation` is called on a session whose
  `:tmux-saved-layout` is `nil`
- **THEN** no `select-layout` effect is emitted
- **AND** the existing teardown effects run normally

### Requirement: +presentation-mode minor mode on rendered buffers

The system SHALL provide a buffer-local minor mode
`+presentation-mode`.  Every render path
(`+presentation--render-narrative`, `+presentation--render-file`,
`+presentation--render-diff`, and the per-pane buffers produced by
`+presentation--render-layout`) SHALL set a buffer-local
`+presentation--session-key` to the active session key and enable
`+presentation-mode` on the produced buffer before it is displayed.

`+presentation-mode` SHALL define a keymap binding:

- `C-n` and `C-f` to a `next-slide` command
- `C-p` and `C-b` to a `previous-slide` command

`next-slide` SHALL advance the deck by one position via the existing
`+presentation--deck-goto` helper.  `previous-slide` SHALL retreat by
one.  Both commands SHALL no-op (without error) when the requested
target index is out of `[0, slide_count)`.

The minor mode keymap SHALL take precedence over the buffer's major
mode bindings for the keys it owns; other keys SHALL pass through to
the major mode unchanged.

#### Scenario: file-slide buffer has navigation enabled

- **WHEN** a `file` slide is rendered for session `K`
- **THEN** the resulting file buffer has `+presentation-mode`
  enabled
- **AND** `+presentation--session-key` is buffer-locally bound to `K`

#### Scenario: C-n advances the deck

- **WHEN** point is in a presentation buffer for session `K`
- **AND** the session's current slide index is `i` with `i + 1 <
  slide_count`
- **AND** the user invokes the binding for `C-n`
- **THEN** the session's current slide index becomes `i + 1`
- **AND** the slide at index `i + 1` is rendered

#### Scenario: C-n at the last slide is a no-op

- **WHEN** the user invokes `C-n` while the current slide index
  equals `slide_count - 1`
- **THEN** the current slide index is unchanged
- **AND** no render is performed

#### Scenario: C-p at index 0 is a no-op

- **WHEN** the user invokes `C-p` while the current slide index is `0`
- **THEN** the current slide index remains `0`
- **AND** no render is performed

#### Scenario: layout panes both carry the minor mode

- **WHEN** a `layout` slide is rendered
- **THEN** both pane buffers have `+presentation-mode` enabled
- **AND** both have `+presentation--session-key` set to the session
  key

### Requirement: User-driven navigation emits a channel notification

The system SHALL emit a `notifications/claude/channel` notification
via the MCP transport whenever the current slide index changes due
to user-driven navigation (`+presentation-next-slide` /
`+presentation-previous-slide`).

The notification's `content` SHALL be a human-readable English
sentence describing the move (e.g. `"User advanced to slide 3 of
7."` for forward navigation; `"User retreated to slide 2 of 7."`
for backward).

The notification's `meta` SHALL carry these string-valued keys:

- `key`: the session key.
- `current_slide`: the new index, decimal string.
- `prior_slide`: the old index, decimal string.
- `kind`: the slide kind at the new index (`narrative` / `file` /
  `diff` / `layout`).
- `title`: the slide's title when present; key SHALL be omitted
  otherwise.

When the MCP server is not connected, when channels are not
supported by the connected client, or when the notification call
errors for any reason, navigation SHALL succeed and the failure
SHALL be silently swallowed.

#### Scenario: forward navigation emits channel event

- **WHEN** the user invokes `+presentation-next-slide` from slide
  index 2 to index 3 in a session of 7 slides
- **THEN** an MCP notification with method
  `notifications/claude/channel` is emitted
- **AND** its `params.content` reads
  `"User advanced to slide 3 of 7."`
- **AND** its `params.meta.current_slide` is `"3"`
- **AND** its `params.meta.prior_slide` is `"2"`
- **AND** its `params.meta.key` matches the session key

#### Scenario: backward navigation emits channel event

- **WHEN** the user invokes `+presentation-previous-slide` from
  slide index 3 to index 2
- **THEN** the emitted notification's `content` reads
  `"User retreated to slide 2 of 7."`

#### Scenario: agent-driven mutation does not emit

- **WHEN** `goto_slide` is invoked
- **OR** `push_slide` is invoked with `set_current: true`
- **OR** `replace_slide` is invoked with an index equal to the
  current slide index
- **THEN** no `notifications/claude/channel` notification is emitted

#### Scenario: notification suppressed when MCP unavailable

- **WHEN** user-driven navigation occurs while the MCP transport is
  disconnected
- **THEN** the navigation completes successfully
- **AND** no error is signalled to the user

#### Scenario: title meta key omitted when slide has no title

- **WHEN** the user navigates to a slide whose plist has no `:title`
- **THEN** the emitted notification's `meta` SHALL NOT include a
  `title` key

### Requirement: Channel capability declared by MCP server

The system SHALL ensure the MCP server's initialize-response
declares `experimental.claude/channel: {}` so that Claude Code
registers a channel notification listener for the presentation
session.

The implementation MAY achieve this via an upstream patch to
`claude-code-ide-mcp.el` that exposes an additional-capabilities
extension point, or via local `advice` on the initialize-response
handler.  Either approach SHALL produce the same wire output.

When the underlying MCP package version does not support either
mechanism, the capability SHALL silently not be declared, and
navigation notifications (still emitted by the elisp side) SHALL be
ignored by the client.

#### Scenario: capability appears in initialize response

- **WHEN** the MCP server handles an `initialize` request after
  module load
- **THEN** the response's `result.capabilities.experimental` object
  contains the key `claude/channel` with an empty-object value
</content>
</invoke>