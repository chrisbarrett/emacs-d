## ADDED Requirements

### Requirement: Deck mutation MCP tools

The system SHALL register four additional MCP tools via
`claude-code-ide-make-tool`: `push_slide`, `replace_slide`,
`truncate_after`, and `goto_slide`.  Each tool SHALL operate against
the session identified by a required `key` argument and SHALL signal a
user-error for unknown keys without mutating any state.

`push_slide` SHALL accept `key` and `slide` (a slide spec object),
append the slide to the session's deck, render it as the current
slide, and return the new slide's integer index.

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

#### Scenario: push_slide returns appended index

- **WHEN** `push_slide` is invoked on a session whose deck has length N
- **THEN** the slide is stored at index N
- **AND** the tool returns the integer N
- **AND** the session's current slide index becomes N

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

## MODIFIED Requirements

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
