## ADDED Requirements

### Requirement: push_slide set_current opt-in

`push_slide` SHALL accept an optional boolean `set_current` field
(default `false`).  When `set_current` is `true`, the session's
current slide index SHALL be set to the new slide's index and the
slide SHALL be rendered.  When `set_current` is `false` or absent,
the deck SHALL be appended to without changing the current slide
index or re-rendering.

#### Scenario: default push appends without rendering

- **WHEN** `push_slide` is invoked with no `set_current` field on a
  session whose current slide index is `i`
- **THEN** the slide is appended to the deck
- **AND** the session's current slide index remains `i`
- **AND** no render is performed

#### Scenario: set_current true advances and renders

- **WHEN** `push_slide` is invoked with `set_current: true` on a
  session whose deck has length `N`
- **THEN** the slide is appended at index `N`
- **AND** the session's current slide index becomes `N`
- **AND** the slide at index `N` is rendered

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

## MODIFIED Requirements

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
