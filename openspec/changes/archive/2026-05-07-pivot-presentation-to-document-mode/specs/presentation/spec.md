## MODIFIED Requirements

### Requirement: Narrative slide rendering

The system SHALL render slides whose `kind` is `"narrative"` from
either an inline `:markdown` string or a `:path` to a file on disk,
exactly one of which SHALL be present.

When `:markdown` is given, the renderer SHALL insert the string into
the splash buffer (`*presentation: <key>*`) and enable the configured
markdown major mode for fontification.

When `:path` is given, the renderer SHALL resolve the path against
the session's worktree (when relative) and call
`find-file-noselect` on the result; the returned buffer SHALL become
the displayed buffer.  The renderer SHALL signal a `user-error`
when the path does not exist.

#### Scenario: render replaces buffer contents atomically (markdown)

- **WHEN** a narrative slide with `:markdown` is rendered
- **THEN** the splash buffer's prior contents are replaced
- **AND** the major mode is set to the configured markdown mode

#### Scenario: render attaches a real file buffer (path)

- **WHEN** a narrative slide with `:path` is rendered
- **AND** the resolved file exists
- **THEN** the displayed buffer is the buffer returned by
  `find-file-noselect` for that path
- **AND** the buffer's `default-directory` matches the file's
  containing directory (so claude-code-ide's project lookup
  routes selections to the project's session)

#### Scenario: render rejects missing path

- **WHEN** a narrative slide with `:path` is rendered
- **AND** the resolved file does not exist
- **THEN** a `user-error` is signalled whose message names the
  missing path
- **AND** no buffer is created and the deck is unchanged

### Requirement: Slide validation

`+presentation--validate-slide` SHALL signal `user-error` for each of
the following invalid inputs without mutating any state:

- Unknown `:kind` value (anything outside `narrative`, `file`,
  `diff`, `layout`).
- A `narrative` slide that has neither `:path` nor `:markdown`, or
  that has both.  When `:markdown` is present it SHALL be a string;
  when `:path` is present it SHALL be a string.
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

#### Scenario: narrative with neither path nor markdown rejected

- **WHEN** `+presentation--validate-slide` is called with a
  narrative slide carrying neither `:path` nor `:markdown`
- **THEN** a `user-error` is signalled

#### Scenario: narrative with both path and markdown rejected

- **WHEN** `+presentation--validate-slide` is called with a
  narrative slide carrying both `:path` and `:markdown`
- **THEN** a `user-error` is signalled

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

## ADDED Requirements

### Requirement: present_document MCP tool

The system SHALL register an MCP tool `present_document` via
`claude-code-ide-make-tool`.

`present_document` SHALL accept the following arguments:

| Name           | Type    | Optional | Notes                                  |
| :------------- | :------ | :------- | :------------------------------------- |
| `worktree`     | string  | no       | Absolute path                          |
| `tmux_session` | string  | no       | tmux session name                      |
| `tmux_window`  | string  | no       | tmux window id or index                |
| `slug`         | string  | no       | Kebab-case identifier; no `/` or whitespace |
| `markdown`     | string  | no       | Document body                          |
| `split`        | string  | yes      | `"horizontal"` (default), `"vertical"` |
| `file_slides`  | array   | yes      | Array of slide specs, each `kind: "file"` |

The tool SHALL compute the document path as

```
<worktree>/.claude/presentations/<YYYY>-<MM>-<DD>T<HH>-<MM>-<slug>.md
```

using the current local time at minute precision, creating
`<worktree>/.claude/presentations/` when absent.

The tool SHALL write `markdown` to the computed path, then invoke
the same code path as `start_presentation` with an `initial_slide`
of `{ kind: "narrative", path: <computed-path> }`, then push each
entry in `file_slides` (in order) via the same code path as
`push_slide`.

The tool SHALL return an alist with keys `key` (session key),
`path` (the computed document path), and `slide_count` (integer
equal to `1 + (length file_slides)`).

The tool SHALL signal a `user-error` without writing any file or
creating any session when:

- `slug` is empty, contains `/`, or contains whitespace.
- Any entry in `file_slides` is not a valid file slide per
  `+presentation--validate-slide`.

#### Scenario: tool registration

- **WHEN** the `presentation` module is initialised
- **THEN** `claude-code-ide-mcp-server-tools` contains an entry
  whose `:name` field is `"present_document"`

#### Scenario: tool composes write + start + pushes

- **WHEN** `present_document` is invoked with two `file_slides`
- **THEN** the effect plan includes (in order) creation of
  `<worktree>/.claude/presentations/` if absent, a write of the
  document body to the computed path, a `start_presentation`
  invocation with `initial_slide` of kind `narrative` and `:path`
  matching the computed path, and two `push_slide` invocations

#### Scenario: tool returns key, path, slide_count

- **WHEN** `present_document` succeeds with `N` `file_slides`
- **THEN** the returned alist has `key` (string), `path` (string),
  and `slide_count` equal to `1 + N`

#### Scenario: invalid slug rejected

- **WHEN** `present_document` is invoked with `slug` of `""`,
  `"foo/bar"`, or `"foo bar"`
- **THEN** a `user-error` is signalled
- **AND** no file is written
- **AND** no session is created

#### Scenario: non-file file_slide rejected

- **WHEN** `present_document` is invoked with a `file_slides`
  entry whose `kind` is not `"file"`
- **THEN** a `user-error` is signalled
- **AND** no file is written
- **AND** no session is created

### Requirement: Markdown link dispatch in narrative buffers

The system SHALL intercept markdown-link follows in narrative
buffers (those whose major mode is `markdown-mode` AND that carry a
non-nil buffer-local `+presentation--session-key`) and dispatch URLs
to deck navigation when their form indicates a deck reference.

Two URL forms SHALL be intercepted:

- `slide:N` (where `N` is a non-negative integer) SHALL invoke
  `goto_slide` with index `N` against the active session.  Indices
  outside `[0, slide_count - 1]` SHALL signal a `user-error`
  through the existing `goto_slide` validation.
- `<path>#L<start>` or `<path>#L<start>-L<end>` SHALL be looked up
  in the active session's deck.  A match is a slide whose `:kind`
  is `"file"`, whose `:path` (resolved against the session's
  worktree) equals the link's path, whose `:start-line` equals
  `<start>`, and whose `:end-line` equals `<end>` (or `<start>`
  when no range was given).  On a match, the system SHALL invoke
  `goto_slide` with the matched index.  On a miss, the system
  SHALL fall back to plain `find-file` of the resolved path
  followed by `goto-line` of `<start>`, with no overlays applied.

All other URL forms (e.g. `https://`, `mailto:`, plain paths
without anchors) SHALL pass through to `markdown-mode`'s default
link-following behaviour unchanged.

The dispatch SHALL be active only when the buffer's major mode is
`markdown-mode` AND `+presentation--session-key` is non-nil.
File-kind, diff-kind, and layout-pane buffers SHALL NOT carry the
dispatch.

#### Scenario: slide:N dispatches to goto_slide

- **WHEN** the user follows a markdown link with URL `slide:2` in
  a narrative buffer for a session whose deck has length 3
- **THEN** `goto_slide` is invoked with index 2
- **AND** the slide at index 2 is rendered as the current slide

#### Scenario: exact path-and-range match dispatches to goto_slide

- **WHEN** the deck contains a file slide with `:path
  modules/auth/init.el`, `:start-line 42`, `:end-line 67`
- **AND** the user follows a markdown link with URL
  `modules/auth/init.el#L42-L67` in the narrative buffer
- **THEN** `goto_slide` is invoked with that slide's index

#### Scenario: non-match falls back to find-file

- **WHEN** the user follows a markdown link with URL
  `modules/auth/init.el#L42-L67`
- **AND** the deck contains no file slide with that exact path and
  range
- **THEN** the system invokes `find-file` on the resolved path
  followed by `goto-line` of 42
- **AND** no presentation overlays are applied to the buffer

#### Scenario: plain URL passes through

- **WHEN** the user follows a markdown link with URL
  `https://example.com/foo` or `modules/auth/init.el` (no anchor)
- **THEN** the link is followed by `markdown-mode`'s default
  handler with no presentation-side dispatch

#### Scenario: dispatch inactive in non-presentation markdown buffers

- **WHEN** a markdown buffer has no buffer-local
  `+presentation--session-key`
- **AND** the user follows a `slide:0` link in that buffer
- **THEN** the link is followed by `markdown-mode`'s default
  handler with no presentation-side dispatch
