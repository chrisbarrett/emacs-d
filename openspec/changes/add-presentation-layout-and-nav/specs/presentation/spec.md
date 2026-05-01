## ADDED Requirements

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

## MODIFIED Requirements

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
