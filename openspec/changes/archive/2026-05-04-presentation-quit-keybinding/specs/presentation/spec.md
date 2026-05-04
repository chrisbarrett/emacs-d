## MODIFIED Requirements

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
- `C-c q` to a `quit` command that ends the session owning the
  current buffer

`next-slide` SHALL advance the deck by one position via the existing
`+presentation--deck-goto` helper.  `previous-slide` SHALL retreat by
one.  Both commands SHALL no-op (without error) when the requested
target index is out of `[0, slide_count)`.

`quit` SHALL resolve the session via the buffer-local
`+presentation--session-key` and invoke `+presentation-end` on it.
When the buffer-local key is `nil`, or no session for that key
exists in the session table, `quit` SHALL no-op without error.

The minor mode keymap SHALL take precedence over the buffer's major
mode bindings for the keys it owns; other keys SHALL pass through to
the major mode unchanged.  Bindings SHALL be callable from any evil
state (normal, insert, visual, …) — the keymap is registered via
`evil-make-overriding-map`.

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

#### Scenario: C-c q ends the session

- **WHEN** point is in a presentation buffer for session `K`
- **AND** session `K` is registered in the session table
- **AND** the user invokes the binding for `C-c q`
- **THEN** `+presentation-end` is called with `K`
- **AND** session `K` is removed from the session table

#### Scenario: C-c q in a stale buffer is a no-op

- **WHEN** point is in a buffer whose buffer-local
  `+presentation--session-key` references a session no longer
  present in the session table
- **AND** the user invokes the binding for `C-c q`
- **THEN** no error is signalled
- **AND** no teardown effect is run

#### Scenario: C-c q is callable from evil normal state

- **WHEN** point is in a presentation buffer
- **AND** the active evil state is `normal`
- **AND** the user types `C-c q`
- **THEN** the `quit` command is invoked
