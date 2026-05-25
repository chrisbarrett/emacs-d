## ADDED Requirements

### Requirement: Slide-count header line

`gfm-present-mode` SHALL display the current slide's 1-based ordinal
and the document's total slide count in the buffer's header line,
formatted as `<n>/<m>` (e.g. `2/5`), left-aligned, with no other
content.

The header line content SHALL be stored in `header-line-format` as a
plain string set buffer-locally.  The value SHALL be refreshed at the
following points and no others:

- Mode enable, after the initial narrowing.
- `gfm-present-next-slide`, after the new narrowing.
- `gfm-present-previous-slide`, after the new narrowing.
- `gfm-present-follow-link`, after the slug-branch re-narrow.
- `gfm-present--after-anchor-jump`, after the re-narrow.
- `gfm-present--restore-position`, after the revert re-narrow.

When the document has no top-level H1 headings, or point is before
the first H1 such that no enclosing slide can be identified,
`header-line-format` SHALL be nil in the buffer.

On mode disable, `header-line-format` SHALL be reset to nil in the
buffer.

#### Scenario: Enabling on a slide sets the counter

- **WHEN** `gfm-present-mode` is enabled in a buffer with five H1
  headings and point is inside the second H1's region
- **THEN** `header-line-format` is the string `"2/5"`

#### Scenario: Next-slide advances the counter

- **WHEN** the buffer is showing `2/5` in the header line
- **AND** the user invokes `gfm-present-next-slide`
- **THEN** the header line shows `3/5`

#### Scenario: Previous-slide retreats the counter

- **WHEN** the buffer is showing `2/5` in the header line
- **AND** the user invokes `gfm-present-previous-slide`
- **THEN** the header line shows `1/5`

#### Scenario: Follow-link to another slide updates the counter

- **WHEN** the buffer is showing `1/5` in the header line
- **AND** the user follows an in-doc heading link whose target H1 is
  the fourth slide
- **THEN** the header line shows `4/5`

#### Scenario: Document with no H1s leaves header line nil

- **WHEN** `gfm-present-mode` is enabled in a buffer that contains no
  `^# ` heading lines
- **THEN** `header-line-format` is nil in the buffer

## MODIFIED Requirements

### Requirement: `gfm-present-mode` buffer-local minor mode

The system SHALL define `gfm-present-mode` as a buffer-local minor
mode.  Enabling the mode SHALL narrow the buffer to the H1 region
containing point (or the first H1 region when point is before the
first H1, or the last H1 region when point is after the last H1).
Disabling the mode SHALL widen the buffer.

The mode keymap SHALL bind:

| Key            | Command                          |
| :------------- | :------------------------------- |
| `C-n` / `C-f`  | `gfm-present-next-slide`         |
| `C-p` / `C-b`  | `gfm-present-previous-slide`     |
| `C-c q`        | `gfm-present-quit`               |
| `RET`          | `gfm-present-follow-link`        |
| `[remap widen]`| disable `gfm-present-mode`       |

The `widen` remap SHALL invoke a command that disables
`gfm-present-mode` in the current buffer.  The mode-disable branch
already widens, so the remap SHALL NOT bury, kill, or otherwise
dispose of the buffer.

The keymap SHALL take precedence over the buffer's major-mode
bindings for the keys it owns.  Bindings SHALL be callable from any
evil state.

#### Scenario: enabling narrows to first heading

- **WHEN** `gfm-present-mode` is enabled in a buffer containing
  three H1 headings and point is at `point-min`
- **THEN** the buffer is narrowed to the region from the first H1's
  beginning to the second H1's beginning (exclusive)

#### Scenario: enabling narrows to enclosing heading when point is mid-slide

- **WHEN** point is on a line inside the second H1's region
- **AND** `gfm-present-mode` is enabled
- **THEN** the buffer is narrowed to the second H1's region

#### Scenario: disabling widens

- **WHEN** `gfm-present-mode` is enabled and the buffer is
  narrowed
- **AND** the user disables the mode
- **THEN** the buffer is widened

#### Scenario: keymap is callable from evil normal state

- **WHEN** point is in a `gfm-present-mode` buffer
- **AND** the active evil state is `normal`
- **AND** the user types `C-n`
- **THEN** `gfm-present-next-slide` is invoked

#### Scenario: widen keybinding disables the mode

- **WHEN** `gfm-present-mode` is enabled and the buffer is narrowed
- **AND** the user types `C-x n w`
- **THEN** `gfm-present-mode` is disabled in the buffer
- **AND** the buffer is widened
- **AND** the buffer is not buried or killed

#### Scenario: widen remap reachable from evil normal state

- **WHEN** point is in a `gfm-present-mode` buffer
- **AND** the active evil state is `normal`
- **AND** the user types `C-x n w`
- **THEN** `gfm-present-mode` is disabled in the buffer
- **AND** the buffer is widened
