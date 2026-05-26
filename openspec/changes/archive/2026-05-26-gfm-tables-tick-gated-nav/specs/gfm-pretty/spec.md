## ADDED Requirements

### Requirement: Engine per-decorator overlay-freshness tick

The engine SHALL track a per-decorator `last-rebuild-tick` value in
`gfm-pretty--state`, set to `(buffer-chars-modified-tick)` at the end of every
successful `gfm-pretty--rebuild`. A predicate
`gfm-pretty--stale-p (DECORATOR-NAME)` SHALL return non-nil when the slot is
nil or strictly less than the current buffer tick; nil otherwise.

The tick is buffer-local (via the existing `defvar-local` of
`gfm-pretty--state`) and survives across window-configuration changes. It is
cleared to nil when the decorator is disabled or when the engine performs a
full state reset (e.g. `before-revert-hook` teardown).

Decorators MAY consult `gfm-pretty--stale-p` from any context — motion
commands, post-command hooks, etc. — to gate behaviour that reads stale
overlay-derived data (integer positions, fixnum offset vectors, hash keys)
that does not auto-adjust to buffer edits.

#### Scenario: Tick set on rebuild

- **WHEN** `gfm-pretty--rebuild` completes for a decorator in a buffer with
  tick `T`
- **THEN** `gfm-pretty--stale-p` for that decorator returns nil
- **AND** the next buffer modification advances the tick beyond `T`
- **AND** `gfm-pretty--stale-p` returns non-nil until the next rebuild
  completes

#### Scenario: Tick cleared on disable

- **WHEN** a decorator is disabled via
  `gfm-pretty--disable-decorator`
- **THEN** its `last-rebuild-tick` slot is nil
- **AND** `gfm-pretty--stale-p` returns non-nil

### Requirement: Engine revert lifecycle

The engine SHALL install `before-revert-hook` and `after-revert-hook`
handlers buffer-locally in every buffer where `gfm-pretty-mode` is active.

`before-revert-hook` SHALL, for each enabled decorator: remove every
registered overlay via the decorator's registry and clear the decorator's
`dirty-region`, `last-window-state`, `hidden-ovs`, `anchors-laid`, and
`last-rebuild-tick` state slots. The pending engine idle rebuild timer
SHALL be cancelled.

`after-revert-hook` SHALL synchronously run the engine's scheduled-rebuild
routine for each enabled decorator (full rebuild, since `dirty-region` is
nil), producing a fresh overlay set before control returns to the user.

The handlers SHALL be removed by `gfm-pretty--remove-engine-hooks`
together with the existing engine hooks.

#### Scenario: Overlay state cleared before revert

- **WHEN** `before-revert-hook` runs in a `gfm-pretty-mode` buffer with an
  enabled decorator
- **THEN** that decorator's `overlays` state slot is empty
- **AND** every position-bearing state slot (`dirty-region`,
  `last-window-state`, `hidden-ovs`, `anchors-laid`,
  `last-rebuild-tick`) is nil

#### Scenario: Synchronous rebuild after revert

- **GIVEN** a `gfm-pretty-mode` buffer with the tables decorator enabled
- **WHEN** `revert-buffer t t` completes
- **THEN** `after-revert-hook` has run a full rebuild for tables
- **AND** the decorator's `last-rebuild-tick` equals the buffer's current
  `buffer-chars-modified-tick`
- **AND** the first subsequent user command sees a fresh overlay set
  before executing

## MODIFIED Requirements

### Requirement: Structural cell motion

The tables decorator SHALL bind cell-motion commands (next-cell,
previous-cell, next-row-same-column, previous-row-same-column) on its
mode keymap.

Cell-aware motion commands SHALL consult `gfm-pretty--stale-p` for the
`tables` decorator before reading `gfm-pretty-tables-cell-bounds` or
`gfm-pretty-tables-display-cell-bounds`. When stale, the command SHALL
fall through to the underlying evil command (or the equivalent generic
motion outside evil) exactly as if point were outside any table, without
calling `goto-char` on any cached integer offset.

#### Scenario: Tab moves to next cell

- **WHEN** point is in cell 1 and user presses TAB
- **THEN** point moves to the first character of cell 2

#### Scenario: j after burst edit falls through to evil-next-line

- **GIVEN** point inside a table cell at tick `T0`
- **AND** a buffer modification advances the tick to `T1` before the
  engine's idle rebuild fires
- **WHEN** the user presses `j`
- **THEN** the cell-aware shim takes the fall-through path
- **AND** point moves as `evil-next-line` would move it
- **AND** no `goto-char` is issued against a `cell-bounds` value laid
  down at tick `T0`

### Requirement: Snap-to-cell on row entry

Point SHALL snap to the first cell's first character on row entry from
outside the table (vertical motion), rather than landing on a separator
or column gap.

The snap SHALL be skipped when `gfm-pretty--stale-p` returns non-nil for
the `tables` decorator. In that case point remains where the underlying
motion command left it; the next rebuild restores correct snap
behaviour.

#### Scenario: Down-arrow into table

- **WHEN** point is on the line above a table and user presses
  down-arrow
- **THEN** point lands on the first character of the header row's
  first cell

#### Scenario: Stale snap is skipped

- **GIVEN** the `tables` decorator's `last-rebuild-tick` is older than
  the current buffer tick
- **WHEN** `gfm-pretty-tables--maybe-snap-to-cell` is invoked
- **THEN** point is not moved
