## ADDED Requirements

### Requirement: TAB-key dispatch in `gfm-pretty-mode`

`gfm-pretty-mode` SHALL bind `TAB` in its own keymap
(`gfm-pretty-mode-map`) to a command that dispatches by point context,
and SHALL NOT fall through to `indent-for-tab-command` /
`markdown-indent-line` in non-list contexts.

The dispatch SHALL be:

- On a heading line (`markdown-on-heading-p`) — invoke
  `markdown-cycle` interactively so heading visibility cycling
  (including its `last-command` state machine) keeps working.
- Inside a table (`markdown-table-at-point-p`) — invoke
  `markdown-table-forward-cell`.
- On a list-item prefix slot — invoke `markdown-indent-line` once.
  The "prefix slot" predicate SHALL match
  `^\s-*\(?:[-*+]\|[0-9]+[.)]\)\s-+` and require point ≤
  `(match-end 0)`; this branch SHALL additionally require
  `(and (bound-and-true-p evil-mode) (evil-insert-state-p))`.
- Everywhere else — no-op. No whitespace SHALL be inserted into the
  buffer.

Disabling `gfm-pretty-mode` SHALL restore the prior `TAB` binding
(`markdown-cycle`) by deactivating the buffer-local keymap.

#### Scenario: TAB on a callout marker line is a no-op

- **GIVEN** a buffer with `> [!NOTE]` at BOL and `gfm-pretty-mode`
  enabled
- **WHEN** point is at column 0 of the marker line and the user
  presses `TAB`
- **THEN** the buffer SHALL be unchanged (no spaces inserted)
- **AND** the callout block SHALL still parse and decorate correctly

#### Scenario: TAB on a heading cycles visibility

- **GIVEN** a buffer with `## Section` and `gfm-pretty-mode` enabled
- **WHEN** point is on the heading line and the user presses `TAB`
- **THEN** the engine SHALL invoke `markdown-cycle` interactively
- **AND** the heading's subtree visibility state SHALL advance

#### Scenario: TAB inside a table moves to the next cell

- **GIVEN** a GFM table with point inside a cell and
  `gfm-pretty-mode` enabled
- **WHEN** the user presses `TAB`
- **THEN** point SHALL move to the next cell via
  `markdown-table-forward-cell`

#### Scenario: TAB at list-item prefix slot indents in evil insert state

- **GIVEN** a buffer with `- item` at BOL, `gfm-pretty-mode` enabled,
  and `evil-insert-state-p` returning non-nil
- **WHEN** point is between BOL and the start of `item` and the user
  presses `TAB`
- **THEN** the engine SHALL invoke `markdown-indent-line` once
- **AND** the list-item line SHALL be indented one step

#### Scenario: TAB at list-item prefix slot outside evil insert state is a no-op

- **GIVEN** a buffer with `- item` at BOL, `gfm-pretty-mode` enabled,
  and `evil-insert-state-p` returning nil
- **WHEN** point is between BOL and the start of `item` and the user
  presses `TAB`
- **THEN** the buffer SHALL be unchanged

#### Scenario: TAB in paragraph body is a no-op

- **GIVEN** a buffer with a plain paragraph line and
  `gfm-pretty-mode` enabled
- **WHEN** point is mid-paragraph and the user presses `TAB`
- **THEN** the buffer SHALL be unchanged

## MODIFIED Requirements

### Requirement: Scoped post-edit rebuild routing

The engine SHALL own the routing decision for scoped post-edit
rebuilds. After an `after-change-functions` burst settles, the engine's
idle timer SHALL invoke a per-decorator routing function that, given
the decorator's accumulated dirty region:

1. Calls the decorator's optional `:full-rebuild-required-p` with the
   dirty region. If non-nil, the engine SHALL perform a full rebuild
   via `:rebuild-fn` (or generic teardown + reapply when
   `:rebuild-fn` is absent).
2. Otherwise calls the decorator's `:collect-fn` (cache-hit normal)
   and matches the dirty region against each block's `:range-fn`
   result. If exactly one block fully contains the dirty region, the
   engine SHALL rebuild that single block via `:apply-block-fn` per
   currently-displayed window. If no collected block overlaps the
   dirty region (the edit destroyed the block whose overlays the
   decorator owned), the engine SHALL perform a full rebuild via
   `:rebuild-fn` (or generic teardown + reapply when `:rebuild-fn` is
   absent) so stale overlays are evacuated. Otherwise (multiple
   overlapping blocks) the engine SHALL also perform a full rebuild.

A decorator that does not register `:full-rebuild-required-p` SHALL
still receive correct scoped rebuilds via the block-containment
fallback (step 2).

#### Scenario: Edit inside one block body triggers single-block rebuild

- **GIVEN** a buffer with two callouts and no other decorators
- **WHEN** the user types a character inside callout #1's body
- **AND** the idle timer fires
- **THEN** the engine SHALL match the dirty region against the
  collected blocks
- **AND** rebuild only callout #1 via its `:apply-block-fn` per
  displayed window
- **AND** callout #2's overlays SHALL be untouched

#### Scenario: Structural-line edit forces full rebuild

- **GIVEN** a buffer with two fenced code blocks
- **WHEN** the user inserts a backtick on block #1's opening fence
  line
- **AND** the idle timer fires
- **THEN** the fences decorator's `:full-rebuild-required-p` SHALL
  return non-nil for the dirty region
- **AND** the engine SHALL invoke the fences decorator's
  `:rebuild-fn` (full rebuild)

#### Scenario: Adjacency edit invalidates indent block

- **GIVEN** a buffer with an indented code block preceded by a blank
  line
- **WHEN** the user types on the blank line directly above the indent
  block
- **AND** the idle timer fires
- **THEN** the fences decorator's `:full-rebuild-required-p` SHALL
  return non-nil for the dirty region
- **AND** the engine SHALL invoke the fences decorator's
  `:rebuild-fn` (full rebuild)

#### Scenario: Edit destroys a tracked block, leaving no overlap

- **GIVEN** a buffer containing a single callout `> [!NOTE]` with
  the callouts decorator enabled
- **WHEN** an edit inserts a leading space at BOL of the marker line
  (e.g. via a fallback indent path), so the marker no longer parses
  as a callout
- **AND** the idle timer fires
- **THEN** `:collect-fn` SHALL return no callout block overlapping
  the dirty region
- **AND** the engine SHALL perform a full rebuild of the callouts
  decorator
- **AND** no overlays tagged with the callouts decorator's registry
  SHALL remain in the now-destroyed block's former range
