## ADDED Requirements

### Requirement: Blockquotes decorator registration and toggle

The system SHALL register a decorator named `blockquotes` that applies a left-rail overlay decoration on plain GFM blockquote blocks (runs of `^>`-prefixed lines that do NOT belong to a callout block).

`(gfm-pretty-toggle-decorator 'blockquotes)` SHALL flip the decorator on and off.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing a plain blockquote
- **THEN** each plain blockquote line shows a `│` rail in place of the leading `>` (or `> `)
- **AND** soft-wrapped continuation visual rows show a `│ ` prefix in `gfm-pretty-blockquotes-rail-face`

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'blockquotes)` flips the enable bit off
- **THEN** every blockquote rail overlay is removed
- **AND** the raw `>` characters and `markdown-mode`'s default `wrap-prefix "> "` reappear on continuation rows

### Requirement: Blockquote block discovery

The blockquotes decorator's `:collect` SHALL recognise a blockquote block as a maximal run of consecutive lines whose first character is `>`. The block range SHALL run from the first line's BOL to the last line's EOL.

Runs that contain a `> [!TYPE]` marker line (where `TYPE` is one of `NOTE`, `TIP`, `IMPORTANT`, `WARNING`, `CAUTION`, `CRITICAL`) SHALL be excluded so the callouts decorator retains ownership of those blocks.

Adjacent `>`-prefixed runs separated by one or more blank lines SHALL be returned as distinct blocks.

Discovery SHALL widen before scanning so a narrowed buffer (e.g. under `gfm-present-mode`) still sees the full block boundaries.

#### Scenario: Single-line blockquote

- **GIVEN** a buffer line `> Pain: something`
- **WHEN** `:collect` runs
- **THEN** the returned block range covers the line only

#### Scenario: Multi-line blockquote

- **GIVEN** three consecutive lines `> a`, `> b`, `> c`
- **THEN** the block range covers all three lines as one block

#### Scenario: Two blockquotes separated by blank line

- **GIVEN** `> a\n\n> b`
- **THEN** `:collect` returns two distinct blocks

#### Scenario: Callout excluded

- **GIVEN** a callout `> [!NOTE]\n> body` adjacent to a plain blockquote `> other`
- **WHEN** the two are separated by a blank line
- **THEN** `:collect` returns one block (the plain `> other`) and does not return the callout

### Requirement: Blockquote left-rail rendering

The blockquotes decorator's `:apply-block-fn` SHALL render a left rail with:

- A `│ ` substitution for the two-char `> ` prefix on each blockquote line, painted in `gfm-pretty-blockquotes-rail-face`.
- A `│` substitution for the one-char bare `>` form (a blockquote line with no trailing space, used for blank rows between body paragraphs).
- A per-line anchor overlay whose `wrap-prefix` is the propertised string `"│ "` in `gfm-pretty-blockquotes-rail-face`, so soft-wrapped visual continuation rows show a continuous rail.

The decorator SHALL NOT paint a tinted background, a top/bottom border, or a right-edge rail.

The decorator's `:apply-block-fn` SHALL call `gfm-pretty-borders--apply-with-anchors` (or an equivalent engine seam) so anchor overlays are laid at most once per (block, rebuild pass) while per-window display overlays apply once per window.

#### Scenario: Plain blockquote

- **GIVEN** `> Pain: clutter`
- **WHEN** the decorator renders
- **THEN** the line shows `│ Pain: clutter` with the rail in `gfm-pretty-blockquotes-rail-face`
- **AND** no raw `>` is visible on that line

#### Scenario: Bare-`>` line in middle of block

- **GIVEN** `> first paragraph\n>\n> second paragraph`
- **WHEN** the decorator renders
- **THEN** the middle line shows `│` at the left edge with no raw `>` visible
- **AND** the rail is visually continuous across all three rows

#### Scenario: Soft-wrapped long line

- **GIVEN** a single source line `> ` followed by 200 characters of text, displayed in an 80-column window with `visual-line-mode` enabled
- **WHEN** the decorator renders
- **THEN** visual row 1 shows `│ ` at column 0 followed by the first chunk of text
- **AND** visual row 2 shows `│ ` at column 0 followed by the wrapped continuation (the `wrap-prefix` overlay wins over `markdown-mode`'s `wrap-prefix "> "` text property)

### Requirement: Blockquote rail face

The system SHALL define a face `gfm-pretty-blockquotes-rail-face` whose default spec inherits from `shadow`.

The rail glyph in the display overlay and the `wrap-prefix` in the anchor overlay SHALL both use this face.

#### Scenario: Face inheritance

- **GIVEN** the default theme
- **WHEN** `face-attribute` reads `gfm-pretty-blockquotes-rail-face :inherit`
- **THEN** it returns `shadow`

### Requirement: Blockquote source reveal

The blockquotes decorator SHALL carry the engine's revealable property (`gfm-pretty-blockquotes-revealable`) on each per-line `> ` → `│ ` (or `>` → `│`) display overlay.

The engine's reveal walker SHALL hide those overlays in the selected window when point lies on them, exposing the raw `>` source. The property name SHALL be derived from the blockquotes registry's `tag`; the decorator SHALL NOT register it separately. Reveal SHALL be scoped to the selected window.

The anchor overlay's `wrap-prefix` SHALL remain `│ ` even when the display overlay is revealed; reveal toggles only the leading-character display string.

#### Scenario: Point on blockquote line

- **GIVEN** a plain blockquote with point on its first line in window W1 (selected)
- **THEN** W1 shows the raw `> ` text
- **AND** other windows showing the buffer continue to show the `│ ` rail

#### Scenario: Region overlapping blockquote

- **GIVEN** an active region spanning two lines of a plain blockquote in W1 (selected)
- **THEN** every covered line in W1 shows the raw `> ` source
- **AND** other windows continue to show the rail

### Requirement: Blockquote narrowing-resilient discovery and teardown

`:collect` SHALL widen the buffer for the duration of its scan. Overlay teardown via the engine's bulk-cleanup helper SHALL also widen so the registry list and on-buffer overlay set stay in lockstep regardless of any current narrowing.

When the engine rebuilds within a sub-region (e.g. under `gfm-present-mode` narrowing), the decorator's overlay set SHALL converge with the same overlays that would be present in a clean widened rebuild — the suite `(narrow → rebuild → widen → rebuild)` SHALL produce a steady state.

#### Scenario: Narrowed rebuild then widen

- **GIVEN** a buffer with three blockquotes narrowed to the second
- **WHEN** `gfm-pretty--rebuild` runs, then the buffer is widened, then rebuilt again
- **THEN** the final overlay set SHALL match what a fresh widened rebuild produces
- **AND** no `args-out-of-range` SHALL signal

### Requirement: Blockquote scoped post-edit rebuild

The blockquotes decorator SHALL register `:full-rebuild-required-p` that returns non-nil when the dirty region:

- overlaps the first character of any blockquote line (structural-line case — the `>` character is the partition decider), or
- overlaps a line directly above or below an existing blockquote block (adjacency case — edits there can grow or shrink the block), or
- overlaps a `> [!TYPE]` marker line (so a callout-to-plain conversion routes through full rebuild and the partition shifts to blockquotes).

The three conditions are OR-combined inside the predicate. The engine's routing SHALL use the predicate to choose between full rebuild and single-block scoped rebuild.

When the dirty region intersects a blockquote body line (not adjacent to a block boundary and not changing the leading character), the engine SHALL rebuild only the containing blockquote via its `:apply-block-fn` (per displayed window). When the dirty region does not overlap any blockquote block range and does not trigger the predicate, the blockquotes decorator SHALL NOT contribute work to the rebuild iteration.

#### Scenario: Edit inside one blockquote body

- **GIVEN** two blockquotes A and B with point inside A's text (not changing the leading `>`)
- **WHEN** the user types into A
- **THEN** only A's overlays are rebuilt
- **AND** B's overlays are untouched

#### Scenario: New `>` line typed at start of paragraph

- **GIVEN** a plain paragraph immediately above a blockquote
- **WHEN** the user types `> ` at the start of that paragraph's line
- **THEN** the predicate fires (adjacency case)
- **AND** a full rebuild reassigns the line into the blockquote block

#### Scenario: Callout marker typed into plain blockquote

- **GIVEN** a plain blockquote `> Pain: ...`
- **WHEN** the user edits the line to `> [!IMPORTANT] Pain: ...`
- **THEN** the predicate fires (structural-line case for callouts)
- **AND** the rebuild releases the line from blockquotes and assigns it to the callouts decorator
