## Purpose

Render GitHub Flavored Markdown (GFM) tables in markdown buffers as
visually formatted grids using overlays, providing aligned columns,
box-drawing borders, header emphasis, and zebra striping while keeping
the underlying source intact and editable.

## Requirements

### Requirement: Mode toggle

The system SHALL provide a buffer-local minor mode `gfm-tables-mode` that
applies and removes the table decoration overlays as a unit.

#### Scenario: Enabling the mode in a markdown buffer

- **WHEN** `gfm-tables-mode` is enabled in a buffer containing GFM tables
- **THEN** each table is decorated with row overlays, an outer box, a rule
  line, header emphasis, and zebra striping

#### Scenario: Disabling the mode

- **WHEN** `gfm-tables-mode` is disabled in a buffer where it had decorated
  tables
- **THEN** all overlays created by the mode are removed and the buffer
  visually matches its raw markdown source

### Requirement: Table block discovery

The system SHALL identify GFM tables by locating a delimiter row matching
`^\| *:?-+:? *(\| *:?-+:? *)*\|$`, treating the line immediately above it
as the header row and consecutive `|`-prefixed lines below it as body rows.

#### Scenario: Standard GFM table

- **WHEN** the buffer contains a header row, a delimiter row, and one or
  more body rows
- **THEN** the system identifies the contiguous block as one table with
  the correct header, delimiter, and body partitioning

#### Scenario: Tables inside fenced code blocks are skipped

- **WHEN** a sequence of `|`-prefixed lines lies entirely inside a fenced
  code block discovered by `gfm-code-fences`
- **THEN** the system does not decorate it as a table

#### Scenario: Lone delimiter-shaped line is not a table

- **WHEN** a line matching the delimiter regex appears with no preceding
  pipe-prefixed line
- **THEN** the system does not treat it as a table

### Requirement: Cell parser

The system SHALL split each table row into cells on top-level unescaped `|`
characters, treating `\|` as a literal pipe inside a cell and ignoring `|`
characters that occur inside a backtick-delimited code span.

#### Scenario: Escaped pipe inside cell

- **WHEN** a row contains `| a \| b | c |`
- **THEN** the parser yields two cells: `a | b` and `c`

#### Scenario: Pipe inside single-backtick code span

- **WHEN** a row contains `` | a | `b|c` | d | ``
- **THEN** the parser yields three cells, with the second cell content
  including the literal pipe inside the code span

#### Scenario: Pipe inside double-backtick code span

- **WHEN** a row contains `` | a | ``b|c`` | d | ``
- **THEN** the parser yields three cells; the double-backtick span is
  recognised as a code span and its interior pipe is not a delimiter

### Requirement: Column width normalisation

The system SHALL compute a per-column maximum content width across the
header and all body rows, and SHALL render every cell padded to that width
with at least one space of padding inside the cell on each side.

#### Scenario: Unaligned source renders as a uniform grid

- **WHEN** body rows have varying cell widths in source (typical of
  agent-generated tables)
- **THEN** every rendered cell in a given column has identical visual
  width and at least one space of padding on each side

#### Scenario: Source is already aligned

- **WHEN** body rows already have uniform cell widths in source
- **THEN** the rendered table preserves the source's apparent column widths
  without adding extra padding beyond the per-cell minimum

### Requirement: Border and rule decoration

The system SHALL bracket each table with a synthesised top border
(`┌─…─┐`), a continuous rule line (`├─…─┤`) replacing the delimiter row,
and a synthesised bottom border (`└─…─┘`).

#### Scenario: Outer box brackets the table

- **WHEN** a table is decorated
- **THEN** the rendered output begins with a top border line above the
  header row and ends with a bottom border line below the last body row

#### Scenario: Delimiter row replaced with continuous rule

- **WHEN** a table contains a delimiter row in source
- **THEN** the rendered output replaces that line with a single
  `├─…─┤` line of the same width as the box

### Requirement: Exterior pipe rendering

The system SHALL render the leading and trailing `|` of every row as `│`
(box-drawing vertical), aligned with the corners of the outer box.

#### Scenario: Leading and trailing pipes rendered as box edges

- **WHEN** a body or header row is rendered
- **THEN** the first and last visible characters of the row are `│`
  characters that line up with the `┌`/`└` and `┐`/`┘` corners

### Requirement: Interior column gap rendering

The system SHALL render the position of every interior `|` as a single
space using the default theme background, so that on rows with the alt-bg
stripe, the gap punches through the stripe and reveals column boundaries.

#### Scenario: Gap punches through stripe on alt-bg rows

- **WHEN** a body row has the alt-bg stripe applied
- **THEN** the column-gap positions render with the default theme
  background, producing visible vertical channels through the stripe

#### Scenario: Gap is contiguous with cell padding on default-bg rows

- **WHEN** a body row has no stripe (default-bg row)
- **THEN** the column-gap positions render at the default background and
  are visually continuous with the surrounding cell padding

### Requirement: Header emphasis

The system SHALL render the row immediately preceding the delimiter row
with bold weight, on the default theme background.

#### Scenario: Header row renders bold without distinct background

- **WHEN** a table has a header row identified by the delimiter row's
  position
- **THEN** the header row's text renders bold and the row's background
  matches the default theme background (no stripe)

### Requirement: Body row zebra striping

The system SHALL apply the `gfm-tables-row-alt-face` background to
even-numbered body rows (counting from 1 below the rule line), leaving
odd-numbered body rows on the default background.

#### Scenario: Alternating row backgrounds

- **WHEN** a table has at least two body rows
- **THEN** the first body row renders at the default background, the
  second body row renders with `gfm-tables-row-alt-face`, the third at
  default, and so on

### Requirement: Stripe face

The system SHALL define a face `gfm-tables-row-alt-face` with distinct
backgrounds for light and dark themes, and SHALL allow customisation
through `customize-face`.

#### Scenario: Light theme stripe colour

- **WHEN** the active theme has `light` background type
- **THEN** `gfm-tables-row-alt-face` uses background `#efe9dd` by default

#### Scenario: Dark theme stripe colour

- **WHEN** the active theme has `dark` background type
- **THEN** `gfm-tables-row-alt-face` uses background `#313244` by default

### Requirement: Cursor reveal

The system SHALL suppress the row decoration overlay when point is
located inside that row, exposing the underlying source for editing,
and SHALL restore the decoration when point leaves the row.

#### Scenario: Entering a decorated row reveals source

- **WHEN** point moves into a row whose decoration overlay is active
- **THEN** the overlay's display is suppressed for that row only and the
  source line becomes visible

#### Scenario: Leaving a row restores decoration

- **WHEN** point moves out of a previously-revealed row
- **THEN** the row's decoration overlay re-applies its display

### Requirement: Debounced rebuild

The system SHALL rebuild all table overlays via a 0.2-second idle timer
in response to buffer modifications and window-configuration changes, and
SHALL skip rebuilds in indirect buffers.

#### Scenario: Rebuild after edit

- **WHEN** the user modifies a decorated table and waits for idle
- **THEN** the overlays are rebuilt to reflect the new source

#### Scenario: Rebuild after window resize

- **WHEN** the window configuration changes (e.g. window resized) and
  the buffer becomes idle
- **THEN** the overlays are rebuilt

#### Scenario: Indirect buffer skipped

- **WHEN** the current buffer has a base buffer
- **THEN** the rebuild scheduler does not schedule a rebuild for the
  indirect buffer

### Requirement: Performance instrumentation

The system SHALL maintain per-buffer performance statistics
(rebuild count, total duration, last duration, max duration, table
count from the most recent rebuild) and SHALL surface them via a command
`gfm-tables-stats`.

#### Scenario: Stats accumulate across rebuilds

- **WHEN** the rebuild has run multiple times in a buffer
- **THEN** `gfm-tables-stats` reports the cumulative count, total
  duration, last and max single-rebuild durations, and the table count
  from the most recent rebuild

#### Scenario: Slow rebuild emits a warning

- **WHEN** a single rebuild duration exceeds
  `gfm-tables-slow-rebuild-threshold` (default 0.05 s)
- **THEN** the system emits a `message` line identifying the buffer and
  the duration

### Requirement: Theme change responsiveness

The system SHALL re-evaluate any cached default-bg colour used for
column-gap rendering when the active theme changes, so that gaps remain
visually correct across theme switches.

#### Scenario: Theme switch refreshes gap colour

- **WHEN** the active theme changes from light to dark (or vice versa)
- **THEN** subsequent rebuilds use the new theme's default background
  for column-gap rendering
