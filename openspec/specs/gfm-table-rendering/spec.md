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

### Requirement: Auto-composition does not skew column widths

The system SHALL ignore auto-compositions (those produced by
`composition-function-table' rather than an explicit `composition'
text property) when measuring a cell's visible width.  Overlay
display strings do not run auto-composition during redisplay, so
counting them would under-pad the cell and push the closing border
off the box's grid.

#### Scenario: Cells containing ligature-prone text align with the box

- **WHEN** a cell contains a sequence the active font would
  auto-compose into a single glyph (e.g. `fl`, `--`) and is rendered
  inside a decorated table
- **THEN** the row's closing `│` aligns with the box's right edge,
  matching the rendered visible width

### Requirement: Cell-edit commit preserves the row

The system SHALL preserve the surrounding table row when committing
an indirect cell-edit.  Specifically, when the buffer's edit-indirect
machinery would otherwise leave a stray newline at the end of the
committed cell content (because `markdown--edit-indirect-after-commit-function`
treats every committed region as a code block), the system SHALL
strip that trailing newline before redisplay.

#### Scenario: Committing an unchanged cell edit leaves the row intact

- **WHEN** the user opens an indirect cell edit, makes no changes,
  and commits
- **THEN** the table row's source contents are byte-for-byte
  identical to before the edit; in particular, no newline is
  inserted between the committed cell and the next cell on the same
  row

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

The system SHALL respond to buffer modifications and window-state
changes via a 0.2-second idle timer, SHALL skip rebuilds in indirect
buffers, SHALL scope post-edit rebuilds to the table whose source
range intersects the changed region, and SHALL react to
window-state changes only when an actual rendering input has changed
\(width of an existing window, or arrival/departure of a window
showing the buffer).

When the changed region intersects no decorated table, the system
SHALL NOT rebuild. When the changed region intersects exactly one
table, the system SHALL tear down and re-decorate only that table.
When the changed region intersects multiple tables, crosses a table
boundary, or overlaps a fenced-code-block fence line, the system
SHALL fall back to a full-buffer rebuild.

When `window-configuration-change-hook` fires but the per-window
\(window . max-chars-per-line) snapshot is unchanged from the
previous rebuild, the system SHALL NOT schedule a rebuild.

#### Scenario: Edit inside a single decorated table

- **WHEN** the user modifies a cell inside one decorated table and
  waits for idle
- **THEN** only that table's overlays are rebuilt; other decorated
  tables in the buffer retain their existing overlay objects
  unchanged

#### Scenario: Edit outside every decorated table

- **WHEN** the user inserts text in a region that does not intersect
  any decorated table and waits for idle
- **THEN** no overlays are rebuilt

#### Scenario: Edit spans two tables

- **WHEN** the user deletes a region that spans more than one
  decorated table and waits for idle
- **THEN** all overlays in the buffer are rebuilt

#### Scenario: Edit overlaps a code fence boundary

- **WHEN** the changed region overlaps the opening or closing fence
  line of a fenced code block and the buffer becomes idle
- **THEN** all overlays in the buffer are rebuilt

#### Scenario: Window-state change with no width effect

- **WHEN** a window-configuration event fires that does not change any
  window's `window-max-chars-per-line' and does not add or remove a
  window showing the buffer (e.g. minibuffer activity, focus shift)
- **THEN** no rebuild is scheduled

#### Scenario: Indirect buffer skipped

- **WHEN** the current buffer has a base buffer
- **THEN** the rebuild scheduler does not schedule a rebuild for the
  indirect buffer

### Requirement: Per-window table rendering

The system SHALL produce display overlays sized to each window
currently showing the buffer, so that the same buffer split across
multiple windows (or shared between a graphical and terminal frame)
renders at each window's own width.  Source-side bookkeeping (cell
boundaries, the row keymap) SHALL be carried by anchor overlays
shared across windows; rendering (display strings, borders, packed
display-cell-bounds) SHALL be carried by display overlays restricted
to a single window via Emacs's `window' overlay property.

When no window currently displays the buffer (e.g. background
buffer), the system SHALL produce a single unrestricted display
overlay per row at a sane fallback width and replace it with
window-restricted overlays as soon as a window starts showing the
buffer.

#### Scenario: Buffer split across windows of different widths

- **WHEN** the buffer is shown in two windows whose
  `window-max-chars-per-line' differ
- **THEN** each window renders the table sized to its own width,
  with one display overlay per row per window

#### Scenario: Anchor overlays are shared across windows

- **WHEN** the buffer is shown in N windows
- **THEN** each source row is covered by exactly one anchor overlay
  carrying `gfm-tables-cell-bounds' and the row keymap, regardless of
  N

#### Scenario: Active-cell highlight targets the selected window

- **WHEN** the user holds point in a cell while the buffer is shown
  in two windows
- **THEN** the active-cell highlight repaints only the selected
  window's display overlay; the other window's display overlay is
  left untouched

### Requirement: Prioritised window rebuild

The system SHALL prioritise visible-window tables when rebuilding
display overlays in response to a window-state change, rendering
visible blocks immediately and deferring off-screen blocks to the
next idle tick.

#### Scenario: Visible blocks render before off-screen blocks

- **WHEN** a window resize fires a rebuild
- **THEN** display overlays for blocks intersecting any visible
  window range are recreated synchronously, and overlays for the
  remaining blocks are recreated on a follow-up idle timer

### Requirement: Selective per-window reconciliation

The system SHALL act on a per-window diff when reconciling display
overlays after a window-state change: only added or resized windows
trigger fresh rendering, and only removed windows have their display
overlays deleted.  Display overlays belonging to windows whose width
is unchanged SHALL retain their existing overlay objects.

#### Scenario: Resizing one of two windows leaves the other untouched

- **WHEN** the buffer is shown in two windows and only one window's
  width changes
- **THEN** the resized window's display overlays are replaced with
  fresh ones, while the other window's display overlay objects
  survive `eq` comparison

#### Scenario: Closing a window cleans up only its overlays

- **WHEN** a window showing the buffer is deleted
- **THEN** the deleted window's display overlays are removed at the
  next reconciliation, while overlays for surviving windows are not
  disturbed

#### Scenario: Opening a new window on the buffer renders just that window

- **WHEN** a fresh window starts showing a buffer that already has
  decorated tables
- **THEN** display overlays for the new window are produced (visible
  blocks first, off-screen on the next idle); existing display
  overlays for already-shown windows are not rebuilt

### Requirement: Performance instrumentation

The system SHALL maintain per-buffer performance statistics covering
rebuild count, total duration, last duration, max duration, table
count from the most recent rebuild, and a per-phase breakdown
\(block discovery, row parsing, layout, display composition, overlay
application). The system SHALL surface these statistics via a command
`gfm-tables-stats`.

#### Scenario: Stats accumulate across rebuilds

- **WHEN** the rebuild has run multiple times in a buffer
- **THEN** `gfm-tables-stats` reports the cumulative count, total
  duration, last and max single-rebuild durations, and the table
  count from the most recent rebuild

#### Scenario: Phase breakdown surfaced

- **WHEN** `gfm-tables-stats` is invoked after one or more rebuilds
- **THEN** the report includes a per-phase total covering at least
  block discovery, row parsing, layout, display composition, and
  overlay application

#### Scenario: Slow rebuild emits a warning

- **WHEN** a single rebuild duration exceeds
  `gfm-tables-slow-rebuild-threshold` (default 0.05 s)
- **THEN** the system emits a `message` line identifying the buffer
  and the duration

### Requirement: Theme change responsiveness

The system SHALL re-evaluate any cached default-bg colour used for
column-gap rendering when the active theme changes, so that gaps remain
visually correct across theme switches.

#### Scenario: Theme switch refreshes gap colour

- **WHEN** the active theme changes from light to dark (or vice versa)
- **THEN** subsequent rebuilds use the new theme's default background
  for column-gap rendering
