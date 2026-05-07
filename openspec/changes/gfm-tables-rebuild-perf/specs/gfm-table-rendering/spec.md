## MODIFIED Requirements

### Requirement: Debounced rebuild

The system SHALL respond to buffer modifications and window-configuration
changes via a 0.2-second idle timer, SHALL skip rebuilds in indirect
buffers, and SHALL scope post-edit rebuilds to the table whose source
range intersects the changed region.

When the changed region intersects no decorated table, the system SHALL
NOT rebuild. When the changed region intersects exactly one table, the
system SHALL tear down and re-decorate only that table. When the changed
region intersects multiple tables, crosses a table boundary, or overlaps
a fenced-code-block fence line, the system SHALL fall back to a full-
buffer rebuild. Window-configuration changes SHALL continue to trigger a
full-buffer rebuild.

#### Scenario: Edit inside a single decorated table

- **WHEN** the user modifies a cell inside one decorated table and waits
  for idle
- **THEN** only that table's overlays are rebuilt; other decorated tables
  in the buffer retain their existing overlay objects unchanged

#### Scenario: Edit outside every decorated table

- **WHEN** the user inserts text in a region that does not intersect any
  decorated table and waits for idle
- **THEN** no overlays are rebuilt

#### Scenario: Edit spans two tables

- **WHEN** the user deletes a region that spans more than one decorated
  table and waits for idle
- **THEN** all overlays in the buffer are rebuilt

#### Scenario: Edit overlaps a code fence boundary

- **WHEN** the changed region overlaps the opening or closing fence line
  of a fenced code block and the buffer becomes idle
- **THEN** all overlays in the buffer are rebuilt

#### Scenario: Rebuild after window resize

- **WHEN** the window configuration changes (e.g. window resized) and
  the buffer becomes idle
- **THEN** all overlays in the buffer are rebuilt

#### Scenario: Indirect buffer skipped

- **WHEN** the current buffer has a base buffer
- **THEN** the rebuild scheduler does not schedule a rebuild for the
  indirect buffer

### Requirement: Performance instrumentation

The system SHALL maintain per-buffer performance statistics covering
rebuild count, total duration, last duration, max duration, table count
from the most recent rebuild, and a per-phase breakdown (block discovery,
row parsing, layout, display composition, overlay application). The
system SHALL surface these statistics via a command `gfm-tables-stats`.

#### Scenario: Stats accumulate across rebuilds

- **WHEN** the rebuild has run multiple times in a buffer
- **THEN** `gfm-tables-stats` reports the cumulative count, total
  duration, last and max single-rebuild durations, and the table count
  from the most recent rebuild

#### Scenario: Phase breakdown surfaced

- **WHEN** `gfm-tables-stats` is invoked after one or more rebuilds
- **THEN** the report includes a per-phase total covering at least
  block discovery, row parsing, layout, display composition, and overlay
  application

#### Scenario: Slow rebuild emits a warning

- **WHEN** a single rebuild duration exceeds
  `gfm-tables-slow-rebuild-threshold` (default 0.05 s)
- **THEN** the system emits a `message` line identifying the buffer and
  the duration
