## ADDED Requirements

### Requirement: Narrowing-resilient discovery and teardown

The system SHALL discover callouts and tear down its overlays
identically regardless of the buffer's current restriction.
`gfm-callouts--find-blocks-1` SHALL widen the restriction for the
duration of its scan; the buffer-wide teardown path used by the
mode's full rebuild and by `gfm-callouts-mode` disable SHALL clear
all overlays tagged by the mode regardless of restriction.

#### Scenario: Narrowed rebuild does not duplicate overlays

- **WHEN** `gfm-callouts-mode` is enabled in a widened buffer
  containing callouts in two separate top-level regions
- **AND** the buffer is then narrowed to a region containing only
  the first callout
- **AND** `gfm-callouts--rebuild` is invoked
- **AND** the buffer is subsequently widened
- **THEN** the count of overlays tagged `gfm-callouts` on the
  buffer equals the length of `gfm-callouts--overlays`

#### Scenario: Cached block list is narrowing-independent

- **WHEN** `gfm-callouts--find-blocks` is called once in the
  widened buffer and once after narrowing to a sub-region
- **THEN** the second call returns the same data as the first for
  the same `buffer-chars-modified-tick`

#### Scenario: Full teardown leaves no off-narrowing zombies

- **WHEN** the buffer is narrowed and
  `gfm-callouts--remove-overlays` is invoked with no
  `BEG`/`END` arguments
- **AND** the buffer is subsequently widened
- **THEN** no overlay tagged `gfm-callouts` remains on the buffer

## MODIFIED Requirements

### Requirement: Block-discovery cache

The system SHALL memoise `gfm-callouts--find-blocks` by
`buffer-chars-modified-tick` so repeat calls without an
intervening edit reuse the cached scan.  The cache SHALL be a pure
function of buffer contents — narrowing or widening the buffer
SHALL NOT invalidate or alter cached results.

#### Scenario: Repeated find-blocks calls without edits return cached result

- **WHEN** `gfm-callouts--find-blocks` is called twice with no
  buffer modification in between
- **THEN** both calls return `eq` block lists

#### Scenario: Edit invalidates cache

- **WHEN** the buffer is modified between two calls to
  `gfm-callouts--find-blocks`
- **THEN** the second call returns a fresh block list reflecting
  the new state

#### Scenario: Narrowing does not invalidate cache

- **WHEN** `gfm-callouts--find-blocks` is called once in the widened
  buffer and the buffer is then narrowed without any modification
- **THEN** the next call returns the same `eq` block list (the
  widened scan, including callouts outside the current restriction)
