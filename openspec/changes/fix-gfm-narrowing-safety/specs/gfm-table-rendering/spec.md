## ADDED Requirements

### Requirement: Narrowing-resilient discovery and teardown

The system SHALL discover tables and tear down its overlays
identically regardless of the buffer's current restriction.  Block
discovery SHALL scan the full buffer (widening the restriction for
the duration of the scan); the buffer-wide teardown path used by
the mode's full rebuild and by `gfm-tables-mode` disable SHALL
clear all overlays tagged by the mode regardless of restriction.

#### Scenario: Narrowed rebuild does not signal

- **WHEN** `gfm-tables-mode` is enabled in a widened buffer
  containing tables in two separate top-level regions
- **AND** the buffer is then narrowed to a region containing only
  the first table
- **AND** `gfm-tables--rebuild` is invoked (e.g. via a
  window-configuration change)
- **THEN** the rebuild completes without signalling an error

#### Scenario: Cached block list is narrowing-independent

- **WHEN** `gfm-tables--find-blocks` is called once in the widened
  buffer (caching a block list spanning the whole document) and
  once after narrowing to a sub-region
- **THEN** both calls return block lists with the same source
  positions for the same `buffer-chars-modified-tick`

#### Scenario: Full teardown leaves no off-narrowing zombies

- **WHEN** the buffer is narrowed and
  `gfm-tables--remove-overlays` is invoked with no `BEG`/`END`
  arguments (the full-clear branch)
- **AND** the buffer is subsequently widened
- **THEN** no overlay tagged `gfm-tables` remains on the buffer

#### Scenario: Tracking list and on-buffer overlays stay in lockstep

- **WHEN** `gfm-tables-mode` performs a rebuild while the buffer
  is narrowed
- **AND** the buffer is subsequently widened
- **THEN** the count of overlays tagged `gfm-tables` on the buffer
  equals the length of `gfm-tables--overlays`
