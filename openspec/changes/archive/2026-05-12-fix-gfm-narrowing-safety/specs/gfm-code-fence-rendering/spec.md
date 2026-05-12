## ADDED Requirements

### Requirement: Narrowing-resilient discovery and teardown

The system SHALL discover fenced blocks, the YAML helmet, and
indented blocks identically regardless of the buffer's current
restriction.  Each `--find-*-1` scanner SHALL widen the
restriction for the duration of its scan; the buffer-wide
teardown path SHALL clear all overlays tagged by the mode
regardless of restriction.

#### Scenario: Narrowed rebuild does not duplicate overlays

- **WHEN** `gfm-code-fences-mode` is enabled in a widened buffer
  containing fenced code blocks in two separate top-level regions
- **AND** the buffer is then narrowed to a region containing only
  the first fence
- **AND** `gfm-code-fences--rebuild` is invoked
- **AND** the buffer is subsequently widened
- **THEN** the count of overlays tagged `gfm-code-fences` on the
  buffer equals the length of `gfm-code-fences--overlays`

#### Scenario: Cached block lists are narrowing-independent

- **WHEN** each of `gfm-code-fences--find-blocks`,
  `gfm-code-fences--find-yaml-helmet`, and
  `gfm-code-fences--find-indent-blocks` is called once in the
  widened buffer and once after narrowing to a sub-region
- **THEN** the second call returns the same data as the first for
  the same `buffer-chars-modified-tick`

#### Scenario: Full teardown leaves no off-narrowing zombies

- **WHEN** the buffer is narrowed and
  `gfm-code-fences--remove-overlays` is invoked with no
  `BEG`/`END` arguments
- **AND** the buffer is subsequently widened
- **THEN** no overlay tagged `gfm-code-fences` remains on the
  buffer

#### Scenario: Fence opening outside the narrowing is discovered

- **WHEN** a fenced code block's opening line sits before the
  current narrowing's start position and its closing line sits
  inside the narrowing
- **THEN** `gfm-code-fences--find-blocks` returns a single block
  whose opening and closing positions match the source

## MODIFIED Requirements

### Requirement: Block-discovery cache

The system SHALL memoise each of the three block-discovery functions
(`--find-blocks`, `--find-yaml-helmet`, `--find-indent-blocks`) by
`buffer-chars-modified-tick`, so repeat calls without an intervening
edit reuse the cached scan.  Indented-block discovery still accepts
its excluded-fenced-ranges parameter as call-site data, not part of
the cache key.  The cache SHALL be a pure function of buffer
contents — narrowing or widening the buffer SHALL NOT invalidate or
alter cached results.

#### Scenario: Repeated find-blocks calls without edits return cached result

- **WHEN** `--find-blocks` is called twice with no buffer modification
  in between
- **THEN** both calls return `eq` block lists

#### Scenario: Edit invalidates cache

- **WHEN** the buffer is modified between two calls to `--find-blocks`
- **THEN** the second call returns a fresh block list reflecting the
  new state

#### Scenario: Narrowing does not invalidate cache

- **WHEN** `--find-blocks` is called once in the widened buffer and
  the buffer is then narrowed without any modification
- **THEN** the next `--find-blocks` call returns the same `eq` block
  list (the widened scan, including blocks outside the current
  restriction)
