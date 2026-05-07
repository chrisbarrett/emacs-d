## Context

Phase-level instrumentation of `gfm-tables--rebuild` against a 12-table
markdown reference document showed:

- Steady-state rebuild ~290 ms (74 ms on a freshly opened buffer; the 4×
  multiplier appears once font-lock has fully decorated the buffer).
- ~50% of rebuild time is spent in `gfm-tables--wrap-row-into-lines`, which
  is invoked twice per row: once via `compose-multiline-row` and again via
  `multiline-row-char-bounds`. Pure duplicate work.
- ~46% of rebuild time is `gfm-tables--visible-width`, ~190 calls per
  table, ~9.5 calls per cell. Same fontified cell measured from
  `column-widths`, `compose-row`, `row-char-bounds`, and `wrap-cell`.
- Every keystroke at `point-min` rebuilds all 12 tables, regardless of
  whether the change touched a table at all.

The existing `gfm-tables-stats` command reports rebuild count and totals,
but not which phase dominates — diagnosis required out-of-tree advice.

## Goals / Non-Goals

**Goals:**

- Cut wall-time per keystroke at `point-min` from ~800 ms (rebuild +
  font-lock) to a level where edits outside any table are imperceptible
  and edits inside a table touch only that table.
- Halve per-rebuild work for the cases where a full rebuild is still
  needed (window resize, theme change, mode toggle).
- Make phase-level breakdown available via `gfm-tables-stats` so future
  regressions are diagnosable in-place.

**Non-Goals:**

- Optimising the parent buffer's font-lock / `syntax-propertize` cost.
  That dominates the keystroke wall-time outlier (4.9 s max observed for
  full-buffer `font-lock-ensure`) but lives in `markdown-mode`, not in
  this module.
- Lazy / off-screen rebuild. All visible tables continue to be decorated
  unconditionally.
- Persisting any cache across rebuilds. All caches are scoped to a single
  rebuild so correctness on table edits is trivially preserved.

## Decisions

### Scope rebuild to the changed table

`gfm-tables--schedule-rebuild` currently ignores the change region passed
by `after-change-functions`. The new behaviour:

1. Capture the `(beg . end)` region of every change since the last
   rebuild in a buffer-local "dirty region" var.
2. When the idle timer fires, locate the table block (if any) intersecting
   the dirty region.
3. If no block intersects: do nothing. Clear the dirty region.
4. If exactly one block intersects: tear down only that block's overlays
   and reapply the table.
5. If multiple blocks intersect (e.g. a region delete spanning tables) or
   the change crosses a table boundary in a way that may have created /
   destroyed a block: fall back to a full rebuild.

`window-configuration-change-hook` continues to trigger a full rebuild —
column widths derive from `window-max-chars-per-line` so all tables must
be re-fitted on resize.

**Why region-based, not block-id-based?** `after-change-functions` only
gives us positions; we don't know which block a position belonged to
before the change. Region intersection against the post-edit
`gfm-tables--find-blocks` result is the simplest correct rule.

**Alternative considered:** maintain a list of decorated block ranges and
intersect against that. Rejected: ranges drift under insertion/deletion
without per-position update tracking, and the marker-based equivalent
adds complexity without measurable benefit over re-finding blocks (find
is <1 ms in our profile).

### Compute row layout once per rebuild

`apply-table` will compute the wrapped layout (per-cell, per-visual-line
strings) once per row and pass it to both display composition and
char-bound calculation. New helper `gfm-tables--row-layout` returns:

```
((line-cells …)   ; one entry per visual line
 (line-bounds …)) ; one entry per visual line
```

`compose-multiline-row` and `multiline-row-char-bounds` become thin
wrappers that consume an already-computed layout. The redundant second
`wrap-row-into-lines` call disappears.

### Memoise `visible-width` per rebuild

A fresh `eq`-keyed hash table is created at the start of each rebuild and
attached to a dynamically-bound variable. `visible-width` checks the
table first and stores the result on miss. The cache is `eq`-keyed
because the same fontified cell string flows through `column-widths`,
`compose-row`, and friends as the same object reference.

A fast path short-circuits the char-by-char scan when the string carries
no `display`, `composition`, or `invisible` text properties anywhere —
verified via a single `next-single-property-change` walk. Most cells
qualify.

### Phase-level stats

Replace the flat `gfm-tables--stats` alist with one that also tracks per-
phase totals: `find-blocks`, `parse`, `layout`, `compose`, `apply`. The
phases are timed inline inside `apply-table` (no advice). `gfm-tables-stats`
prints the breakdown sorted by total descending.

## Risks / Trade-offs

- **Risk**: scoped-rebuild misses a case where a change one block away
  invalidates another block (e.g. a fenced-code-block opening fence is
  deleted, exposing a previously-hidden table). → **Mitigation**: include
  fenced-code-fence delimiters in the "force full rebuild" trigger; when
  the changed region overlaps any fence boundary line, fall back.
- **Risk**: `eq`-keyed memoisation breaks if a caller substrings a cell
  before re-measuring. → **Mitigation**: cache lives only inside one
  rebuild; all current callers measure the same string objects produced
  by `fontify-cell`. Add a regression test that substrings a fontified
  cell and confirms the substring's width is computed correctly (cache
  miss, not stale hit).
- **Trade-off**: scoped rebuild adds branching to a hot path. The branch
  is one `cl-find` over <20 blocks per rebuild — negligible vs. the
  rebuild cost it avoids.
