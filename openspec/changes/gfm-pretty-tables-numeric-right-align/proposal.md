## Why

GFM tables in `gfm-pretty-mode` always left-align cell content. Numeric
columns (LOC counts, durations, IDs) read poorly when their digits are
left-flush — magnitudes don't line up by place value, and the eye has
to re-scan each row to compare them.

## What Changes

- Auto-detect numeric columns at render time: a column whose body
  cells are all numeric-or-empty (with at least one non-empty cell)
  renders right-aligned.
- Right-aligned columns pad their cells on the leading edge instead
  of the trailing edge, in both header and body rows, across all
  visual lines of wrapped cells.
- Detection runs on trimmed source-cell text, not the fontified
  string, so markdown inline markup never disqualifies a numeric
  cell.
- Source-side `:---` / `---:` / `:---:` delimiter-row markers remain
  ignored. Honouring them is out of scope for this change.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `gfm-pretty`: tables decorator gains a per-column alignment axis
  driven by body-cell content; previously every column was
  left-aligned.

## Impact

- `lisp/gfm/gfm-pretty-tables.el`: new column-alignment computation;
  `--compose-row` honours alignment; `--apply-table-display` threads
  the alignment vector through layout → compose.
- `lisp/gfm/gfm-pretty-tests.el` (or sibling tests file): new ERT
  coverage for the detection rule and the rendered padding shape.
- No API surface added or removed outside the decorator's internals.
- No change to overlay lifecycle, bounds-vec layout, or per-window
  rendering — segment lengths stay identical, only the position of
  the padding inside each segment changes.
