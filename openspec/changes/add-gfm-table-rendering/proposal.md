## Why

GFM tables in markdown buffers render as raw `|`-delimited rows that are noisy
to read and frequently misaligned in agent-generated content. The `lang-markdown`
module already adorns fenced code blocks (`+gfm-code-fences`) and callouts
(`+gfm-callouts`) with overlay-driven box-drawing chrome; tables deserve the
same treatment so that prose-heavy markdown buffers feel coherent visually.

## What Changes

- Add a new minor mode `gfm-tables-mode` (in `+gfm-tables.el` under
  `modules/lang-markdown/lib/`) that adorns GFM tables with overlays:
  - exterior `|` characters render as `│`
  - delimiter row (`|---|---|---|`) renders as a continuous `├──…──┤` rule
  - interior `|` regions render as default-bg gaps that punch through
    alternating row backgrounds, making column structure legible
  - cells are normalised to a per-column max width with ≥1 space of internal
    padding on each side, so unaligned source still renders columnar
  - body rows alternate between default bg and a stripe face
  - header row (the row immediately preceding the delim row) renders bold
  - synthesised `┌─…─┐` / `└─…─┘` borders bracket the table top and bottom
  - cursor-driven reveal: when point enters a row, that row's display
    overlay is suppressed so the source can be edited; restored on leave
  - debounced idle-timer rebuild after edits and window-config changes
- Add a `gfm-tables-row-alt-face` defface with light/dark variants modelled on
  `org-column` (light: `#efe9dd`, dark: `#313244`)
- Add lightweight performance instrumentation (rebuild count, total/last/max
  durations, slow-rebuild warnings) surfaced via `M-x gfm-tables-stats`
- Wire `gfm-tables-mode` into the existing markdown-mode setup alongside
  `gfm-code-fences-mode` and `gfm-callouts-mode`
- Skip table-shaped content that overlaps with a fenced code block
  (mirrors how `+gfm-code-fences` excludes indented blocks)

## Capabilities

### New Capabilities

- `gfm-table-rendering`: visual treatment of GFM tables in markdown buffers,
  including parsing, padding, zebra striping, the gap-trick for column
  visibility, outer box decoration, cursor-driven reveal, and rebuild
  scheduling

### Modified Capabilities

<!-- none -->

## Impact

- New file: `modules/lang-markdown/lib/+gfm-tables.el`
- Modified file: `modules/lang-markdown/init.el` (or wherever the existing
  `gfm-code-fences-mode` / `gfm-callouts-mode` are wired in) to enable the
  new mode for markdown buffers
- New face: `gfm-tables-row-alt-face` (and possibly border + header faces)
- New tests: `modules/lang-markdown/tests.el` additions covering cell parsing,
  padding computation, rebuild idempotence, and the fenced-block skip rule
- No external dependencies; reuses the existing `+theme-default-background`
  cached colour and the `parenthesis` border face used by sibling modules
