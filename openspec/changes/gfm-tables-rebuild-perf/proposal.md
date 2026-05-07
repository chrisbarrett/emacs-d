## Why

`gfm-tables-mode` rebuilds every overlay in the buffer on every edit. After
font-lock has fully fontified the buffer, a single keystroke at `point-min`
incurs ~300 ms of rebuild work plus ~500 ms of font-lock — close to one full
second of latency per character — even when the edit is not inside any table.
The cost is dominated by full-buffer scope and by redundant per-row work
inside the rebuild itself.

## What Changes

- Scope the debounced rebuild to the table containing the changed region, if
  any; edits outside every table become a no-op.
- Compute each row's wrapped layout once per rebuild and share it between
  display composition and per-cell character-bound calculations.
- Memoise visible-width measurements per cell string for the duration of a
  rebuild, since the same fontified cell is measured from several call sites.
- Extend the existing `gfm-tables-stats` output to include a phase-level
  breakdown (block discovery, parse, layout, compose, overlay apply) so future
  regressions are diagnosable from inside Emacs.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-table-rendering`: tighten the rebuild requirement to scope by changed
  region, and broaden the performance-instrumentation requirement to surface
  per-phase timings.

## Impact

- Affected code: `modules/lang-markdown/lib/+gfm-tables.el` —
  `gfm-tables--schedule-rebuild`, `gfm-tables--rebuild`,
  `gfm-tables--apply-table`, `gfm-tables--compose-multiline-row`,
  `gfm-tables--multiline-row-char-bounds`, `gfm-tables--visible-width`,
  `gfm-tables--init-stats` / `gfm-tables--record-stats` /
  `gfm-tables-stats`.
- Behaviour change: edits outside any decorated table no longer trigger a
  rebuild. Window-configuration changes still rebuild the whole buffer (column
  widths depend on window width).
- No external API changes; no new dependencies.
