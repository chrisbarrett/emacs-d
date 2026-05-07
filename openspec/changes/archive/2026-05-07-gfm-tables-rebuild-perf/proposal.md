## Why

`gfm-tables-mode` rebuilds every overlay in the buffer on every edit.
After font-lock has fully fontified the buffer, a single keystroke at
`point-min` incurs ~300 ms of rebuild work plus ~500 ms of font-lock —
close to one full second of latency per character — even when the edit
is not inside any table.  The cost is dominated by full-buffer scope
and by redundant per-row work inside the rebuild itself.

In parallel, the renderer sized every overlay against a single
"available width" sampled from one window, so the same buffer split
across two windows (or shared between a graphical and terminal frame)
rendered correctly in only one of them.

## What Changes

- Scope the debounced rebuild to the table containing the changed
  region, if any; edits outside every table become a no-op.
- Compute each row's wrapped layout once per rebuild and share it
  between display composition and per-cell character-bound
  calculations.
- Memoise visible-width measurements per cell string for the duration
  of a rebuild, since the same fontified cell is measured from several
  call sites.
- Cache the result of `gfm-tables--find-blocks` by
  `buffer-chars-modified-tick`, so cell-wise motion does not re-scan
  the buffer between edits.
- Extend the existing `gfm-tables-stats` output to include a
  phase-level breakdown (block discovery, parse, layout, compose,
  overlay apply) so future regressions are diagnosable from inside
  Emacs.
- Split the per-row overlay into an anchor overlay (source-side
  bookkeeping shared across windows) and a display overlay (rendering
  restricted to a single window via the `window' overlay property),
  so a buffer shown in multiple windows renders at each window's
  width.
- Reconcile per-window display overlays incrementally on
  window-configuration changes: added or resized windows get a
  prioritised rebuild (visible-window blocks now, off-screen on the
  next idle tick); removed windows have their display overlays
  swept up; untouched windows keep their existing overlay objects.
- Suppress redundant rebuilds when a window-configuration event
  fires but no `(window . max-chars-per-line)' pair has actually
  changed.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-table-rendering`: tighten the rebuild requirement to scope by
  changed region, broaden the rebuild trigger to be window-state-aware
  with prioritised and selective per-window reconciliation, broaden
  the performance-instrumentation requirement to surface per-phase
  timings, and add per-window rendering plus a guarantee that
  cell-edit commit preserves the surrounding row.

## Impact

- Affected code: `modules/lang-markdown/lib/+gfm-tables.el` —
  `gfm-tables--schedule-rebuild`, `gfm-tables--rebuild`,
  `gfm-tables--apply-table` (split into `--parse-table` plus
  `--apply-table-display`), the new
  `gfm-tables--reconcile-windows' / `--rebuild-window-prioritised' /
  `--rebuild-block-for-window' / `--remove-display-overlays-*'
  helpers, the new anchor / display overlay split,
  `gfm-tables--visible-width', `gfm-tables--find-blocks',
  `gfm-tables--init-stats' / `--record-stats' / `gfm-tables-stats',
  and the cell-edit commit pipeline (`--cell-edit-mark-pending' +
  `--cell-edit-after-commit').
- Behaviour change: edits outside any decorated table no longer
  trigger a rebuild.  Window-configuration changes that do not affect
  any window's width are no-ops.  Buffers shown in multiple windows
  render at each window's own width.  Cell-edit commits no longer
  leave a stray newline behind.
- No external API changes; no new dependencies.
