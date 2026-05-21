## Context

`gfm-pretty-tables` decorates GFM tables by parsing each row into cells,
fontifying every cell, and overlaying a composed display string on top of
the source row. To make rendering width-correct in the presence of *other*
decorators (notably `gfm-pretty-links`, which rewrites `[name](url)` to a
narrower display string), the cell-fontify path calls
`gfm-pretty-tables--transcribe-source-overlays`
(`lisp/gfm/gfm-pretty-tables.el:90`) to splice any width-affecting
`display` overlay text into the cell string before measurement.

The bug: this splicing is unconditional — it includes the tables
decorator's *own* per-row display overlays. The main `--rebuild` path
clears all overlays before re-parsing, so it never observes its own
output. But the per-window paths
(`--rebuild-block-for-window` at line 1042, called via `--apply-block`,
`--reconcile-windows`, `--rebuild-window-prioritised`) parse the table
*before* removing the old display overlays:

```
(parsed (gfm-pretty-tables--parse-table …))             ;; sees old overlays
(gfm-pretty-tables--remove-display-overlays-in-block …) ;; too late
(gfm-pretty-tables--apply-table-display-for-parsed parsed window)
```

The old row-level display overlay covers each cell's source region
exactly, so its display string is spliced verbatim into every cell of
the new parse. The next per-window rebuild compounds, and so on — by the
fourth or fifth pass cells contain kilobytes of nested pipes. Triggered
in practice by `gfm-present-mode` because slide navigation churns
window-start/window-end repeatedly, firing
`window-configuration-change-hook` → `--reconcile-windows`.

Verified empirically against
`/private/tmp/presentations/2026-05-21T22-07-drift-detection-onboarding.md`:
`(gfm-pretty-tables--fontify-row-cells 1737 (line-end-position))` on the
header row returned a 6 KB string per cell instead of `"Workflow"`.

## Goals / Non-Goals

**Goals:**

- Make the tables decorator idempotent under repeated rebuilds at every
  entry point (full and per-window), not just the full-clear path.
- Preserve the existing splicing behaviour for *foreign* width-affecting
  overlays (links decorator), so column widths stay correct.
- Add regression coverage so the property is enforced by the test suite,
  not by ordering discipline at each caller.

**Non-Goals:**

- Rewriting the per-window rebuild architecture.
- Reordering every caller (brittle; misses any new caller that
  forgets the ordering rule).
- Changing the public decorator registration protocol.

## Decisions

### Decision: skip own overlays in transcribe, not reorder callers

`--transcribe-source-overlays` SHALL skip any overlay carrying our own
`gfm-pretty-tables-display` property. The check is a one-line
`(unless (overlay-get ov 'gfm-pretty-tables-display) …)` inside the
`dolist` at line 111.

**Alternatives considered:**

- **Reorder `--rebuild-block-for-window`** to remove overlays before
  parsing. Local fix, but the invariant lives at every caller — easy to
  break with a future entry point. Also fragile under partial rebuilds
  that intentionally want to inspect prior state.
- **Tag display overlays with `priority`/`category` and filter on that**.
  No advantage over the dedicated property the overlays already carry.
- **Have `--parse-table` widen and snapshot, then remove, then re-parse**.
  Doubles parse cost on every rebuild.

The skip approach defends every entry point, costs O(1) per overlay
checked, and aligns with how `--make-display`
(`lisp/gfm/gfm-pretty-tables.el:874`) already tags its display overlays.

### Decision: regression test asserts double-rebuild convergence

The test SHALL build the table, capture each row overlay's `display`
string, rebuild a second time, and assert character-for-character
equality. This catches the bug independent of *which* caller invokes
the second rebuild. A second test under the existing
`narrowing-regression` ert tag drives the
`(enable → narrow-to-slide → reconcile → widen → rebuild)` sequence so
present-mode regressions surface in `make test-quick`.

**Alternatives considered:**

- **Snapshot-test the corrupted output**. Brittle: composed row text
  depends on window width and theme.
- **Property-based fuzz over rebuild sequences**. Overkill for a
  single-axis invariant.

## Risks / Trade-offs

- **[Risk]** A future decorator might legitimately want to read its own
  prior display overlay (e.g. for diff-based incremental rendering).
  **Mitigation:** the skip is keyed on `gfm-pretty-tables-display`
  specifically, not a generic "ignore decorator-owned overlays" rule.
  Any future opt-out can pass that explicit name.
- **[Risk]** Links overlays placed *inside* a row's source region
  during the window between parse and removal could be skipped if they
  happen to carry the tables tag. **Mitigation:** none observed —
  only `--make-display` sets that tag, and it's scoped to row-wide
  display overlays.
- **[Trade-off]** The fix moves the invariant from "callers must remove
  before parsing" to "parser must filter its own output". The latter is
  easier to test in isolation and survives caller refactors.
