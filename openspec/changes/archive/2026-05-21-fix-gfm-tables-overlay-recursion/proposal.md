## Why

In presentation mode (and any other per-window rebuild path), GFM table
overlays compound on themselves: each rebuild splices the previous render's
display string back into every cell, so after a few window-reconciliation
passes the table renders as a nested 6 KB mess of pipes and column-name
fragments. Root cause is `gfm-pretty-tables--rebuild-block-for-window`
parsing the table *before* removing the old display overlays — the parser's
`--transcribe-source-overlays` then bakes the prior render into the new cells.

## What Changes

- Make table parse ignore the tables decorator's own display overlays.
  `gfm-pretty-tables--transcribe-source-overlays` SHALL skip any overlay
  carrying `gfm-pretty-tables-display`, so the splicing path only ever bakes
  *foreign* overlay decoration (e.g. links) into cell text.
- Add a regression test asserting `(rebuild → rebuild)` converges: the
  second rebuild's display strings equal the first's, character-for-character.
- Add a presentation-shaped regression test under the existing
  `narrowing-regression` tag covering the
  `(narrow → reconcile-windows → widen → rebuild)` sequence used by
  `gfm-present-mode`.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: tables decorator rebuild contract gains an idempotency
  requirement — repeated rebuilds SHALL NOT feed prior render output back
  as cell content.

## Impact

- Code: `lisp/gfm/gfm-pretty-tables.el` (`--transcribe-source-overlays`),
  `lisp/gfm/gfm-pretty-tests.el` (new regression tests).
- No public API change. No keybinding change. No spec axis added.
- Affects any caller of the per-window rebuild path:
  `--apply-block`, `--reconcile-windows`, `--rebuild-window-prioritised`.
  Most visibly: `gfm-present-mode` slide navigation.
