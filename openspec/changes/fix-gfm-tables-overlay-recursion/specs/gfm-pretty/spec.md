## ADDED Requirements

### Requirement: Table rebuilds are idempotent

The tables decorator's cell parse SHALL ignore its own display overlays.
`gfm-pretty-tables--transcribe-source-overlays` SHALL skip every overlay
carrying the `gfm-pretty-tables-display` property when computing
width-affecting splices, so the parser never bakes prior render output
into cell text.

The property holds at every rebuild entry point — the full-clear
`--rebuild`, the engine-driven `--apply-block`, the per-window
`--rebuild-block-for-window`, and the visible-first
`--rebuild-window-prioritised`. Foreign width-affecting overlays
(e.g. links decorator display strings) MUST continue to splice into
cell text so column widths reflect the decorated content.

Internals-facing.

#### Scenario: Repeated rebuild converges

- **GIVEN** a GFM buffer with `gfm-pretty-mode` enabled and at least
  one table whose rows have already been decorated
- **WHEN** the tables decorator's rebuild path runs a second time
  without an intervening edit
- **THEN** the per-row display overlay's `display` string equals the
  string produced by the first rebuild, character-for-character
- **AND** no display string contains the substring `│ │ │ │ │ ` (five
  consecutive border chars — the signature of a self-fed parse)

#### Scenario: Per-window rebuild after narrowing

- **GIVEN** a buffer with a GFM table whose source spans the visible
  region, with display overlays already applied
- **WHEN** the buffer is narrowed to a region containing the table and
  the per-window rebuild path (`--reconcile-windows` →
  `--rebuild-window-prioritised` → `--rebuild-block-for-window`) runs
- **THEN** the resulting row display strings match a fresh
  full-rebuild on a widened buffer with no prior overlays
- **AND** the table renders correctly under `gfm-present-mode` slide
  navigation, which churns `window-configuration-change-hook`

#### Scenario: Foreign overlay decoration still splices

- **GIVEN** a table cell whose source is `[name](https://example.com)`
  and the links decorator has placed a `display`-carrying overlay over
  the link's URL
- **WHEN** the tables decorator parses the cell
- **THEN** the parser splices the links overlay's display string into
  the cell text (column-width measurement honours the decorated width)
- **AND** the parser does NOT splice any `gfm-pretty-tables-display`
  overlay text into the cell, even if such an overlay also covers the
  region
