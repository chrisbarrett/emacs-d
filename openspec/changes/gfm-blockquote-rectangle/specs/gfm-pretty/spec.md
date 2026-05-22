## MODIFIED Requirements

### Requirement: Blockquotes decorator registration and toggle

The system SHALL register a decorator named `blockquotes` that applies a tinted-rectangle overlay decoration on plain GFM blockquote blocks (runs of `^>`-prefixed lines that do NOT belong to a callout block).

The rectangle SHALL render with a left rail (`▌ `) at a configurable column inset, a tinted background painted by `gfm-pretty-blockquotes-bg-face`, and no top, right, or bottom border glyphs.

`(gfm-pretty-toggle-decorator 'blockquotes)` SHALL flip the decorator on and off.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing a plain blockquote
- **THEN** each plain blockquote line shows a `▌` rail glyph at column `gfm-pretty-blockquotes-inset-cols`
- **AND** each plain blockquote line shows a tinted background painted by `gfm-pretty-blockquotes-bg-face`
- **AND** soft-wrapped continuation visual rows show the same inset gutter and `▌ ` rail prefix in `gfm-pretty-blockquotes-rail-face`

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'blockquotes)` flips the enable bit off
- **THEN** every blockquote rectangle overlay (anchor, prefix display, rhs after-string) is removed
- **AND** the raw `>` characters and `markdown-mode`'s default `wrap-prefix "> "` reappear on continuation rows

### Requirement: Blockquote left-rail rendering

The blockquotes decorator's `:apply-block-fn` SHALL render a tinted rectangle with a left rail and an inset gutter:

- A per-line anchor overlay whose `face` is `(:inherit gfm-pretty-blockquotes-bg-face :extend t)`. The face MUST be referenced by name (not baked as a `:background <hex>` colour string) so a theme change propagates to existing overlays at the next redisplay without a full decorator rebuild.
- A per-line anchor overlay whose `before-string` is `<inset-spaces>` in the default face, painting the untinted leading gutter. The before-string MUST live on the anchor (not on the per-window prefix display) so reveal exposing the raw `> ` source does NOT also drop the gutter — without this, point-on-line would visually unshift the body text by `inset-cols` columns.
- A per-line anchor overlay whose `wrap-prefix` is the propertised string `<inset-spaces>▌<space>` (where `<inset-spaces>` is `gfm-pretty-blockquotes-inset-cols` spaces in the default face, and `▌<space>` is painted with `gfm-pretty-blockquotes-rail-face` + the bg face's background) so soft-wrapped visual continuation rows show a continuous inset gutter and rail. Continuation visual rows have no buffer char at column 0 to host a before-string, so the inset is baked into the wrap-prefix.
- A per-window display overlay substituting the two-char `> ` prefix on each blockquote line with `▌<space>`. The face spec inherits `(gfm-pretty-blockquotes-rail-face gfm-pretty-blockquotes-bg-face)` by reference (not as baked colours).
- A per-window display overlay substituting the one-char bare `>` form with `▌` (no trailing space — matches the 1-char source).
- A per-window rhs after-string at line-end whose first cell pads bg-tinted out to `(space :align-to (- right 1))` and whose second cell is `(space :align-to right)` in the default face. The window-relative `align-to` SHALL be used (not a box-width-derived absolute column) so the right edge terminates at a predictable column — one cell before the window margin — on every blockquote line regardless of content length.

The decorator SHALL NOT clamp the rectangle to a narrower box-width. Earlier iterations clamped to `(min (- text-width inset-cols) (max 80 (+ max-content 4)))`, which produced a stepped right edge under word-wrap because Emacs' `:extend t` paints past EOL only on the visual row containing EOL — continuation rows of a word-wrapped line have no EOL, so they leave default-bg cells past the wrap point and visually undercut a clamped right edge. Window-margin termination sidesteps that by not pretending the rectangle has a defined right column narrower than the window.

The decorator SHALL NOT paint a top border, a bottom border, or a right-edge border glyph.

The decorator's `:apply-block-fn` SHALL call `gfm-pretty-borders--apply-with-anchors` (or an equivalent engine seam) so anchor overlays are laid at most once per (block, rebuild pass) while per-window display overlays apply once per window.

#### Scenario: Plain blockquote

- **GIVEN** `> Pain: clutter` with `gfm-pretty-blockquotes-inset-cols` = 4
- **WHEN** the decorator renders
- **THEN** the line shows four columns of untinted leading gutter, then `▌ Pain: clutter` with the rail in `gfm-pretty-blockquotes-rail-face`
- **AND** the content cells of the line are painted with `gfm-pretty-blockquotes-bg-face`'s background
- **AND** no raw `>` is visible on that line

#### Scenario: Bare-`>` line in middle of block

- **GIVEN** `> first paragraph\n>\n> second paragraph`
- **WHEN** the decorator renders
- **THEN** the middle line shows the inset gutter followed by `▌` at column `inset-cols` with no raw `>` visible
- **AND** the tinted background is visually continuous across all three rows
- **AND** the rhs after-string still fires on the bare line so the rectangle's right edge aligns with the surrounding rows

#### Scenario: Soft-wrapped long line

- **GIVEN** a single source line `> ` followed by 200 characters of text, displayed in a window with `visual-line-mode` enabled
- **WHEN** the decorator renders
- **THEN** visual row 1 shows the inset gutter then `▌ ` then the first chunk of text
- **AND** visual row 2 shows the inset gutter then `▌ ` then the wrapped continuation (the `wrap-prefix` overlay wins over `markdown-mode`'s `wrap-prefix "> "` text property)
- **AND** the rhs after-string on the LAST wrapped visual row pads bg out to one column before the window margin and terminates with a default-face `(space :align-to right)` span at the margin
- **AND** continuation visual rows above the last MAY appear ragged on the right where word-wrap left default-bg cells past the wrap point — this is an inherent Emacs constraint (`:extend t` applies only past EOL, not past a soft wrap), not a decorator bug

#### Scenario: Rectangle right edge does not leak past terminator

- **GIVEN** a plain blockquote in a window with `hl-line-mode` enabled
- **WHEN** point is on a blockquote line
- **THEN** the `hl-line` background does NOT paint past the column where the rhs after-string's default-face `(space :align-to right)` span begins (one column before the window margin)
- **AND** the tinted rectangle background does NOT paint past the same column

#### Scenario: Theme change refreshes overlay tint

- **GIVEN** `gfm-pretty-mode' is enabled in a buffer containing plain blockquotes
- **WHEN** the active theme changes such that `gfm-pretty-blockquotes-bg-face's `:background' resolves to a new colour
- **THEN** the existing anchor and prefix-display overlays render with the new background at the next redisplay without a decorator rebuild (because the face spec carries `:inherit gfm-pretty-blockquotes-bg-face' rather than a frozen `:background <hex>')

### Requirement: Blockquote rail face

The system SHALL define a face `gfm-pretty-blockquotes-rail-face` whose default spec inherits from `font-lock-constant-face`.

The rail glyph in the display overlay and the `wrap-prefix` in the anchor overlay SHALL both use this face, composed at render time with `gfm-pretty-blockquotes-bg-face`'s background so the rail cell shows the tinted bg.

#### Scenario: Face inheritance

- **GIVEN** the default theme
- **WHEN** `face-attribute` reads `gfm-pretty-blockquotes-rail-face :inherit`
- **THEN** it returns `font-lock-constant-face`

#### Scenario: Rail cell shares tinted bg

- **GIVEN** a blockquote line where the rail glyph sits at column `inset-cols`
- **WHEN** the line renders
- **THEN** the rail cell's background matches `gfm-pretty-blockquotes-bg-face`'s background (rail glyph is NOT untinted against the surrounding tint)

### Requirement: Blockquote source reveal

The blockquotes decorator SHALL carry the engine's revealable property (`gfm-pretty-blockquotes-revealable`) on each per-line prefix display overlay.

The engine's reveal walker SHALL hide those overlays in the selected window when point lies on them, exposing the raw `>` (or `> `) source. The property name SHALL be derived from the blockquotes registry's `tag`; the decorator SHALL NOT register it separately. Reveal SHALL be scoped to the selected window.

Each prefix display overlay SHALL carry paired `gfm-pretty-display-masked` (the inset + rail string) and `gfm-pretty-display-bare` (the raw source with `gfm-pretty--str-with-region-bg` applied) properties so reveal flips the `display` between them based on point/selection state.

Each rhs after-string overlay SHALL carry paired `gfm-pretty-after-masked` (tinted pad + default terminator) and `gfm-pretty-after-bare` (region-tinted pad + `gfm-pretty--region-tail`) properties so reveal flips the `after-string` in lockstep with the prefix.

The anchor overlay's `wrap-prefix` and `face` SHALL remain in place when the display overlays are revealed; reveal toggles only the per-window display and after-string properties.

#### Scenario: Point on blockquote line

- **GIVEN** a plain blockquote with point on its first line in window W1 (selected)
- **THEN** W1 shows the raw `> ` text in place of the rail prefix
- **AND** the untinted inset gutter remains visible to the left of the raw source (the anchor's `before-string`), so body content does NOT shift horizontally between masked and revealed states
- **AND** other windows showing the buffer continue to show the inset+rail rectangle

#### Scenario: Region overlapping blockquote

- **GIVEN** an active region spanning two lines of a plain blockquote in W1 (selected)
- **THEN** every covered line in W1 shows the raw `> ` source
- **AND** the bg face's tint is overlaid with `region` bg across the line
- **AND** other windows continue to show the masked rectangle

## ADDED Requirements

### Requirement: Blockquote inset gutter customisation

The system SHALL define a defcustom `gfm-pretty-blockquotes-inset-cols` whose default is the symbol `tab-width` (evaluated at render time so the inset tracks the buffer-local `tab-width` unless the user pins a different value).

The decorator SHALL read this value once per `:apply-block-fn` pass and use it as the width of the leading gutter on every blockquote line, every wrap-prefix, and every rhs after-string padding calculation.

#### Scenario: Default tracks `tab-width`

- **GIVEN** a buffer with `tab-width` = 4 and `gfm-pretty-blockquotes-inset-cols` at its default
- **WHEN** the decorator renders a blockquote
- **THEN** the inset gutter is 4 columns wide

#### Scenario: User overrides defcustom

- **GIVEN** `(setq gfm-pretty-blockquotes-inset-cols 2)` in a buffer with `tab-width` = 4
- **WHEN** the decorator renders a blockquote
- **THEN** the inset gutter is 2 columns wide regardless of `tab-width`

### Requirement: Blockquote background face

The system SHALL define a face `gfm-pretty-blockquotes-bg-face` with theme-mode-reactive backgrounds matching `gfm-pretty-tables-row-alt-face` (the alternating-row stripe colour used by GFM tables) and `:extend t` so the bg paints past EOL until the rhs after-string's terminator suppresses the leak.

The face SHALL be an independent copy of the tables face — it MUST NOT use `:inherit gfm-pretty-tables-row-alt-face` — so users can re-theme the blockquote rectangle without altering the tables stripe.

The decorator's per-line anchor overlay's `face` property SHALL be an anonymous spec carrying only `(:background <bg-of-this-face> :extend t)` so emphasis faces (bold, italic, link, inline code) on buffer text merge through without being clobbered.

#### Scenario: Default backgrounds match tables alt-row

- **GIVEN** the default light theme
- **WHEN** `face-background` reads both faces
- **THEN** `gfm-pretty-blockquotes-bg-face`'s `:background` equals `gfm-pretty-tables-row-alt-face`'s `:background` at definition time

#### Scenario: Re-theming does not couple to tables

- **GIVEN** the user calls `(set-face-background 'gfm-pretty-blockquotes-bg-face "#abcdef")`
- **THEN** `gfm-pretty-tables-row-alt-face`'s background is unchanged
