## MODIFIED Requirements

### Requirement: Blockquotes decorator registration and toggle

The system SHALL register a decorator named `blockquotes` that applies a left-rail overlay decoration on plain GFM blockquote blocks (runs of `^>`-prefixed lines that do NOT belong to a callout block).

The decorator SHALL render the rail (`▌ `) at a configurable column inset (default `tab-width`).  The inset columns to the left of the rail render in the default face (no tint).  No top, right, or bottom decoration glyph is painted.

`(gfm-pretty-toggle-decorator 'blockquotes)` SHALL flip the decorator on and off.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing a plain blockquote
- **THEN** each plain blockquote line shows a `▌` rail glyph at column `gfm-pretty-blockquotes-inset-cols`
- **AND** soft-wrapped continuation visual rows show the same inset gutter and `▌ ` rail prefix in `gfm-pretty-blockquotes-rail-face`

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'blockquotes)` flips the enable bit off
- **THEN** every blockquote overlay (anchor, prefix display) is removed
- **AND** the raw `>` characters and `markdown-mode`'s default `wrap-prefix "> "` reappear on continuation rows

### Requirement: Blockquote left-rail rendering

The blockquotes decorator's `:apply-block-fn` SHALL render the rail with an inset gutter:

- A per-line anchor overlay whose `before-string` is `<inset-spaces>` in the default face, painting the leading gutter.  The before-string MUST live on the anchor (not on the per-window prefix display) so reveal exposing the raw `> ` source does NOT also drop the gutter — without this, point-on-line would visually unshift the body text by `inset-cols` columns.
- A per-line anchor overlay whose `wrap-prefix` is the propertised string `<inset-spaces>▌<space>` (inset in default face, `▌` in `gfm-pretty-blockquotes-rail-face`, trailing space unfaced) so soft-wrapped visual continuation rows show the inset gutter and rail.  Continuation visual rows have no buffer char at column 0 to host a before-string, so the inset is baked into the wrap-prefix.
- A per-window display overlay substituting the two-char `> ` prefix on each blockquote line with `▌<space>` (rail in `gfm-pretty-blockquotes-rail-face`, trailing space unfaced).
- A per-window display overlay substituting the one-char bare `>` form with `▌` (no trailing space — matches the 1-char source).

The decorator SHALL NOT paint a tinted background on the blockquote rectangle, a top / bottom border, or a right-edge terminator after-string.  An earlier draft applied a tinted background spanning each line's content + an after-string padding the bg out to the window margin — it was removed after the user found that Emacs' `:extend t` paints past EOL only on the visual row containing EOL, so a word-wrapped blockquote's intermediate continuation rows were left with default-bg cells between content and the intended right edge (a ragged stepped appearance).  Without per-visual-row padding mechanics (which Emacs does not expose), no clean rectangle is reachable; the rail-only treatment side-steps the issue.

The decorator's `:apply-block-fn` SHALL call `gfm-pretty-borders--apply-with-anchors` (or an equivalent engine seam) so anchor overlays are laid at most once per (block, rebuild pass) while per-window display overlays apply once per window.

#### Scenario: Plain blockquote

- **GIVEN** `> Pain: clutter` with `gfm-pretty-blockquotes-inset-cols` = 4
- **WHEN** the decorator renders
- **THEN** the line shows four columns of leading gutter (default face), then `▌ Pain: clutter` with the rail in `gfm-pretty-blockquotes-rail-face`
- **AND** no raw `>` is visible on that line

#### Scenario: Bare-`>` line in middle of block

- **GIVEN** `> first paragraph\n>\n> second paragraph`
- **WHEN** the decorator renders
- **THEN** the middle line shows the inset gutter followed by `▌` at column `inset-cols` with no raw `>` visible

#### Scenario: Soft-wrapped long line

- **GIVEN** a single source line `> ` followed by 200 characters of text, displayed in a window with `visual-line-mode` enabled
- **WHEN** the decorator renders
- **THEN** visual row 1 shows the inset gutter then `▌ ` then the first chunk of text
- **AND** continuation visual rows show the inset gutter then `▌ ` then the wrapped continuation (the `wrap-prefix` overlay wins over `markdown-mode`'s `wrap-prefix "> "` text property)

### Requirement: Blockquote rail face

The system SHALL define a face `gfm-pretty-blockquotes-rail-face` whose default spec inherits from `font-lock-constant-face`.

The rail glyph in the display overlay and the `▌` cell of the `wrap-prefix` on the anchor overlay SHALL both use this face.

#### Scenario: Face inheritance

- **GIVEN** the default theme
- **WHEN** `face-attribute` reads `gfm-pretty-blockquotes-rail-face :inherit`
- **THEN** it returns `font-lock-constant-face`

### Requirement: Blockquote source reveal

The blockquotes decorator SHALL carry the engine's revealable property (`gfm-pretty-blockquotes-revealable`) on each per-line prefix display overlay.

The engine's reveal walker SHALL hide those overlays in the selected window when point lies on them, exposing the raw `>` (or `> `) source.  The property name SHALL be derived from the blockquotes registry's `tag`; the decorator SHALL NOT register it separately.  Reveal SHALL be scoped to the selected window.

Each prefix display overlay SHALL carry paired `gfm-pretty-display-masked` (the rail string) and `gfm-pretty-display-bare` (the raw source with `gfm-pretty--str-with-region-bg` applied) properties so the variant walker flips the `display` between them based on selection state.

The anchor overlay's `before-string` and `wrap-prefix` SHALL remain in place when the display overlay is revealed; reveal toggles only the per-window prefix display.

#### Scenario: Point on blockquote line

- **GIVEN** a plain blockquote with point on its first line in window W1 (selected)
- **THEN** W1 shows the raw `> ` text in place of the rail prefix
- **AND** the inset gutter remains visible to the left of the raw source (the anchor's `before-string`), so body content does NOT shift horizontally between masked and revealed states
- **AND** other windows showing the buffer continue to show the rail

#### Scenario: Region overlapping blockquote

- **GIVEN** an active region spanning two lines of a plain blockquote in W1 (selected)
- **THEN** every covered line in W1 shows the raw `> ` source with `region` bg painted through
- **AND** other windows continue to show the masked rail

## ADDED Requirements

### Requirement: Blockquote inset gutter customisation

The system SHALL define a defcustom `gfm-pretty-blockquotes-inset-cols` whose default is the symbol `tab-width` (evaluated at render time so the inset tracks the buffer-local `tab-width` unless the user pins a different value).

The decorator SHALL read this value once per `:apply-block-fn` pass and use it as the width of the leading gutter on every blockquote line and every wrap-prefix.

#### Scenario: Default tracks `tab-width`

- **GIVEN** a buffer with `tab-width` = 4 and `gfm-pretty-blockquotes-inset-cols` at its default
- **WHEN** the decorator renders a blockquote
- **THEN** the inset gutter is 4 columns wide

#### Scenario: User overrides defcustom

- **GIVEN** `(setq gfm-pretty-blockquotes-inset-cols 2)` in a buffer with `tab-width` = 4
- **WHEN** the decorator renders a blockquote
- **THEN** the inset gutter is 2 columns wide regardless of `tab-width`
