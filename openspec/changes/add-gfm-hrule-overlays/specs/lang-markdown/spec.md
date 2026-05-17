## ADDED Requirements

### Requirement: HR mode toggle

The system SHALL provide a buffer-local minor mode `gfm-hrule-mode`
that, when enabled, decorates GFM dash-form thematic break lines
with a full-width unicode horizontal bar, and when disabled removes
all such decorations from the buffer.

The mode SHALL auto-enable in `gfm-mode` via `gfm-mode-hook`.

#### Scenario: Mode enable installs overlays

- **WHEN** `gfm-hrule-mode` is enabled in a buffer containing a
  dash-form HR line
- **THEN** the HR line carries a display overlay rendering a
  unicode bar across the displaying window's width

#### Scenario: Mode disable removes all overlays

- **WHEN** `gfm-hrule-mode` is disabled in a buffer that previously
  had HR decorations
- **THEN** no overlays tagged with the HR mode remain in the buffer
  and the source `---` characters are visible at their original
  positions

#### Scenario: Mode auto-enables under gfm-mode

- **WHEN** a buffer enters `gfm-mode`
- **THEN** `gfm-hrule-mode` is enabled as part of `gfm-mode-hook`

### Requirement: HR block discovery

The system SHALL discover HR lines for decoration by walking the
buffer for ranges where markdown-mode has set the `markdown-hr`
text property, filtered to ranges whose underlying source starts
with a `-` character (dash form).  Asterisk-form (`***`) and
underscore-form (`___`) HRs SHALL NOT be decorated.

HR lines whose first non-whitespace character is `>` (HRs nested
inside a blockquote or callout body) SHALL NOT be decorated.

#### Scenario: Dash-form HR is discovered

- **WHEN** the buffer contains `\n---\n`
- **THEN** the system identifies one HR block spanning the BOL to
  EOL of the dash line

#### Scenario: Asterisk-form HR is not decorated

- **WHEN** the buffer contains `\n***\n`
- **THEN** the system does not produce an HR decoration for that
  line, leaving markdown-mode's font-lock rendering in place

#### Scenario: Underscore-form HR is not decorated

- **WHEN** the buffer contains `\n___\n`
- **THEN** the system does not produce an HR decoration for that
  line

#### Scenario: Setext-2 heading underline is not decorated

- **WHEN** the buffer contains `Heading\n---\n` (text on the
  preceding non-blank line)
- **THEN** markdown-mode does not set the `markdown-hr` text
  property on the dash line, and the system produces no HR
  decoration

#### Scenario: Dashes inside a fenced code block are not decorated

- **WHEN** dash-only lines appear between fence delimiters
- **THEN** the system produces no HR decoration for those lines

#### Scenario: HR nested in a blockquote is not decorated

- **WHEN** the buffer contains `\n> ---\n`
- **THEN** the system produces no HR decoration for that line

### Requirement: HR rendering

The system SHALL render each discovered HR line as a single visual
row composed of the character `─` (U+2500 BOX DRAWINGS LIGHT
HORIZONTAL) repeated to the displaying window's available character
width, propertised with face `+markdown-gfm-hrule-face`.

The decoration SHALL replace the buffer's HR characters via a
`display` property on a per-window display overlay covering the HR
line's `[BEG, EOL]` range.

#### Scenario: HR renders as a full-width unicode bar

- **WHEN** a dash-form HR line is decorated in a window whose
  `window-max-chars-per-line` is 100
- **THEN** the rendered line is 100 `─` characters wide

#### Scenario: HR face matches the configured face

- **WHEN** an HR line is decorated
- **THEN** every character in the bar carries `+markdown-gfm-hrule-face`

### Requirement: HR per-window rendering

The system SHALL produce display overlays sized to each window
currently showing the buffer, so the same buffer split across
windows of different widths renders a bar at each window's own
width.  Display overlays SHALL be restricted to a single window via
Emacs's `window` overlay property.

When no window currently displays the buffer, the system SHALL
produce a single unrestricted display overlay set at a fallback
width derived from `gfm-block-borders--available-width` (which
falls back to `fill-column` then 80), and replace it with
window-restricted overlays as soon as a window starts showing the
buffer.

#### Scenario: Buffer split across windows of different widths

- **WHEN** the buffer is shown in two windows whose
  `window-max-chars-per-line` are 100 and 60
- **THEN** the 100-width window's overlay renders 100 dashes and
  the 60-width window's renders 60 dashes

#### Scenario: Window resize rebuilds the bar

- **WHEN** a window currently displaying a decorated HR is resized
- **THEN** the next reconciliation rebuilds that window's HR
  overlay at the new width

### Requirement: HR cursor reveal

The system SHALL suppress the display of an HR line's overlay in the
selected window when point lies on that line, exposing the
underlying source `---` for editing.  The display SHALL be restored
when point leaves the line.  Reveal SHALL be scoped to the selected
window: cursor in window A SHALL NOT expose source in window B.

#### Scenario: Entering the HR line reveals source

- **WHEN** point moves onto a decorated HR line
- **THEN** the overlay's display is suppressed in the selected
  window and the raw `---` becomes visible

#### Scenario: Leaving the HR line restores decoration

- **WHEN** point moves off a previously-revealed HR line
- **THEN** the overlay's display is restored

#### Scenario: Per-window reveal does not expose source in other windows

- **WHEN** the buffer is shown in two windows and point on the HR
  line lives in only one of them
- **THEN** the other window's overlay continues to render the bar

### Requirement: HR debounced rebuild

The system SHALL debounce overlay rebuilds in response to buffer
edits and window-configuration changes via the shared
`gfm-block-borders--arm-rebuild-timer` scheduler, so a burst of
edits or rapid resize events results in at most one rebuild on the
trailing idle.

#### Scenario: Rapid edits coalesce into one rebuild

- **WHEN** five consecutive single-character insertions occur on an
  HR line within one idle window
- **THEN** the rebuild scheduler runs exactly once

### Requirement: HR narrowing-resilient discovery and teardown

The system SHALL widen the buffer for the duration of HR discovery
so that narrowing does not hide HR lines outside the current
restriction from being recorded for rebuild correctness.  Overlay
teardown SHALL operate on the buffer (not the narrowing) so leaving
narrowed editing modes does not leave stale overlays from the wider
view.

#### Scenario: HR outside narrowing is still tracked

- **WHEN** the buffer is narrowed to a region not containing an HR
  line that exists elsewhere in the buffer
- **THEN** widening restores all HR decorations without rebuilding
  from scratch
