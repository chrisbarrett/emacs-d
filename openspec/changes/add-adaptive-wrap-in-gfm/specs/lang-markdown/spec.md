## ADDED Requirements

### Requirement: Adaptive wrap-prefix on visual-line continuation rows

GFM buffers SHALL enable `adaptive-wrap-prefix-mode` such that wrapped visual lines inherit the originating line's leading-whitespace indent.

The mode is activated via `gfm-mode-hook` alongside `visual-line-mode`. The `adaptive-wrap` package is declared as a project dependency in `modules/lang-markdown/packages.eld`.

Overlay-set `wrap-prefix` properties installed by `gfm-pretty` decorators (callouts, fences, tables) MUST continue to take effect inside their respective block ranges; `adaptive-wrap`'s text-property `wrap-prefix` applies only where no overlay supersedes it.

#### Scenario: Nested list item continuation aligns under content column

- **GIVEN** a GFM buffer containing a nested list item whose body text is hard-wrapped to a width narrower than the window and indented to the bullet's content column in the source
- **WHEN** the buffer is rendered in a window narrower than the source's hard-wrap column, forcing visual-line wrapping
- **THEN** continuation visual lines of that list item SHALL be prefixed to match the source line's leading-whitespace column, not column 0

#### Scenario: Blockquote continuation preserves quote-marker indent

- **GIVEN** a GFM buffer containing a blockquote line that exceeds the window width
- **WHEN** the line wraps under `visual-line-mode`
- **THEN** the continuation row SHALL be indented to match the blockquote line's leading whitespace

#### Scenario: gfm-pretty decorator wrap-prefix is unaffected

- **GIVEN** a GFM buffer containing a callout, a fenced code block, and a table — each producing overlay `wrap-prefix` via its `gfm-pretty` decorator
- **WHEN** `adaptive-wrap-prefix-mode` is enabled in the same buffer
- **THEN** the callout left bar, fence borders, and table borders SHALL continue to render with their decorator-supplied wrap-prefix on continuation rows

#### Scenario: Disabling adaptive-wrap restores flush continuation

- **GIVEN** a GFM buffer with `adaptive-wrap-prefix-mode` enabled
- **WHEN** the user toggles `adaptive-wrap-prefix-mode` off
- **THEN** continuation visual lines outside `gfm-pretty` decorator ranges SHALL fall back to column 0
- **AND** decorator-managed ranges SHALL continue to render their overlay `wrap-prefix` unchanged
