## MODIFIED Requirements

### Requirement: Callout bordered-block rendering

The callouts decorator's `:apply-block-fn` SHALL render a bordered
callout box with:

- A top border using the type's coloured face and the type label as
  the upper-right caption.
- A `│ ` substitution for the body-line blockquote prefix on each
  body line, using the border face. The substitution SHALL cover
  both the two-char `> ` prefix and the one-char bare `>` form (a
  blockquote continuation line with no trailing space, used in
  practice for blank rows between body paragraphs).
- A right-edge `│` painted via `gfm-pretty-right-after` (or its
  overflow variant on wrapped body lines) so the right border
  aligns to the box width regardless of body-line wrapping.
- A bottom border using the type's coloured face.
- A tinted body background using the type's body-face background
  computed by `gfm-pretty-callouts--tint-bg` (10% from the type
  face foreground toward the theme background).

The decorator's `:apply-block-fn` SHALL call
`gfm-pretty-borders--apply-with-anchors` so anchor overlays
(carrying the body-face property and a `wrap-prefix` of `│ `) are
laid at most once per (block, rebuild pass) while per-window
display overlays (borders and right-edge after-strings) apply once
per window.

#### Scenario: NOTE callout

- **GIVEN** `> [!NOTE]\n> hello`
- **WHEN** the decorator renders
- **THEN** the top border shows `┌── NOTE ──┐` (right-aligned label)
- **AND** the body line shows `│ hello` with a blue-tinted background
- **AND** the bottom border shows `└──────────┘`

#### Scenario: Bare-`>` body line

- **GIVEN** `> [!IMPORTANT]\n> first paragraph\n>\n> second paragraph`
  (note the middle body line is a single `>` with no trailing space)
- **WHEN** the decorator renders
- **THEN** the middle body line shows `│` at the left edge (display
  string `│ `) with no raw `>` character visible
- **AND** the right-edge `│` lands on the box-width column on that
  line, matching the surrounding body rows
- **AND** the per-window body-prefix overlay over the bare `>`
  carries `gfm-pretty-callouts-kind 'body-prefix` and the
  `gfm-pretty-callouts-revealable` property so the reveal walker
  exposes the raw `>` when point sits on the line in the selected
  window
