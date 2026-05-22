## ADDED Requirements

### Requirement: Callout overlay tint faces

The system SHALL define one derived tint face per callout type:

- `gfm-pretty-callouts-note-tint-face`
- `gfm-pretty-callouts-tip-tint-face`
- `gfm-pretty-callouts-important-tint-face`
- `gfm-pretty-callouts-warning-tint-face`
- `gfm-pretty-callouts-caution-tint-face`

Each face's default spec MUST be empty (`'((t))`) — the `:background`
is set dynamically from the per-type blend at theme-change time, not
specified statically.  CAUTION and CRITICAL callouts SHALL share the
caution tint face (mirroring the existing type-face mapping).

The callouts decorator's `:apply-block-fn` SHALL reference these
faces by name (via `:inherit`) in every overlay face spec it
constructs; it MUST NOT bake `(face-background <tint-face>)` or
`(gfm-pretty-callouts--tinted-bg <border-face>)` as a literal
`:background "#hex"` string into any overlay's face plist.

#### Scenario: Tint faces exist with empty default specs

- **GIVEN** `gfm-pretty-callouts` is loaded
- **THEN** each of the five tint faces SHALL be defined
- **AND** `face-attribute <tint-face> :background nil 'default` SHALL
  return `unspecified` (no static colour in the defface)

#### Scenario: Overlay face specs reference tint faces

- **GIVEN** a NOTE callout rendered in a buffer with `gfm-pretty-mode`
  enabled
- **WHEN** the anchor overlay's `face` property is inspected
- **THEN** the face spec SHALL carry
  `:inherit gfm-pretty-callouts-note-tint-face`
- **AND** the face spec SHALL NOT carry a `:background "#hex"`
  literal colour string

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
- A tinted body background sourced from the type's tint face (e.g.
  `gfm-pretty-callouts-note-tint-face`). Every overlay face spec
  SHALL reference the tint face by name via `:inherit`; the
  decorator SHALL NOT bake a `:background "#hex"` literal into any
  overlay face plist (so theme changes propagate at the next
  redisplay without a decorator rebuild).

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

#### Scenario: Overlay face specs carry no baked colour strings

- **GIVEN** a TIP callout rendered in a buffer
- **WHEN** any anchor, prefix-display, or rhs-after-string overlay
  for the block is inspected
- **THEN** none of those overlays' `face` specs SHALL contain a
  `:background` plist key whose value is a string (colour literal)
- **AND** each spec SHALL reference `gfm-pretty-callouts-tip-tint-face`
  via `:inherit`

### Requirement: Theme change responsiveness

The callouts decorator's `:on-enable-fn` SHALL add a face-refresh
helper to `+theme-changed-hook` that recomputes per-type tints from
the current theme background.  `:on-disable-fn` SHALL remove it.

The helper SHALL, in one pass per type, recompute the 10%-toward-bg
blend once and set `:background` on BOTH the body face (e.g.
`gfm-pretty-callouts-note-body-face`) AND the tint face (e.g.
`gfm-pretty-callouts-note-tint-face`) for that type.  Sharing the
single blend call between body and tint faces SHALL prevent drift
between buffer-char tinting (body face, applied via font-lock) and
overlay-cell tinting (tint face, referenced via `:inherit` from
overlay face specs).

Because overlay face specs reference the tint face by name (not by
baked colour), existing rendered overlays SHALL pick up the new
`:background` at the next redisplay without a decorator rebuild.

#### Scenario: Theme switch — body and tint faces refresh together

- **WHEN** the user switches from a light to a dark theme
- **THEN** `+theme-changed-hook` fires
- **AND** each `gfm-pretty-callouts-*-body-face` background is
  recomputed
- **AND** each `gfm-pretty-callouts-*-tint-face` background is
  recomputed in the same pass
- **AND** the next redisplay shows correctly tinted body backgrounds

#### Scenario: Theme switch propagates to already-rendered overlays

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and at least one
  callout rendered
- **WHEN** the user switches theme
- **AND** `gfm-pretty-mode` is NOT toggled off and on
- **THEN** the existing overlays SHALL re-resolve their face specs at
  the next redisplay
- **AND** the rendered callout panel SHALL show the new theme's tint
  colour, not the prior theme's
