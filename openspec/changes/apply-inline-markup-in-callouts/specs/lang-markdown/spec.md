## MODIFIED Requirements

### Requirement: Callout bordered-block rendering

The system SHALL adorn every discovered callout with a curved
border composed of:

- a top border `┌─ TYPE ─…─┐` covering the marker line and
  extending across a window-sized width
- a left edge `│ ` substituting the `> ` prefix on every body line
- a right edge sized to the box width via an after-string
  containing `space :align-to`
- a bottom border `└─…─┘` on its own visual row beneath the last
  body line (or beneath the marker line when the body is empty)
- a `wrap-prefix` of `│ ` on continuation visual lines so wrapped
  content stays visually inside the box
- a tinted background face derived by blending the type face's
  foreground 10 % toward the default background, painted across
  every line of the callout
- a per-line right-edge fill that paints the visual line to the
  window edge in the default face, so any `:extend t` background —
  whether carried by a text property or by another mode's overlay
  (such as `hl-line` or `region`) — has no past-EOL region left to
  fill and cannot extend past the right border

The body-line anchor overlay's face SHALL specify only
`:background` (when a tint is resolvable) and `:extend t`, leaving
every other face attribute unspecified.  This allows the markdown
emphasis faces carried by buffer text under the anchor
(`markdown-italic-face`, `markdown-bold-face`,
`markdown-strike-through-face`, `markdown-link-face`,
`markdown-inline-code-face`) to merge through and render with their
respective attributes (slant, weight, underline, strike-through,
foreground).

To prevent `markdown-blockquote-face`'s default italic from being
applied across callout body lines (and across plain blockquotes
elsewhere in the buffer), the configuration SHALL detach
`markdown-blockquote-face` from `font-lock-doc-face` so it no longer
carries `:slant italic`.

#### Scenario: Callout renders with curved box and label

- **WHEN** an `[!IMPORTANT]` callout with two body lines is
  decorated
- **THEN** the rendered output has a top border whose left segment
  reads `┌─ IMPORTANT ─` and right segment is dash fill ending in
  `┐`, body lines bracketed by `│ ` and a right-aligned `│`, and a
  matching bottom border on its own row

#### Scenario: Body-less callout renders with bottom border on marker line

- **WHEN** an `[!NOTE]` callout with no body lines is decorated
- **THEN** the bottom border attaches to the marker line's trailing
  after-string so the box still closes

#### Scenario: Body line with an extend background does not leak past the border

- **WHEN** a callout body line carries a face with `:extend t` — for
  example `hl-line` while point is on that line
- **THEN** the `:extend` background is confined to the box interior and
  does not paint past the right-edge `│` to the window edge

#### Scenario: Bold inline markup inside a callout body renders bold

- **WHEN** a callout body line contains `**bold**`
- **THEN** the characters covered by `markdown-bold-face` render
  with the bold weight contributed by that face, on top of the
  callout's tinted background

#### Scenario: Italic inline markup inside a callout body renders italic

- **WHEN** a callout body line contains `*italic*`
- **THEN** the characters covered by `markdown-italic-face` render
  with the italic slant contributed by that face, on top of the
  callout's tinted background

#### Scenario: Link text inside a callout body renders with link styling

- **WHEN** a callout body line contains `[label](url)`
- **THEN** the link-text characters covered by `markdown-link-face`
  render with that face's foreground / underline, on top of the
  callout's tinted background

#### Scenario: Inline code inside a callout body renders with code styling

- **WHEN** a callout body line contains `` `code` ``
- **THEN** the code characters covered by `markdown-inline-code-face`
  render with that face's attributes (fixed pitch, contrasting
  foreground), on top of the callout's tinted background

#### Scenario: Plain callout body text renders without italic

- **WHEN** a callout body line contains text with no inline markup
- **THEN** the text renders without `:slant italic`, because
  `markdown-blockquote-face` no longer inherits italic from
  `font-lock-doc-face`
