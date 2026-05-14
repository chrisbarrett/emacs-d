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
- an extend-clip anchor over the callout body that confines any
  `:extend t` background face — whether carried by a text property or
  by another mode's overlay (such as `hl-line` or `region`) — to the
  box interior, so the background never extends past the right border

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

### Requirement: Code-fence bordered-block rendering

The system SHALL adorn every discovered block (fenced, YAML helmet,
or indented) with a curved-border box composed of:

- a top border (`┌─…─┐`) on or above the opening line
- a left edge `│ ` prefix on every body line, applied as a
  `before-string` for fenced and YAML blocks and as a covering
  `display` for indent blocks (which have no marker line)
- a right edge sized to the block's box width via an `after-string`
  with `display: space :align-to`
- a bottom border (`└─…─┘`) on or below the closing line
- a `wrap-prefix` of `⋱ ` on continuation visual lines so wrapped
  content stays visually inside the box
- an extend-clip anchor over the block body that confines any
  `:extend t` background face — whether carried by a text property
  (such as the `diff-added` / `diff-removed` faces applied by native
  fontification of a ` ```diff ` block) or by another mode's overlay
  (such as `hl-line` or `region`) — to the box interior, so the
  background never extends past the right border

The border face is `+markdown-overlay-border-face` for fenced and
indent blocks, and `font-lock-constant-face` for YAML helmets.

#### Scenario: Fenced block renders with curved box and icon

- **WHEN** a fenced block with a recognised language tag is decorated
- **THEN** the rendered output has a top border whose right-aligned
  position carries the nerd-icons icon for that language's major
  mode, body lines bracketed by `│ ` and right-aligned `│`, and a
  matching bottom border

#### Scenario: YAML helmet renders with `meta` label

- **WHEN** a YAML helmet is decorated
- **THEN** the rendered output has a top border whose right-aligned
  position carries the bold label `meta`, body lines bracketed by `│
  ` and a right-aligned `│`, and a matching bottom border

#### Scenario: Indent block renders with full-width borders

- **WHEN** an indent block is decorated
- **THEN** the rendered output has a top border whose entire width
  precedes the first body line (no marker line to share), and a
  matching bottom border after the last body line

#### Scenario: Diff body line background does not leak past the border

- **WHEN** a ` ```diff ` fenced block is decorated and native
  fontification has applied the `:extend t` `diff-added` face to a `+`
  line, including its trailing newline
- **THEN** the `diff-added` background is confined to the box interior
  and does not paint past the right-edge `│` to the window edge
