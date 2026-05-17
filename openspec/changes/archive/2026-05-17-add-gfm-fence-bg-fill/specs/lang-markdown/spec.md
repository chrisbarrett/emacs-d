## ADDED Requirements

### Requirement: Code-fence body background fill

The system SHALL paint a fenced or indent code-block body line's
right-edge after-string padding with that line's background whenever the
line carries a face with both `:extend t` and a `:background` — for
example the `diff-added` / `diff-removed` faces that native fontification
applies inside a fenced `diff` block — so the highlighted band fills the
box interior up to the right border `│`. When a body line carries no
such face, the right-edge padding SHALL retain the border face's default
appearance.

This applies to the wrapped (overflow) right-edge after-string as well
as the non-wrapped one. It does not apply to callouts, whose right-edge
padding is already filled with the callout tint, nor to YAML helmets,
whose body the decorator fontifies itself.

#### Scenario: Diff added line fills to the border

- **WHEN** a fenced `diff` block is decorated and native
  fontification has applied the `:extend t` `diff-added` face to a `+`
  body line
- **THEN** that line's right-edge after-string padding is painted with
  `diff-added`'s background, so the highlight band reaches the
  right-edge `│`

#### Scenario: Plain code line keeps the default gap

- **WHEN** a fenced block body line carries no face with `:extend t` and
  a `:background`
- **THEN** that line's right-edge after-string padding renders with the
  border face, unchanged from prior behaviour

#### Scenario: Background absent until fontification runs

- **WHEN** a fenced block is decorated before native fontification has
  applied any `:extend t` background to its body
- **THEN** the right-edge padding renders with the default border
  appearance, and a later rebuild repaints it once the `:extend t`
  background is present
