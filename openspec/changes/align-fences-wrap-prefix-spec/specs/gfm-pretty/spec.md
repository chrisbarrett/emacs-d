## MODIFIED Requirements

### Requirement: Code-fence bordered-block rendering

The fences decorator's `:apply-block-fn` SHALL render a
curved-border box with:

- A top border using the border face and (when resolvable) a
  language icon at the upper-right.
- A `│ ` body-line *before-string* substituted in for the opener's
  indent on each body line, plus a continuation glyph as the
  `wrap-prefix` so word-wrapped visual rows are inset under the
  body content rather than starting at column 0.
- A right-edge `│` so wrapped lines align to the box width.
- A bottom border.

The decorator's `:apply-block-fn` SHALL call
`gfm-pretty-borders--apply-with-anchors` so anchor overlays
(width-independent props such as the wrap-prefix and body
background fill) are laid at most once per (block, rebuild pass)
while per-window display overlays (borders and right-edge
after-strings, restricted to `WINDOW`) apply once per window.

The wrap-prefix glyph is a styling choice owned by the implementation
(`gfm-pretty--wrap-prefix` in `lisp/gfm/gfm-pretty-borders.el`); the
requirement constrains *that* a continuation glyph appears, not which
glyph.

#### Scenario: Fence with language

- **GIVEN** `\`\`\`bash\necho hi\n\`\`\``
- **THEN** the top border renders `┌──── …  ┐` with the bash icon
  right-aligned
- **AND** body line renders `│ echo hi` with a default-bg fill behind
  it
- **AND** bottom border `└──── …  ┘`

#### Scenario: Wrapped body line shows continuation glyph

- **GIVEN** a fenced block whose body line is wider than the window
- **WHEN** the line wraps across two or more visual rows
- **THEN** each continuation visual row begins with the decorator's
  continuation glyph (the `wrap-prefix`), not with `│ `
- **AND** the right-edge `│` lands at the box width on the final
  wrapped row
