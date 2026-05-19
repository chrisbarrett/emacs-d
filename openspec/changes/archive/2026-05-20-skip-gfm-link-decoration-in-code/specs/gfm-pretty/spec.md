## ADDED Requirements

### Requirement: Link decoration skips code regions

The links decorator SHALL NOT collect or render a link whose source
starts inside a markdown code region. Code regions include:

- fenced code blocks (``` and `~~~`)
- indented (pre) code blocks
- inline code spans (`` `code` ``)

Detection SHALL delegate to markdown-mode's own helpers:
`markdown-code-block-at-pos` for fenced and indented blocks,
`markdown-inline-code-at-pos-p` for inline code. The check uses the
link record's start position (the title-side opening bracket, or
for autolinks / bare URLs the first character of the matched span).

The exclusion mirrors the existing table-cell and
reference-definition-line exclusions: code-region text is governed
by its own rendering layer, and overlaying link decoration on top
would misrepresent the source.

#### Scenario: Link inside fenced code block

- **GIVEN** a fenced block containing the literal `[foo](bar)`
- **THEN** the links decorator does NOT decorate that span
- **AND** the code block renders its content unchanged

#### Scenario: Link inside indented code block

- **GIVEN** a four-space-indented line containing `[foo](bar)`
- **THEN** the links decorator does NOT decorate that span

#### Scenario: Link inside inline code

- **GIVEN** an inline code span `` `[foo](bar)` ``
- **THEN** the links decorator does NOT decorate the bracketed
  text
- **AND** the inline code renders its content unchanged

#### Scenario: Link adjacent to code does decorate

- **GIVEN** the line `See [foo](bar) and \`some-code\``
- **THEN** the `[foo](bar)` link decorates normally
- **AND** the inline code renders its content unchanged
