## ADDED Requirements

### Requirement: Annotation kinds

Slide annotations SHALL accept an optional `:kind` field whose value
is one of `"inline"` (default), `"callout"`, or `"margin"`.  Each
kind dispatches to a distinct rendering primitive.

`"inline"` annotations SHALL render as `before-string` (when
`:position` is `"before"`) or `after-string` (when `:position` is
`"after"`, the default) on an overlay anchored at point-at-bol or
point-at-eol of the target line, respectively.

`"callout"` annotations SHALL render as a multi-line box drawn after
the target line, using border characters
`┌` `─` `┐` `│` `└` `┘`.  The box width SHALL be `max(80, content
+ 2)`.  The box SHALL carry a label header naming the severity
(`NOTE` / `TIP` / `WARNING`).  Callouts SHALL NOT accept a
`:position` field; supplying one SHALL signal a `user-error`.

`"margin"` annotations SHALL render in the buffer's left or right
margin via a `display` property of the form
`(margin <side> <text>)`.  `:position` SHALL be `"left"` or
`"right"` (default `"right"`); the values `"before"` and `"after"`
SHALL be normalised to `"right"`.  When any margin annotation is
present on a slide, the renderer SHALL ensure
`left-margin-width` (for left-side annotations) or
`right-margin-width` (for right-side) is at least 12 columns,
saving the prior value as a restorer in the slide's
`:render-state`.

#### Scenario: inline annotation with position "after" anchors at EOL

- **WHEN** an `inline` annotation with `:position` `"after"` is
  applied at line N
- **THEN** the resulting overlay's start position equals
  point-at-eol of line N
- **AND** the overlay carries an `after-string` containing the
  annotation text

#### Scenario: inline annotation with position "before" anchors at BOL

- **WHEN** an `inline` annotation with `:position` `"before"` is
  applied at line N
- **THEN** the resulting overlay's start position equals
  point-at-bol of line N
- **AND** the overlay carries a `before-string` containing the
  annotation text

#### Scenario: callout annotation rendered as a box

- **WHEN** a `callout` annotation with severity `"warning"` and
  body `"watch out"` is applied at line N
- **THEN** an overlay anchored at point-at-eol of line N carries an
  `after-string` whose first character is `┌` and whose last line
  ends with `┘`
- **AND** the box's border characters carry the warning severity face

#### Scenario: callout rejects :position

- **WHEN** a slide spec includes a `callout` annotation with any
  `:position` value
- **THEN** `+presentation--validate-slide` signals a `user-error`
  whose message names the kind and the offending field

#### Scenario: margin annotation lives in the right margin

- **WHEN** a `margin` annotation with `:position` `"right"` is
  applied at line N
- **THEN** the resulting overlay carries a `display` property whose
  value is `(margin right-margin <text>)`
- **AND** the buffer's `right-margin-width` is at least 12

#### Scenario: margin restores prior margin width on cleanup

- **WHEN** a slide with margin annotations is rendered against a
  buffer with `right-margin-width` of 0
- **AND** the slide is later left (`+presentation--cleanup-render-state`)
- **THEN** the buffer's `right-margin-width` is restored to 0

### Requirement: Annotation severity

Slide annotations SHALL accept an optional `:severity` field whose
value is one of `"note"` (default), `"tip"`, or `"warning"`.
Severity SHALL drive the face used for the annotation's primary
visual element (border for callouts, text face for inline,
margin-string face for margin).

The system SHALL define three faces:
- `+presentation-annotation-note-face`, inheriting from
  `+markdown-gfm-callout-note-face` when available.
- `+presentation-annotation-tip-face`, inheriting from
  `+markdown-gfm-callout-tip-face` when available.
- `+presentation-annotation-warning-face`, inheriting from
  `+markdown-gfm-callout-warning-face` when available.

When the inherited face is unavailable, each face SHALL fall back
to a sensible default (`shadow` for note, `success` for tip,
`warning` for warning).

#### Scenario: severity selects matching face

- **WHEN** a `callout` annotation with severity `"tip"` is rendered
- **THEN** the box border characters carry
  `+presentation-annotation-tip-face`

#### Scenario: severity defaults to note

- **WHEN** an annotation omits `:severity`
- **THEN** the rendered face is `+presentation-annotation-note-face`

#### Scenario: invalid severity rejected at validation

- **WHEN** a slide annotation carries `:severity` `"alarm"`
- **THEN** `+presentation--validate-slide` signals a `user-error`
  whose message names the offending value

### Requirement: Focus highlight bounded to text glyphs

The `file` slide focus highlight SHALL be implemented as one
overlay per line covered by the focus range, each spanning exactly
the line's `point-at-bol` to `point-at-eol`.  The overlays SHALL
carry `+presentation-focus-face`, which SHALL NOT use `:extend t`
and SHALL use a theme-aware muted background colour.

The face SHALL paint only over real text glyphs; lines shorter than
the window width SHALL NOT have their trailing whitespace painted
to the window edge.

#### Scenario: short focus line not extended to window width

- **WHEN** a `file` slide's `:focus` covers a line whose content is
  10 characters wide
- **AND** the window is 100 columns wide
- **THEN** the focus overlay on that line ends at column 10
- **AND** columns 11..100 of that line carry the default background

#### Scenario: focus face does not collide with annotation faces

- **WHEN** a `file` slide has both a `:focus` covering line N and
  an annotation on line N
- **THEN** the annotation's overlay placement is unaffected by the
  focus overlay
- **AND** the annotation text is readable against the focus
  background (severity face foreground contrasts with focus bg)

## MODIFIED Requirements

### Requirement: Annotation validation

`+presentation--validate-annotation` SHALL signal `user-error` for:

- `:line` not a positive integer.
- `:text` (when present) not a string.
- `:kind` (when present) not one of `"inline"` / `"callout"` /
  `"margin"`.
- `:severity` (when present) not one of `"note"` / `"tip"` /
  `"warning"`.
- `:position` accepted values dependent on `:kind`:
  - `inline`: must be one of `"before"` / `"after"` / absent.
  - `callout`: must be absent.
  - `margin`: must be one of `"left"` / `"right"` / `"before"` /
    `"after"` / absent.
- Any unknown field on the annotation plist (strict for
  forward-compatibility).

#### Scenario: unknown kind rejected

- **WHEN** a slide annotation carries `:kind` `"banner"`
- **THEN** validation signals a `user-error` whose message names
  `"banner"` and lists the accepted values

#### Scenario: position on callout rejected

- **WHEN** a callout annotation includes `:position` `"after"`
- **THEN** validation signals a `user-error`

#### Scenario: missing kind defaults to inline at validation

- **WHEN** a slide annotation has no `:kind` field
- **AND** has `:position` `"after"`
- **THEN** validation accepts the annotation
- **AND** at render time the annotation is treated as
  `kind: "inline"`
