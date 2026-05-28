## MODIFIED Requirements

### Requirement: Argc directive blocks render as Unicode boxes

When `argc-mode` is enabled, the library SHALL group contiguous
argc directive lines (including comment continuations between
directives) into a "block" and draw a Unicode box around each
block using overlay `before-string` / `after-string` properties.
A block is bounded above by `┌─…─┐` and below by `└─…─┘`. Each
content line gets a left rail at its leading `#` (see substitution
requirement below) and a right-aligned `│` suffix.

The leading `#` of each body line SHALL be replaced by a single-
char overlay carrying `display "│"` painted in the normalised
border face and `evaporate t` (so the overlay self-destroys if
the underlying `#` is deleted).  Mirrors the `> ` → `│ `
substitution idiom in `gfm-pretty-callouts--apply-block-display`.
The space following `#` is left in the buffer and serves as the
visual separator between the rail and directive content.

When point sits on a substitution overlay (i.e. on the source
`#`), the overlay's `display` SHALL be transiently suppressed so
the source `#` is revealed, and SHALL be restored when point
moves off.  Implemented via `post-command-hook`; mirrors
`prettify-symbols-unprettify-at-point` at the overlay layer.

The box SHALL be sized per displaying window. The top border,
bottom border, and per-line right `│` SHALL be positioned using a
`(space :align-to right)` (or `(space :align-to (- right N))`)
display spec so the border lands flush with each window's right
edge at redisplay time, rather than at a fixed buffer-wide column.

Each per-line overlay SHALL set a `wrap-prefix` property carrying
the left rail (`│ `) so that under `visual-line-mode` or
`word-wrap` the rail continues on soft-wrapped visual continuation
rows. Under `truncate-lines` no continuation rows are rendered and
the `wrap-prefix` has no visible effect.

The right-edge after-string SHALL end with a
`(space :align-to right)` segment painted in the `default` face,
so any sibling face with `:extend t` (`region`, `hl-line`,
`diff-added`, `diff-removed`) is masked past the right `│` and
does not bleed to the window edge.

Border glyphs SHALL be painted through a normalised face spec
that inherits `argc-box-face` and explicitly clears
`:slant`, `:weight`, `:underline`, `:overline`, `:strike-through`,
and `:box`, and pins `:background "unspecified-bg"`. This prevents
prose font-lock attributes from leaking into border chars via face
composition, and prevents text-property backgrounds (e.g.
`diff-added` on the buffer line) from bleeding into border /
before-string / after-string chars.

When a function definition appears as the next non-blank form
after the block, the top border SHALL include the function name as
a right-aligned bold label inside the border. Functions are
matched as either `function <name> { … }` or `<name>() { … }`,
with `<name>` accepting alphanumerics, `_`, and `-`.

Blocks SHALL NOT render around plain shell comments that contain
no argc directive.

#### Scenario: Single block produces one set of box overlays

- **GIVEN** a buffer with `argc-mode` enabled containing one
  contiguous block of two directive lines
- **WHEN** the box overlays are built
- **THEN** the buffer SHALL contain exactly one top-border
  before-string and one bottom-border after-string

#### Scenario: Right border aligns to the window edge

- **GIVEN** a buffer with `argc-mode` enabled containing a
  directive block, displayed in a window
- **WHEN** the per-line right-edge after-string is inspected
- **THEN** its `display` spec SHALL include a
  `(space :align-to right)` (or `(space :align-to (- right N))`)
  segment positioning the `│` at the window's right edge

#### Scenario: Continuation rows continue the left rail

- **GIVEN** a buffer with `argc-mode` enabled and
  `visual-line-mode` or `word-wrap` active
- **WHEN** a per-line overlay's properties are inspected
- **THEN** the overlay SHALL carry a `wrap-prefix` property whose
  value contains the rail string `│ `

#### Scenario: After-string masks past-EOL `:extend t` leaks

- **GIVEN** a per-line overlay's `after-string`
- **WHEN** the trailing segment is inspected
- **THEN** it SHALL end with a `(space :align-to right)` display
  segment painted in the `default` face

#### Scenario: Border face does not inherit prose styling

- **GIVEN** the face spec used for border glyphs
  (`┌`, `─`, `┐`, `│`, `└`, `┘`)
- **WHEN** the spec is inspected
- **THEN** it SHALL explicitly set `:slant normal`,
  `:underline nil`, `:overline nil`, `:strike-through nil`,
  `:box nil`, and `:background "unspecified-bg"`

#### Scenario: Leading `#` is substituted with `│`

- **GIVEN** a buffer with `argc-mode` enabled containing a
  directive block
- **WHEN** the box overlays are built
- **THEN** for each body line the buffer SHALL contain a single-
  char overlay covering the leading `#` with `evaporate t` and
  `display "│"`

#### Scenario: Point on substitution reveals source `#`

- **GIVEN** a buffer with `argc-mode` enabled and a substitution
  overlay over a body line's leading `#`
- **WHEN** point moves onto the overlay's start position
- **THEN** the overlay's `display` SHALL be transiently nil so
  the source `#` shows; once point moves off the overlay's range,
  `display` SHALL be restored

#### Scenario: Top border labels the next function

- **GIVEN** a buffer with `argc-mode` enabled containing a
  directive block immediately followed by a shell function
  definition `my_func() { … }`
- **WHEN** the box overlays are built
- **THEN** the top border before-string SHALL contain the literal
  `my_func`

#### Scenario: Regular comments do not get boxed

- **GIVEN** a buffer with `argc-mode` enabled containing only
  plain `#` comments (no argc directive)
- **WHEN** the box overlays are built
- **THEN** the buffer SHALL contain no overlay carrying the
  `argc-box` property
