# argc-mode Specification

## Purpose

The `argc-mode` library at `lisp/argc-mode/argc-mode.el` provides a
buffer-local minor mode that fontifies [argc](https://github.com/sigoden/argc)
CLI directive comments in shell-script buffers. It owns the faces, the
directive-cascade overlay rules, the Unicode box rendering around directive
blocks, a debounced rebuild scheduler that skips indirect buffers, and a
`spell-fu` advice that suppresses marking on directive lines. The activation
gate `+argc-maybe-enable` that wires the mode into `sh-mode-hook` /
`bash-ts-mode-hook` lives in `modules/lang-shscript` as composition glue and
is out of scope here.

## Requirements

### Requirement: `argc-mode` lives in a self-contained library

The `argc-mode` library SHALL live at `lisp/argc-mode/argc-mode.el`
and provide the symbol `argc-mode`. The library SHALL define
`argc-mode` as a minor mode, plus all faces, regexes, overlay
helpers, the rebuild scheduler, and the spell-fu advice the mode
needs. No other module SHALL contain `argc-` symbol definitions.

The activation gate `+argc-maybe-enable` and its `sh-mode-hook` /
`bash-ts-mode-hook` registrations SHALL remain in
`modules/lang-shscript/init.el` because they are composition glue,
not library behaviour.

#### Scenario: Library file exists and provides argc-mode

- **GIVEN** the working tree at HEAD
- **WHEN** `lisp/argc-mode/argc-mode.el` is loaded
- **THEN** `(featurep 'argc-mode)` SHALL be non-nil
- **AND** `(fboundp 'argc-mode)` SHALL be non-nil
- **AND** no source file other than `lisp/argc-mode/argc-mode.el`
  and `lisp/argc-mode/argc-mode-tests.el` SHALL contain `argc-`
  symbol definitions

#### Scenario: `+argc-maybe-enable` stays in `lang-shscript`

- **GIVEN** the working tree at HEAD
- **WHEN** `modules/lang-shscript/init.el` is searched for
  `+argc-maybe-enable`
- **THEN** the function definition SHALL appear there
- **AND** the function SHALL NOT appear under `lisp/argc-mode/`

### Requirement: `argc-mode` is autoloaded

The minor mode `argc-mode` SHALL be marked `;;;###autoload` so that
`lisp/+autoloads.el` exposes it without an explicit `require`. The
harvested autoload entry SHALL point at the new library path.

#### Scenario: `argc-mode` is callable without `require`

- **GIVEN** a fresh Emacs session that has loaded
  `lisp/+autoloads.el` but has not `require`'d `argc-mode`
- **WHEN** `(fboundp 'argc-mode)` is evaluated
- **THEN** the result SHALL be non-nil

### Requirement: `argc-mode` defines a fixed set of faces

The library SHALL define the following faces in the `argc` group:
`argc-directive-face`, `argc-param-name-face`, `argc-flag-face`,
`argc-modifier-face`, `argc-notation-face`, `argc-choice-face`,
`argc-default-value-face`, `argc-bind-env-face`, `argc-fn-face`,
`argc-box-face`. Each face SHALL inherit from a sensible built-in face
by default
(`argc-directive-face` from `font-lock-keyword-face`,
`argc-param-name-face` from `font-lock-variable-name-face`,
`argc-flag-face` from `font-lock-constant-face`,
`argc-modifier-face` / `argc-notation-face` / `argc-choice-face`
from `font-lock-type-face`,
`argc-default-value-face` from `font-lock-string-face`,
`argc-bind-env-face` from `font-lock-variable-name-face`,
`argc-fn-face` from `font-lock-function-name-face`,
`argc-box-face` from `shadow`).

#### Scenario: All faces are defined in the `argc` group

- **GIVEN** the library is loaded
- **WHEN** `face-list` is queried for symbols beginning with `argc-`
- **THEN** the list SHALL include each face named above

### Requirement: Argc directive comments fontify per directive type

When `argc-mode` is enabled, the library SHALL apply face overlays
to argc directive comment lines based on the directive type. The
rules SHALL run in cascade order, with later rules creating
higher-priority overlays that visually override earlier ones on
overlapping ranges:

1. Description text following any directive carries
   `font-lock-doc-face`.
2. The `@<directive>` tag (`@describe`, `@version`, `@cmd`,
   `@alias`, `@arg`, `@option`, `@flag`, `@env`, `@meta`) carries
   `argc-directive-face`.
3. `@arg <name>` and `@env <NAME>` argument identifiers carry
   `argc-param-name-face`; their trailing modifier carries
   `argc-modifier-face`. A modifier is `!` or `~`, or a `*` / `+`
   optionally followed by one multi-value delimiter character
   (`,`, `:`, `;`, `@`, `|`, `/`); the delimiter is part of the
   `argc-modifier-face` span.
4. `@option` / `@flag` short and long flags carry `argc-flag-face`;
   their trailing modifier carries `argc-modifier-face` under the
   same modifier-plus-delimiter rule as item 3.
5. `@meta` keys and `@alias` name lists carry
   `argc-param-name-face`.
6. Angle-bracket notations (`<FILE>`, `<NUM>`) carry
   `argc-notation-face`.
7. Choice lists (`[a|b|c]`) on `@option`, `@arg`, and `@env`
   carry `argc-choice-face`.
8. Default values (`=value`, not inside brackets) on `@option`,
   `@arg`, and `@env` carry `argc-default-value-face`. argc grants
   `@env` the same `param-value` slot as the others, so these
   forms SHALL be faced on `@env` identically.
9. An environment-binding suffix — ` $$` (anonymous) or ` $NAME`
   where `NAME` is uppercase ASCII and underscore — on `@arg`,
   `@option`, `@flag`, or `@env` carries `argc-bind-env-face`.
10. A backtick-delimited function reference appearing in a
    function default (`` =`fn` ``) or a function choice list
    (`` [`fn`] ``, `` [?`fn`] ``) carries `argc-fn-face`,
    overriding the enclosing default / choice face on the
    `` `fn` `` span. The optional `?` in `` [?`fn`] `` keeps the
    choice face.

The `@env` name charset is uppercase ASCII and underscore, matching
argc's `is_env_name_char`; the existing `@env <NAME>` rule already
faces exactly this charset, so no relaxation is made (recorded so it
is not revisited).

Regular shell comments that do NOT start with an argc directive
SHALL NOT receive any `argc-*` face.

#### Scenario: `@cmd` tag and description fontify

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# @cmd Upload a file`
- **WHEN** the buffer is fontified
- **THEN** `@cmd` SHALL carry `argc-directive-face`
- **AND** `Upload a file` SHALL carry `font-lock-doc-face`

#### Scenario: `@arg` parameter name and modifier fontify

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# @arg target! File to upload`
- **WHEN** the buffer is fontified
- **THEN** `target` SHALL carry `argc-param-name-face`
- **AND** `!` SHALL carry `argc-modifier-face`

#### Scenario: `@option` flags and notation fontify

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# @option -t --tries <NUM> Set number`
- **WHEN** the buffer is fontified
- **THEN** the flag run `-t --tries` SHALL carry `argc-flag-face`
- **AND** `<NUM>` SHALL carry `argc-notation-face`

#### Scenario: `@version` tag and value fontify

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# @version 1.0.0`
- **WHEN** the buffer is fontified
- **THEN** `@version` SHALL carry `argc-directive-face`
- **AND** `1.0.0` SHALL carry `font-lock-doc-face`
- **AND** the line SHALL open a box block

#### Scenario: `@env` default value and choices fontify

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# @env AUTOSEND=0` and `# @env LEVEL[debug|info|warn]`
- **WHEN** the buffer is fontified
- **THEN** `=0` SHALL carry `argc-default-value-face`
- **AND** `[debug|info|warn]` SHALL carry `argc-choice-face`

#### Scenario: Multi-value modifier with delimiter fontifies

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# @option --tags*, Comma-separated tags`
- **WHEN** the buffer is fontified
- **THEN** the `*,` run SHALL carry `argc-modifier-face`

#### Scenario: Environment-binding suffix fontifies

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# @flag -v --verbose $VERBOSE Verbose mode` and
  `# @option --port $$ Port`
- **WHEN** the buffer is fontified
- **THEN** `$VERBOSE` SHALL carry `argc-bind-env-face`
- **AND** `$$` SHALL carry `argc-bind-env-face`

#### Scenario: Backtick function reference fontifies

- **GIVEN** a buffer with `argc-mode` enabled containing
  ``# @option --file[`_choice_fn`] File`` and
  ``# @arg name=`_default_fn` Name``
- **WHEN** the buffer is fontified
- **THEN** the `` `_choice_fn` `` span SHALL carry `argc-fn-face`
- **AND** the `` `_default_fn` `` span SHALL carry `argc-fn-face`

#### Scenario: Non-argc comments are untouched

- **GIVEN** a buffer with `argc-mode` enabled containing
  `# This is a regular comment`
- **WHEN** the buffer is fontified
- **THEN** no character SHALL carry `argc-directive-face`

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

The box SHALL be sized to the displaying window. The top and
bottom borders SHALL draw their horizontal rule with literal `─`
glyphs spanning the box width (`window-body-width` less the two
columns the right rail is inset), so the rule is a continuous
visible line — a blank `(space :align-to …)` stretch SHALL NOT
stand in for the rule, since it renders no glyphs. Because the
dash count is fixed at build time, the overlays SHALL be rebuilt
on `window-configuration-change-hook` so the borders re-fit after
a resize.

The per-line right `│` SHALL be positioned using a
`(space :align-to right)` (or `(space :align-to (- right N))`)
display spec so the rail lands flush with each window's right
edge at redisplay time, rather than at a fixed buffer-wide column.
The top / bottom border corners land on the same column as that
rail.

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

#### Scenario: Top and bottom borders draw a continuous dash rule

- **GIVEN** a top or bottom border built for a known box width
- **WHEN** its string is inspected
- **THEN** the horizontal rule SHALL be filled with literal `─`
  glyphs spanning the width, AND SHALL NOT contain a
  `(space :align-to right)` / `(space :align-to (- right N))`
  stretch in place of the rule

#### Scenario: Borders re-fit on window configuration change

- **GIVEN** a buffer with `argc-mode` enabled
- **WHEN** the mode's hook registration is inspected
- **THEN** `argc--schedule-rebuild` SHALL be on the buffer-local
  `window-configuration-change-hook`

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

### Requirement: Mode toggling is idempotent and reversible

Calling `(argc-mode 1)` when the mode is already on SHALL NOT
duplicate overlays. Calling `(argc-mode -1)` SHALL remove every
overlay carrying the `argc` property, remove the
`after-change-functions` hook, remove the `spell-fu-mark-incorrect`
advice, and cancel any pending rebuild timer.

#### Scenario: Re-enabling does not double overlays

- **GIVEN** a buffer with `argc-mode` already enabled and
  N argc-tagged overlays present
- **WHEN** `(argc-mode 1)` is called a second time
- **THEN** the count of argc-tagged overlays SHALL remain N

#### Scenario: Disabling removes all overlays

- **GIVEN** a buffer with `argc-mode` enabled and overlays present
- **WHEN** `(argc-mode -1)` is called
- **THEN** the buffer SHALL contain no overlay carrying the
  `argc` property

### Requirement: Rebuild scheduler is debounced and skips indirect buffers

The library SHALL install a buffer-local `after-change-functions`
hook that debounces overlay rebuilds via an idle timer (~0.2 s).
The scheduler SHALL be a no-op in indirect buffers (where
`buffer-base-buffer` is non-nil) because base-buffer overlays are
already visible through the indirect view.

#### Scenario: Indirect buffer does not schedule a rebuild

- **GIVEN** a base buffer with `argc-mode` enabled and an indirect
  buffer over it
- **WHEN** `argc--schedule-rebuild` is called in the indirect
  buffer
- **THEN** `argc--rebuild-timer` SHALL remain nil in the indirect
  buffer

### Requirement: `spell-fu` advice skips directive lines

When `argc-mode` is enabled, the library SHALL install an
`:around` advice on `spell-fu-mark-incorrect` that suppresses
marking when the call's begin position lies on an argc directive
line. The advice SHALL pass through unchanged when the position is
not on a directive line. Disabling `argc-mode` SHALL remove the
advice.

#### Scenario: Advice suppresses spell-fu on a directive line

- **GIVEN** a buffer with `argc-mode` enabled and an argc
  directive line at point
- **WHEN** the advised `spell-fu-mark-incorrect` is invoked with
  begin position on the directive line
- **THEN** the original function SHALL NOT be called

#### Scenario: Advice passes through on a non-directive line

- **GIVEN** a buffer with `argc-mode` enabled and a non-comment
  line at point
- **WHEN** the advised `spell-fu-mark-incorrect` is invoked with
  begin position on the non-directive line
- **THEN** the original function SHALL be called with the
  unchanged begin/end positions
