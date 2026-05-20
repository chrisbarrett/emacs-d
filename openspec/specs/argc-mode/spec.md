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
`argc-default-value-face`, `argc-box-face`. Each face SHALL inherit
from a sensible built-in face by default
(`argc-directive-face` from `font-lock-keyword-face`,
`argc-param-name-face` from `font-lock-variable-name-face`,
`argc-flag-face` from `font-lock-constant-face`,
`argc-modifier-face` / `argc-notation-face` / `argc-choice-face`
from `font-lock-type-face`,
`argc-default-value-face` from `font-lock-string-face`,
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
2. The `@<directive>` tag (`@describe`, `@cmd`, `@alias`, `@arg`,
   `@option`, `@flag`, `@env`, `@meta`) carries
   `argc-directive-face`.
3. `@arg <name>` and `@env <NAME>` argument identifiers carry
   `argc-param-name-face`; their trailing modifier character
   (`!`, `*`, `+`, `,`, `~`) carries `argc-modifier-face`.
4. `@option` / `@flag` short and long flags carry `argc-flag-face`;
   their trailing modifier carries `argc-modifier-face`.
5. `@meta` keys and `@alias` name lists carry
   `argc-param-name-face`.
6. Angle-bracket notations (`<FILE>`, `<NUM>`) carry
   `argc-notation-face`.
7. Choice lists (`[a|b|c]`) carry `argc-choice-face`.
8. Default values (`=value`, not inside brackets) carry
   `argc-default-value-face`.

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
content line gets a `│ ` prefix and a right-aligned ` │` suffix.

Box width SHALL be `max(80, max-line-length + 4)` columns.

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

#### Scenario: Box width respects the 80-column floor

- **GIVEN** a buffer with `argc-mode` enabled whose directive lines
  are all shorter than 76 characters
- **WHEN** the box overlays are built
- **THEN** the rendered box width SHALL be 80 columns

#### Scenario: Box width expands for long directive lines

- **GIVEN** a buffer with `argc-mode` enabled containing a
  directive line of 100 characters
- **WHEN** the box overlays are built
- **THEN** the rendered box width SHALL be at least 104 columns
  (line length + 4)

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
