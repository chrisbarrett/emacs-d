## MODIFIED Requirements

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
