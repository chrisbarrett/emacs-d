## ADDED Requirements

### Requirement: `opam-mode` lives in a self-contained library

The `opam-mode` library SHALL live at `lisp/opam-mode/opam-mode.el`
and provide the symbol `opam-mode`. The library SHALL define the
major mode `opam-mode` and its `auto-mode-alist` registration. No
other module SHALL contain an `opam-mode` or `opam-config-mode`
definition. The library SHALL NOT carry policy decisions about
specific opam-file contents (e.g. the read-only-on-generated-files
hook stays in `modules/lang-ocaml/init.el`).

#### Scenario: Library file exists and provides opam-mode

- **GIVEN** the working tree at HEAD
- **WHEN** `lisp/opam-mode/opam-mode.el` is loaded
- **THEN** `(featurep 'opam-mode)` SHALL be non-nil
- **AND** `(fboundp 'opam-mode)` SHALL be non-nil

#### Scenario: Legacy `opam-config-mode` symbol is gone

- **GIVEN** the working tree at HEAD
- **WHEN** `(fboundp 'opam-config-mode)` is evaluated
- **THEN** the result SHALL be nil

#### Scenario: Mode hook variable exists for downstream composition

- **GIVEN** the library is loaded
- **WHEN** `(boundp 'opam-mode-hook)` is evaluated
- **THEN** the result SHALL be non-nil

### Requirement: `opam-mode` derives from `conf-colon-mode`

`opam-mode` SHALL be defined via `define-derived-mode` with parent
`conf-colon-mode`. The mode-line lighter SHALL be `"OPAM Config"`.

#### Scenario: Mode parent is `conf-colon-mode`

- **GIVEN** the library is loaded
- **WHEN** `(get 'opam-mode 'derived-mode-parent)` is evaluated
- **THEN** the result SHALL be `conf-colon-mode`

### Requirement: `*.opam` files activate `opam-mode`

The library SHALL register `opam-mode` in `auto-mode-alist` for
files whose path ends in `.opam`. The registration SHALL be
`;;;###autoload`-cookied so it applies at startup without an
explicit `require`.

#### Scenario: `.opam` file activates the mode

- **GIVEN** a file path `/tmp/pkgs/foo.opam`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `opam-mode`

#### Scenario: A `foo.opamignore` file does NOT activate the mode

- **GIVEN** a file path `/tmp/pkgs/foo.opamignore`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL NOT be `opam-mode`
