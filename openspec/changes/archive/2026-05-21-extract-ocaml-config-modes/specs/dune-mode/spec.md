## ADDED Requirements

### Requirement: `dune-mode` lives in a self-contained library

The `dune-mode` library SHALL live at `lisp/dune-mode/dune-mode.el`
and provide the symbol `dune-mode`. The library SHALL define the
major mode `dune-mode` and its `auto-mode-alist` registration. No
other module SHALL contain a `dune-mode` or `dune-config-mode`
definition.

#### Scenario: Library file exists and provides dune-mode

- **GIVEN** the working tree at HEAD
- **WHEN** `lisp/dune-mode/dune-mode.el` is loaded
- **THEN** `(featurep 'dune-mode)` SHALL be non-nil
- **AND** `(fboundp 'dune-mode)` SHALL be non-nil

#### Scenario: Legacy `dune-config-mode` symbol is gone

- **GIVEN** the working tree at HEAD
- **WHEN** `(fboundp 'dune-config-mode)` is evaluated
- **THEN** the result SHALL be nil

### Requirement: `dune-mode` derives from `lisp-data-mode`

`dune-mode` SHALL be defined via `define-derived-mode` with parent
`lisp-data-mode`. The mode-line lighter SHALL be `"Dune Config"`.
The mode SHALL set `comment-add` to 0 (so `M-;` inserts a single
`;` instead of `;;`).

#### Scenario: Mode parent is `lisp-data-mode`

- **GIVEN** the library is loaded
- **WHEN** `(get 'dune-mode 'derived-mode-parent)` is evaluated
- **THEN** the result SHALL be `lisp-data-mode`

#### Scenario: `comment-add` is set to 0

- **GIVEN** a buffer that has just had `(dune-mode)` called on it
- **WHEN** the value of `comment-add` is read
- **THEN** the value SHALL be 0

### Requirement: Dune build files activate `dune-mode`

The library SHALL register `dune-mode` in `auto-mode-alist` for
files whose path ends in `dune`, `dune-workspace`, or
`dune-project` with no extension. The registration SHALL be
`;;;###autoload`-cookied so it applies at startup without an
explicit `require`.

#### Scenario: Plain `dune` file activates the mode

- **GIVEN** a file path `/tmp/proj/dune`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `dune-mode`

#### Scenario: `dune-workspace` file activates the mode

- **GIVEN** a file path `/tmp/proj/dune-workspace`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `dune-mode`

#### Scenario: `dune-project` file activates the mode

- **GIVEN** a file path `/tmp/proj/dune-project`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `dune-mode`

#### Scenario: A `dune.txt` file does NOT activate the mode

- **GIVEN** a file path `/tmp/proj/dune.txt`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL NOT be `dune-mode`
