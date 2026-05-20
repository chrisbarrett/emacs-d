## ADDED Requirements

### Requirement: bats-mode activates on `.bats` files and on the `bats` interpreter

The package SHALL register `bats-mode` in `auto-mode-alist` for files
matching `\\.bats\\'` and in `interpreter-mode-alist` for the interpreter
basename `bats`. `bats-mode` SHALL derive from `bash-ts-mode` so that
inherited bash editing (TS indent, syntax table, structural navigation,
bash font-lock) remains available.

#### Scenario: Opening a .bats file selects bats-mode

- **GIVEN** a file `/tmp/foo.bats`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `bats-mode`
- **AND** `derived-mode-p` on `bash-ts-mode` SHALL return non-nil

#### Scenario: Bats shebang without .bats extension selects bats-mode

- **GIVEN** a file whose first line is `#!/usr/bin/env bats`
  and whose filename has no `.bats` extension
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `bats-mode`

#### Scenario: bash-ts-mode features remain available

- **GIVEN** a buffer in `bats-mode` containing a plain bash function
  `foo() { echo bar; }`
- **WHEN** the buffer is fontified
- **THEN** the bash function keyword and identifier SHALL carry the
  faces `bash-ts-mode` would have applied in a non-bats buffer

### Requirement: Bats directives fontify with `bats-directive-face`

A face `bats-directive-face` SHALL be defined. Font-lock SHALL apply
`bats-directive-face` to the following bats-specific surface tokens
when they appear at the head of a statement in a `bats-mode` buffer:
`@test`, `setup`, `teardown`, `setup_file`, `teardown_file`,
`setup_suite`, `teardown_suite`, `load`, `bats_load_library`,
`bats_require_minimum_version`, `run`, `bats_pipe`, `skip`, and
`bats::on_failure`. Font-lock SHALL layer on top of `bash-ts-mode`'s
tree-sitter font-lock so inherited bash faces continue to apply
elsewhere in the buffer.

#### Scenario: @test gets the directive face

- **GIVEN** a buffer in `bats-mode` containing `@test "adds" { run true; }`
- **WHEN** `font-lock-ensure` runs
- **THEN** the `@test` token SHALL carry `bats-directive-face`

#### Scenario: Fixture functions get the directive face

- **GIVEN** a buffer in `bats-mode` containing
  `setup() { :; }` and `teardown_file() { :; }`
- **WHEN** `font-lock-ensure` runs
- **THEN** `setup` and `teardown_file` SHALL carry `bats-directive-face`

### Requirement: Assertion fontification is gated by detected helper profiles

A face `bats-assertion-face` SHALL be defined. The package SHALL ship a
profile alist mapping helper-library identifiers
(`:bats-assert`, `:bats-support`, `:bats-file`) to the assertion
function names they export. Font-lock SHALL apply
`bats-assertion-face` to a given assertion name only when the
corresponding profile is detected as active for the current buffer.

Profile detection SHALL:

1. Scan the buffer for lines of the form
   `(load | bats_load_library) <argument>`.
2. Activate `:bats-assert`, `:bats-support`, or `:bats-file` when the
   matched argument names that helper library.
3. When the argument resolves to a local bash file relative to the
   buffer (e.g. `test_helper.bash` for `load 'test_helper'`), read that
   file once and repeat step 1 against its contents — at most one
   level of recursion.
4. Cache the resulting profile set per buffer and invalidate it on
   `after-save-hook`.

#### Scenario: Buffer loads bats-assert directly

- **GIVEN** a buffer in `bats-mode` whose first lines include
  `bats_load_library 'bats-assert'` followed by
  `@test "x" { assert_equal 1 1; }`
- **WHEN** `font-lock-ensure` runs
- **THEN** `assert_equal` SHALL carry `bats-assertion-face`

#### Scenario: Buffer loads helper that loads bats-assert

- **GIVEN** a buffer in `bats-mode` whose first lines include
  `load 'test_helper'`, alongside a sibling `test_helper.bash` file
  whose contents include `bats_load_library 'bats-assert'`,
  and a test body containing `assert_equal 1 1`
- **WHEN** `font-lock-ensure` runs in the bats buffer
- **THEN** `assert_equal` SHALL carry `bats-assertion-face`

#### Scenario: Buffer that does not load bats-assert leaves assertions un-faced

- **GIVEN** a buffer in `bats-mode` with no `load` or
  `bats_load_library` line referencing `bats-assert`, and a test body
  containing the bare token `assert_equal 1 1`
- **WHEN** `font-lock-ensure` runs
- **THEN** `assert_equal` SHALL NOT carry `bats-assertion-face`

#### Scenario: Profile cache refreshes after save

- **GIVEN** a buffer in `bats-mode` whose initial scan detected no
  helper profiles, and whose body uses `assert_equal`
- **WHEN** the user adds `bats_load_library 'bats-assert'` to the
  buffer, saves it, and re-runs `font-lock-ensure`
- **THEN** `assert_equal` SHALL carry `bats-assertion-face`

### Requirement: `$BATS_*` special variables fontify with `bats-variable-face`

A face `bats-variable-face` SHALL be defined. Font-lock SHALL apply
`bats-variable-face` to `$BATS_*` references that exactly match the
documented set of bats special variables (e.g. `$BATS_TEST_NAME`,
`$BATS_TMPDIR`, `$BATS_RUN_COMMAND`, `$BATS_TEST_DIRNAME`,
`$BATS_FILE_TMPDIR`, `$BATS_SUITE_TMPDIR`, `$BATS_TEST_TMPDIR`,
`$BATS_RUN_TMPDIR`, `$BATS_TEST_DESCRIPTION`, `$BATS_TEST_NAMES`,
`$BATS_TEST_NUMBER`, `$BATS_SUITE_TEST_NUMBER`, `$BATS_TEST_TAGS`,
`$BATS_VERSION`, `BATS_TEST_NAME_PREFIX`, `BATS_TEST_RETRIES`,
`BATS_TEST_TIMEOUT`, `BATS_FILE_EXTENSION`). User-coined `$BATS_*`-prefixed
identifiers SHALL NOT match.

#### Scenario: Documented bats variable gets the variable face

- **GIVEN** a buffer in `bats-mode` containing `echo "$BATS_TEST_NAME"`
- **WHEN** `font-lock-ensure` runs
- **THEN** `$BATS_TEST_NAME` SHALL carry `bats-variable-face`

#### Scenario: User-coined BATS-prefixed variable does not match

- **GIVEN** a buffer in `bats-mode` containing
  `local BATS_USER_THING=1`
- **WHEN** `font-lock-ensure` runs
- **THEN** `BATS_USER_THING` SHALL NOT carry `bats-variable-face`

### Requirement: Imenu composes Tests, Fixtures, and inherited bash functions

`bats-mode` SHALL install an `imenu-create-index-function` whose result
is a nested alist with three sections in this order: `Tests`,
`Fixtures`, `Functions`. The `Tests` section SHALL contain one entry
per `@test "<description>" { … }` in the buffer, keyed by the literal
description text and pointing to the `@test` line. The `Fixtures`
section SHALL contain one entry for each of `setup`, `teardown`,
`setup_file`, `teardown_file`, `setup_suite`, `teardown_suite` that
appears as a function definition in the buffer. The `Functions`
section SHALL contain the bash function entries that
`bash-ts-mode`'s tree-sitter imenu would have produced for the same
buffer.

#### Scenario: @test entries appear under Tests

- **GIVEN** a buffer in `bats-mode` containing
  `@test "adds" { run true; }` and `@test "subtracts" { run false; }`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** the result SHALL contain a `Tests` section whose entries
  include `"adds"` and `"subtracts"`, each pointing to its `@test` line

#### Scenario: Fixture functions appear under Fixtures

- **GIVEN** a buffer in `bats-mode` containing
  `setup() { :; }` and `teardown_file() { :; }`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** the result SHALL contain a `Fixtures` section whose entries
  include `setup` and `teardown_file`

#### Scenario: Plain bash helpers appear under Functions

- **GIVEN** a buffer in `bats-mode` containing
  `helper_fn() { echo hi; }` alongside a `@test`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** the result SHALL contain a `Functions` section that
  includes `helper_fn`
