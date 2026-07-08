# contributor-internals Delta

## ADDED Requirements

### Requirement: Module tests exercise post-load interfaces

Module test suites (`modules/*/tests.el`) SHALL assert against
post-load observable state — enabled modes, variable values, hook
membership, key bindings, command behaviour — or SHALL invoke the
module's public commands. A test SHALL NOT read a module's own source
files (`init.el`, `lib.el`, `lib/*.el`) as text in order to assert on
their contents. Reading test fixtures or scratch buffers as text
remains permitted.

#### Scenario: configuration is asserted as state

- **GIVEN** a module init that sets a variable (e.g.
  `evil-undo-system` to `undo-redo`)
- **WHEN** the test suite verifies the setting
- **THEN** the test loads the module and asserts the variable's value
- **AND** it does not `insert-file-contents` the module's init.el

#### Scenario: refactoring init files does not break tests

- **GIVEN** a semantically neutral refactor of a module's init.el
  (reordering forms, changing `:custom` to `setopt`, reformatting)
- **WHEN** the module's test suite runs after the refactor
- **THEN** every test that passed before still passes

#### Scenario: no source-text assertions remain

- **WHEN** `modules/*/tests.el` files are searched for
  `insert-file-contents` targeting the module's own `init.el`,
  `lib.el`, or `lib/*.el`
- **THEN** no match remains
