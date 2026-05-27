## ADDED Requirements

### Requirement: `.jq` files open in `jq-mode`

The `lang-jq` module SHALL register `jq-mode` for files matching
`\\.jq\\'` in `auto-mode-alist`, so visiting a `.jq` file selects
`jq-mode` as the major mode.

#### Scenario: Opening a .jq file selects jq-mode

- **GIVEN** the `lang-jq` module is loaded
- **WHEN** a file named `filter.jq` is visited
- **THEN** the buffer's major mode SHALL be `jq-mode`

#### Scenario: jq-mode is callable without explicit require

- **GIVEN** the `lang-jq` module is loaded and `jq-mode` has been
  installed by elpaca
- **WHEN** `(fboundp 'jq-mode)` is evaluated
- **THEN** the result SHALL be non-nil

### Requirement: `jq-interactively` is bound in JSON buffers

The `lang-jq` module SHALL bind `jq-interactively` to `C-c C-j` in
both `json-mode-map` and `json-ts-mode-map`, so a user editing a JSON
buffer can run a jq filter live over the buffer contents.

The binding SHALL be installed after `jq-mode` is loaded so the
command symbol is bound when the keymap entry resolves.

#### Scenario: C-c C-j in a json-ts-mode buffer runs jq-interactively

- **GIVEN** the `lang-jq` module is loaded and a JSON buffer in
  `json-ts-mode`
- **WHEN** the key sequence `C-c C-j` is looked up in
  `json-ts-mode-map`
- **THEN** the resolved command SHALL be `jq-interactively`

#### Scenario: C-c C-j in a json-mode buffer runs jq-interactively

- **GIVEN** the `lang-jq` module is loaded and `json-mode` has been
  required
- **WHEN** the key sequence `C-c C-j` is looked up in `json-mode-map`
- **THEN** the resolved command SHALL be `jq-interactively`
