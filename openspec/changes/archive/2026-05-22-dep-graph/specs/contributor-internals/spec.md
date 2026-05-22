## MODIFIED Requirements

### Requirement: Local hooks invoke `--affected`

The three local hooks (`ert-tests`, `byte-compile`, `checkdoc`) SHALL
declare `pass_filenames = false` and `always_run = true`, and their
`entry` SHALL invoke the underlying script with the `--affected`
argument. The script-side transitive-dependent expansion SHALL go
through `scripts/dep-graph` (backed by the `dep-graph` library at
`lisp/dep-graph/`). The legacy bash parsers `scripts/affected.sh` and
`scripts/affected-tests.sh` SHALL NOT exist. (A follow-up change can
flip the hooks to `pass_filenames = true` now that scope expansion
lives in Emacs.)

#### Scenario: byte-compile hook runs the affected target

- **GIVEN** any commit
- **WHEN** prek runs the `byte-compile` hook
- **THEN** the spawned process SHALL be
  `nix develop --command ./scripts/byte-compile.sh --affected`
- **AND** `pass_filenames` for the hook SHALL be `false`

#### Scenario: Tests hook runs the affected target

- **GIVEN** any commit
- **WHEN** prek runs the `ert-tests` hook
- **THEN** the spawned process SHALL be
  `nix develop --command ./scripts/run-tests.sh --affected`

#### Scenario: Bash affected parsers are gone

- **GIVEN** the repository root
- **WHEN** `scripts/` is listed
- **THEN** `affected.sh` SHALL NOT exist
- **AND** `affected-tests.sh` SHALL NOT exist
- **AND** `scripts/dep-graph` SHALL exist
