# contributor-internals Specification

## Purpose

Internals-facing. Covers the contributor-facing tooling axis: the prek
pre-commit pipeline, its baseline checks, the contract between prek and
the local gate scripts, and the relationship between the flake devShell,
the Makefile, and `scripts/`. The pipeline gates every commit and is the
source of truth for "what passes/fails before merge".

## Requirements

### Requirement: Pre-commit config is native prek TOML

The repository SHALL configure prek hooks via a `prek.toml` file at the
repo root, NOT a `.pre-commit-config.yaml`. The TOML file SHALL begin
with the editor schema directive
`#:schema https://json.schemastore.org/prek.json` on its first line so
editors and validators surface completion and structural errors.

#### Scenario: TOML config exists and yaml does not

- **GIVEN** the repository root
- **WHEN** the files are inspected
- **THEN** `prek.toml` SHALL exist
- **AND** `.pre-commit-config.yaml` SHALL NOT exist
- **AND** the first line of `prek.toml` SHALL be the prek schema directive

#### Scenario: prek validates the config

- **GIVEN** the devShell is active
- **WHEN** `prek validate-config` is run
- **THEN** it SHALL exit zero with no errors

### Requirement: Baseline hygiene checks are enabled

The pipeline SHALL enable the following hooks from
`pre-commit/pre-commit-hooks` v6.0.0: `trailing-whitespace` with
`--chars= \t` to preserve form-feed (`^L`) page-break markers,
`end-of-file-fixer`, `mixed-line-ending` with `--fix=lf`, `check-yaml`,
`check-toml`, `check-json`, `check-merge-conflict`,
`check-added-large-files`, `check-executables-have-shebangs`,
`check-shebang-scripts-are-executable`, `check-case-conflict`.

#### Scenario: All baseline hooks listed in config

- **GIVEN** `prek.toml`
- **WHEN** the `[[repos]]` block for `pre-commit/pre-commit-hooks` is read
- **THEN** every hook id listed above SHALL be present

#### Scenario: Pipeline passes on a clean tree

- **GIVEN** a clean working tree
- **WHEN** `prek run --all-files` is invoked inside the devShell
- **THEN** every built-in hook SHALL exit zero

#### Scenario: Form-feed markers are preserved

- **GIVEN** an elisp file containing `^L` (U+000C) page-break separators
- **WHEN** `prek run trailing-whitespace --all-files` is invoked
- **THEN** the form-feed characters SHALL remain in place

### Requirement: Fixers exclude literal-content and generated paths

Built-in hooks that rewrite files (`trailing-whitespace`,
`end-of-file-fixer`, `mixed-line-ending`) SHALL NOT touch files under
`templates/`, `capture-templates/`, `file-templates/`, `site/`, `var/`,
`elpaca/`, `eln-cache/`, or `.worktrees/`. The exclusion SHALL be
expressed as a top-level `exclude` glob in `prek.toml` so it applies
uniformly to every fixer.

#### Scenario: Template directories excluded

- **GIVEN** a file under `file-templates/` containing trailing whitespace
- **WHEN** `prek run --all-files` runs
- **THEN** the `trailing-whitespace` hook SHALL skip the file
- **AND** the file SHALL remain unmodified

### Requirement: Local hooks invoke `--affected`

The three local hooks (`ert-tests`, `byte-compile`, `checkdoc`) SHALL
declare `pass_filenames = false` and `always_run = true`, and their
`entry` SHALL invoke the underlying script with the `--affected`
argument. The script-side transitive-dependent expansion via
`scripts/affected.sh` and `scripts/affected-tests.sh` SHALL remain the
source of truth for scope. (A follow-up change relocates that
expansion into Emacs, after which the hooks can flip to
`pass_filenames = true`.)

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

### Requirement: Local hooks execute inside the devShell

Each local hook's `entry` SHALL be prefixed with
`nix develop --command` so that `TREESIT_EXTRA_LOAD_PATH` is populated
regardless of whether the user invokes the hook from inside or outside
the devShell. Double-nested invocation (inside devShell calling
`nix develop --command`) SHALL be tolerated; the cost is acceptable.

#### Scenario: Hook outside devShell still resolves treesit grammars

- **GIVEN** a shell with no `IN_NIX_SHELL` env var
- **WHEN** `prek run ert-tests --all-files` is invoked
- **THEN** the hook SHALL spawn
  `nix develop --command ./scripts/run-tests.sh --affected`
- **AND** the spawned script SHALL see `TREESIT_EXTRA_LOAD_PATH` set

### Requirement: Single canonical hook install path

`prek install` SHALL be invoked from exactly one place: the devShell
`shellHook` in `flake.nix`. The Makefile SHALL NOT carry a
`setup-hooks` target, and `make test` SHALL NOT depend on one.

#### Scenario: Makefile does not install hooks

- **GIVEN** `Makefile`
- **WHEN** the targets are listed
- **THEN** `setup-hooks` SHALL NOT appear
- **AND** the `test` target's prerequisites SHALL NOT include `setup-hooks`

#### Scenario: devShell entry installs the hook

- **GIVEN** a fresh checkout with no `.git/hooks/pre-commit`
- **WHEN** `nix develop` is entered
- **THEN** `.git/hooks/pre-commit` SHALL exist after the shellHook runs
