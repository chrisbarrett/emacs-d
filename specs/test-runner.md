# Feature: Test Runner

Batch-mode test runner with Elpaca load-path setup.

## Usage

```bash
emacs -Q --batch -l tests.el                     # run all tests
emacs -Q --batch -l tests.el lisp/+compile       # run tests for file
emacs -Q --batch -l tests.el "prefix-*"          # run tests matching pattern
```

## Behavior

### Load Path Setup

**Given** tests.el is loaded
**When** load paths are configured
**Then** `lisp/` is added to load-path
**And** all `elpaca/builds/*/` directories are added to load-path

### Test Discovery

| Argument         | Interpretation                          |
| ---------------- | --------------------------------------- |
| (none)           | Run all `*-tests.el` files              |
| `path/to/file`   | Run `path/to/file-tests.el`             |
| `"pattern"`      | Pass pattern to `ert-run-tests-batch`   |

**Given** no arguments
**When** tests run
**Then** all files matching `**/*-tests.el` are loaded and executed

**Given** a file path argument (without `-tests.el` suffix)
**When** tests run
**Then** corresponding `-tests.el` file is loaded and executed

**Given** a quoted pattern argument
**When** tests run
**Then** pattern is passed to ERT selector

### Exit Behavior

**Given** tests complete
**When** all tests pass
**Then** exit code is 0

**Given** tests complete
**When** any test fails
**Then** exit code is non-zero
**And** failure summary is printed

## Implementation Notes

- Use `command-line-args-left` for argument parsing
- Use `directory-files-recursively` for test discovery
- Clear `command-line-args-left` after parsing to prevent Emacs processing

## Provided API

| Symbol                  | Type     | Description                    |
| ----------------------- | -------- | ------------------------------ |
| `+test-runner-run`      | function | Main entry point               |
| `+test-runner-load-paths` | function | Set up Elpaca load paths     |

## Feature: Pre-commit Orchestration

Makefile-driven checks for staged files, running only affected targets.

### R1: Dependency Analysis Script

`scripts/affected.sh` computes transitive dependents of input files.

**Given** a list of Emacs Lisp files
**When** the script runs
**Then** it outputs all files that transitively depend on the inputs
**And** dependency is defined as: `load`, `require`, or `use-package :after`

**Verify:** `make test` passes

### R2: Build Affected Files

**Given** staged files include Emacs Lisp changes
**When** `make build-affected` runs
**Then** byte-compilation runs on transitively affected files
**And** exits non-zero on warnings or errors

**Verify:** `make build-affected` with clean files exits 0

### R3: Test Affected Files

**Given** staged files include Emacs Lisp changes
**When** `make test-affected` runs
**Then** ERT tests run for transitively affected files and modules
**And** test discovery uses existing `-tests.el` convention

**Verify:** `make test-affected` with clean files exits 0

### R4: Lint Affected Files

**Given** staged files include Emacs Lisp changes
**When** `make lint-affected` runs
**Then** checkdoc runs on directly affected files (not transitive)
**And** exits non-zero on violations

**Verify:** `make lint-affected` with clean files exits 0

### R5: Pre-commit Composition

**Given** a git pre-commit hook
**When** `make pre-commit` runs
**Then** it executes lint-affected, build-affected, test-affected in sequence
**And** fails fast on first error

**Verify:** `make pre-commit` with clean files exits 0

## Tasks

- [x] [R1] Create `scripts/affected.sh` with dependency parser
- [x] [R1] Handle `load`, `require`, `use-package :after` patterns
- [x] [R2] Add `build-affected` target to Makefile
- [x] [R3] Add `test-affected` target to Makefile
- [x] [R4] Add `lint-affected` target to Makefile
- [x] [R5] Add `pre-commit` target composing all checks
- [x] [R5] Update `make setup-hooks` to install pre-commit hook
