# Incremental Pre-Commit Hooks

## Context

Pre-commit hooks currently run all checks on every commit, causing slow commit
times even for single-file changes. The `affected.sh` infrastructure exists but
hooks don't use it.

## Requirements

### R1: Byte-compile only affected files

**Given** staged changes to `lisp/*.el` or `lib/*.el`
**When** the byte-compile hook runs
**Then** only changed files and their dependents are compiled

**Verify:** Stage one file, run hook, confirm only that file's dependency chain compiles

### R2: Checkdoc only affected files

**Given** staged changes to `lisp/*.el` or `lib/*.el`
**When** the checkdoc hook runs
**Then** only directly changed files are linted

**Verify:** Stage one file, run hook, confirm only that file is checked

### R3: Tests only for affected files

**Given** staged changes to source files
**When** the ert-tests hook runs
**Then** only tests corresponding to affected files run

**Verify:** Stage `lisp/+foo.el`, confirm only `lisp/+foo-tests.el` runs (if exists)

### R4: Skip hooks when no relevant files changed

**Given** staged changes to non-elisp files (e.g., `.md`, `.yaml`)
**When** hooks run
**Then** elisp hooks exit early with success

**Verify:** Stage only `README.md`, confirm hooks pass instantly

### R5: Full validation remains available

**Given** user wants to run all checks
**When** `make test` or `prek run --all-files` is invoked
**Then** all files are validated regardless of changes

**Verify:** `make test` runs full suite even with no staged changes
