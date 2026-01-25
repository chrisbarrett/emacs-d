# Feature: Repository Structure

Spec-driven Emacs configuration with self-contained feature units.

## User Outcomes

- Features are self-contained: spec, packages, init, lib, tests co-located
- Clear mapping from spec to implementation
- LLM agents can work autonomously within feature boundaries
- Incremental migration from existing config

## Structure

```
features/
  {slug}/
    spec.md        # feature specification
    packages.eld   # elpaca specs (data only)
    init.el        # evaluated during init
    lib.el         # autoloaded functions
    lib/           # alternative: multiple lib files
      *.el
    tests.el       # ERT tests

specs/
  NNNN-{slug}.md   # symlinks to active specs
```

**Verify:** `fd -t d . features/ | head -1 | xargs ls` shows expected files

## R1: Feature Discovery

**Given** features exist under `features/`
**When** Emacs starts
**Then** all feature directories are discovered

**Verify:** Unit test with mock directory structure

## R2: Package Loading

**Given** discovered features with `packages.eld` files
**When** packages are loaded
**Then** each `packages.eld` is read as data (not eval'd)
**And** elpaca specs are collected and processed

`packages.eld` format:
```elisp
;; -*- lisp-data -*-
((evil :host github :repo "emacs-evil/evil")
 (evil-collection))
```

**Verify:** Unit test parses sample eld; integration test installs package

## R3: Autoload Registration

**Given** discovered features
**When** autoloads are registered
**Then** functions in `lib.el` are autoloaded
**And** functions in `lib/**/*.el` are autoloaded

**Verify:** `fboundp` returns t for autoloaded symbol; symbol is not yet loaded

## R4: Init Evaluation

**Given** packages loaded and autoloads registered
**When** init runs
**Then** all `init.el` files are evaluated

Order: packages.eld (all) → autoloads (all) → init.el (all)

**Verify:** Test buffer shows expected init side-effects

## R5: Active Specs Workflow

**Given** a feature under active development
**When** spec is promoted to active
**Then** symlink exists at `specs/NNNN-{slug}.md`

**Given** a feature that has stabilized
**When** spec is demoted
**Then** symlink is removed; `spec.md` remains in feature dir

**Verify:** `readlink specs/NNNN-*.md` resolves to feature dir

## R6: Test Execution

**Given** a feature with `tests.el`
**When** tests are run
**Then** ERT executes in batch mode
**And** exit code reflects pass/fail

**Verify:** `emacs --batch -l tests.el -f ert-run-tests-batch-and-exit`

## Constraints

- Flat structure initially; nesting permitted later as patterns emerge
- Feature names are simple slugs (e.g., `modal-editing`)
- No conditional package installation
- Incremental migration: existing config untouched until manually moved

## Tasks

- [ ] [R1] Implement feature discovery function
- [ ] [R2] Implement packages.eld reader
- [ ] [R2] Integrate with elpaca
- [ ] [R3] Implement autoload registration
- [ ] [R4] Implement init.el loader
- [ ] [R5] Document active specs workflow
- [ ] [R6] Add test runner script
