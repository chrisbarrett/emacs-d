# Implementation Plan

Plan generated from specs: test-runner.md, repo-structure.md, 003-config-analysis.md

## Phase 1: Pre-commit Orchestration (test-runner.md)

Foundation for quality gates. Enables faster feedback on staged changes.

### 1.1 Create `scripts/affected.sh`

**Status:** Complete

Create the dependency analysis script that computes transitive dependents.
Current `affected-tests.sh` only finds test files; `affected.sh` returns
affected *source* files for use by build/lint targets.

Must handle:
- `require` forms
- `load` forms
- `use-package :after` declarations

**Acceptance:** `./scripts/affected.sh` with staged .el changes outputs affected files

### 1.2 Add `build-affected` Makefile target

**Status:** Complete

Byte-compile only transitively affected files from staged changes.

```makefile
build-affected:
	@./scripts/affected.sh | xargs ./scripts/byte-compile.sh
```

**Acceptance:** `make build-affected` exits 0 with clean tree

### 1.3 Add `test-affected` Makefile target

**Status:** Complete

Run ERT tests only for transitively affected files.

**Acceptance:** `make test-affected` exits 0 with clean tree

### 1.4 Add `lint-affected` Makefile target

**Status:** Complete

Run checkdoc on directly affected files (not transitive).

**Acceptance:** `make lint-affected` exits 0 with clean tree

### 1.5 Add `pre-commit` Makefile target

**Status:** Complete

Compose all checks: lint-affected, build-affected, test-affected.
Fail fast on first error.

**Acceptance:** `make pre-commit` exits 0 with clean tree

---

## Phase 2: Module System (repo-structure.md)

Establishes self-contained module structure for incremental migration.

### 2.1 Implement module discovery function

**Status:** Complete

Create `+modules-discover` that finds all directories under `modules/`.

**Acceptance:** Unit test with mock directory structure passes

### 2.2 Implement packages.eld reader

**Status:** Complete

Create `+modules-read-packages` that reads packages.eld as data (not eval'd).

Format:
```elisp
;; -*- lisp-data -*-
((evil :host github :repo "emacs-evil/evil")
 (evil-collection))
```

**Acceptance:** Unit test parses sample eld correctly

### 2.3 Integrate packages with elpaca

**Status:** Complete

Wire package specs into elpaca installation.

**Acceptance:** Integration test installs a package from packages.eld

### 2.4 Implement autoload registration

**Status:** Complete

Register autoloads from `lib.el` and `lib/**/*.el` in modules.

**Acceptance:** `fboundp` returns t for autoloaded symbol before load

### 2.5 Implement init.el loader

**Status:** Complete

Load all module `init.el` files after packages and autoloads.

Order: packages.eld (all) → autoloads (all) → init.el (all)

**Acceptance:** Test buffer shows expected init side-effects

### 2.6 Document active specs workflow

**Status:** Complete

Document in CLAUDE.md how to promote/demote specs via symlinks.

**Acceptance:** `readlink specs/NNN-*.md` resolves to module dir

---

## Phase 3: Configuration Analysis (003-config-analysis.md)

Reverse-engineer existing config into feature specs.

### 3.1 Inventory files and identify feature groupings

**Status:** Complete

Analyze all files in config/, init/, lisp/, root.
Group by coupling indicators (shared deps, cross-calls, require).

**Output:** FEATURE_INVENTORY.md with 47 feature groupings for user confirmation

### 3.2 Analyze features and write specs

**Status:** Complete

For each confirmed feature:
- Extract dependencies (packages, built-ins, other features)
- Document behavior (Given/When/Then or tables)
- Extract API (commands, functions, variables, keybindings)
- Define testable properties

**Output:** specs/004-050 written for all 47 features from FEATURE_INVENTORY.md

---

## Phase 4: Module Migration

Migrate existing configuration to self-contained module structure.

### 4.1 Create first pilot module

**Status:** Complete

Select a well-bounded feature (low dependencies, clear scope) for pilot migration.

Recommended pilot: **theme** (8 behaviors, 2 files, minimal cross-cutting concerns)

Create:
```
modules/theme/
  spec.md        # symlink or copy from specs/008-theme.md
  packages.eld   # catppuccin-theme, modus-themes
  init.el        # extracted from init/init-theme.el
  lib.el         # extracted from lisp/+theme.el
  tests.el       # new tests based on spec
```

**Acceptance:** `make test` passes; theme loading works from module

### 4.2 Wire module loading into init sequence

**Status:** Not started

Update `init.el` to call module system at appropriate point in startup:

1. `+modules-install-packages` early (package installation)
2. `+modules-register-autoloads` before feature usage
3. `+modules-load-inits` after autoloads registered

**Acceptance:** Pilot module loads successfully during Emacs startup

### 4.3 Migrate additional modules

**Status:** Not started

Migrate features in dependency order (leaf features first):

| Priority | Feature       | Reason                              |
| -------- | ------------- | ----------------------------------- |
| 1        | theme         | No cross-cutting deps               |
| 2        | spellcheck    | Minimal deps                        |
| 3        | help          | Minimal deps                        |
| 4        | templates     | Self-contained                      |
| 5        | format        | Depends only on apheleia            |
| 6        | search        | Depends only on wgrep               |
| 7        | diff          | No external deps                    |
| 8        | debug         | No external deps                    |
| 9        | editing       | Core but minimal deps               |
| 10       | lang-*        | Each language is self-contained     |

**Acceptance:** Each migrated module has tests; `make test` passes

### 4.4 Deprecate old file structure

**Status:** Not started

Once modules are stable:
1. Add deprecation warnings to old init/*.el files
2. Remove duplication (old files just load modules)
3. Eventually delete old files

**Acceptance:** No functional duplication between modules/ and init/config/

---

## Task Priority

Execute in order listed. Each task should be completed and committed
before starting the next.

Current task: **4.1 Create first pilot module** (theme recommended)
