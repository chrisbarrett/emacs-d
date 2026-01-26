# Implementation Plan

Plan generated from specs: test-runner.md, repo-structure.md, 003-config-analysis.md

## Phase 1: Pre-commit Orchestration (test-runner.md)

Foundation for quality gates. Enables faster feedback on staged changes.

### 1.1 Create `scripts/affected.sh`

**Status:** Not started

Create the dependency analysis script that computes transitive dependents.
Current `affected-tests.sh` only finds test files; `affected.sh` returns
affected *source* files for use by build/lint targets.

Must handle:
- `require` forms
- `load` forms
- `use-package :after` declarations

**Acceptance:** `./scripts/affected.sh` with staged .el changes outputs affected files

### 1.2 Add `build-affected` Makefile target

**Status:** Not started

Byte-compile only transitively affected files from staged changes.

```makefile
build-affected:
	@./scripts/affected.sh | xargs ./scripts/byte-compile.sh
```

**Acceptance:** `make build-affected` exits 0 with clean tree

### 1.3 Add `test-affected` Makefile target

**Status:** Not started

Run ERT tests only for transitively affected files.

**Acceptance:** `make test-affected` exits 0 with clean tree

### 1.4 Add `lint-affected` Makefile target

**Status:** Not started

Run checkdoc on directly affected files (not transitive).

**Acceptance:** `make lint-affected` exits 0 with clean tree

### 1.5 Add `pre-commit` Makefile target

**Status:** Not started

Compose all checks: lint-affected, build-affected, test-affected.
Fail fast on first error.

**Acceptance:** `make pre-commit` exits 0 with clean tree

---

## Phase 2: Module System (repo-structure.md)

Establishes self-contained module structure for incremental migration.

### 2.1 Implement module discovery function

**Status:** Not started

Create `+modules-discover` that finds all directories under `modules/`.

**Acceptance:** Unit test with mock directory structure passes

### 2.2 Implement packages.eld reader

**Status:** Not started

Create `+modules-read-packages` that reads packages.eld as data (not eval'd).

Format:
```elisp
;; -*- lisp-data -*-
((evil :host github :repo "emacs-evil/evil")
 (evil-collection))
```

**Acceptance:** Unit test parses sample eld correctly

### 2.3 Integrate packages with elpaca

**Status:** Not started

Wire package specs into elpaca installation.

**Acceptance:** Integration test installs a package from packages.eld

### 2.4 Implement autoload registration

**Status:** Not started

Register autoloads from `lib.el` and `lib/**/*.el` in modules.

**Acceptance:** `fboundp` returns t for autoloaded symbol before load

### 2.5 Implement init.el loader

**Status:** Not started

Load all module `init.el` files after packages and autoloads.

Order: packages.eld (all) → autoloads (all) → init.el (all)

**Acceptance:** Test buffer shows expected init side-effects

### 2.6 Document active specs workflow

**Status:** Not started

Document in CLAUDE.md how to promote/demote specs via symlinks.

**Acceptance:** `readlink specs/NNN-*.md` resolves to module dir

---

## Phase 3: Configuration Analysis (003-config-analysis.md)

Reverse-engineer existing config into feature specs.

### 3.1 Inventory files and identify feature groupings

**Status:** Not started

Analyze all files in config/, init/, lisp/, root.
Group by coupling indicators (shared deps, cross-calls, require).

**Output:** Feature list for user confirmation

### 3.2 Analyze features and write specs

**Status:** Not started

For each confirmed feature:
- Extract dependencies (packages, built-ins, other features)
- Document behavior (Given/When/Then or tables)
- Extract API (commands, functions, variables, keybindings)
- Define testable properties

**Output:** specs/NNN-{slug}.md for each feature (starting at 004)

---

## Task Priority

Execute in order listed. Each task should be completed and committed
before starting the next.

Current task: **1.1 Create `scripts/affected.sh`**
