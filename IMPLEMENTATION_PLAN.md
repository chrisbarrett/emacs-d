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

**Status:** Complete

Update `init.el` to call module system at appropriate point in startup:

1. `+modules-install-packages` early (package installation)
2. `+modules-register-autoloads` before feature usage
3. `+modules-load-inits` after autoloads registered

**Acceptance:** Pilot module loads successfully during Emacs startup

### 4.3 Migrate additional modules

**Status:** Complete

All 47 features from FEATURE_INVENTORY.md migrated to modules/:

| Module          | Spec  | Package Count | Tests |
| --------------- | ----- | ------------- | ----- |
| anthropic       | 033   | 0             | 22    |
| auth            | 031   | 1             | 6     |
| claude          | 032   | 1             | 14    |
| compile         | 027   | 0             | 19    |
| completion      | 006   | 10            | 32    |
| core            | 004   | 6             | 41    |
| debug           | 028   | 0             | 5     |
| diff            | 029   | 0             | 11    |
| dired           | 017   | 3             | 20    |
| editing         | 011   | 0             | 18    |
| eglot           | 019   | 1             | 14    |
| evil            | 005   | 10            | 33    |
| format          | 012   | 1             | 11    |
| help            | 014   | 2             | 15    |
| hex             | 050   | 0             | 7     |
| input-methods   | 030   | 0             | 10    |
| lang-c          | 041   | 0             | 12    |
| lang-conf       | 044   | 1             | 9     |
| lang-elixir     | 038   | 1             | 14    |
| lang-erlang     | 047   | 0             | 5     |
| lang-js         | 035   | 0             | 17    |
| lang-latex      | 043   | 0             | 6     |
| lang-lisp       | 034   | 2             | 18    |
| lang-markdown   | 042   | 1             | 22    |
| lang-nix        | 037   | 1             | 9     |
| lang-ocaml      | 039   | 2             | 17    |
| lang-rust       | 036   | 0             | 10    |
| lang-shscript   | 046   | 0             | 7     |
| lang-terraform  | 040   | 2             | 13    |
| lang-text       | 045   | 0             | 3     |
| lang-zig        | 048   | 1             | 4     |
| leader          | 007   | 0             | 28    |
| nav             | 010   | 3             | 34    |
| org             | 023   | 7             | 34    |
| org-agenda      | 024   | 1             | 21    |
| org-capture     | 025   | 3             | 24    |
| org-roam        | 026   | 2             | 23    |
| project         | 021   | 1             | 18    |
| reader          | 049   | 0             | 4     |
| search          | 013   | 1             | 8     |
| shells          | 018   | 1             | 17    |
| spellcheck      | 016   | 2             | 14    |
| templates       | 015   | 1             | 14    |
| theme           | 008   | 1             | 18    |
| treesit         | 020   | 1             | 9     |
| ui              | 009   | 13            | 57    |
| vcs             | 022   | 7             | 28    |

**Acceptance:** All tests pass; `make test` succeeds

### 4.4 Deprecate old file structure

**Status:** Complete

47 init files deprecated with warnings pointing to their module locations.
4 bootstrap files remain (init-elpaca.el, init-hooks.el, init-system.el,
init-readonly.el) - these must load before the module system.

**Acceptance:** No functional duplication between modules/ and init/config/

---

## Phase 5: Cleanup

Complete module migration by deprecating and removing legacy files.

### 5.1 Remove explicit deprecated init loads from init.el

**Status:** Complete

Remove explicit `(use-package init-leader :demand t)` and
`(use-package init-input :demand t)` from init.el. These files are:
- Already deprecated (emit warnings)
- Already loaded via the directory scan (lines 99-103)
- Causing duplicate loading and redundant warnings

**Acceptance:** `make test` passes; no duplicate deprecation warnings

### 5.2 Deprecate config/*.el files

**Status:** Not started

17 config files have been migrated to modules but not deprecated:

| File                    | Module                    |
| ----------------------- | ------------------------- |
| mod-beframe.el          | modules/nav/              |
| mod-browse-at-remote.el | modules/vcs/              |
| mod-bufler.el           | modules/nav/              |
| mod-compilation.el      | modules/compile/          |
| mod-eshell.el           | modules/shells/           |
| mod-evil.el             | modules/evil/             |
| mod-input-methods.el    | modules/input-methods/    |
| mod-magit.el            | modules/vcs/              |
| mod-ocaml.el            | modules/lang-ocaml/       |
| mod-org-agenda.el       | modules/org-agenda/       |
| mod-org-capture.el      | modules/org-capture/      |
| mod-org-link.el         | modules/org/              |
| mod-org.el              | modules/org/              |
| mod-pulsar.el           | modules/ui/               |
| mod-tabs.el             | modules/nav/              |
| mod-tty-frames.el       | modules/nav/              |
| mod-worktrees.el        | modules/vcs/              |

Add DEPRECATED comments and deprecation warnings to each file.

**Acceptance:** Each file has deprecation warning; `make test` passes

### 5.3 Deprecate lisp/*.el files superseded by modules

**Status:** Not started

Lisp files with module equivalents that can be deprecated:

| File                    | Module lib.el             |
| ----------------------- | ------------------------- |
| +agenda.el              | modules/org-agenda/lib.el |
| +anthropic.el           | modules/anthropic/lib.el  |
| +capture.el             | modules/org-capture/lib.el|
| +clockreport.el         | modules/org-agenda/lib.el |
| +compile.el             | modules/compile/lib.el    |
| +consult-imenu-elisp.el | modules/lang-lisp/lib.el  |
| +edit-cmds.el           | modules/editing/lib.el    |
| +evil-collection.el     | modules/evil/lib.el       |
| +file-templates.el      | modules/templates/lib.el  |
| +git.el                 | modules/vcs/lib.el        |
| +theme.el               | modules/theme/lib.el      |
| +window.el              | modules/nav/lib.el        |

Core infrastructure files to KEEP (not deprecate):
- +corelib.el (foundational macros used everywhere)
- +modules.el (module system itself)
- +load-incrementally.el (deferred loading system)
- +files.el (file utilities used by multiple modules)
- evil-tty-cursor.el (standalone utility)

**Acceptance:** Deprecated files have warnings; `make test` passes

### 5.4 Remove deprecated files entirely

**Status:** Not started

After 5.1-5.3 complete and tested:
1. Remove deprecated init/*.el files (47 files)
2. Remove deprecated config/*.el files (17 files)
3. Remove deprecated lisp/*.el files (12 files)
4. Update init.el to remove directory scan of init/ if empty

**Acceptance:** `make test` passes; Emacs starts cleanly

### 5.5 Evaluate bootstrap file migration

**Status:** Not started

Evaluate whether init-elpaca.el, init-hooks.el, init-system.el, and
init-readonly.el can be moved into modules/core/ or loaded via the
module system.

Constraints:
- These files must load BEFORE the module system initializes
- init-elpaca.el bootstraps the package manager
- init-hooks.el defines lifecycle hooks used by modules
- init-system.el sets up exec-path-from-shell, envrc
- init-readonly.el protects read-only files

May require module system changes to support "pre-init" modules.

**Acceptance:** Document decision in AGENTS.md; implement if feasible

---

## Task Priority

Execute in order listed. Each task should be completed and committed
before starting the next.

Current task: **5.2 Deprecate config/*.el files**
