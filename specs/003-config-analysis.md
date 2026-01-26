# Feature: Configuration Analysis

Reverse-engineer specs from existing Emacs configuration to enable test-driven module migration.

## User Outcomes

- Every logical feature in the config has a spec capturing its behavior
- Specs include testable acceptance criteria
- Subsequent agents can generate modules with tests from these specs
- No premature module boundary decisions - specs describe what exists

## Scope

Analyze all files in:

| Path            | File Pattern   | Count |
| --------------- | -------------- | ----- |
| `config/`       | `mod-*.el`     | 17    |
| `init/`         | `init-*.el`    | 51    |
| `lisp/`         | `+*.el`        | 18    |
| root            | `init.el`      | 1     |
| root            | `early-init.el`| 1     |

## Output

One spec per logical feature in `specs/` with 3-digit IDs starting at 004.

Format follows existing specs (see 0001, 0002):
- Feature name and summary
- Dependencies (packages, built-ins, other features)
- Behavior (Given/When/Then or tables)
- Provided API (commands, functions, variables, keybindings)
- Properties to Verify (testable acceptance criteria)

## R1: Feature Identification

**Given** the file inventory above
**When** the agent analyzes the codebase
**Then** it identifies logical features by grouping tightly-coupled files

Coupling indicators:
- Shared package dependencies (e.g., evil + evil-collection)
- Cross-file function calls
- Related functionality (e.g., all org-mode config)
- Explicit `require` or `load` relationships

**Output:** List of features with constituent files before writing specs.

## R2: Dependency Extraction

**Given** a feature's source files
**When** dependencies are extracted
**Then** the spec lists:
- External packages (from `use-package`, `elpaca`, `require`)
- Built-in Emacs features
- Other features in this config (cross-references)

## R3: Behavior Documentation

**Given** a feature's source files
**When** behavior is documented
**Then** the spec captures:
- What the feature does (user-visible effects)
- Configuration applied (setq, setopt, custom-set)
- Hooks registered
- Advice added
- Mode-specific settings

Use Given/When/Then for complex behaviors; tables for simple mappings.

## R4: API Extraction

**Given** a feature's source files
**When** API is extracted
**Then** the spec lists:
- Interactive commands (with keybindings if any)
- Public functions (non-`--` prefixed)
- User-facing variables and customization options
- Provided features (`provide` forms)

## R5: Testable Properties

**Given** documented behavior and API
**When** properties are derived
**Then** each spec includes verifiable acceptance criteria

Property types:
- Function behavior: input → expected output
- Side effects: action → observable state change
- Keybindings: key sequence → bound command
- Hooks: hook → registered functions
- Settings: variable → expected value

## R6: Spec Writing

**Given** analyzed features
**When** specs are written
**Then** each spec is written to `specs/NNN-{slug}.md`
**And** slug is kebab-case feature name
**And** IDs are sequential starting at 004

## Constraints

- Do not prescribe module boundaries - specs describe current state
- Do not refactor or improve - document what exists
- Group only obviously-coupled files; when uncertain, keep separate
- Preserve existing naming where sensible (e.g., "evil" not "modal-editing")
- Skip trivial single-line configs unless they have distinct behavior

## Suggested Feature Groupings

Initial hypothesis (agent should refine based on actual analysis):

| Feature Slug       | Likely Files                                           |
| ------------------ | ------------------------------------------------------ |
| evil               | init-evil, mod-evil, +evil-collection, evil-tty-cursor |
| completion         | init-completion                                        |
| org                | init-org, mod-org, mod-org-agenda, mod-org-capture, mod-org-link, +agenda, +capture, +clockreport |
| org-roam           | init-org-roam                                          |
| magit              | init-vcs, mod-magit, mod-worktrees, mod-browse-at-remote, +git |
| project            | init-project                                           |
| eglot              | init-eglot                                             |
| theme              | init-theme, +theme                                     |
| ui                 | init-ui, init-modeline, mod-pulsar                     |
| editing            | init-editing, +edit-cmds                               |
| dired              | init-dired                                             |
| shells             | init-shells, mod-eshell                                |
| treesit            | init-treesit                                           |
| leader             | init-leader                                            |
| search             | init-search                                            |
| nav                | init-nav                                               |
| templates          | init-templates, +file-templates                        |
| compile            | mod-compilation, +compile                              |
| lang-*             | init-{lang} for each language                          |
| core               | init.el, early-init.el, init-elpaca, init-hooks, init-system, +corelib, +files, +window, +load-incrementally |

## Execution Plan

1. Read all source files in scope
2. Build dependency graph (requires, use-package :after, function calls)
3. Identify feature clusters
4. Present feature list for confirmation
5. Write specs in parallel (one agent per feature)
6. Review for completeness

## Tasks

- [x] [R1] Inventory all files and identify feature groupings
- [x] [R1] Present feature list for user confirmation
- [x] [R2-R5] Analyze each feature's files
- [x] [R6] Write spec for each feature
- [x] Review specs for completeness and consistency
