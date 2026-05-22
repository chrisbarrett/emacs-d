# dep-graph Specification

## Purpose

Internals-facing. Covers the `dep-graph` library at
`lisp/dep-graph/dep-graph.el` and its companion CLI wrapper
`scripts/dep-graph`. The library reads `.el` files in the repository
with the Emacs reader, builds a reverse-adjacency graph of
file-level dependencies (`provide` / `require` / `load` /
`use-package :after`), and answers transitive-dependent queries for
the pre-commit pipeline. The pure core (build, dependents, affected)
operates on hand-built form summaries and is testable without
filesystem fixtures; the IO shell (`scan-file`, `scan-root`) supplies
those summaries from real source trees. The CLI exposes `affected`,
`affected-tests`, and `print` subcommands so prek hooks, the
Makefile, and humans all consume the same graph through one
interface.

## Requirements

### Requirement: Pure core builds reverse-adjacency graph from forms

`dep-graph-build` SHALL accept a list of file-form summaries — each an
alist entry `(PATH :provides FEATURES :requires FEATURES)` — and
return a hash-table keyed by feature symbol whose values are the list
of file paths that require that feature. The function SHALL NOT touch
the filesystem.

#### Scenario: Direct dependent

- **GIVEN** input `(("a.el" :provides (a) :requires (b))
                     ("b.el" :provides (b) :requires ()))`
- **WHEN** `dep-graph-build` is called
- **THEN** the returned graph maps feature `b` to a list containing
  `"a.el"`

#### Scenario: Empty graph

- **GIVEN** an empty input list
- **WHEN** `dep-graph-build` is called
- **THEN** the returned graph contains no entries

### Requirement: Transitive dependent expansion

`dep-graph-dependents` SHALL accept a graph and a starting file path
and return every file path that depends on the starting file
transitively. The starting file itself SHALL appear in the result.
Cycles SHALL terminate via a visited set without raising an error.

#### Scenario: Two-hop chain

- **GIVEN** files `a.el → b.el → c.el` (a requires b, b requires c)
- **WHEN** `dep-graph-dependents` is called with `"c.el"`
- **THEN** the result SHALL contain `"a.el"`, `"b.el"`, and `"c.el"`

#### Scenario: Cycle does not loop

- **GIVEN** files `a.el ↔ b.el` (a requires b, b requires a)
- **WHEN** `dep-graph-dependents` is called with `"a.el"`
- **THEN** the call SHALL return a result containing both files
  without entering an infinite loop

### Requirement: Affected-files union

`dep-graph-affected` SHALL accept a graph, a list of starting file
paths, and an optional filter predicate, and return the union of the
transitive dependents of each starting file. When the filter predicate
is supplied, only paths satisfying the predicate SHALL appear in the
result.

#### Scenario: Filter selects test files

- **GIVEN** a graph and a filter predicate matching
  `^.+-tests\\.el$`
- **WHEN** `dep-graph-affected` is called with one source file and
  the filter
- **THEN** the result SHALL contain only the matching test file paths

### Requirement: Reader-based form extraction

`dep-graph-scan-file` SHALL open a file with `insert-file-contents`,
walk top-level forms with `read` until end-of-file, and extract
`provide`, `require`, `load`, and `use-package` forms. Forms inside
docstrings, comments, or string literals SHALL NOT be extracted.

#### Scenario: Docstring `require` is not extracted

- **GIVEN** a file whose only `require`-like text is inside a defun
  docstring
- **WHEN** `dep-graph-scan-file` runs on the file
- **THEN** the returned `:requires` list SHALL be empty

#### Scenario: `use-package :after` accepts a list

- **GIVEN** a top-level `(use-package foo :after (a b))` form
- **WHEN** `dep-graph-scan-file` runs on the file
- **THEN** the file's `:requires` list SHALL contain both `a` and `b`

#### Scenario: Optional `require` is still a dependency

- **GIVEN** a top-level `(require 'foo nil t)` form
- **WHEN** `dep-graph-scan-file` runs on the file
- **THEN** the file's `:requires` list SHALL contain `foo`

### Requirement: Multiple providers raise an error

`dep-graph-build` SHALL signal a user-error if two distinct file paths
both `:provides` the same feature symbol. The error message SHALL
identify the conflicting feature and both file paths.

#### Scenario: Duplicate provider rejected

- **GIVEN** input where both `a.el` and `a-other.el` provide feature
  `a`
- **WHEN** `dep-graph-build` is called
- **THEN** a user-error SHALL be raised mentioning feature `a` and
  both file paths

### Requirement: Root scan walks lisp/ and lib/

`dep-graph-scan-root` SHALL accept a root directory and return a
file-forms alist suitable for `dep-graph-build`, covering every `.el`
file (including `-tests.el`) under the root's `lisp/` and `lib/`
subdirectories. Generated and vendored paths (`elpaca/`, `eln-cache/`,
`var/`) SHALL be skipped.

#### Scenario: Scan ignores elpaca

- **GIVEN** a root containing `lisp/foo.el` and `elpaca/builds/bar.el`
- **WHEN** `dep-graph-scan-root` runs on the root
- **THEN** the result SHALL contain `lisp/foo.el`
- **AND** the result SHALL NOT contain any `elpaca/...` path

### Requirement: CLI exposes affected, affected-tests, print

The wrapper script `scripts/dep-graph` SHALL accept three
subcommands:

- `affected` — emit transitive-dependent source file paths
- `affected-tests` — emit transitive-dependent test file paths
- `print` — emit a debug dump of the reverse graph

`affected` and `affected-tests` SHALL accept either positional file
paths or the `--from-git` flag (which derives the staged + unstaged
changed file set). Output SHALL be newline-separated, with the
sentinels `all` (no input files) and `none` (input present but no
output) preserved from the predecessor scripts.

#### Scenario: Positional input

- **GIVEN** `scripts/dep-graph affected lisp/+core.el`
- **WHEN** the CLI runs
- **THEN** stdout SHALL contain `lisp/+core.el` and every transitive
  dependent of it, one path per line

#### Scenario: Empty git input emits `all`

- **GIVEN** no staged or unstaged changes
- **WHEN** `scripts/dep-graph affected --from-git` runs
- **THEN** stdout SHALL be the single line `all`
