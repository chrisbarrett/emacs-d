## Context

`scripts/affected.sh` (200 lines) and `scripts/affected-tests.sh` (180
lines) each implement a regex-based Lisp parser in bash, then build a
reverse-dependency map, then traverse it from a set of git-derived
staged files. The two scripts share ~70% of their parsing logic but
duplicate it because shell composition of associative arrays is
painful.

Two real bugs the bash parser hides:

- Regex `\(require '([^)]+)\)` matches inside docstrings and inside
  commented-out forms.
- The `:after` extractor `:after [^:)]+` swallows whatever follows
  `:after` until a `:` or `)` — fine for `:after foo` but mangled for
  `:after (foo bar)`.

The legitimate questions to answer here are:

1. Can we build the same reverse graph using the Emacs reader instead
   of regex, fast enough that we don't need a cache?
2. What's the smallest API surface that satisfies every caller
   (byte-compile, checkdoc, ert-tests, Makefile)?
3. How do we prove parity with the bash version before deleting it?

## Goals / Non-Goals

**Goals:**

- A pure-core / IO-shell split so the graph algorithms are testable
  without filesystem fixtures.
- Reader-based form extraction — no regex over source.
- Single CLI (`scripts/dep-graph`) replacing the two bash scripts.
- Parity with the bash behaviour on real inputs (modulo the bugs
  above — those we intentionally fix).
- All existing prek hooks and Makefile targets keep working.

**Non-Goals:**

- Caching. Recompute the graph on every CLI call. If profiling shows
  it's slow enough to hurt the dev loop, add a cache in a follow-up
  change keyed on file mtimes.
- Flipping prek hooks to `pass_filenames=true`. Belongs to the
  follow-up change once this lands.
- Collapsing the three gate scripts (`byte-compile.sh` /
  `checkdoc.sh` / `run-tests.sh`) into one dispatcher. Separate
  change.
- Cross-package autoloads / `lisp/+autoloads.el` regeneration —
  separate concern (`Makefile` already handles it).
- Loading every file to walk `load-history`. Static scan only.

## Decisions

### Decision: new axis `dep-graph`

The library covers one self-contained domain: building and querying an
Elisp file-dependency graph. Spec name = library base name per the
`gfm-pretty` / `gfm-present` precedent (`lisp/<family>/<lib>.el` →
axis `<lib>`). Internals-facing — declared in the spec's Purpose.

### Decision: reader-based scan, not file load

Each `.el` file is opened with `with-temp-buffer` + `insert-file-contents`
and walked with `read` until end-of-file. Top-level forms are
pattern-matched for `provide`, `require`, `load`, and `use-package`
calls. This respects Lisp syntax — strings, comments, and
shebang/coding lines are skipped by the reader naturally.

Alternatives considered:
- **Load every file, walk `load-history`**: correct (the canonical
  source) but pays full init cost per CLI invocation. Rejected.
- **Continue with regex**: rejected; the bugs above are why we're
  here.

### Decision: pure core, IO shell

Module split:

```
(dep-graph-build FILE-FORMS)            ; pure: alist → graph
(dep-graph-dependents GRAPH FILE)       ; pure: transitive closure
(dep-graph-affected GRAPH FILES &opt FILTER) ; pure: union of dependents

(dep-graph-scan-file FILE)              ; IO: file → form summary
(dep-graph-scan-root ROOT)              ; IO: root → file-forms alist
```

The pure core takes hand-built input in tests — no temp files. The IO
shell is a thin loop over `scan-file` for everything under a root
directory.

`FILE-FORMS` shape (pure-core input):

```elisp
(("lisp/a.el" :provides (a)   :requires (b))
 ("lisp/b.el" :provides (b)   :requires ()))
```

`GRAPH` shape: hash-table from feature-symbol → list of file paths that
require it (reverse adjacency).

### Decision: forms extracted

| Form | Extract as | Why |
|---|---|---|
| `(provide 'feature)` | provides | direct |
| `(provide 'feature ...)` | provides | extra args ignored |
| `(require 'feature)` | requires | direct |
| `(require 'feature FILE)` | requires | second arg ignored for graph |
| `(require 'feature nil t)` | requires | optional `require` still represents intent |
| `(load "path")` | requires (basename of path) | parity with bash |
| `(use-package NAME ... :after DEP ...)` | NAME's requires += DEP | parity |
| `(use-package NAME ... :after (a b))` | NAME's requires += a, b | bash version mangled this |

`:after` extraction walks the property list inside the `use-package`
form rather than regex-matching. `(use-package foo)` itself implies a
`(require 'foo)`-equivalent? No — `use-package` doesn't always
`require` the package (only when `:demand` or no autoload). Treat
`use-package NAME` as a `provide`-like declaration that NAME is
present in this file, so dependents can find it. Document this in the
spec.

### Decision: cycle handling = fixed-point traversal

Transitive closure done with a worklist + visited set. Cycles
terminate naturally — once visited, never re-enqueued. No topological
sort, no error.

### Decision: multiple providers = error

If two files both `(provide 'foo)`, the graph has an ambiguous
forward edge. We surface this loudly with an error during `build` so
the user notices, rather than silently picking one. Real-world: should
never happen in a sane repo; if it does, that's a bug to fix in the
source tree, not in the graph.

### Decision: CLI shape

```
scripts/dep-graph affected         [--from-git | <file>...]
scripts/dep-graph affected-tests   [--from-git | <file>...]
scripts/dep-graph print
```

- `--from-git`: read staged + unstaged + (optional ref) changed files
  via the same git logic the bash scripts used. Kept for ergonomics
  and as the path callers use today (`./scripts/affected.sh` with no
  args reads from git).
- Positional file args: take an explicit set, no git involvement.
  Used by tests and by future `pass_filenames=true` hooks.
- `print`: dump the full reverse graph for debugging.

Output: newline-separated paths, or one of the sentinels `all` /
`none`, matching the existing bash contract.

### Decision: bootstrap via `emacs -Q --batch`

CLI wrapper:

```bash
#!/usr/bin/env bash
cd "$(dirname "$0")/.."
exec emacs -Q --batch \
  --eval "(add-to-list 'load-path \"$(pwd)/lisp/dep-graph\")" \
  --eval "(load \"dep-graph\")" \
  --eval "(dep-graph-main)" \
  -- "$@"
```

`dep-graph-main` reads `argv` and dispatches. Sub-second cold-start on
this machine.

### Decision: parity test as a tasks.md step, not a permanent gate

Before deleting `affected.sh` / `affected-tests.sh`, run the new CLI
and the bash versions on a representative staged input and assert
identical (modulo sort order) output. One-shot verification — the bash
scripts get deleted in the same change, so the test can't outlive
them.

## Risks / Trade-offs

- **Reader-based scan is slower than regex** → measure first; if it
  hurts, the follow-up cache change addresses it. For ~hundreds of
  files, expect sub-second.
- **`use-package` semantics are subtle** → the `:after` parity decision
  may not match the bash version exactly (bash was buggy on
  `:after (a b)`). Document the new behaviour in the spec so future
  readers see it's a deliberate fix.
- **CLI's `--from-git` mode duplicates the git-diff dance from the
  bash version** → acceptable interim. The dep-graph change isn't
  about killing the git-diff coupling; that goes when hooks flip to
  `pass_filenames=true`.
- **Build error on duplicate providers** → harsh by default. If we
  hit this in practice, soften to warning. Start strict.
- **Hidden bash bugs become spec changes** → the `:after (foo bar)`
  fix is a behavioural improvement that may expand the affected set
  on some commits. Verify nothing relies on the old broken scope.

## Migration Plan

1. TDD-build the pure core with hand-constructed `FILE-FORMS` inputs.
   Cover direct dependents, transitive, cycles, unknown features,
   multiple providers (error), `:after` (scalar + list), `load` forms,
   optional `require`.
2. Build the IO shell with one fixture-based integration test: a
   handful of temp `.el` files, scan, assert the graph matches.
3. Build the CLI on top of the library. Test `print` output structure.
4. Parity sweep: run both old and new CLI on a curated staged input
   set and the full repo; diff outputs, fix discrepancies.
5. Switch callers (`byte-compile.sh`, `checkdoc.sh`, `run-tests.sh`,
   `Makefile`) to invoke `scripts/dep-graph`.
6. Delete `affected.sh` and `affected-tests.sh`.
7. Verify: `prek run --all-files` green; `make test` / `make
   pre-commit` / `make test-quick` / `make lint-affected` all behave
   identically.

Rollback: `git revert` the change; the bash scripts return from
history; callers see them again. No state outside the repo.
