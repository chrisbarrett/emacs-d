# Design: prune-corelib

## Context

`+corelib.el` is loaded by effectively every module and functions as
the config's stdlib; its contents define the vocabulary contributors
must know. Verified caller counts (non-test, outside corelib itself):
`+separate` 0, `+alist-from-hash-table` 0, `+syntax-ppss` family used
only via `+point-in-comment-p` in `modules/evil/init.el`. An earlier
audit claim that `+local-leader-set-key` was unused is wrong — it has
8 call sites and stays.

## Goals / Non-Goals

**Goals**

- Corelib contains only utilities with ≥2 consuming modules or genuine
  bootstrap roles (hook macros, logging, `+read-eld`, dirlocals).
- Evil-only machinery lives with evil.

**Non-Goals**

- Renaming anything that stays.
- A general collections library — the surviving low-use helpers
  (`+chunk-by`, `+tree-map`, `+split-with`, `+plist-delete`,
  `+plist-from-hash-table`) have live callers and don't justify a new
  home until a second pressure appears.
- A dedicated corelib axis — the scope rule lands as one requirement
  under `contributor-internals`, which already covers contributor-facing
  conventions.

## Decisions

### Decision: delete, don't deprecate

This is a private config, not a published library; nothing external
can depend on the removed symbols. Deletion with a green test suite is
sufficient. Corresponding tests for deleted functions are removed in
the same commit.

### Decision: `+point-in-comment-p` and the memo pair move to `modules/evil/lib.el`

They move together (the predicate is the interface, the memoised
`+syntax-ppss` its implementation detail) and keep their names —
autoload scanning makes them available identically, and renaming would
churn evil's init for no depth gain. If a second module later needs
comment-position predicates, that's the moment to promote them back.

## Risks / Trade-offs

- [A caller hides somewhere grep missed (advice strings, symbol
  interning)] → byte-compile the whole config (`make test` includes
  build) and grep including docstrings before deleting; the memo pair
  also has its reset hook registration to move, not just defs.
- [Autoload staleness after the move] → `+autoloads-rebuild` runs via
  the Makefile prerequisite; verify the regenerated `+autoloads.el` no
  longer lists corelib as the source.

## Migration Plan

Two commits: (1) move the evil helpers + hook registration, regenerate
autoloads, tests green; (2) delete dead functions and their tests.
Rollback is a revert.

## Open Questions

- None.
