# contributor-internals Delta

## ADDED Requirements

### Requirement: Corelib carries only shared utilities

`lisp/+corelib.el` SHALL contain only definitions that are consumed by
two or more modules or that serve the bootstrap/module-loading path.
Utilities with a single consuming module SHALL live in that module's
lib; utilities with no callers SHALL be deleted rather than retained.

In particular: `+separate` and `+alist-from-hash-table` SHALL NOT
exist, and the syntax-memoisation helpers (`+syntax-ppss`,
`+sppss-memo-reset-h`, `+point-in-comment-p`) SHALL be defined in the
evil module, their sole consumer.

#### Scenario: dead utilities are gone

- **WHEN** the repository is searched for definitions of `+separate`
  and `+alist-from-hash-table`
- **THEN** no definition exists

#### Scenario: evil-only helpers live with evil

- **WHEN** the definition sites of `+syntax-ppss`,
  `+sppss-memo-reset-h`, and `+point-in-comment-p` are located
- **THEN** each is defined under `modules/evil/`
- **AND** `lisp/+corelib.el` defines none of them
- **AND** evil's join-line comment handling behaves as before the move
