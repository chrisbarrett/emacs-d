## Why

`lisp/+corelib.el` (~577 lines) is the implicit interface every module
author imports, and it carries weight that isn't core: `+separate` and
`+alist-from-hash-table` have zero callers; the memoised
`+syntax-ppss`/`+point-in-comment-p` pair exists solely for evil's
join-line logic (`modules/evil/init.el:74,81`). Dead and single-client
utilities in the core enlarge the surface a contributor must scan to
know "what the config's stdlib is".

## What Changes

- **Delete dead utilities**: `+separate` and `+alist-from-hash-table`
  (zero non-test callers) are removed.
- **Relocate evil-only machinery**: `+syntax-ppss`,
  `+sppss-memo-reset-h`, and `+point-in-comment-p` move to the evil
  module (its lib), since evil's join logic is their only consumer.
- **No API breaks for real callers**: everything with callers outside
  its own module stays (`+local-leader-set-key` has 8 call sites and is
  explicitly retained; `+chunk-by`, `+tree-map`, `+split-with`,
  `+plist-delete`, `+plist-from-hash-table` are low-use but live).

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `contributor-internals`: gains a corelib-scope requirement — corelib
  carries only utilities shared by two or more modules or serving the
  bootstrap path; single-consumer helpers live with their consumer.

## Impact

- `lisp/+corelib.el` — shrinks; no observable behaviour change.
- `modules/evil/` — gains the syntax-memoisation helpers; join-line
  behaviour unchanged.
- `lisp/+autoloads.el` — regenerated (moved autoloaded defs).
- Byte-compilation and `make test` gate the move (dep-graph picks up
  the affected files).
