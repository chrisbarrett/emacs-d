# Tasks: prune-corelib

## 1. Relocate evil-only helpers

- [ ] 1.1 Re-verify callers: grep whole repo (including docstrings and
      hook registrations) for `+syntax-ppss`, `+sppss-memo-reset-h`,
      `+point-in-comment-p`
- [ ] 1.2 Move the three definitions and the memo-reset hook
      registration from `lisp/+corelib.el` to `modules/evil/lib.el`;
      move/adjust their tests
- [ ] 1.3 Regenerate autoloads; `make test` green; evil join-line
      behaviour spot-checked (J on commented lines)

## 2. Delete dead utilities

- [ ] 2.1 Re-verify zero callers for `+separate` and
      `+alist-from-hash-table`
- [ ] 2.2 Delete both definitions and their tests
- [ ] 2.3 `make test` green; byte-compile clean across affected files

## 3. Close out

- [ ] 3.1 Confirm `+local-leader-set-key` and the surviving low-use
      helpers are untouched (retention is deliberate)
- [ ] 3.2 Full `make test`
