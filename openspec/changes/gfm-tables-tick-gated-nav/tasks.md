## 1. Engine freshness tick

- [x] 1.1 Document the `last-rebuild-tick` slot in the `gfm-pretty--state` docstring at `lisp/gfm/gfm-pretty-engine.el`.
- [x] 1.2 Stamp `last-rebuild-tick` to `(buffer-chars-modified-tick)` at the end of `gfm-pretty--rebuild` (after `last-window-state` is set).
- [x] 1.3 Clear `last-rebuild-tick` to nil in `gfm-pretty--disable-decorator` alongside the other state-slot resets.
- [x] 1.4 Add `gfm-pretty--stale-p (NAME)` predicate that returns non-nil when the slot is nil or `<` the current buffer tick.

## 2. Engine revert hooks

- [x] 2.1 Add `gfm-pretty--before-revert` handler: for each enabled decorator, call `gfm-pretty--remove-overlays` (registry-wide), nil `dirty-region` / `last-window-state` / `hidden-ovs` / `anchors-laid` / `last-rebuild-tick`; cancel `gfm-pretty--rebuild-timer`.
- [x] 2.2 Add `gfm-pretty--after-revert` handler: synchronously call `gfm-pretty--scheduled-rebuild` so every enabled decorator rebuilds before control returns.
- [x] 2.3 Wire both handlers buffer-locally in `gfm-pretty--install-engine-hooks`; mirror the removals in `gfm-pretty--remove-engine-hooks`.

## 3. Tables freshness gating

- [x] 3.1 Gate `gfm-pretty-tables--maybe-snap-to-cell` on `(not (gfm-pretty--stale-p 'tables))` — bail before reading any overlay's `cell-bounds` when stale.
- [x] 3.2 Extend the `gfm-pretty-tables--define-evil-motion` macro so the generated wrapper consults `gfm-pretty--stale-p` first; when stale, fall through to the evil command (and apply snap only when `fall-through` is `snap` AND the post-call state is fresh — i.e. skip snap on stale).
- [x] 3.3 Gate `gfm-pretty-tables--update-cursor-highlight` on freshness: when stale, hide any existing cell highlight and return without reading `cell-bounds`.
- [x] 3.4 Audit other tables nav helpers that dereference `cell-bounds` / `display-cell-bounds` (`--cell-info-at-point`, `--row-on-relative-line`, `--goto-cell`, `--cell-content-bounds`, `--apply-cell-highlight`) — none should be called from a context that bypasses the freshness gate; add a defensive `(gfm-pretty--stale-p 'tables)` check at any entry point that survives the audit.

## 4. Tests

- [x] 4.1 Add `gfm-pretty-tables/nav-after-revert` test in `lisp/gfm/gfm-pretty-tests.el`: open a temp file with a 3-row table, enable mode, force initial rebuild, rewrite the file on disk so the table moves N lines down, call `revert-buffer t t`, assert the new `cell-bounds` reflect the shifted positions and `gfm-pretty-tables-row-down` from inside the new table lands in the next row's first cell.
- [x] 4.2 Add `gfm-pretty-tables/nav-after-burst-edit-is-stale-safe` test: enable mode, wait for rebuild, run `(insert "stuff")` programmatically so the tick advances but the engine timer has not yet fired, call `gfm-pretty-tables--evil-j`, assert behaviour matches `evil-next-line` and that no cached integer offset was used (e.g. by comparing point to where `evil-next-line` lands from the same start).
- [x] 4.3 Add `gfm-pretty-engine/stale-p` unit tests covering set-on-rebuild, nil-on-disable, advance-on-edit transitions.

## 5. Verification

- [x] 5.1 Run `make test-quick` and address any regressions.
- [x] 5.2 Run `make test` (full suite, including narrowing regression tag) inside `nix develop` so `TREESIT_EXTRA_LOAD_PATH` is set for tree-sitter-dependent tests.
- [ ] 5.3 Manually reproduce the original symptom: open a markdown file with a table near top, enable `gfm-pretty-mode`, externally rewrite the file so the table shifts, observe `j`/`k` motion behaves correctly without pinballing.
