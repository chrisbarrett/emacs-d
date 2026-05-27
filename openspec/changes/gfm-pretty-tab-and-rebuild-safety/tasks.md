## 1. Engine null-match safety

- [ ] 1.1 In `lisp/gfm/gfm-pretty-engine.el`, change the `(null
      matching)` branch of `gfm-pretty--rebuild-scoped-by-block` to
      escalate to a full rebuild (call the decorator's `:rebuild-fn`,
      falling back to generic teardown + reapply when absent) instead
      of returning `nil`.
- [ ] 1.2 Add an ERT test in `lisp/gfm/gfm-pretty-tests.el` reproducing
      the null-match path: enable `gfm-pretty-mode` on a callout, insert
      a leading space at BOL of the marker line, invoke the scheduled
      rebuild synchronously, and assert no overlays tagged with the
      callouts registry remain in the destroyed block's former range.

## 2. TAB wrapper in `gfm-pretty-mode`

- [ ] 2.1 In `lisp/gfm/gfm-pretty.el`, add `defvar-keymap
      gfm-pretty-mode-map` and wire it into the `define-minor-mode
      gfm-pretty-mode` declaration via `:keymap`.
- [ ] 2.2 Implement `gfm-pretty-tab-dwim` per the dispatch table in
      `design.md`: heading → `markdown-cycle` (interactive); table →
      `markdown-table-forward-cell`; list-item prefix slot AND
      `evil-insert-state-p` → `markdown-indent-line` once; otherwise
      no-op.
- [ ] 2.3 Bind `TAB` to `gfm-pretty-tab-dwim` in `gfm-pretty-mode-map`.
- [ ] 2.4 Add ERT tests in `lisp/gfm/gfm-pretty-tests.el` for each
      dispatch case (heading cycles, table forward-cell, list-item
      prefix indents in insert state, list-item prefix is no-op outside
      insert state, callout marker line is no-op, paragraph body is
      no-op). Stub `evil-insert-state-p` per the design.

## 3. Verification

- [ ] 3.1 Run `make test-quick` and confirm new tests pass and no
      existing test regresses.
- [ ] 3.2 Manual smoke test in `gfm-mode`: press `TAB` on a callout
      marker line, a fence line, a heading, inside a table, and on a
      list-item prefix in evil insert state — observe the documented
      behaviour and no leftover decoration corruption.
