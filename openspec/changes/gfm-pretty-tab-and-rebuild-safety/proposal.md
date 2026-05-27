## Why

`markdown-cycle` (`TAB`) at BOL of a callout / fence / other non-heading
block silently falls through to `indent-for-tab-command`, inserts leading
whitespace, destroys the block's source signature, and leaves the
decorator's overlays orphaned. The engine's scoped rebuild
(`gfm-pretty--rebuild-scoped-by-block`) returns `nil` when no current
block contains the dirty region — so the stale overlays are never
cleaned up, producing visible decoration corruption.

## What Changes

- Engine: `gfm-pretty--rebuild-scoped-by-block` SHALL escalate to a
  full decorator rebuild when the dirty region overlaps no current
  block (i.e. the edit destroyed the block whose overlays we own).
  Currently this branch is a no-op.
- `gfm-pretty-mode` SHALL install a buffer-local `TAB` binding (a
  thin wrapper) that:
  - Delegates to `markdown-cycle`'s heading-visibility branch on
    headings.
  - Delegates to `markdown-table-forward-cell` in tables.
  - Permits list-item indent only when point is between BOL and the
    start of item content (or EOL) AND `evil-insert-state-p` returns
    non-nil. Applies to bullets (`-` `*` `+`) and ordered markers
    (`1.` `1)`).
  - Is a no-op everywhere else (no silent indent fallback).

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: adds an engine invariant that source mutations
  destroying a tracked block trigger a full rebuild, and adds the
  `TAB` wrapper as part of the umbrella mode's keymap.

## Impact

- `lisp/gfm/gfm-pretty-engine.el`: one branch in
  `gfm-pretty--rebuild-scoped-by-block`.
- `lisp/gfm/gfm-pretty.el`: new `defvar-keymap gfm-pretty-mode-map`,
  wired into `define-minor-mode gfm-pretty-mode` via `:keymap`; new
  command implementing the TAB dispatch.
- `lisp/gfm/gfm-pretty-tests.el`: new ERT tests covering the engine
  null-match path and the TAB wrapper's per-context dispatch.
- No effect on consumers that disable `gfm-pretty-mode`.
