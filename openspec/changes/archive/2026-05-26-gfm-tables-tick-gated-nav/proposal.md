## Why

The tables decorator's cell-aware motion shims (`j`, `k`, TAB, snap-to-cell, …) read source positions from the `gfm-pretty-tables-cell-bounds` overlay property — a list of **plain integers**, not markers. After any buffer modification, those offsets are stale until the engine's 0.2 s idle timer fires a rebuild. If the user mashes keys before Emacs goes idle (or `auto-revert-mode` replaces the file's contents while the user is typing), every `j`/`k` reads stale offsets and `goto-char`s to an old position. The visible symptom is point bouncing inside a narrow extent — "wrapping scroll" — even though the table has moved or been deleted. There is also no `before-revert-hook` / `after-revert-hook` handler in the engine, so revert never gets an immediate rebuild.

## What Changes

- Tables motion shims and `--maybe-snap-to-cell` SHALL refuse to trust overlay-derived nav data while the decorator's `dirty-region` is non-nil (rebuild pending), falling through to the underlying evil command instead.
- The engine SHALL track a `last-rebuild-tick` per decorator (set on every rebuild to `buffer-chars-modified-tick`) so dependents can detect "overlays out of date with buffer".
- The engine SHALL install `before-revert-hook` + `after-revert-hook` handlers in every gfm-pretty buffer: before-revert tears down all decorator overlays and nils their position-bearing state; after-revert force-runs the scheduled rebuild synchronously rather than waiting for idle.
- No change to the public motion API (`gfm-pretty-tables-row-up`, `-down`, `cell-tab`, …); they keep returning nil at table edges, but become no-ops when overlays are stale so the shim's fall-through path takes over.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: tables motion + engine lifecycle gain a freshness contract — nav shims must not act on overlay state older than the current `buffer-chars-modified-tick`, and revert must produce a fully-rebuilt overlay set before the next user command.

## Impact

- `lisp/gfm/gfm-pretty-engine.el`: new `last-rebuild-tick` slot in `gfm-pretty--state`; `before-revert-hook` / `after-revert-hook` handlers installed by `gfm-pretty--install-engine-hooks` and removed by `gfm-pretty--remove-engine-hooks`; `gfm-pretty--rebuild` stamps the tick on completion.
- `lisp/gfm/gfm-pretty-tables.el`: motion shim macro (`--define-evil-motion`) and `--maybe-snap-to-cell` gain a freshness guard reading the engine's tick; nav helpers (`row-on-relative-line`, `cell-info-at-point`) become defensive against stale `cell-bounds`.
- Tests: `gfm-pretty-tests.el` gains coverage for nav-after-revert and nav-after-large-edit-before-idle scenarios.
- No change to user-facing key bindings, faces, or rendering.
