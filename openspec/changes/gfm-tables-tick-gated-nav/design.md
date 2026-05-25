## Context

`gfm-pretty-tables.el` stores per-row source positions as **plain integers** on overlay properties — `gfm-pretty-tables-cell-bounds` (a list of `(BEG . END)` per cell, `lisp/gfm/gfm-pretty-tables.el:386`) and `gfm-pretty-tables-display-cell-bounds` (a fixnum offset vector, `lisp/gfm/gfm-pretty-tables.el:733`). Overlay *endpoints* are markers and adjust to buffer edits, but these property values never do. They are correct only between rebuilds.

The engine debounces rebuilds: every modification arms a 0.2 s idle timer (`gfm-pretty-engine.el:680` `gfm-pretty--arm-engine-timer`, threshold defined inline). If the user keeps typing — or if `auto-revert-mode` replaces the buffer contents mid-burst — Emacs never goes idle and the rebuild never fires. Meanwhile every `j`/`k` keypress runs the cell-aware shim (`gfm-pretty-tables.el:2017–2029`), which dereferences stale `cell-bounds` and calls `goto-char` on an old offset. The visible bug is point pinballing inside the original table extent.

The engine installs no `before-revert-hook` / `after-revert-hook` (only `gfm-present.el:361` does, for unrelated narrowing restoration), so revert receives no special treatment — it is just a sequence of `after-change-functions` calls that arm the same idle timer.

The block-discovery cache (`blocks-cache`, keyed by `buffer-chars-modified-tick` at `gfm-pretty-engine.el:387`) does self-invalidate. What is missing is an analogous freshness signal for *overlay state*: a way for nav callers to detect "overlays are stale relative to the current buffer tick" without inspecting every overlay.

## Goals / Non-Goals

**Goals:**

- Nav shims and snap-to-cell never `goto-char` an offset that came from an overlay laid down before the current `buffer-chars-modified-tick`.
- Revert produces a fully-rebuilt overlay set before the next user command, regardless of idle state.
- No change to user-facing key bindings, faces, or rendering.
- Coverage in `gfm-pretty-tests.el` for the two regression vectors (nav-after-revert, nav-after-large-edit-before-idle).

**Non-Goals:**

- Converting `cell-bounds` / `display-cell-bounds` to markers. Cells churn often; the cost of allocating/destroying N markers per row per rebuild is not worth it when a single tick comparison suffices.
- Reworking the engine's debounce delay. The 0.2 s figure is right for steady-state editing; the bug is the *absence* of an escape hatch for "user wants to move now and overlays are stale".
- Touching the link previews or other decorators' nav (links has its own reveal-fn). The freshness contract is a per-decorator capability; other decorators opt in if/when they grow integer-keyed nav state.

## Decisions

### Decision 1: `last-rebuild-tick` slot in `gfm-pretty--state`

Add an engine-owned per-decorator slot `last-rebuild-tick`. `gfm-pretty--rebuild` (`gfm-pretty-engine.el:603`) sets it to `(buffer-chars-modified-tick)` on completion. A helper `gfm-pretty--stale-p (name)` returns non-nil when `last-rebuild-tick` is nil or strictly less than the current buffer tick.

**Alternatives considered:**

- *Inspect every overlay's "laid-on-tick" property.* Forces every `make-anchor` / `make-display` to stamp the tick, and forces every nav call to walk overlays. Higher overhead, no benefit over a single decorator-scoped value.
- *Use `dirty-region` non-nil as the freshness signal.* Sound for the "user typed, idle hasn't fired" case, but `dirty-region` is cleared at the *start* of `--scheduled-rebuild` (`gfm-pretty-engine.el:704`), so a rebuild already in progress (or a synchronous rebuild path that forgets to clear it) leaves false signals on either edge. A monotonic tick comparison is more robust.

### Decision 2: Freshness-gated nav in tables

The `--define-evil-motion` macro (`gfm-pretty-tables.el:1996`) wraps every cell-aware motion. Add a single guard at the top: if `(gfm-pretty--stale-p 'tables)` is non-nil, fall straight through to the underlying evil command — exactly as if point were outside any table.

`--maybe-snap-to-cell` (`gfm-pretty-tables.el:1969`) gets the same guard: when stale, skip the snap entirely. The post-command highlight updater (`--update-cursor-highlight`, `gfm-pretty-tables.el:1696`) becomes a no-op under the stale guard so it does not write `cursor t` text-properties to stale offsets.

**Alternatives considered:**

- *Validate each `cell-bounds` value against the overlay's current `start`/`end`.* Works only when bounds happen to fall inside the overlay's range — bounds that originally pointed at adjacent cells could still match. Not reliable.
- *Always run a synchronous rebuild before nav when stale.* Couples motion latency to rebuild cost. Tables can be large; cost is unpredictable. Bail-out + scheduled rebuild is cheaper and visibly identical.

### Decision 3: Revert hooks in the engine

Install in `gfm-pretty--install-engine-hooks` (`gfm-pretty-engine.el:777`), remove in the corresponding remove fn:

- `before-revert-hook`: for every enabled decorator, call `gfm-pretty--remove-overlays` (full clear via the registry) and nil `dirty-region`, `last-window-state`, `hidden-ovs`, `anchors-laid`, `last-rebuild-tick`. This makes the buffer overlay-clean *before* `insert-file-contents REPLACE=t` mutates the contents, so no stale overlay can survive into the post-revert window.
- `after-revert-hook`: cancel the pending engine timer and call `gfm-pretty--scheduled-rebuild` synchronously. Every enabled decorator's `dirty-region` is nil at this point, so the scheduler routes through `gfm-pretty--rebuild` (full path) per decorator.

The revert path is buffer-local and only fires for buffers where `gfm-pretty-mode` is on; impact on other modes is nil.

**Alternatives considered:**

- *Rely on `after-change-functions` firing for the diff hunks.* That's the status quo, and the bug exists because of it. The user-keys-vs-idle race is intrinsic to the debouncer.
- *Hook `revert-buffer-restore-functions` (Emacs 29+).* That hook is for restoring local state, not invalidating it; semantically wrong fit.

### Decision 4: Test surface

Two new tests in `gfm-pretty-tests.el`:

1. **nav-after-revert**: open a buffer with a 3-row table at top, enable mode, wait for initial rebuild, replace file contents on disk so the table moves down by N lines, call `revert-buffer t t`. Assert: `gfm-pretty--state-get 'tables 'overlays` reflects the new table positions; `gfm-pretty-tables-row-down` from inside the new table moves to the next row's first cell.
2. **nav-after-burst-edit**: enable mode, wait for rebuild, programmatically run `after-change-functions` for a large region without giving Emacs idle time, then call `gfm-pretty-tables--evil-j`. Assert: behaviour matches plain `evil-next-line` (no snap to stale offset).

## Risks / Trade-offs

- **[Risk] Freshness guard hides genuine cell-aware nav for one keypress after every edit.** → Mitigation: 0.2 s idle rebuild covers steady-state editing; the guard only diverts when overlays are actually stale. The fall-through path is plain evil motion, which is what the user gets anyway when point is outside a table. Worst case is one keypress of "non-cell-aware" motion immediately after a burst edit — strictly better than the current "snap to a stale buffer offset".
- **[Risk] Synchronous rebuild on `after-revert-hook` can stall the UI for very large buffers.** → Mitigation: the engine's `:rebuild-fn` already runs without user input during `revert-buffer`; the current debounce just defers it to the next idle, which is when the user *would* have noticed the cost anyway. Moving it to after-revert merely makes the cost predictable and pre-emptive instead of arriving mid-keystroke.
- **[Trade-off] `last-rebuild-tick` is per-decorator, not engine-wide.** Decorators that never grow integer-keyed nav state (callouts, blockquotes, fences, hrule, borders) pay the cost of writing one extra plist entry per rebuild and never read it. Negligible.
- **[Risk] `before-revert-hook` runs full teardown even when the revert turns out to leave the buffer logically unchanged.** → Mitigation: the after-revert hook reapplies; the net effect is one full rebuild per revert, which is exactly what the engine would have done anyway under the existing timer once an idle window opened.
