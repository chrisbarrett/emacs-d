## Why

`gfm-code-fences-mode` lags `gfm-tables-mode` on three fronts: it lacks a
spec entirely, it pays full-buffer rebuild cost on every edit and every
window-config event, and it can hang Emacs on a vertical window split
(`C-x 3`) due to an unguarded path in `--simulate-wrap` and a redisplay
storm from overlays sized for the wrong window's width.  The hang is a
real user blocker; the perf gap is a daily papercut on long markdown
docs with many fenced or indented code blocks.

## What Changes

- Capture existing `gfm-code-fences-mode` behaviour as a `gfm-code-fence-rendering` spec (block discovery for fenced / YAML helmet / indented blocks, shared bordered-block rendering, marker reveal, language-icon resolution, lang-mode aliasing, debounced rebuild, indirect-buffer skip).
- Introduce **per-window display overlays** using a hybrid anchor-vs-display split (Path C):
  - Anchor overlays carry width-independent props (`before-string '│ '`, `wrap-prefix '⋱ '`, `cursor-intangible`).
  - Display overlays carry width-dependent props (top/bottom border splits, right-edge `:align-to` after-strings) and are restricted to a single window via the `window` overlay property.
- Add a **chars-tick-keyed discovery cache** for each of the three discovery functions.
- Add **scoped post-edit rebuild**: when the dirty region intersects exactly one decorated block and is fully contained in it, rebuild only that block.  Fall back to a full rebuild when the region spans multiple blocks, overlaps a fence opening/closing line, or overlaps a blank line adjacent to an indent block.
- Add **window-state diff reconciliation**: skip rebuild when no window's `window-max-chars-per-line` changed; on diff, only added or resized windows trigger fresh rendering, removed windows have their display overlays deleted, untouched windows keep their existing overlays (`eq`-stable).
- Add **block-level visible-first prioritised rebuild**: blocks whose source range intersects any visible window range render synchronously; off-screen blocks defer to the next idle tick.
- Add **performance instrumentation** mirroring `gfm-tables-stats`: rebuild count, total / last / max time, table count from most recent rebuild, per-phase totals (`find-fenced`, `find-yaml`, `find-indent`, `compose-borders`, `compose-overflow`, `apply`), and a slow-rebuild warning gated by `gfm-code-fences-slow-rebuild-threshold` (default 0.05 s).
- **Fix split hang**:
  - Defensive guard in `--simulate-wrap`: clamp `line-width ≥ 1` so the loop always advances, eliminating the elisp infinite-loop when `text-width ≤ cont-prefix-w`.
  - Per-window rendering eliminates the redisplay storm caused by overlays sized for one window's width being rendered in another.
  - Verify the joint fix on a representative buffer before declaring the hang closed.

## Capabilities

### New Capabilities
- `gfm-code-fence-rendering`: Visual treatment of GFM fenced code blocks, YAML frontmatter, and indented code blocks — block discovery, bordered-box rendering, marker reveal, language icons, per-window display, debounced and scoped rebuilds, performance instrumentation.

### Modified Capabilities
<!-- None. -->

## Impact

- **Code**: `modules/lang-markdown/lib/+gfm-code-fences.el` (extensive). New file shape mirrors `+gfm-tables.el`'s anchor / display / cache / scheduler primitives.
- **Tests**: `modules/lang-markdown/tests.el` gains regression tests for the simulate-wrap guard, the scoped rebuild, the window-state diff, and the per-window overlay split.
- **Cross-module**: `gfm-tables` already calls `gfm-code-fences--find-blocks` for fenced-range exclusion.  The cache benefits both modules; the discovery function's signature is unchanged.
- **No external API changes**: `gfm-code-fences-mode` toggle and integration with `gfm-mode-hook` stay as-is.
- **Performance**: Window-config events on unchanged-width snapshots become no-ops (today: full rebuild).  Edits inside one block stop rebuilding every other block in the buffer.  The slow-rebuild warning surfaces regressions early.
