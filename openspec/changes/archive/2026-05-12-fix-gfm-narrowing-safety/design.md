## Context

The three GFM decorator modes share an architecture the
`gfm-callouts-per-window-rendering` change locked in:

- Per-mode `find-blocks-1` scanners discover blocks by walking
  the buffer from `(point-min)`, returning lists of source-range
  records.
- A buffer-local cache keyed on `buffer-chars-modified-tick`
  memoises the scan so non-edit operations (cell motion, cursor
  reveal, WCC reconciliation) reuse a single scan between edits.
- A buffer-local overlay registry (anchor + display overlays)
  tracked in `gfm-{tag}--overlays` is torn down by
  `gfm-block-borders--remove-overlays REGISTRY [BEG END]`.  No-arg
  form removes overlays over `(point-min)…(point-max)` and resets
  the tracking list to nil.

None of these primitives widens.  `buffer-chars-modified-tick` is
unaffected by `narrow-to-region` / `widen`.  In a non-narrowing
workflow these choices are correct: scans see everything, the
tracking list mirrors the buffer, and the cache is invalidated
only by real edits.

`+presentation-mode` makes the cache and the teardown narrowing-
dependent for the lifetime of a slide.  The mode itself is well-
behaved — `widen` + `narrow-to-region` on slide navigation, no
manual cache or overlay manipulation — but every transition
exposes a path the gfm modules don't handle:

```
gfm-mode-hook  ─→  initial widened rebuild
                   cache := full-doc blocks
                   tracking := full-doc overlays
+presentation-mode 1
                   narrow-to-region (point-min..H1-of-slide-2)
                   tick UNCHANGED → cache survives
edit / WCC inside slide
                   schedule-rebuild → --rebuild
                       --remove-overlays nil nil
                           remove-overlays (point-min)(point-max)  ← narrowed!
                           overlays outside narrowing: SURVIVE
                           tracking := nil
                       --apply-overlays
                           --find-blocks → cache hit, full-doc blocks
                           dolist b in blocks:
                               --apply-block b   ← may touch out-of-narrow pos
```

For `gfm-tables`, the application path calls `buffer-substring-no-
properties` / `goto-char` on the cached header position; out-of-
narrowing positions signal `args-out-of-range`.  For
`gfm-code-fences` and `gfm-callouts`, `make-overlay` accepts the
out-of-narrowing positions silently — overlays are created but the
pre-narrow ones for the same source positions remain on the
buffer as zombies (the tracking list having been reset).

After `widen` the zombies become visible.  They display correctly
*until* the next full rebuild applies a fresh widened scan and the
duplicate overlays start stacking visually (same `display`
property, overlapping `before-string` / `after-string`).

## Goals / Non-Goals

**Goals:**

- Eliminate the hard error on `gfm-tables--rebuild` under
  narrowing with widened cache.
- Eliminate the zombie/duplicate overlay path under narrowing for
  all three modes.
- Keep the cache and tracking list narrowing-independent so a
  buffer's gfm rendering state is identical whether the buffer is
  currently narrowed or widened.
- Preserve all current behaviour for non-narrowed buffers (zero
  visual change, zero perf regression).

**Non-Goals:**

- Wiring `narrow-to-region` / `widen` as rebuild triggers.  With
  scanners widened the only thing that changes on narrow is the
  *visible* window range, which the per-window reconciler already
  handles via `window-configuration-change-hook`.
- Coordinating with `+presentation-mode`.  The presentation
  module never needs to know gfm is loaded.
- Touching the cache key or the post-edit `dirty-region`
  bookkeeping.

## Decisions

### Where to widen

Two surface choices: widen at the *scanner* level (inside
`find-blocks-1`) or at the *rebuild* level (inside `--rebuild` /
`--rebuild-prioritised`).

Chose **scanner level**.  The cache stores scan output, so the
cache contract is "blocks in this buffer".  Making it "blocks
visible in the current restriction" would require either
invalidating on every narrowing change (which `buffer-chars-
modified-tick` cannot detect) or keying the cache on
`(tick narrowing-bounds)`.  Both worsen the contract.  Widening at
the scanner makes the cache a pure function of buffer contents,
which is what every existing caller already assumes.

Rebuild-level widening would also need to widen for the per-block
application path, which is called from multiple sites (scoped
rebuild, window reconcile, dirty-region rebuild).  The scanner
fix is one place; the rebuild fix would be many.

### Teardown widen

The full-clear branch of `gfm-block-borders--remove-overlays`
(both `BEG` and `END` nil) is the only call site that needs to
match "the tracking list says everything is gone, the buffer's
overlay state must match".  Scoped calls
(`--remove-overlays REGISTRY BEG END`) come from per-block
teardown and per-window cleanup where the caller has computed
real buffer positions — they must NOT widen, or they'd delete
unrelated overlays.

The change is one branch.  The widened `remove-overlays` call
operates on a single buffer and is O(n) in overlay count — no
worse than the current narrowed call.

### `gfm-tables--remove-overlays` consolidation

`gfm-tables` predates the shared `+gfm-block-borders.el` and has
its own `--remove-overlays` body.  This change replaces it with a
call into `gfm-block-borders--remove-overlays gfm-tables--registry
…`, in line with the existing fences/callouts wiring.  Removes
the duplicate, brings the fix automatically.

If consolidation churns more than necessary in this change, the
fallback is a local `save-restriction` + `widen` in
`gfm-tables--remove-overlays`.  Same correctness; lose the
de-duplication.  Preference is the consolidation.

## Verification

A repro test per mode, modelled on the sandbox repro:

1. Create a temp buffer with two slides, each containing one
   block kind (table / fence / callout).
2. Enable the mode under `gfm-mode`.  Snapshot:
   - cache (block positions),
   - registry tracking list length,
   - total tagged overlays on buffer.
3. `narrow-to-region` to slide 1.
4. Call `<mode>--rebuild`.
5. Assert: no `args-out-of-range` signal.
6. `widen`.  Assert: total tagged overlays on buffer ==
   tracking list length.  No zombie tally.
7. Re-rebuild widened.  Assert: same total as step 2.

The cross-slide partial-block case is covered by an additional
test per mode: place an opening fence/header before the slide
and assert that the (post-widen) overlay set is identical to a
freshly rebuilt widened scan — i.e. the narrowed rebuild did not
mutate state.

## Risks

- **Hidden caller depending on narrowed cache.**  No call site
  observed in the codebase relies on narrowing-scoped block
  lists.  Tests will catch any that surface.
- **Performance regression on huge buffers** if widening exposes
  many more blocks than the slide.  In practice presentations
  are short and the scan was already O(buffer-size) — widening
  just restores the cost that was already happening on widened
  rebuilds before narrowing.
- **Partial-block visuals.**  After the fix, a fence opening at
  the H1 of slide 2 will have *both* its open and close in cache.
  Under narrowing to slide 1, only the close (if any inside
  slide 1) is in the narrowing — but the scanner now sees a
  complete block.  Rendering attempts the full block; only the
  in-narrowing portion is visible.  This matches Emacs's normal
  overlay-under-narrowing behaviour and matches what users
  already see for tables and callouts in mid-slide.
