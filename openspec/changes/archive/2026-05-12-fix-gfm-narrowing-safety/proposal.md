## Why

The three GFM decorator modes (`gfm-tables-mode`,
`gfm-code-fences-mode`, `gfm-callouts-mode`) all scan the buffer
with `(goto-char (point-min))` and tear down overlays with
`(remove-overlays (point-min) (point-max) …)`.  Both use the
**narrowed** `point-min`/`point-max` when the buffer is narrowed.
None of the three widens inside `save-restriction` for either
operation, and the block-discovery cache key
(`buffer-chars-modified-tick`) is not affected by narrowing — so a
widened cache survives a narrowing event and a narrowed cache
survives a widening event.

This is normally invisible — narrowing is rare in markdown buffers
— but `+presentation-mode` makes it the steady state: every slide
is a narrowing.  Reproducible failures observed in an isolated
sandbox (`emacs -Q --bg-daemon=claude-sandbox`):

1. **Hard crash on narrowed rebuild with a widened-cache** —
   `gfm-tables`.  A widened initial rebuild caches table positions
   spanning the whole doc.  Narrowing to slide 1 and triggering any
   rebuild (e.g. via a width-affecting WCC) reuses the cache.
   `--apply-table` then reaches a table whose header is outside
   the narrowing and signals `(args-out-of-range BUFFER OUT-POS
   POINT-MAX)`.  Reproduced:

   ```
   (cache-before ((12 41) (54 83)))
   (narrow 1..43)
   gfm-tables--rebuild
   → (args-out-of-range #<buffer …> 54 43)
   ```

2. **Silent overlay duplication on the same path** —
   `gfm-code-fences`.  Same cache-survives-narrowing path, but
   `make-overlay` accepts the out-of-narrowing positions without
   error.  The narrowed `--remove-overlays` only clears overlays
   inside the narrowing; the tracking list is reset to nil; then
   `--apply-overlays` creates *fresh* overlays for both in- and
   out-of-narrowing blocks.  The pre-narrow off-slide overlays
   remain on the buffer as untracked zombies.  Reproduced:
   widened rebuild leaves 10 tagged overlays; one narrowed rebuild
   later, the buffer carries 13 tagged overlays and the tracking
   list holds 9 of them — 4 zombies remain.

3. **Registry orphaning on full-clear under narrowing** — root
   cause of (2).  `gfm-block-borders--remove-overlays REGISTRY`
   (no `BEG`/`END`) deletes overlays inside the narrowing and
   resets the registry's list to nil unconditionally.  Surviving
   off-narrowing overlays are now off-registry: future
   `--prune-dead-overlays`, per-window cleanup, and
   `mode -1` teardown all miss them.

These failures cascade into `+presentation-mode` because the mode's
documented event flow guarantees the harmful sequence: gfm modes
initialise widened (from `gfm-mode-hook`), then `+presentation-mode
1` narrows.  Any subsequent edit or WCC inside the slide will hit
path (1) or (2).

Out-of-scope for this change but worth flagging in the same fix:
the same scan pattern means a code fence whose opening `` ``` ``
sits outside the slide's narrowing is invisible to the scanner —
the closing fence inside the slide looks like an open with no
close, and the scanner may bind to an unrelated later fence.  A
widened scan also fixes that.

## What Changes

- **Scanners widen during discovery.**
  `gfm-tables--find-blocks-1`, `gfm-code-fences--find-blocks-1`,
  `gfm-code-fences--find-yaml-helmet-1`,
  `gfm-code-fences--find-indent-blocks-1`, and
  `gfm-callouts--find-blocks-1` wrap their body in
  `save-restriction` + `widen`.  Cached block lists become
  narrowing-independent.  The cache key (chars-modified-tick) is
  unchanged.

- **Full-buffer overlay teardown widens.**
  `gfm-block-borders--remove-overlays REGISTRY nil nil` (the
  "clear everything" call site used by `--rebuild` and `mode -1`)
  widens before calling `remove-overlays` so the registry list and
  the on-buffer overlay set stay in lockstep.  Scoped calls (with
  explicit `BEG`/`END`) keep their literal behaviour — callers
  pass widened positions already.

- **`gfm-tables--remove-overlays` adopts the same widen.**
  Currently a standalone implementation; merge it onto the shared
  `gfm-block-borders--remove-overlays` so all three modes share the
  fix.

- **No public behaviour change when the buffer is not narrowed.**
  The fix is invisible outside `+presentation-mode` and any other
  consumer that narrows a markdown buffer (e.g.
  `markdown-narrow-to-subtree`, `org-narrow-to-subtree`-style
  workflows).  Cache contents, overlay layout, performance budget,
  and per-window rendering all preserve current widened behaviour
  bit-for-bit.

Out of scope:

- Changing the cache key.  The existing
  `buffer-chars-modified-tick` key is correct once scans are
  narrowing-independent.
- Triggering rebuilds on `narrow-to-region` / `widen`.  After this
  fix neither event invalidates the rendered state; no rebuild is
  required.
- Changes to `+presentation-mode`.  The mode is already correct;
  its hooks just expose latent gfm bugs.
- Visual treatment of partially-narrowed blocks (e.g. a fence
  whose opener sits outside the narrowing).  Scanners will now
  *see* the full block; whether the partial rendering inside the
  narrowing looks polished is a separate spec discussion.

## Capabilities

### New Capabilities

<!-- None -->

### Modified Capabilities

- `gfm-table-rendering`: adds a "Narrowing-resilient discovery"
  requirement; clarifies the existing "Block-discovery cache" and
  "Mode toggle" requirements to assert narrowing-independence of
  the cache and of the teardown path.

- `gfm-code-fence-rendering`: same additions, covering fenced,
  YAML helmet, and indented block discovery.

- `gfm-callout-rendering`: same additions, covering callout block
  discovery.

## Impact

- Modified files:
  - `modules/lang-markdown/lib/+gfm-block-borders.el` — wrap the
    full-clear branch of `--remove-overlays` in `save-restriction`
    + `widen`.
  - `modules/lang-markdown/lib/+gfm-tables.el` — wrap scanner
    body in `save-restriction` + `widen`; route the
    `--remove-overlays` full-clear through the shared lib (or
    apply the same fix locally if keeping the standalone).
  - `modules/lang-markdown/lib/+gfm-code-fences.el` — wrap each
    of three scanner bodies in `save-restriction` + `widen`.
  - `modules/lang-markdown/lib/+gfm-callouts.el` — wrap scanner
    body in `save-restriction` + `widen`.
  - `modules/lang-markdown/tests.el` (or a sibling) — three new
    regression tests, one per mode, asserting that
    `mode-rebuild` under a narrowing that excludes some blocks
    (a) does not signal, (b) leaves no untracked tagged overlays,
    (c) renders the same overlay set after `widen` as a freshly
    rebuilt widened buffer.
- No spec file deletions; spec additions per capability listed
  above.
- No external package or API changes.  No new dependencies.
- Performance: scanners run on identical regions to the widened
  case; the only added cost is a single `save-restriction` /
  `widen` per rebuild and per cache miss.  Negligible.
