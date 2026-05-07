## Context

`+gfm-code-fences.el` decorates GFM fenced code blocks, leading YAML
frontmatter, and 4-space-indented code blocks with a curved-border box,
language icons, and reveal-on-cursor for fence markers.  The current
implementation is single-window, full-rebuild on every event, and has a
dormant elisp infinite-loop in `--simulate-wrap` that bites when window
width drops below ~3 columns during a split transient.

`+gfm-tables.el` (the sibling module) recently went through the same
maturation: per-window display overlays, anchor-vs-display split,
discovery cache, scoped rebuilds, window-state diff, visible-first
prioritisation, and per-buffer performance instrumentation.  The
patterns are documented in `openspec/specs/gfm-table-rendering/spec.md`
and battle-tested.  This change ports those patterns to fences while
keeping fences' distinct rendering model (Emacs-native word wrap with
`wrap-prefix`, rather than composed display strings per row).

Cross-module: `gfm-tables` calls `gfm-code-fences--find-blocks` for
fenced-range exclusion during table discovery.  The new chars-tick cache
on the fenced discovery function benefits both modules without an API
change.

## Goals / Non-Goals

**Goals:**

- Capture existing behaviour as a `gfm-code-fence-rendering` capability spec so future regressions are visible.
- Eliminate the split hang.  Joint fix: defensive guard in `--simulate-wrap` plus per-window rendering to avoid the redisplay storm.
- Reach quality parity with `gfm-tables-mode` on perf and multi-window resizing: per-window overlays, scoped rebuilds, window-state diff, visible-first prioritisation, perf stats with slow-rebuild warning.
- Mirror function shapes (`--visible-window-ranges`, `--block-visible-p`, `--reconcile-windows`) with `gfm-tables` so a future shared scanner extraction is mechanical.

**Non-Goals:**

- A unified viewport scanner shared across modules.  Defer until a third decorated element type lands.
- Switching fences to a composed-display-string rendering model (Path B from grilling).  Keeps Emacs-native wrap; selection / yank semantics unchanged.
- An edit-indirect entry point for fenced blocks (open code in language major mode).  Out of scope for this change.
- Touching `gfm-tables`.  No refactor or extraction in tables; fences match tables' shapes by writing fences's helpers to the same names and signatures.

## Decisions

### D1. Per-window rendering: hybrid anchor / display split (Path C)

**Decision:** Each decorated block produces:
- One anchor overlay per body line carrying width-independent props: `before-string '│ '`, `wrap-prefix '⋱ '`, `cursor-intangible` (indent blocks).  Shared across all windows.
- Per-window display overlays carrying width-dependent props: top/bottom border splits and right-edge `:align-to` after-strings.  Restricted to a single window via the `window` overlay property.

**Alternatives considered:**

- **Path A** (every overlay restricted per window): N copies of every overlay; overlay count balloons linearly with window count.  Reveal logic has to find the right window's overlay.
- **Path B** (composed display string per row, mirroring tables exactly): full rewrite of rendering.  Loses Emacs-native word wrap.  Selection of body text becomes display-string copying, which subtly changes yank behaviour.

**Why C:** width-independent props (`│ `, `⋱ `, intangibility) genuinely don't change between windows, so duplicating them is waste.  Borders and right-alignment are the only width-dependent pieces — restricting *those* per window is exactly the right grain.  Native word wrap stays.

### D2. Discovery cache: three separate, chars-tick-keyed

**Decision:** Each of `--find-blocks`, `--find-yaml-helmet`, `--find-indent-blocks` gets its own cache keyed on `buffer-chars-modified-tick`.  Indent discovery still takes its excluded-fenced-ranges parameter; that's call-site data, not cache key.

**Alternatives considered:**

- One unified cache returning a `(fenced . yaml . indent)` tuple: forces callers needing one to pay for the others, including `gfm-tables` which only wants fenced exclusion.

**Why separate:** clean call-site boundaries.  `gfm-tables`'s use of the fenced cache is a free win.  No coupling between the three discovery domains.

### D3. Scoped rebuild: full-rebuild fallback on three triggers

**Decision:** When the dirty region is fully contained in exactly one decorated block, rebuild that block only.  Fall back to a full rebuild when:

- The region overlaps a fence opening or closing line (could create or destroy a block).
- The region overlaps a blank line adjacent to an indent block (blank lines gate indent-block discovery; an edit could synthesise or dissolve a block).
- The region spans more than one block.

**Alternatives considered:**

- Always full rebuild (current behaviour).
- Always scoped rebuild with no fallback: misses block-creation / block-destruction cases.

**Why this:** mirrors `gfm-tables--rebuild-scoped` with one fence-specific addition (the blank-line guard for indent-block fragility).

### D4. Window-state reconciliation: per-window diff

**Decision:** On `window-configuration-change-hook`, compare the snapshot of `(window . max-chars-per-line)` pairs against the one cached at the last rebuild.  Skip if equal.  Otherwise:

- Removed windows: delete their display overlays only.
- Added windows: produce display overlays via prioritised rebuild.
- Resized windows: replace display overlays via prioritised rebuild; anchor overlays untouched.
- Untouched windows: existing display overlays survive `eq` comparison.

**Why:** a window-config event covers many causes (minibuffer activity, focus, scroll-margin) that don't touch rendering inputs.  Diffing skips those completely.  Mirrors `gfm-tables--reconcile-windows`.

### D5. Visible-first prioritisation at block granularity

**Decision:** On a width-affecting change, render blocks whose source range intersects any visible window range synchronously; defer the rest to an idle timer.

**Alternatives considered:**

- Visible-line-only (render only the visible body lines of a partially-visible block synchronously, defer the rest of the block): leaves overlay state mid-block (top border but no bottom), which is fragile and complicates reveal.

**Why block-level:** matches gfm-tables.  Big blocks are rare; the deferred-idle fallback cushions multi-block buffers.  Promote to line-level only if profiling shows otherwise.

### D6. Hang fix: defensive guard plus per-window rendering

**Decision:** Two fixes that compound:

- Inside `--simulate-wrap`, clamp `line-width` to `≥ 1` so `space-left` is always positive, ensuring `pos` advances each iteration.  The function always terminates.
- Per-window rendering eliminates the redisplay storm where overlays sized for one window's width are rendered at another's, forcing the redisplay engine to recompute alignment per frame.

**Verification task:** reproduce the hang on a representative buffer, confirm guard alone is insufficient, confirm per-window rendering eliminates the residual storm.  Documents whether both halves of the joint fix were needed.

**Verification result (2026-05-07):** Reproduced on a 364-line, 13.5KB markdown
buffer with 20 fenced blocks (longest line 166 chars) in a 107-col tty frame.
`C-x 3` halves the window to ~53 cols, forcing every body line through the
overflow path.  CPU sat at ~10% (not the 100% one-core peg of a tight elisp
loop) and `toggle-debug-on-quit` + `C-g` caught the engine inside
`gfm-code-fences--apply-bordered-block` → `--right-after` → `propertize` —
i.e. mid-rebuild, not stuck spinning in `--simulate-wrap`.

**Conclusion (revised 2026-05-08):** Bisected by stripping rendering back
to a minimal baseline and re-adding features one at a time.  All steps
worked — including per-window restriction, top/bottom borders, overflow
path, and `cursor-intangible-mode` — until the rebuild scheduler was
wired to `window-configuration-change-hook`.  Adding a runaway-detector
to the body-line loop revealed the actual fault: `(forward-line 1)`,
called inside the body-overlay creation loop, was failing to advance
the point past a specific buffer position (a 4-space-indented body line
at position 749 in the repro buffer).  The loop spun in place,
re-creating overlays at the same position, and the consequent redisplay
churn produced the hang.  In a static test from the same position
`forward-line` advanced normally — the failure was specific to the
overlay-creation context (almost certainly an interaction with
`cursor-intangible-mode` and our overlays' `cursor`/`display`
text properties, which Emacs's motion code consults).

**Real fix:** body-overlay loops now iterate via explicit text-position
math (`(setq p (1+ (line-end-position)))`) instead of `forward-line`.
Three loops were affected:

- `gfm-code-fences--apply-bordered-display` body-line loop
- `gfm-code-fences--apply-indent-anchors` body-line loop
- `gfm-code-fences--apply-indent-display` body-line loop

The `--simulate-wrap` clamp and per-window rendering remain in the
change.  Both are correct and useful — the clamp is genuine defensive
coding for the dormant infinite-loop class, and per-window rendering
fixes the multi-window-different-width correctness concern.  Neither
caused the hang.  The hang was a `forward-line` interaction that no
amount of caching, scoped rebuild, or window-state diffing could have
addressed; only the loop-iteration mechanism mattered.

A bisect-CSI profile of a window rotate after the fix confirmed
`gfm-code-fences--rebuild` runs at ≪1% of timer-event-handler cost —
the fence work is no longer measurable next to the surrounding ecosystem
(which-key, doom-modeline, redisplay).

### D7. Performance instrumentation: mirror tables, with adapted phase keys

**Decision:** Add `gfm-code-fences-stats` command, per-buffer rebuild count / total / last / max time, and per-phase totals.  Phase keys named for fences' work shape:

- `find-fenced` — `--find-blocks`
- `find-yaml` — `--find-yaml-helmet`
- `find-indent` — `--find-indent-blocks`
- `compose-borders` — top / bottom border string composition
- `compose-overflow` — `--simulate-wrap` and `--right-after-overflow`
- `apply` — overlay creation

Slow-rebuild warning gated by `gfm-code-fences-slow-rebuild-threshold` (default 0.05 s) emits a `message` line identifying buffer and duration.

### D8. Reveal-on-entry: keep per-marker reveal

**Decision:** Marker lines (top + bottom of fenced and YAML blocks) stay revealable on cursor entry.  Indent blocks have no marker line, so no reveal.

**Why:** fence markers are *editable structural delimiters* — users need to see them to add a language tag, change backtick count, fix typos.  This is unlike gfm-tables' per-cell highlight, which decorates content rather than syntax.

## Risks / Trade-offs

- **Risk:** Per-window display overlays multiply overlay count by `N` for `N` windows showing the buffer.  → **Mitigation:** anchors are shared, so the multiplier is `2 + body-lines` per block per extra window, not `4 + body-lines`.  In practice users rarely show one buffer in >2 windows simultaneously.

- **Risk:** Scoped rebuild could miss a block-creation / block-destruction scenario we haven't anticipated, leaving overlays inconsistent with source.  → **Mitigation:** conservative full-rebuild fallbacks (fence line, blank line adjacent to indent, multi-block).  Add regression tests for the boundary cases.

- **Risk:** Indent-block blank-line guard fires too often, defeating the scoped-rebuild win for users editing prose adjacent to indented code.  → **Mitigation:** the guard only triggers when the dirty region overlaps a blank line *whose adjacency affects an indent block*, not every blank line.  If perf instrumentation shows the guard dominates, refine.

- **Risk:** The `--simulate-wrap` defensive guard masks a deeper bug elsewhere that produced the tiny `text-width`.  → **Mitigation:** the guard is purely defensive; if `text-width = 0` arrives, the function returns rather than spins.  Add a one-line `message` instrumentation behind a debug flag if the guard ever fires in production.

- **Risk:** YAML body fontification still uses a long-lived hidden buffer keyed on the yaml-mode symbol.  Per-window doesn't change this, but the buffer persists across rebuilds.  → **Mitigation:** out of scope; revisit only if profiling implicates it.

- **Risk:** Per-window display overlays for a buffer not currently shown in any window need a fallback (unrestricted overlay) that gets replaced when a window starts showing the buffer.  → **Mitigation:** mirror gfm-tables' `(or windows '(nil))` pattern; existing tests already cover this path for tables.
