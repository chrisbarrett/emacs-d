## 1. Per-rebuild caches

- [x] 1.1 Add `gfm-tables--width-cache` dynamic variable bound to a fresh
      `eq`-keyed hash table at the start of each rebuild.
- [x] 1.2 Update `gfm-tables--visible-width` to consult / populate the
      cache when bound; keep current behaviour when unbound.
- [x] 1.3 Add a fast-path branch in `gfm-tables--visible-width` that
      returns `string-width` directly when the input string carries no
      `display`, `composition`, or `invisible` text properties anywhere.
- [x] 1.4 Add ERT coverage: cached width matches uncached for cells with
      and without `display` / `composition` / `invisible` props; cache is
      not consulted across `eq`-distinct strings; substring of a
      fontified cell is measured correctly (no stale hit).

## 2. Shared row layout

- [x] 2.1 Introduce `gfm-tables--row-layout` returning a record with
      per-visual-line cell strings and per-visual-line char-bounds for a
      row.
- [x] 2.2 Refactor `gfm-tables--compose-multiline-row` to consume a
      pre-computed layout instead of calling `wrap-row-into-lines`.
- [x] 2.3 Refactor `gfm-tables--multiline-row-char-bounds` to consume a
      pre-computed layout instead of calling `wrap-row-into-lines`.
- [x] 2.4 Update `gfm-tables--apply-table` to compute one layout per row
      and feed it to both helpers.
- [x] 2.5 Confirm via ERT that header, body-default, and body-alt rows
      still render identically (string equality on the resulting overlay
      `display` strings against a captured baseline).

## 3. Scoped rebuild

- [x] 3.1 Add buffer-local `gfm-tables--dirty-region` storing the union
      of after-change ranges since the last rebuild.
- [x] 3.2 Modify `gfm-tables--schedule-rebuild` to extend the dirty
      region with each `(beg end len)` change and arm the idle timer.
- [x] 3.3 Add `gfm-tables--rebuild-scoped` that consumes the dirty
      region: locate intersecting blocks, decide between no-op,
      single-block rebuild, or full rebuild per the spec rules.
- [x] 3.4 Add `gfm-tables--remove-overlays-in-block` and use it in the
      single-block path so unrelated overlays survive.
- [x] 3.5 Detect changes that overlap a fenced-code-block fence line and
      force a full rebuild in that case.
- [x] 3.6 Wire `window-configuration-change-hook` to a separate handler
      that always triggers a full rebuild (no dirty-region merge).
- [x] 3.7 Add ERT coverage for each scenario in the modified Debounced
      rebuild requirement: edit inside one table, edit outside any
      table, edit spanning two tables, edit overlapping a code fence,
      window resize.

## 4. Phase-level instrumentation

- [x] 4.1 Extend `gfm-tables--init-stats` with phase totals
      (`find-blocks`, `parse`, `layout`, `compose`, `apply`).
- [x] 4.2 Inline-time each phase inside `gfm-tables--rebuild` /
      `gfm-tables--apply-table` and accumulate into the alist.
- [x] 4.3 Update `gfm-tables-stats` to print the phase breakdown sorted
      by total descending alongside the existing summary line.
- [x] 4.4 Confirm via ERT that phase totals are non-negative and sum to
      no more than the recorded `total-time` (allowing for
      uninstrumented overhead).

## 5. Verification

- [x] 5.1 Run `make test` to confirm existing tests pass.
- [x] 5.2 Re-run `/tmp/gfm-tables-bench.el` against the may-i
      REFERENCE.md and record the new per-rebuild average and
      edit-path average; compare against the baselines documented in
      `design.md`.
- [x] 5.3 Manually verify in Emacs: typing at `point-min` outside any
      table produces no rebuild (observe `gfm-tables-stats` rebuild
      count); typing inside one table rebuilds only that table.
