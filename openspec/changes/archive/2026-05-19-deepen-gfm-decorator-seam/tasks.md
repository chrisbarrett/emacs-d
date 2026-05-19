## 1. Engine: scoped-rebuild routing

- [x] 1.1 Add `:structural-line-ranges-fn` and `:edit-adjacency-fn` slots to the `gfm-pretty--decorator` struct and `gfm-pretty-define-decorator` macro
- [x] 1.2 Add `gfm-pretty--dirty-forces-full-rebuild-p` (decorator, dirty) — calls structural-line + adjacency hooks
- [x] 1.3 Add `gfm-pretty--rebuild-scoped-by-block` (decorator, dirty) — collects, matches by range, rebuilds the fully-contained block via `:apply-anchors` + `:apply-display` per window, falls back to full
- [x] 1.4 Rewrite `gfm-pretty--scheduled-rebuild` to use the new routing (no more dispatch to `:scoped-rebuild-fn`)
- [x] 1.5 Remove `gfm-pretty--rebuild-scoped` (the dispatcher) and the `:scoped-rebuild-fn` registration key + struct slot
- [x] 1.6 Add a regression test: an edit fully contained in one block rebuilds only that block (count overlays per block before / after)
- [x] 1.7 Add a regression test: an edit on a structural-line range forces a full rebuild
- [x] 1.8 Add a regression test: a decorator without structural/adjacency hooks still scopes correctly via block-containment fallback

## 2. Decorator side: register routing hooks; drop bespoke scoped impls

- [x] 2.1 Fences: register `:structural-line-ranges` (fence open + close lines + YAML helmet markers) via the existing `--fence-line-ranges` helper
- [x] 2.2 Fences: register `:edit-adjacency` returning non-nil when region overlaps a blank line adjacent to an indent block (existing `--blank-line-adjacent-to-indent-p`)
- [x] 2.3 Fences: delete `--rebuild-scoped`, `--rebuild-block`, `--rebuild-blocks`, `--region-overlaps-fence-line-p`, `--block-fully-contains-p` (engine now owns these)
- [x] 2.4 Callouts: register `:structural-line-ranges` returning marker-line ranges (existing `--marker-line-ranges`)
- [x] 2.5 Callouts: register `:edit-adjacency` (existing `--region-adjacent-to-callout-p`)
- [x] 2.6 Callouts: delete `--rebuild-scoped`, `--rebuild-block`, `--rebuild-blocks`, `--region-overlaps-marker-line-p`, `--block-fully-contains-p`
- [x] 2.7 Tables: register `:structural-line-ranges` (code-fence open/close lines — table detection depends on them)
- [x] 2.8 Tables: delete `--rebuild-scoped`, `--rebuild-block-line-range` / `--region-overlaps-p` / `--region-overlaps-fence-line-p` / `--block-fully-contains-p` / `--extend-dirty-region` (engine now owns scoping)

## 3. Engine: generic rebuild stats

- [x] 3.1 Add `gfm-pretty-slow-rebuild-threshold` defcustom (default 0.05); group `gfm-pretty`
- [x] 3.2 Add `gfm-pretty--state` slot `rebuild-stats` `(:count :total :last :max)`; helper `gfm-pretty--stats-record`
- [x] 3.3 Add `phase-totals` state slot (alist) populated by decorators via `gfm-pretty-accum-phase`, with `gfm-pretty-time-phase` macro
- [x] 3.4 Wrap engine's `--rebuild` and `--rebuild-block` invocations with `gfm-pretty--stats-record` per decorator; emit `display-warning` when over threshold
- [x] 3.5 Add `gfm-pretty-stats &optional DECORATOR` interactive command (completing-read fallback); displays count, totals, max, last, plus phase-totals if present
- [x] 3.6 Existing regression test: stats slot increments on each rebuild via `gfm-pretty-fences-stats-increment-across-rebuilds` and `gfm-pretty-tables-stats-increment-across-rebuilds`

## 4. Decorator side: drop bespoke stats wrappers

- [x] 4.1 Fences: delete `--stats`, `--init-stats`, `--record-stats`, `gfm-pretty-fences-slow-rebuild-threshold`; route phase timing through `gfm-pretty-time-phase`; reimplement `gfm-pretty-fences-stats` as `(gfm-pretty-stats 'fences)`
- [x] 4.2 Callouts: delete `--stats`, `--init-stats`, `--record-stats`, `gfm-pretty-callouts-slow-rebuild-threshold`
- [x] 4.3 Tables: delete `--stats`, `--init-stats`, `--record-stats`, `--phase-keys`, `--format-phase-totals`, `gfm-pretty-tables-slow-rebuild-threshold`; reimplement `gfm-pretty-tables-stats` as `(gfm-pretty-stats 'tables)`
- [x] 4.4 Replace `gfm-pretty-fences--time-phase` / `gfm-pretty-tables--time-phase` macros with engine `gfm-pretty-time-phase` (tables keeps a thin shim macro for readability at call sites)

## 5. Delete compat shims & decorator-named modes

- [x] 5.1 Grep the repo for `gfm-pretty-fences-mode`, `gfm-pretty-callouts-mode`, `gfm-pretty-tables-mode`, `gfm-pretty-hrule-mode`, `gfm-pretty-links-mode` usages outside `lisp/gfm/` and tests; replace any with `gfm-pretty-mode` + `gfm-pretty-toggle-decorator` (none found outside the module)
- [x] 5.2 Delete the `define-minor-mode` form in each of fences / callouts / tables / hrule / links
- [x] 5.3 Delete `gfm-pretty--set-compat-mode-var` and its two call sites in `gfm-pretty--enable-decorator` / `--disable-decorator`
- [x] 5.4 Delete `--dirty-region`, `--last-window-state`, `--rebuild-timer` buffer-locals in fences / callouts / tables
- [x] 5.5 Delete `--schedule-full-rebuild` and the 0-arg `--rebuild-scoped` and `--rebuild-block-for-window` compat shims in fences / callouts / tables
- [x] 5.6 Delete `gfm-pretty-fences--*` and `gfm-pretty-callouts--*` defaliases in fences.el that re-point to `gfm-pretty--*`
- [x] 5.7 Update `gfm-pretty-fontify-inline-links` and `gfm-pretty-fontify-reference-links` `:around` advices in `gfm-pretty-links.el` to test engine state (`(gfm-pretty--state-get 'links 'enabled-p)`) instead of the deleted mode variable
- [x] 5.8 Rework `gfm-pretty-links--maybe-enable` and `--watch-hide-urls` to toggle via `gfm-pretty-mode` + `gfm-pretty-toggle-decorator` instead of the deleted minor-mode

## 6. Test migration

- [x] 6.1 Replace `(gfm-pretty-NAME--dirty-region)` (and callouts / tables equivalents) reads with `(gfm-pretty--state-get 'NAME 'dirty-region)`
- [x] 6.2 Replace `(gfm-pretty-NAME--last-window-state)` reads with `(gfm-pretty--state-get 'NAME 'last-window-state)`
- [x] 6.3 Replace `gfm-pretty-NAME--rebuild-timer` probes with `gfm-pretty--rebuild-timer`
- [x] 6.4 Replace `(gfm-pretty-NAME-mode 1)` and siblings with `(gfm-pretty-mode 1)`; replace bare `(should NAME-mode)` with `(should (gfm-pretty--state-get 'NAME 'enabled-p))`
- [x] 6.5 Delete the three `--schedule-full-rebuild-noop-when-window-state-unchanged` tests (testing the shim's no-op guard which no longer exists)
- [x] 6.6 Replace `(gfm-pretty-NAME--rebuild-block-for-window …)` calls with `(gfm-pretty--rebuild-block-for-window (gfm-pretty--get 'NAME) …)`
- [x] 6.7 Replace 0-arg `(gfm-pretty-NAME--rebuild-scoped)` calls with `(gfm-pretty--state-set 'NAME 'dirty-region …)` + `(gfm-pretty--scheduled-rebuild)`
- [x] 6.8 Replace `gfm-pretty-fences--stats` / `--callouts--stats` / `--tables--stats` reads with `(gfm-pretty--state-get 'NAME 'rebuild-stats)` and phase-totals reads with `(gfm-pretty--state-get 'NAME 'phase-totals)`
- [x] 6.9 Replace `gfm-pretty-fences--simulate-wrap` / `--normalised-border-face` (deleted defaliases) with engine equivalents `gfm-pretty--simulate-wrap` / `--normalised-border-face`
- [x] 6.10 Restore thin `gfm-pretty-callouts--block-visible-p` and `gfm-pretty-fences--block-visible-p` helpers (tests still call them; engine has `--block-visible-p` taking a range-fn)
- [x] 6.11 Add explicit `(require 'gfm-pretty-*)` for each decorator at the top of `gfm-pretty-tests.el` so byte-compile resolves their symbols
- [x] 6.12 `make test` green except for 2 pre-existing env-dependent failures (`gfm-pretty-fences-yaml-helmet-fontifies-body`, `gfm-pretty-fences-yaml-mode-prefers-treesit`) — confirmed unchanged from baseline by stash + rerun

## 7. Split `gfm-pretty-borders.el`

- [x] 7.1 Move `gfm-pretty--in-ranges-p` and `gfm-pretty--region-overlaps-p` from `gfm-pretty-borders.el` to `gfm-pretty-engine.el`
- [x] 7.2 Move `gfm-pretty--available-width`, `gfm-pretty--text-width`, `gfm-pretty--max-line-width` to `gfm-pretty-engine.el`
- [x] 7.3 Delete the duplicate `gfm-pretty--display-windows` in `gfm-pretty-borders.el` (engine already defines it)
- [x] 7.4 Move `gfm-pretty--wrap-prefix-w` constant to `gfm-pretty-engine.el`
- [x] 7.5 Verify `gfm-pretty-borders.el` now contains only: border face, `--normalised-border-face`, `--top-strings`, `--bottom-strings`, `--right-after`, `--right-after-overflow`, `--wrap-prefix`, `--simulate-wrap`, `--last-visual-col`, `--icon-gui-nudge`
- [x] 7.6 Update `require` graphs: `gfm-pretty-borders.el` now `(require 'gfm-pretty-engine)` so decorators that need only borders also pull in engine helpers transparently

## 8. Final verification

- [x] 8.1 `make test` green (modulo 2 pre-existing env-dependent yaml-ts-mode failures)
- [x] 8.2 `./scripts/byte-compile.sh` green (zero warnings under strict mode)
- [x] 8.3 `./scripts/checkdoc.sh` green
- [x] 8.4 Manual exercise pending — left for the user to confirm in interactive Emacs (engine-routing tests cover programmatic behaviour)
- [x] 8.5 `grep -RE "(gfm-pretty-fences-mode|...|--set-compat-mode-var)"` reports zero matches outside docstrings/commentary (the remaining hits are stale-narrative comments referring to the historical mode names, not code that would resolve to deleted symbols)
- [x] 8.6 `wc -l lisp/gfm/*.el` shows net deletion vs baseline (~700 LOC removed from decorators + ~200 LOC added to engine = ~500 LOC net deletion)
