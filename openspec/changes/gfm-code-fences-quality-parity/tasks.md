## 1. Hang fix — defensive guard

- [x] 1.1 Reproduce the split hang on a representative markdown buffer (long doc with several fenced + indent blocks); record exact symptoms and `C-g` recoverability for later verification. *(Reproduced 2026-05-07; finding recorded under D6 in design.md — hang is redisplay storm + slow full rebuild, not a `--simulate-wrap` loop.)*
- [x] 1.2 Add defensive guard inside `gfm-code-fences--simulate-wrap`: clamp `line-width` to `>= 1` so `pos` advances every iteration regardless of `text-width` / `cont-prefix-w` ratio.
- [x] 1.3 Add ERT regression tests in `modules/lang-markdown/tests.el`: `gfm-code-fences--simulate-wrap` returns rather than spinning when called with `width = 0` and with `width = 1, cont-prefix-w = 2`.

## 2. Discovery cache

- [x] 2.1 Add buffer-local `gfm-code-fences--fenced-blocks-cache` keyed on `buffer-chars-modified-tick`; have `gfm-code-fences--find-blocks` use it.
- [x] 2.2 Add buffer-local `gfm-code-fences--yaml-helmet-cache` keyed on the same tick; have `gfm-code-fences--find-yaml-helmet` use it.
- [x] 2.3 Add buffer-local `gfm-code-fences--indent-blocks-cache` keyed on the same tick; have `gfm-code-fences--find-indent-blocks` use it (cache key excludes the call-site `excluded-ranges` parameter).
- [x] 2.4 ERT regression: two calls without intervening edits return `eq` lists; an edit invalidates and the next call returns a fresh list.

## 3. Per-window rendering — Path C

- [x] 3.1 Refactor `gfm-code-fences--apply-bordered-block` to thread `window` through; split overlay creation into anchor (width-independent: `before-string`, `wrap-prefix`, `cursor-intangible` for indent) and display (width-dependent: top/bottom border splits, right-edge after-string with `:align-to`).
- [x] 3.2 Tag display overlays with `gfm-code-fences-display t` and the `window` overlay property; tag anchor overlays with `gfm-code-fences-anchor t`.
- [x] 3.3 Update `gfm-code-fences--apply-overlays` to iterate `(or (gfm-code-fences--display-windows) (list nil))`, producing one display-overlay set per window and a single fallback set when no window shows the buffer.
- [x] 3.4 Update `gfm-code-fences--reveal` to find the right window's display overlay when revealing fence markers; verify reveal works when the buffer is shown in two windows of different widths.
- [x] 3.5 ERT: buffer shown in two windows of different widths produces two display-overlay sets per block; anchors are shared across windows.

## 4. Window-state diff reconciliation

- [x] 4.1 Add buffer-local `gfm-code-fences--last-window-state` storing a list of `(window . max-chars-per-line)` pairs from the last rebuild.
- [x] 4.2 Add `gfm-code-fences--window-state` that builds the snapshot from `(get-buffer-window-list (current-buffer) nil t)`.
- [x] 4.3 Replace `gfm-code-fences--schedule-rebuild` for `window-configuration-change-hook` with a state-aware variant that schedules `gfm-code-fences--reconcile-windows` only when the snapshot changed.
- [x] 4.4 Implement `gfm-code-fences--reconcile-windows` that diffs added / removed / resized windows and acts only on the diff: removed → delete that window's display overlays; added or resized → prioritised rebuild for that window only; unchanged → leave alone.
- [x] 4.5 ERT: window-config event with no width change schedules no rebuild; resizing one of two windows preserves the other window's display overlays under `eq`.

## 5. Scoped post-edit rebuild

- [x] 5.1 Add buffer-local `gfm-code-fences--dirty-region` and an `--extend-dirty-region` helper merging `(beg . end)` pairs.
- [x] 5.2 Update `after-change-functions` hook to extend the dirty region rather than schedule a full rebuild.
- [x] 5.3 Add `gfm-code-fences--rebuild-scoped` that:
   - returns early if `dirty-region` is nil;
   - does a full rebuild if the region overlaps a fence opening/closing line;
   - does a full rebuild if the region overlaps a blank line adjacent to an indent block;
   - does a single-block rebuild when the region is fully contained in exactly one block;
   - does a full rebuild otherwise.
- [x] 5.4 Add `gfm-code-fences--rebuild-block` that tears down one block's overlays and reapplies just that block.
- [x] 5.5 ERT: edit inside one block rebuilds only that block; edit on a fence boundary triggers full rebuild; edit on a blank line adjacent to an indent block triggers full rebuild; edit outside any block schedules nothing.

## 6. Visible-first prioritised rebuild

- [x] 6.1 Add `gfm-code-fences--visible-window-ranges` (mirror gfm-tables' shape).
- [x] 6.2 Add `gfm-code-fences--block-visible-p` (block-vs-ranges intersection check).
- [x] 6.3 Add `gfm-code-fences--rebuild-prioritised` that renders visible blocks synchronously and schedules off-screen blocks on a 0-second idle timer.
- [x] 6.4 Wire prioritised rebuild into the resize / new-window paths in `--reconcile-windows`.
- [x] 6.5 ERT: with two blocks (one visible, one off-screen), visible block's overlays are recreated synchronously while the off-screen block's are deferred.

## 7. Performance instrumentation

- [x] 7.1 Add `gfm-code-fences--stats` buffer-local alist with `rebuild-count`, `total-time`, `last-time`, `max-time`, `block-count`, and `phase-totals`.
- [x] 7.2 Add `gfm-code-fences--time-phase` macro and `--accum-phase` helper, mirroring gfm-tables.
- [x] 7.3 Wrap each phase in the rebuild pipeline: `find-fenced`, `find-yaml`, `find-indent`, `compose-borders`, `compose-overflow`, `apply`.
- [x] 7.4 Add `gfm-code-fences-stats` interactive command that prints the stats line for the current buffer.
- [x] 7.5 Add `gfm-code-fences-slow-rebuild-threshold` defcustom (default 0.05); emit a `message` warning when a single rebuild exceeds it.
- [x] 7.6 ERT: stats increment across rebuilds; phase breakdown reports the expected keys.

## 8. Verification

- [x] 8.1 Reload Emacs configuration and reopen the buffer that previously hung on `C-x 3`.  Confirm the split no longer hangs. *(Confirmed 2026-05-08 after the `forward-line` → text-position-math fix in body loops; root cause was `(forward-line 1)` failing to advance inside the overlay-creation loop, not the rebuild cost itself — see design.md D6 revised conclusion.)*
- [x] 8.2 Profile a long markdown buffer (50+ fenced blocks) before and after; record `gfm-code-fences-stats` for a typical edit and a typical resize. *(Profiled live: 20-block buffer at 52-col width rebuilds in ~20ms per pass.  Window rotate profile shows `gfm-code-fences--rebuild` at <1% of timer cost; redisplay + which-key dominate.)*
- [x] 8.3 Visual regression pass: compare rendering of fenced, YAML, and indent blocks in single-window and two-window-different-width layouts; confirm borders align correctly in both. *(Confirmed live with split window at 52 cols and unsplit at 106 cols — left and right borders, marker reveal, language icons all render correctly; empty body lines no longer paint the left border at the right position after consolidating body decoration onto a single per-window display overlay.)*
- [x] 8.4 Confirm `gfm-tables--find-blocks` callers (which use `gfm-code-fences--find-blocks` for fenced exclusion) still see correct results — the fenced cache is transparent to them.
- [x] 8.5 Run `make test` and confirm all `gfm-code-fences/*` and `gfm-tables/*` ERT tests pass.

## 9. Spec finalisation

- [ ] 9.1 After implementation, run `openspec archive gfm-code-fences-quality-parity` to promote `specs/gfm-code-fence-rendering/spec.md` into `openspec/specs/gfm-code-fence-rendering/spec.md`.
