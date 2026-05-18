## 1. Pass 1 — Split `gfm-pretty-borders.el` into borders + engine

- [x] 1.1 Create `lisp/gfm/gfm-pretty-engine.el`. Move the registry struct (`gfm-pretty--registry`, `gfm-pretty--make-registry`, `gfm-pretty--registry-for`), overlay factories (`gfm-pretty--register`, `gfm-pretty--remove-overlays`, `gfm-pretty--prune-dead-overlays`, `gfm-pretty--remove-display-overlays-in-range`, `gfm-pretty--remove-display-overlays-for-window`, `gfm-pretty--make-anchor`, `gfm-pretty--make-display`), scheduler primitives (`gfm-pretty--extend-dirty-region`, `gfm-pretty--arm-rebuild-timer`), window-state helpers (`gfm-pretty--window-state`, `gfm-pretty--range-visible-p`, `gfm-pretty--block-visible-p`, `gfm-pretty--visible-window-ranges`, `gfm-pretty--display-windows`), and the reconciler (`gfm-pretty--reconciler`, `gfm-pretty--make-reconciler`, `gfm-pretty--rebuild-blocks`, `gfm-pretty--rebuild-block-for-window`, `gfm-pretty--pace-window-rebuild`, `gfm-pretty--rebuild-window-prioritised`, `gfm-pretty--reconcile-windows`) from `gfm-pretty-borders.el`. End the file with `(provide 'gfm-pretty-engine)`.
- [x] 1.2 Strip the engine code from `gfm-pretty-borders.el`. Keep only the graphics toolkit: `gfm-pretty--in-ranges-p`, `gfm-pretty--region-overlaps-p`, `gfm-pretty--available-width`, `gfm-pretty--text-width`, `gfm-pretty--max-line-width`, `gfm-pretty--normalised-border-face`, `gfm-pretty--top-strings`, `gfm-pretty--bottom-strings`, `gfm-pretty--right-after`, `gfm-pretty--simulate-wrap`, `gfm-pretty--last-visual-col`, `gfm-pretty--wrap-prefix`, `gfm-pretty--right-after-overflow`, plus `+markdown-overlay-border-face` (renamed in pass 5) and `gfm-pretty--icon-gui-nudge`. End with `(provide 'gfm-pretty-borders)`.
- [x] 1.3 Update `lisp/gfm/gfm-pretty.el` to `(require 'gfm-pretty-engine)` instead of relying on borders for the registry. Add `gfm-pretty-engine` to the `gfm-pretty--require-all` body.
- [x] 1.4 Update each decorator file's requires:
  - `gfm-pretty-callouts.el`: `(require 'gfm-pretty-engine)` AND `(require 'gfm-pretty-borders)` (draws boxes)
  - `gfm-pretty-fences.el`: both (draws boxes)
  - `gfm-pretty-tables.el`: both (draws boxes)
  - `gfm-pretty-hrule.el`: `(require 'gfm-pretty-engine)` only (no boxes — drops borders require)
  - `gfm-pretty-links.el`: `(require 'gfm-pretty-engine)` only (no boxes — drops borders require)
- [x] 1.5 Run `make test`. Full ERT suite passes, including narrowing-regression tags. Any failure here is unambiguously a pass-1 file-split error.
- [x] 1.6 Commit pass 1 as a distinct commit: `gfm-pretty: split borders.el into borders + engine`.

## 2. Pass 2 — Engine owns lifecycle hooks

- [x] 2.1 In `gfm-pretty-engine.el`, extend `gfm-pretty--decorator` struct with fields: `collect-fn`, `range-fn`, `apply-anchors-fn`, `apply-display-fn`, `font-lock`, `revealable-prop`, `saved-display-prop`, `revealable-p-fn`, `on-enable-fn`, `on-disable-fn`. Keep existing dispatch fields (`enable-fn`, `disable-fn`, `enabled-p-fn`, `block-at-point-fn`, `edit-at-point-fn`) for backward-compat during the migration; mark them deprecated in commentary, remove after pass 2 settles.
- [x] 2.2 Update `gfm-pretty-define-decorator` macro to recognise the new kwargs alongside the old. Existing 5-callback registrations still work; new full registrations populate the engine-owned fields.
- [x] 2.3 Define `defvar-local gfm-pretty--state nil` (alist `(NAME . plist)` of buffer-local decorator state: `overlays`, `hidden-ovs`, `dirty-region`, `last-window-state`, `blocks-cache`, `rebuild-timer`, `enabled-p`). Helpers `gfm-pretty--state-get name slot` and `gfm-pretty--state-set name slot val`.
- [x] 2.4 Define `gfm-pretty--after-change BEG END LEN`: iterates enabled decorators, extends each one's dirty region under `gfm-pretty--state`, arms the single engine idle timer.
- [x] 2.5 Define `gfm-pretty--wcc`: iterates enabled decorators, calls `gfm-pretty--reconcile-windows` for each (the engine-level reconciler now uses per-decorator state from `gfm-pretty--state`, not from decorator-owned defvar-locals).
- [x] 2.6 Define `gfm-pretty--scheduled-rebuild`: the timer callback. Iterates enabled decorators, for each one calls either `gfm-pretty--rebuild-scoped` (if dirty-region set) or `gfm-pretty--rebuild` (full). Resets dirty-region per decorator afterwards.
- [x] 2.7 Rework `gfm-pretty-mode`'s body: on enable, install one `after-change-functions` handler (`gfm-pretty--after-change`), one `window-configuration-change-hook` (`gfm-pretty--wcc`), one `post-command-hook` (`gfm-pretty--reveal` — stub in pass 2, completed in pass 4), initialise `gfm-pretty--state` from the decorator registry (every decorator's `enabled-p` set to t for the umbrella), then call each decorator's `:on-enable-fn` in registration order. On disable: reverse — call `:on-disable-fn` in reverse order, remove the three hooks, cancel the timer, remove all overlays.
- [x] 2.8 Rework `gfm-pretty-toggle-decorator NAME`: flips `(gfm-pretty--state-get name 'enabled-p)`. On enable, calls `:on-enable-fn` and schedules a rebuild for that decorator. On disable, calls `:on-disable-fn` and removes that decorator's overlays.
- [x] 2.9 Rework `gfm-pretty-block-at-point` to consult `gfm-pretty--state-get name 'enabled-p` (engine-tracked bit) instead of the decorator's own `enabled-p-fn`.
- [x] 2.10 Convert each decorator's registration from the 5-callback shim to the full kwargs. For callouts:
  ```elisp
  (gfm-pretty-define-decorator 'callouts
    :collect-fn          #'gfm-pretty-callouts--find-blocks-1  ; uncached, widened
    :range-fn            #'gfm-pretty-callouts--block-range
    :apply-anchors-fn    #'gfm-pretty-callouts--apply-block-anchors
    :apply-display-fn    #'gfm-pretty-callouts--apply-block-display
    :revealable-prop     'gfm-pretty-callouts-revealable
    :saved-display-prop  'gfm-pretty-callouts-saved-display
    :on-enable-fn        #'gfm-pretty-callouts--on-enable   ; faces, font-lock, theme hook
    :on-disable-fn       #'gfm-pretty-callouts--on-disable) ; reverse
  ```
  Similar for fences, hrule, links. Tables omits `:revealable-prop` and `:saved-display-prop` (uses its own cursor model); registers `:on-enable-fn` that installs the keymap, evil edit advice, cursor highlight handler.
- [x] 2.11 Move each decorator's `:on-enable-fn` body — the side effects that today live in `define-minor-mode` ENABLE branch — into a new `--on-enable` function. Symmetrically for `:on-disable-fn`. Examples:
  - Callouts on-enable: install font-lock keywords, add `+theme-changed-hook` watcher, call `--refresh-body-faces`, call `--neutralise-blockquote-face` if not already done.
  - Fences on-enable: install `cursor-intangible-mode 1`.
  - Links on-enable: install xref backend, eldoc function, suppress-compose-region advice, `markdown-hide-urls` watcher.
  - Tables on-enable: install keymap, evil edit advice, cursor handler.
- [x] 2.12 Delete the five per-decorator `define-minor-mode` bodies (`gfm-pretty-callouts-mode`, `gfm-pretty-fences-mode`, `gfm-pretty-tables-mode`, `gfm-pretty-hrule-mode`, `gfm-pretty-links-mode`). Provide thin `define-obsolete-function-alias` shims if any caller exists (verify via `git grep`); otherwise just delete.
- [x] 2.13 Delete the per-decorator scheduler helpers from each file: `--schedule-rebuild`, `--schedule-full-rebuild`, the `--dirty-region` defvar-local, the `--rebuild-timer` defvar-local, the `--last-window-state` defvar-local, `--arm-rebuild-timer`, `--extend-dirty-region`. Their roles move to engine state.
- [x] 2.14 Delete the per-decorator `--rebuild`, `--rebuild-block`, `--rebuild-blocks`, `--rebuild-block-for-window`, `--rebuild-prioritised`, `--reconcile-windows`, `--rebuild-scoped`, `--rebuild-window-prioritised` thin shims; engine functions take their place.
- [x] 2.15 Update `gfm-pretty-define-decorator` to remove the deprecated 5-callback fields after migration (`enable-fn`, `disable-fn`, `enabled-p-fn` go away; `block-at-point-fn` and `edit-at-point-fn` survive). Update doc-string.
- [x] 2.16 Update tests in `lisp/gfm/gfm-pretty-tests.el` that toggle per-decorator modes (`gfm-pretty-callouts-mode 1`, etc.) to toggle via the engine: enable `gfm-pretty-mode`, then optionally `(gfm-pretty-toggle-decorator 'NAME)` for per-decorator scenarios.
- [x] 2.17 Run `make test`. Full ERT suite passes including narrowing-regression. Any failure localises to engine wiring.
- [x] 2.18 Commit pass 2: `gfm-pretty: collapse decorator lifecycle into engine`.

## 3. Pass 3 — Engine memoises `:collect-fn`

- [x] 3.1 In `gfm-pretty-engine.el`, define `gfm-pretty--collect DECORATOR`: reads `gfm-pretty--state-get name 'blocks-cache` (a `(TICK . BLOCKS)` cons), compares car to `(buffer-chars-modified-tick)`, returns cached cdr on hit. On miss: calls `:collect-fn` under `save-restriction` + `widen`, stores the result, returns it.
- [x] 3.2 Update every engine call-site that today invokes a decorator's `--find-blocks` (cached wrapper) to call `gfm-pretty--collect DECORATOR` instead.
- [x] 3.3 In each decorator file, delete:
  - the `--blocks-cache` defvar-local (or `--fenced-blocks-cache` / `--yaml-helmet-cache` / `--indent-blocks-cache` for fences; ALL of these become engine-owned)
  - the `--find-blocks` cached wrapper (keep `--find-blocks-1`, the uncached widened scan, which is what the engine calls)
- [x] 3.4 For the fences decorator's multi-cache layout (fenced + yaml + indent each had their own cache), introduce a single `:collect-fn` that returns the combined block list and let the engine cache the combined result. Internal helpers `--find-fenced-blocks-1`, `--find-yaml-helmet-1`, `--find-indent-blocks-1` survive as private; the public `:collect-fn` composes them.
- [x] 3.5 Run `make test`. The "Block discovery memoisation" scenarios pass; per-decorator narrowing-regression tests still pass.
- [x] 3.6 Commit pass 3: `gfm-pretty: engine memoises collect-fn; drop per-decorator caches`.

## 4. Pass 4 — Engine drives reveal

- [x] 4.1 In `gfm-pretty-engine.el`, define `gfm-pretty--reveal`: iterates decorators with non-nil `enabled-p` AND non-nil `:revealable-prop`. For each, runs the reveal algorithm against that decorator's prop / saved-display prop / hidden-ovs (engine-tracked).
- [x] 4.2 Define `gfm-pretty--reveal-for DECORATOR PROP SAVED-PROP`: the existing algorithm (loop over engine-tracked hidden-ovs for the decorator; restore those point left; loop over `overlays-in (point) (1+ point)`; hide those at point in selected window).
- [x] 4.3 Wire `gfm-pretty--reveal` into the engine's `post-command-hook` (already added as a stub in pass 2).
- [x] 4.4 Delete `--reveal` functions from `gfm-pretty-callouts.el`, `gfm-pretty-fences.el`, `gfm-pretty-hrule.el`, `gfm-pretty-links.el`. Each delete also removes the `--hidden-ovs` defvar-local since engine state tracks `hidden-ovs` per decorator.
- [x] 4.5 Tables: confirm `:on-enable-fn` installs its own cursor handler and that the engine skips tables in the reveal loop (because tables omits `:revealable-prop`). No reveal code in tables to delete — it never had one.
- [x] 4.6 Run `make test`. Full ERT suite passes; per-decorator reveal scenarios pass via the engine.
- [x] 4.7 Commit pass 4: `gfm-pretty: engine-driven reveal; drop per-decorator handlers`.

## 5. Pass 5 — Rename `+markdown-` faces

- [x] 5.1 Rename callout faces in `gfm-pretty-callouts.el`:
  - `+markdown-gfm-callout-note-face` → `gfm-pretty-callouts-note-face`
  - `+markdown-gfm-callout-tip-face` → `gfm-pretty-callouts-tip-face`
  - `+markdown-gfm-callout-important-face` → `gfm-pretty-callouts-important-face`
  - `+markdown-gfm-callout-warning-face` → `gfm-pretty-callouts-warning-face`
  - `+markdown-gfm-callout-caution-face` → `gfm-pretty-callouts-caution-face`
  - `+markdown-gfm-callout-header-face` → `gfm-pretty-callouts-header-face`
  - `+markdown-gfm-callout-note-body-face` → `gfm-pretty-callouts-note-body-face`
  - `+markdown-gfm-callout-tip-body-face` → `gfm-pretty-callouts-tip-body-face`
  - `+markdown-gfm-callout-important-body-face` → `gfm-pretty-callouts-important-body-face`
  - `+markdown-gfm-callout-warning-body-face` → `gfm-pretty-callouts-warning-body-face`
  - `+markdown-gfm-callout-caution-body-face` → `gfm-pretty-callouts-caution-body-face`
  - `+markdown-prettier-ignore-comment-face` → `gfm-pretty-callouts-prettier-ignore-comment-face` (or move to a shared misc face if used elsewhere; verify via grep)
- [x] 5.2 Rename the refresh function:
  - `+markdown-gfm-callout-refresh-body-faces` → `gfm-pretty-callouts--refresh-body-faces`
  - Update `:on-enable-fn` body and its `+theme-changed-hook` registration.
- [x] 5.3 Rename misc faces in other decorator files (verify via grep):
  - `+markdown-overlay-border-face` → `gfm-pretty-border-face` (lives in `gfm-pretty-borders.el`)
  - `+markdown-gfm-hrule-face` → `gfm-pretty-hrule-face` (lives in `gfm-pretty-hrule.el`)
- [x] 5.4 Add `define-obsolete-face-alias` entries for each renamed face (one per face) pointing the old name at the new. Use the current emacs-version string as the deprecation marker.
- [x] 5.5 Update spec scenario at `openspec/specs/gfm-pretty/spec.md` "Theme change responsiveness" — the MODIFIED requirement in this change's delta already names the new symbols; pass 5 makes the code match.
- [x] 5.6 Update test fixtures in `lisp/gfm/gfm-pretty-tests.el` that reference old face names (grep for `+markdown-gfm-callout`).
- [x] 5.7 Run `make test`. Full ERT suite passes.
- [x] 5.8 Commit pass 5: `gfm-pretty: rename +markdown- internal faces to gfm-pretty-`.

## 6. Spec deltas apply

- [x] 6.1 Apply the `gfm-pretty` delta to the stable spec at `openspec/specs/gfm-pretty/spec.md`:
  - ADD the new "Block discovery memoisation" requirement with its three scenarios.
  - MODIFY the "Debounced rebuild scheduler" requirement body (sharpened to engine-owned, one-handler-per-hook).
  - MODIFY the "Per-window cursor reveal" requirement body (engine walks each decorator's `:revealable-prop`).
  - MODIFY the "Theme change responsiveness" requirement body (renamed function and face symbols).
  - REMOVE the "Callout block-discovery cache" requirement (with Reason / Migration).
  - REMOVE the "Code-fence block-discovery cache" requirement (with Reason / Migration).

## 7. Verification

- [x] 7.1 Run `make test`. Full ERT suite passes including all narrowing-regression tags.
- [x] 7.2 `emacs -Q --batch -L lisp -L lisp/gfm --eval "(require 'gfm-pretty)" --eval "(message \"OK\")"`. Confirms the library loads cleanly with the new file structure.
- [ ] 7.3 Open a `.md` file in a fresh `emacs` session loaded with this config. Inspect with `M-x describe-mode` — `gfm-pretty-mode` is active. Confirm: callouts, fenced code blocks, tables, HRs, links all decorated. Toggle each decorator via `M-x gfm-pretty-toggle-decorator NAME`; confirm clean enable/disable.
- [ ] 7.4 Open the buffer in two side-by-side windows of different widths (`C-x 3`). Confirm per-window rendering preserved (each box sized to its window).
- [ ] 7.5 Verify only one of each lifecycle hook installed: `(length after-change-functions)`, `(length window-configuration-change-hook)`, `(length post-command-hook)` each grew by 1 over the disabled state.
- [ ] 7.6 Verify only one idle timer: `(length timer-idle-list)` includes at most one `gfm-pretty--*` timer.
- [ ] 7.7 Activate `gfm-present-mode` on a multi-heading markdown buffer. Confirm slide navigation works and decorations render inside narrowed slides without zombie overlays after navigation. (gfm-present is unchanged by this refactor — should keep working.)
- [ ] 7.8 With point inside a GFM table, invoke the local-leader `t` binding. Confirm `(gfm-pretty-toggle-decorator 'tables)` toggles the tables decorator. With point inside a cell, `gfm-pretty-edit-block-at-point` opens the indirect editor (tables decorator's `:edit-at-point-fn`).
- [x] 7.9 Run `openspec validate deepen-gfm-pretty-engine --strict`. No errors.
- [x] 7.10 Run `make test-integration` if separate from `make test`. (No separate target; integration tests run as part of `make test`.)
