## 1. Engine state ownership

- [x] 1.1 Extend `gfm-pretty--state` slot helpers and docstring to formally own `'overlays` and `'hidden-ovs` lists per decorator name in `lisp/gfm/gfm-pretty-engine.el`.
- [x] 1.2 Rewrite registry-aware overlay primitives (`gfm-pretty--register`, `gfm-pretty--remove-overlays`, `gfm-pretty--prune-dead-overlays`, `gfm-pretty--remove-display-overlays-in-range`, `gfm-pretty--remove-display-overlays-for-window`, `gfm-pretty--make-anchor`, `gfm-pretty--make-display`) to read/write through the state alist instead of `overlays-symbol` / `hidden-ovs-symbol`.
- [x] 1.3 Drop `overlays-symbol` and `hidden-ovs-symbol` slots from `gfm-pretty--registry`; update `gfm-pretty--registry-for` and every `gfm-pretty--registry-for` call site to match.
- [x] 1.4 Add a thin `(gfm-pretty--overlays-for NAME)` debugging accessor in `gfm-pretty-engine.el`.

## 2. Engine protocol surface

- [x] 2.1 In `gfm-pretty--decorator` struct (`gfm-pretty-engine.el`), drop slots: `block-at-point-fn`, `edit-at-point-fn`, `apply-anchors-fn`, `apply-display-fn`, `revealable-prop`, `saved-display-prop`, `revealable-p-fn`, `reconcile-windows-fn`, `structural-line-ranges-fn`, `edit-adjacency-fn`. Add: `apply-block-fn`, `full-rebuild-required-p`.
- [x] 2.2 Rewrite `gfm-pretty-define-decorator` macro to accept the narrowed keyword list (per spec MODIFIED requirement).
- [x] 2.3 Update `gfm-pretty--reveal-for` to derive revealable + saved-display property names from `(gfm-pretty--decorator-registry decorator)` instead of separate slots; drop the custom-predicate branch.
- [x] 2.4 Replace `gfm-pretty--dirty-forces-full-rebuild-p` with a single call to `:full-rebuild-required-p`.
- [x] 2.5 Update `gfm-pretty--rebuild`, `gfm-pretty--rebuild-blocks`, `gfm-pretty--rebuild-block`, `gfm-pretty--rebuild-block-for-window`, `gfm-pretty--rebuild-window-prioritised`, `gfm-pretty--reconcile-windows`, `gfm-pretty--pace-window-rebuild` to call `:apply-block-fn (block window)` instead of the anchors+display pair.
- [x] 2.6 Update `gfm-pretty--wcc` so it no longer dispatches to `:reconcile-windows-fn`; the generic per-window reconciler is the only path.

## 3. Borders helper

- [x] 3.1 Add `gfm-pretty-borders--apply-with-anchors` to `lisp/gfm/gfm-pretty-borders.el` accepting `(block window &key registry anchors-fn display-fn)`. It SHALL call `anchors-fn` at most once per `(decorator-name . block-id)` per rebuild pass, then unconditionally call `display-fn`.
- [x] 3.2 Add the per-rebuild-pass anchors-laid sentinel slot to `gfm-pretty--state` (or reuse `blocks-cache`'s tick value) and reset it at the start of every full or scoped rebuild.

## 4. Callouts decorator

- [x] 4.1 In `lisp/gfm/gfm-pretty-callouts.el`, replace the `:apply-anchors-fn` + `:apply-display-fn` pair with one `gfm-pretty-callouts--apply-block-fn (block window)` that calls `gfm-pretty-borders--apply-with-anchors`.
- [x] 4.2 Replace `:structural-line-ranges-fn` + `:edit-adjacency-fn` with a single `gfm-pretty-callouts--full-rebuild-required-p (dirty)` composing the two checks internally.
- [x] 4.3 Remove the `defvar-local gfm-pretty-callouts--overlays` and any `gfm-pretty-callouts--hidden-ovs`; remove `:revealable-prop` / `:saved-display-prop` from the registration form.
- [x] 4.4 Update the registration block at the bottom of the file to use the narrowed protocol.

## 5. Fences decorator

- [x] 5.1 In `lisp/gfm/gfm-pretty-fences.el`, replace `:apply-anchors-fn` + `:apply-display-fn` (including the `apply-fenced-block-*`, `apply-yaml-block-*`, `apply-indent-block-*` dispatchers) with `gfm-pretty-fences--apply-block-fn (block window)` calling `gfm-pretty-borders--apply-with-anchors` where appropriate.
- [x] 5.2 Replace `:structural-line-ranges-fn` + `:edit-adjacency-fn` with `gfm-pretty-fences--full-rebuild-required-p`.
- [x] 5.3 Remove `defvar-local gfm-pretty-fences--overlays`; remove `:revealable-prop` / `:saved-display-prop`.
- [x] 5.4 Update the registration block.

## 6. Tables decorator

- [x] 6.1 In `lisp/gfm/gfm-pretty-tables.el`, fold the existing `:apply-anchors-fn` no-op + `:apply-display-fn` into a single `:apply-block-fn`.
- [x] 6.2 Move the bespoke window-reconcile logic (currently registered as `:reconcile-windows-fn`) into a buffer-local `window-configuration-change-hook` handler installed by `gfm-pretty-tables--on-enable` and removed by `gfm-pretty-tables--on-disable`.
- [x] 6.3 Move `gfm-pretty-tables--block-at-point` and `gfm-pretty-tables--edit-table-at-point` to the conventional `gfm-pretty-tables--block-at-point` / `gfm-pretty-tables--edit-at-point` exported names; remove the `:block-at-point-fn` / `:edit-at-point-fn` slots from registration.
- [x] 6.4 Replace `:structural-line-ranges-fn` with `gfm-pretty-tables--full-rebuild-required-p`.
- [x] 6.5 Remove `defvar-local gfm-pretty-tables--overlays` and any associated bookkeeping.

## 7. Hrule decorator

- [x] 7.1 In `lisp/gfm/gfm-pretty-hrule.el`, replace the no-op `:apply-anchors-fn` and the `:apply-display-fn` with a single `:apply-block-fn`.
- [x] 7.2 Remove `defvar-local gfm-pretty-hrule--overlays`; remove `:revealable-prop` / `:saved-display-prop`.

## 8. Links decorator

- [x] 8.1 In `lisp/gfm/gfm-pretty-links.el`, fold the no-op `:apply-anchors-fn` and `:apply-display-fn` into a single `:apply-block-fn`.
- [x] 8.2 Remove `defvar-local gfm-pretty-links--overlays` and `gfm-pretty-links--hidden-ovs`; remove `:revealable-prop` / `:saved-display-prop`.
- [x] 8.3 Update `gfm-pretty-links--reveal` to read overlays via `(gfm-pretty--state-get 'links 'overlays)` / `(gfm-pretty--state-get 'links 'hidden-ovs)` instead of the module-local symbols.

## 9. Umbrella commands

- [x] 9.1 In `lisp/gfm/gfm-pretty.el`, update `gfm-pretty-block-at-point` and `gfm-pretty-edit-block-at-point` to iterate over decorators and call `<name>--block-at-point` / `<name>--edit-at-point` (intern by name) when present — the naming convention replacing the dropped registry slots.

## 10. Tests

- [x] 10.1 Update `lisp/gfm/gfm-pretty-tests.el` to assert through `(gfm-pretty--state-get NAME 'overlays)` rather than module-local `*--overlays` symbols.
- [x] 10.2 Update existing protocol tests to reference `:apply-block-fn` and `:full-rebuild-required-p`.
- [x] 10.3 Add per-decorator regression tests asserting that the dirty regions which used to force a full rebuild (structural marker edits in callouts/fences, indent-block blank-line adjacency) still force a full rebuild under the merged predicate.
- [x] 10.4 Confirm the narrowing-regression suite under `modules/lang-markdown/tests.el` (tag `narrowing-regression`) still passes unmodified for every decorator.

## 11. Verification

- [x] 11.1 Run `make test-quick` and confirm green.
- [x] 11.2 Run `make test-integration` and confirm green.
- [x] 11.3 Run `make test` and confirm green.
- [x] 11.4 Open a representative markdown buffer in a sandbox Emacs containing callouts, fenced code blocks, an indent block, a YAML helmet, tables, an hrule, and inline + reference links. `C-x 3` split; narrow; widen; toggle each decorator off and on. Confirm no overlay leaks (e.g. via `(seq-filter (lambda (o) (overlay-get o 'gfm-pretty-callouts)) (overlays-in (point-min) (point-max))))` returning empty after disable).
- [x] 11.5 Run `openspec validate tighten-gfm-pretty-protocol --strict` and resolve any reported issues.
