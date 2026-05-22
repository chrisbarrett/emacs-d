## 1. Module extraction scaffolding

- [ ] 1.1 Create `lisp/gfm/gfm-pretty-link-previews.el` with a `defgroup` and the standard `(require 'gfm-pretty-borders)` / `(require 'gfm-pretty-engine)` deps.
- [ ] 1.2 Define `gfm-pretty-link-previews--registry` via `gfm-pretty--registry-for` and the standard overlay helpers (`--make-display`, `--remove-overlays`, `--prune-dead-overlays`, `--register`).
- [ ] 1.3 Move the preview-cap defconst (`gfm-present--preview-cap` → `gfm-pretty-link-previews--preview-cap`).

## 2. Pure-helper migration

- [ ] 2.1 Move `gfm-present--abbrev-source-path`, `--abbrev-diff-refs`, `--read-line-range`, `--fontify-source`, `--box-display`, `--parse-source-link`, `--parse-diff-link`, `--standalone-link-p`, `--md-link-rx` to the new module under the `gfm-pretty-link-previews--` prefix.
- [ ] 2.2 Add `define-obsolete-function-alias` (and `define-obsolete-variable-alias` for the cap) entries in `gfm-present.el` pointing each old name at the new one.
- [ ] 2.3 Update internal callers within the new module to use the new names.

## 3. Decorator integration

- [ ] 3.1 Move `gfm-present--source-preview-display`, `--diff-preview-display`, `--diff-preview-argv`, `--run-diff-preview` to the new module.
- [ ] 3.2 Define `gfm-pretty-link-previews--collect-blocks` that scans the buffer for standalone source/diff links and returns one block per link (block range covers the link span; payload carries the parsed URL fields).
- [ ] 3.3 Define `gfm-pretty-link-previews--apply-block` that builds the appropriate display string and creates a per-window display overlay covering the link span.
- [ ] 3.4 Define `gfm-pretty-link-previews--full-rebuild-required-p` returning t for any edit (per design D3).
- [ ] 3.5 Register the decorator via `gfm-pretty-define-decorator 'link-previews` with `:collect-fn`, `:range-fn`, `:apply-block-fn`, `:full-rebuild-required-p`, `:on-enable-fn`, `:on-disable-fn`.

## 4. Present-mode delegation

- [ ] 4.1 In `gfm-present.el`, remove `gfm-present--render-link-previews` / `--clear-link-previews` / `--make-preview-overlay` / `--preview-overlays`.  Replace call sites with `(when-let* ((d (gfm-pretty--get 'link-previews))) (gfm-pretty--rebuild d))`.
- [ ] 4.2 Ensure `gfm-present-mode` enable activates `gfm-pretty-mode` (already required for blockquote / callout decoration; verify and document).
- [ ] 4.3 Ensure `gfm-present-mode` disable does NOT explicitly tear down preview overlays (let the engine handle it when `gfm-pretty-mode` toggles off).

## 5. Tests

- [ ] 5.1 Rename existing source-preview / diff-preview tests in `gfm-pretty-tests.el` (or move to a new `gfm-pretty-link-previews-tests.el` co-located file) to use the new function names.
- [ ] 5.2 Add a regression test: enable `gfm-pretty-mode` in a plain `gfm-mode` buffer with a standalone source-range link; assert the preview overlay renders.
- [ ] 5.3 Add a regression test: enable `gfm-pretty-mode`, render a preview, then `gfm-pretty-toggle-decorator 'link-previews` — assert the overlay is removed and re-appears on toggle-on.
- [ ] 5.4 Add a regression test: enable `gfm-pretty-mode` + a preview, edit the link's URL via `replace-regexp-in-string`-style buffer mutation, and assert the rebuild fires (next call to `gfm-pretty--rebuild` re-scans).
- [ ] 5.5 Run the narrowing-regression suite to confirm previews converge under narrow / widen rebuild.
- [ ] 5.6 Run the existing `gfm-present-mode` tests to confirm presentation-mode rendering still works (delegation through decorator).

## 6. Spec sync

- [ ] 6.1 Confirm `openspec validate link-previews-as-decorator --strict` passes.
- [ ] 6.2 Run `make test` and confirm zero failures.

## 7. Memory + docs

- [ ] 7.1 If the present-mode integration surprises (e.g. ordering of `gfm-pretty-mode` enable vs `gfm-present-mode` enable matters in ways not anticipated), capture a feedback memory before archiving.
