## 1. Marker-aware indent (D1)

- [ ] 1.1 Add `:indent` parameter to `gfm-pretty-link-previews--box-display`; when non-zero, prefix the top border, every body line, and the bottom border with `(make-string indent ?\s)`.
- [ ] 1.2 Update `--source-preview-display` and `--diff-preview-display` to accept + forward an `indent` argument.
- [ ] 1.3 In `--apply-block`, compute `indent` per block as `(- link-start (line-beginning-position-at link-start))` (widened) and pass to the display functions.
- [ ] 1.4 Pass `indent` to the broken-preview sentinel paths too so the `[broken preview] …` shadow line aligns under the marker (single-line strings still get the prefix).

## 2. Blockquote-rail suppression (D2)

- [ ] 2.1 Add a helper in `gfm-pretty-blockquotes.el` that returns non-nil when a line is fully covered by a `link-previews` display overlay — walk `(overlays-in lbeg lend)`, check for `(gfm-pretty--registry-display gfm-pretty-link-previews--registry)` property.
- [ ] 2.2 In `gfm-pretty-blockquotes--apply-block-anchors` and `--apply-block-display`, skip per-line overlay creation when the helper reports the line is covered.
- [ ] 2.3 Ensure rebuild ordering: when link-previews lays a new overlay, schedule a blockquotes rebuild for the affected range so the rail gets dropped on the next idle tick (engine rebuild routes scoped vs full — verify the existing `:full-rebuild-required-p` predicate covers this; add a thin link-previews → blockquotes nudge if it doesn't).

## 3. Diff-mode fontification (D3)

- [ ] 3.1 Add `gfm-pretty-link-previews--fontify-diff` modelled on `--fontify-source`: temp buffer, `(diff-mode)`, insert lines, `font-lock-ensure`, `buffer-substring`.
- [ ] 3.2 Thread the fontified body through `--diff-preview-display` so the box-line builder sees `face`-propertised input.
- [ ] 3.3 Confirm `--box-line` / `--truncate-line-to-width` preserve `face` text properties on the kept head of each line.

## 4. RET dispatch (D4 + D5)

- [ ] 4.1 Lift `gfm-present--follow-source-link` and `gfm-present--follow-diff-link` into `gfm-pretty-link-previews.el` under `gfm-pretty-link-previews--follow-source-link` / `--follow-diff-link`.  Update `gfm-present.el` callers to the new names; remove the originals.
- [ ] 4.2 Define `gfm-pretty-link-previews-overlay-map` (a sparse keymap) binding `RET` and `<return>` to `gfm-pretty-link-previews-follow-link-at-point`.
- [ ] 4.3 Define `gfm-pretty-link-previews-follow-link-at-point`: find overlay at point by `gfm-pretty-link-previews-display` property; read `gfm-pretty-link-previews-kind`; re-parse URL via `--parse-source-link` / `--parse-diff-link`; push-mark; dispatch to source or diff follow.
- [ ] 4.4 Diff-follow fallback: when `magit-diff-range` is not `fboundp`, populate `*Diff*` via `call-process "git" "diff" "B...H" "--" P` from worktree; `pop-to-buffer` in `diff-mode`.
- [ ] 4.5 In `--apply-block`, attach `'keymap gfm-pretty-link-previews-overlay-map` to every preview overlay (alongside `display`, `evaporate`, kind tag).

## 5. Tests

- [ ] 5.1 Regression: list-item indent — assert overlay's `display` first / middle / last lines all start with two leading spaces for `- [foo](path#L1-L3)` on a `path` of 3 short lines.
- [ ] 5.2 Regression: blockquote indent — same shape for `> [foo](path#L1-L3)`.
- [ ] 5.3 Regression: indent-0 unchanged — a whole-line link's box starts at column 0 (no leading spaces).
- [ ] 5.4 Regression: blockquote-rail suppression — render a buffer with a `>` line containing a standalone source link and an adjacent `>` line without one; assert no `gfm-pretty-blockquotes` overlay sits on the preview line, but the adjacent `>` line keeps its rail.
- [ ] 5.5 Regression: diff body fontification — stub `call-process` to emit `+added\n-removed\n context\n`; render via `--diff-preview-display`; assert at least one position in the added line has a `face` deriving from `diff-added`, similarly for `diff-removed`.
- [ ] 5.6 Regression: RET on source overlay calls `find-file` with the resolved path and `goto-line` to start (use `cl-letf` to stub `find-file-noselect` + `pop-to-buffer`).
- [ ] 5.7 Regression: RET on diff overlay calls `magit-diff-range` when stubbed `fboundp`; falls back to the `*Diff*` buffer path when stub returns nil.
- [ ] 5.8 Regression: keymap is overlay-local — RET outside the preview overlay still hits `markdown-follow-link-at-point` (or whatever buffer binding) and not the decorator's dispatch.

## 6. Verification

- [ ] 6.1 `openspec validate polish-link-previews --strict` passes.
- [ ] 6.2 `make test` runs clean — including the narrowing-regression suite for sanity.
- [ ] 6.3 Eyeball the demo buffer: list-item, whole-line, blockquote previews align; diff preview shows `diff-added` / `diff-removed` colouring; RET on each preview opens the right destination.
