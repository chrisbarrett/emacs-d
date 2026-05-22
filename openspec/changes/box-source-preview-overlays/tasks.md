# Tasks

## 1. Shared helpers

- [ ] 1.1 Add `gfm-present--abbrev-source-path` (absolute path → project-relative or `~/`-abbreviated). Decide+document return value when path itself is `nil` or empty (return as-is).
- [ ] 1.2 Add `gfm-present--fit-label-into-border` (label string + budget → label possibly leading-ellipsis-truncated to fit, preserving the trailing `:start-end` range and basename).
- [ ] 1.3 Add `gfm-present--truncate-line-to-width` (propertised line + cell budget → propertised line truncated to budget with `…`, measured via `string-width`).
- [ ] 1.4 Add `gfm-present--standalone-link-p` (link match-data → bool). Predicate matches the regex described in the design.
- [ ] 1.5 Write red tests for 1.1–1.4 (one happy + one edge each).

## 2. Box renderer

- [ ] 2.1 Add `gfm-present--box-display` taking `(:label :body :extra :lhs-margin)` returning a propertised multi-line string. Computes box width via `gfm-pretty--available-width` + `max(80, longest-body-line + decoration-w)`; builds top/bottom borders with embedded label/extra; per-line `│ `/` │` (or `│`/`│`) decoration; per-line truncation via 1.3.
- [ ] 2.2 Top border builder: `┌─ <label> ─…─┐` filling to box width with `─` in `gfm-pretty-border-face`.
- [ ] 2.3 Bottom border builder: `└─ +N more lines ─…─┘` when `extra > 0`; bare `└─…─┘` otherwise.
- [ ] 2.4 Red tests: corner glyphs present, label embedded, width respected, LHS-margin mode reduces decoration to 2 cols, truncation footer absent when `extra = 0`.
- [ ] 2.5 Green: implement until 2.4 passes.

## 3. Source-range preview rewrite

- [ ] 3.1 Rewrite `gfm-present--source-preview-display` to:
  - Resolve abbreviated path via 1.1.
  - Build label `<abbrev-path>:<start>-<end>`.
  - Read body via existing `gfm-present--read-line-range` (unchanged).
  - Fontify body via existing `gfm-present--fontify-source` (unchanged).
  - Call `gfm-present--box-display` with `:lhs-margin nil`.
- [ ] 3.2 Error states (`file-not-found`, `invalid-range`) render as bare shadow-faced sentinel `[broken preview] <abbrev-path>:<start>-<end> — <reason>`. No box.
- [ ] 3.3 Drop the `LABEL` parameter from the helper's call sites (proposal: label is dropped). Keep the signature back-compat-clean — the caller in `--render-link-previews` already has `label` in scope but should stop passing it.
- [ ] 3.4 Rewrite tests in `gfm-present-tests.el` covering: corner glyphs, abbreviated path in top border, range fused with `:start-end`, label absence, ellipsis truncation, bare sentinel for error states.

## 4. Diff-range preview rewrite

- [ ] 4.1 Add `gfm-present--abbrev-diff-refs` helper: SHA-shorten any ref matching `(rx bos (= 40 hex) eos)` to first 7 chars; pass branches/tags through.
- [ ] 4.2 Rewrite `gfm-present--diff-preview-fence` (rename suggested: `gfm-present--diff-preview-display`) to:
  - Build label `<base>...<head>` or `<base>...<head> — <path>` via 4.1.
  - Run existing `gfm-present--run-diff-preview` (unchanged).
  - Call `gfm-present--box-display` with `:lhs-margin t`.
- [ ] 4.3 Error states (`no-changes`, git non-zero exit) render as bare shadow-faced sentinel `[broken preview] <base>...<head>[<path>] — <reason>`. No box.
- [ ] 4.4 Rewrite tests in `gfm-present-tests.el` covering: corner glyphs, base...head in top border, em-dash before path qualifier, SHA shortening, LHS-margin body shape, bare sentinels for empty/error states.

## 5. Standalone-link gating

- [ ] 5.1 Modify `gfm-present--render-link-previews` to call `gfm-present--standalone-link-p` between match discovery and preview-overlay creation. Non-standalone matches: no preview overlay.
- [ ] 5.2 Tests: whole-line link gets preview; list-item-only gets preview; blockquote-only gets preview; mid-prose link does not; link with trailing text on its own line does not.

## 6. Integration + cleanup

- [ ] 6.1 Verify the existing `gfm-pretty-links` decorator's `skip-url-p` predicate (`lisp/gfm/gfm-pretty-links.el:159`) still correctly excludes source-range and diff-range URLs so they don't get title-side decoration on top of preview overlays.
- [ ] 6.2 Search the repo for callers of the renamed/removed functions; update or remove.
- [ ] 6.3 Run `make test` and confirm narrowing-regression suite still passes.
- [ ] 6.4 Smoke test in a real presentation buffer (open `/private/tmp/presentations/2026-05-22T06-33-tg-native-drift-filters.md`, navigate slides, observe boxed previews; resize window to verify graceful degradation; test bullet-listed link group).
- [ ] 6.5 Sync deltas to stable spec: `openspec sync box-source-preview-overlays` once tests are green.
