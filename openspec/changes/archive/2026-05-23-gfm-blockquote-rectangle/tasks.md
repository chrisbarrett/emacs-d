## 1. Face + defcustom scaffolding

- [x] 1.1 Add `gfm-pretty-blockquotes-bg-face` defface (light `#f6f1e6`, dark `#262637`, `:extend t`) to `lisp/gfm/gfm-pretty-blockquotes.el`.
- [x] 1.2 Add `gfm-pretty-blockquotes-inset-cols` defcustom (default the symbol `tab-width`, type `(choice integer symbol)`).
- [x] 1.3 Write a failing ERT asserting `(face-background 'gfm-pretty-blockquotes-bg-face nil t)` equals `(face-background 'gfm-pretty-tables-row-alt-face nil t)` at definition time.
- [x] 1.4 Write a failing ERT asserting the defcustom resolves to the current buffer's `tab-width` when at its default.

## 2. Anchor overlay rewrite (RED → GREEN)

- [x] 2.1 Write failing ERT asserting the per-line anchor's `face` property is an anonymous spec containing `(:background <bg-of-bg-face> :extend t)`.
- [x] 2.2 Write failing ERT asserting the anchor's `wrap-prefix` is a 3-segment propertised string of length `(+ inset-cols 2)`, where the first `inset-cols` chars carry default face, char `inset-cols` is `▌` with rail-face + bg face's bg, and char `inset-cols + 1` is ` ` with bg face's bg.
- [x] 2.3 Rewrite `gfm-pretty-blockquotes--apply-block-anchors` to produce both overlays per line.
- [x] 2.4 Confirm RED tests turn GREEN.

## 3. Display overlay rewrite (RED → GREEN)

- [x] 3.1 Write failing ERT for the two-char `> ` swap: display string is `<inset-spaces>▌<space>` with the 3-segment face propertisation from 2.2; overlay covers exactly the 2 source chars.
- [x] 3.2 Write failing ERT for the one-char bare `>` swap: display string is `<inset-spaces>▌` (no trailing space); overlay covers exactly the 1 source char.
- [x] 3.3 Write failing ERT for the rhs after-string on a non-overflowing body line: contains `(space :align-to (+ inset-cols box-width - 2))` in a bg-tinted span, then `"  "` in bg-tinted span, then `(space :align-to right)` in `default` face; carries the `cursor` text property on its first char.
- [x] 3.4 Write failing ERT for the rhs after-string on an overflowing body line: pad length is derived from `gfm-pretty--simulate-wrap` with prefix width `(+ inset-cols 2)`, terminates with the default-face `align-to right` span.
- [x] 3.5 Rewrite `gfm-pretty-blockquotes--apply-block-display` to produce per-window prefix-display + per-window rhs after-string per body line.
- [x] 3.6 Confirm RED tests turn GREEN.

## 4. Reveal: masked↔bare wiring

- [x] 4.1 Write failing ERT asserting each prefix-display overlay carries `gfm-pretty-display-masked` (inset+rail) and `gfm-pretty-display-bare` (source `> ` with region bg) properties.
- [x] 4.2 Write failing ERT asserting each rhs after-string overlay carries `gfm-pretty-after-masked` and `gfm-pretty-after-bare` properties.
- [x] 4.3 Write failing ERT asserting that with point on a blockquote line, the prefix display flips to `gfm-pretty-display-bare` (source visible) and the rhs after-string flips to `gfm-pretty-after-bare` (region tail appended).
- [x] 4.4 Add the paired props in `--apply-block-display`; engine's existing reveal walker handles the flip.
- [x] 4.5 Confirm RED tests turn GREEN.

## 5. Update existing test assertions

- [x] 5.1 Update `lang-markdown/gfm-pretty-blockquotes-plain-renders-rail-prefix-and-wrap` to expect the new `<inset-spaces>▌<space>` display string and wrap-prefix.
- [x] 5.2 Update `lang-markdown/gfm-pretty-blockquotes-bare-gt-produces-one-char-rail` to expect `<inset-spaces>▌`.
- [x] 5.3 Update `lang-markdown/gfm-pretty-blockquotes-wrap-prefix-overlay-wins-over-text-prop` for the new wrap-prefix string.
- [x] 5.4 Update `lang-markdown/gfm-pretty-blockquotes-reveal-swaps-display` to assert the masked/bare flip on the prefix display.

## 6. Visual verification

- [x] 6.1 Open `/private/tmp/presentations/2026-05-22T06-33-tg-native-drift-filters.md` in a running Emacs (or the sandboxed daemon if risk warrants), enable `gfm-pretty-mode`, and confirm: tinted rectangle present, inset visible as untinted gutter, rail glyph at column `tab-width`, no right border glyph, no leak past the rectangle's right edge with `hl-line-mode` active.
- [x] 6.2 Confirm soft-wrap continuation rows show the same inset + rail and tinted body.
- [x] 6.3 Confirm point-on-line drops the inset+rail and exposes raw `> ` in only the selected window.
- [x] 6.4 Confirm a bare `>` continuation line renders as a tinted row with rail glyph but no trailing space.

## 7. Specs + checks

- [x] 7.1 Run `openspec validate gfm-blockquote-rectangle --strict`.
- [x] 7.2 Run `make test` (full suite) and confirm zero failures.
- [x] 7.3 Run the narrowing regression tag (`make test-integration` or the `:tags '(narrowing-regression)` subset) to confirm the new overlays converge under narrow → rebuild → widen → rebuild.

## 8. Memory + docs

- [x] 8.1 If any design decision proves surprising in practice (e.g. an unforeseen interaction with `:extend t`), capture it as a feedback memory entry before archiving.
