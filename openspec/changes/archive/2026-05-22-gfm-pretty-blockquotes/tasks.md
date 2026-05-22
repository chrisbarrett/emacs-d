## 1. Decorator skeleton

- [x] 1.1 Create `lisp/gfm/gfm-pretty-blockquotes.el` with module header, `(require 'cl-lib)`, `(require 'gfm-pretty-borders)`, `(require 'gfm-pretty-engine)`.
- [x] 1.2 Define `defgroup gfm-pretty-blockquotes` under `markdown-faces`.
- [x] 1.3 Define `gfm-pretty-blockquotes-rail-face` with `'((t :inherit shadow))`.
- [x] 1.4 Define registry via `gfm-pretty--registry-for 'blockquotes 'gfm-pretty-blockquotes`.
- [x] 1.5 Define `--make-anchor`, `--make-display`, `--remove-overlays`, `--register`, `--prune-dead-overlays` thin wrappers mirroring `gfm-pretty-callouts.el:125-146`.

## 2. Discovery

- [x] 2.1 Define `gfm-pretty-blockquotes--blockquote-line-re` as `(rx bol ">")`.
- [x] 2.2 Reuse `gfm-pretty-callouts--marker-re` (require `gfm-pretty-callouts` for marker detection) to identify callout-owned runs.
- [x] 2.3 Implement `gfm-pretty-blockquotes--find-blocks`: widen + walk, group consecutive `^>` lines into `(BEG END)` blocks, drop any run containing a `> [!TYPE]` marker line.
- [x] 2.4 Define `cl-defstruct gfm-pretty-blockquotes--block` with `range` and `payload` slots, mirroring callouts.
- [x] 2.5 Implement `gfm-pretty-blockquotes--collect-blocks` returning tagged blocks for the engine.

## 3. Rendering

- [x] 3.1 Implement `gfm-pretty-blockquotes--apply-block-anchors`: per-line anchor with `wrap-prefix` propertised as `"│ "` in rail face. Use widened envelope.
- [x] 3.2 Implement `gfm-pretty-blockquotes--apply-block-display`: per-line per-window display overlay. Dispatch on `(- lend lbeg)`: `>= 2` → display `"│ "` over `[lbeg, lbeg+2]`; `= 1` (bare `>`) → display `"│"` over `[lbeg, lbeg+1]`. Tag with `gfm-pretty-blockquotes-kind 'rail-prefix`, `gfm-pretty-blockquotes-revealable t`, `evaporate t`, `gfm-pretty-display-masked` / `gfm-pretty-display-bare` for the reveal walker.
- [x] 3.3 Implement `gfm-pretty-blockquotes--apply-block` that routes through `gfm-pretty-borders--apply-with-anchors` with the anchor and display fns.

## 4. Engine integration

- [x] 4.1 Implement `gfm-pretty-blockquotes--block-visible-p` via `gfm-pretty--block-visible-p`.
- [x] 4.2 Implement `gfm-pretty-blockquotes--full-rebuild-required-p` OR-combining: dirty overlaps first column of any blockquote line; dirty overlaps line directly above or below a blockquote block; dirty overlaps a `> [!TYPE]` marker line (covers callout-to-plain transitions).
- [x] 4.3 Implement no-op `--on-enable` / `--on-disable` lifecycle hooks.
- [x] 4.4 Register decorator via `gfm-pretty-define-decorator 'blockquotes` inside `with-eval-after-load 'gfm-pretty-engine`, supplying all engine seams.

## 5. Wire into module

- [x] 5.1 In `modules/lang-markdown/init.el`, add `(require 'gfm-pretty-blockquotes)` next to the callouts require, gated on `gfm-pretty-engine` availability.
- [x] 5.2 Confirm `lisp/+autoloads.el` regenerates correctly (if any cookies are needed for face / mode entry).

## 6. Tests

- [x] 6.1 In `lisp/gfm/gfm-pretty-tests.el` (or sibling), add a sandboxed-Emacs test that asserts a plain `> body` line renders a `│ ` display overlay at the prefix and a `gfm-pretty-blockquotes-rail-face` `wrap-prefix` anchor.
- [x] 6.2 Add a test that two blockquotes separated by a blank line yield two distinct blocks via `--find-blocks`.
- [x] 6.3 Add a test that a `> [!NOTE]\n> body` run is excluded from `--find-blocks`.
- [x] 6.4 Add a test for bare `>` line: 1-char display overlay producing `│` (not `│ `).
- [x] 6.5 Add a soft-wrap test: long `> ` line in an 80-col window with `visual-line-mode`; assert wrap-prefix overlay wins over markdown-mode's `wrap-prefix "> "` text property (use `get-char-property` to verify).
- [x] 6.6 Add reveal test: place point on a blockquote line in a sandbox window, assert the display overlay swaps to bare; move point off, assert it returns to masked.
- [x] 6.7 Add narrowing-regression test under `:tags '(narrowing-regression)`: narrow to mid-block, rebuild, widen, rebuild; assert overlay set converges with a clean widened rebuild.

## 7. Verification

- [x] 7.1 Run `make test-quick`; confirm green.
- [x] 7.2 Run `make test-integration` covering the narrowing-regression tag.
- [x] 7.3 Manually open `/private/tmp/presentations/2026-05-22T06-33-tg-native-drift-filters.md` with `visual-line-mode` enabled and confirm the `> Pain:` blockquote renders as a continuous left rail across all wrapped visual rows.
- [x] 7.4 Run `openspec validate gfm-pretty-blockquotes`; confirm no errors.
