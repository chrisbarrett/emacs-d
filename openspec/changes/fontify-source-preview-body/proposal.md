## Why

Source-range previews render the body as a plain `display` string
shaped like a markdown fenced code block. Emacs treats `display`
strings as text — it doesn't re-fontify them as markdown — so the
visible result is:

- the literal ` ``` ` fence delimiters on screen
- the language tag (` ```text ` for unrecognised extensions) on
  screen
- no syntax highlighting in the body

Live evidence in `2026-05-22T06-33-tg-native-drift-filters.md`:
overlay display string starts ` ```text /Users/.../drift.core.yml…
\njobs:\n drift:\n …` — the YAML body shows but with zero
highlighting, and the fence header reads as raw characters
wrapped across multiple visual lines.

Additionally, `.yml` is missing from `gfm-present--ext-to-lang`
(gfm-present.el:288-296), so most infra/config snippets default to
`text` even with the broken fence approach.

## What Changes

- `gfm-present--source-preview-fence` SHALL build the body by:
  1. Resolving the file extension to a major-mode via
     `auto-mode-alist` (whatever Emacs would use for `find-file`).
  2. Inserting the read lines into a temp buffer with that
     major-mode active.
  3. Calling `font-lock-ensure` on the temp buffer.
  4. Returning the propertised `buffer-substring` (text properties
     including `face` survive into the display string).
- The display string SHALL drop the ` ``` ` fence delimiter
  characters. The header line continues to read
  `<label> · <path>:<start>-<end>` propertised with a header face
  (e.g. `markdown-code-face` inherit). The body is the fontified
  source. The optional footer line stays.
- Static `gfm-present--ext-to-lang` alist is removed.
  Auto-mode-alist is the single source of truth for file→mode
  mapping. Tree-sitter modes (`yaml-ts-mode`, `js-ts-mode`, etc.)
  are picked up automatically when configured upstream.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `gfm-present`: `Source-range link preview overlay` requirement
  changes how the preview body is rendered — propertised text via
  major-mode fontification, not a markdown fence string. Surface
  contract (overlay on full link, display property, footer for
  oversize ranges) is preserved.

## Impact

- `lisp/gfm/gfm-present.el` —
  - `gfm-present--source-preview-fence` rewritten to fontify via
    temp buffer.
  - New helper `gfm-present--major-mode-for-path` that consults
    `auto-mode-alist`.
  - `gfm-present--ext-to-lang` constant removed.
  - `gfm-present--build-preview-fence` either inlined or renamed
    (the "fence" semantics no longer apply).
- `lisp/gfm/gfm-present-tests.el` — preview tests update from
  "fence string contains body" to "display string contains
  propertised body with `face` property on tokens". Adds a test
  asserting `.yml` resolves to YAML-mode fontification.
- No new dependencies on top of Emacs built-ins
  (`font-lock-ensure`, `auto-mode-alist`).
- Performance: each preview spawns a temp buffer + font-lock pass.
  Cap of 10 lines per preview keeps cost low. No noticeable user-
  facing latency expected for typical slides (≤ 10 previews).
- Depends on `skip-source-links-in-pretty` landing first — otherwise
  the improved preview is hidden beneath the pretty-links overlay
  stack.
