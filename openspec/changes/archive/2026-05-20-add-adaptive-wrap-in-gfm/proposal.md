## Why

With `visual-line-mode` enabled, continuation lines of soft-wrapped paragraphs, list items, and blockquotes flush to column 0 — losing the structural indent that makes nested Markdown legible. The third-party `adaptive-wrap-prefix-mode` (GNU ELPA) solves this by setting `wrap-prefix` to each line's existing indent, so wrapped fragments align under their content column. Adopting it is a small-surface step that addresses the indent half of the wrapping problem without building anything bespoke.

## What Changes

- Add `adaptive-wrap` to the project's elpaca dependencies.
- Enable `adaptive-wrap-prefix-mode` on `gfm-mode-hook` alongside the existing `visual-line-mode` hook.
- Leave overlay-based `wrap-prefix` set by `gfm-pretty` decorators (callouts, fences) untouched — overlay properties take precedence over the text properties that `adaptive-wrap` installs. (Tables do not set `wrap-prefix`, so the precedence rule does not apply there; their overlays are independent.)

Out of scope: virtually joining soft newlines inside a paragraph (the "reflow" half of the original idea). That remains a possible follow-up under a separate proposal.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `lang-markdown`: add a requirement that GFM buffers enable adaptive wrap-prefix indentation on visual-line continuation rows.

## Impact

- `modules/lang-markdown/init.el`: one extra `:hook` entry; one extra `use-package` form for `adaptive-wrap`.
- `modules/lang-markdown/packages.eld`: declare the new package.
- No code in `lisp/gfm/`. The gfm-pretty engine and decorators are not touched. Overlay-based `wrap-prefix` continues to win over text-property `wrap-prefix` at display time, preserving callout / fence / table rendering.
- New runtime dependency: `adaptive-wrap` (GNU ELPA).
