## Why

`gfm-pretty-links` scans the whole buffer for link shapes and
decorates anything matching `markdown-regex-link-inline`,
`-link-reference`, `-angle-uri`, `-uri`, etc. The scan does not
exclude markdown's code regions, so a literal `[foo](bar)` written
inside a fenced code block, an indented code block, or an inline
`` `code` `` span gets rendered as a link — title text replaces
brackets, the url-side icon appears, RET dispatches. In code that
syntax is *not* a link, just text.

The decorator already excludes table cells and reference-definition
lines for similar reasons. Code regions are the remaining gap.

## What Changes

- `:collect` SHALL skip any link shape whose start position lies
  inside a markdown code region — fenced block, indented (pre)
  block, or inline code span. The decision uses markdown-mode's
  `markdown-code-block-at-pos` (pre + fenced + indented) and
  `markdown-inline-code-at-pos-p` (inline).
- No new faces, no new RET behaviour, no new spec axes.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: one new requirement under the links decorator
  documenting code-region exclusion in `:collect`.

## Impact

- `lisp/gfm/gfm-pretty-links.el` — central predicate
  `gfm-pretty-links--in-code-p` and a single filter step inside
  `gfm-pretty-links--blocks-in-range`. Each per-shape scanner stays
  unchanged.
- `openspec/specs/gfm-pretty/spec.md` — one ADDED requirement
  alongside the existing "Overlay decoration does not skew column
  widths" pattern.
- `lisp/gfm/gfm-pretty-tests.el` — tests for fenced / indented /
  inline-code exclusion.
- No external dependencies change. This sits on
  markdown-mode's text-property + matcher infrastructure.
