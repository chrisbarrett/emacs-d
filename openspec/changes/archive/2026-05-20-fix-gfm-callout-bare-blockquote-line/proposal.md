## Why

Bare `>` blockquote lines inside GFM callouts (a `>` with no trailing space —
commonly used to render a blank row between body paragraphs) leak through
undecorated: the raw `>` character is visible where the left-edge `│` should
sit. The body-prefix substitution gates on `> ` (two chars) and skips the
one-char form. Tinted background and right-edge `│` still attach, so the
result is a visibly broken row mid-box.

## What Changes

- Extend the callouts decorator's body-prefix substitution to cover a bare
  `>` line in addition to `> ` lines. The display string remains `│ `; the
  source range is 1 char for bare `>`, 2 chars for `> `.
- Add a narrowing-safe regression test exercising a callout with a bare-`>`
  body row.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: tighten the callout bordered-block rendering requirement so
  the left-edge `│` substitution covers any `>`-prefixed body line, not just
  `> `-prefixed ones.

## Impact

- `lisp/gfm/gfm-pretty-callouts.el` — body-prefix branch in
  `gfm-pretty-callouts--apply-block-display`.
- `lisp/gfm/gfm-pretty-tests.el` — new ert deftest.
- `openspec/specs/gfm-pretty/spec.md` — delta on "Callout bordered-block
  rendering" requirement.
- No effect on block discovery, anchor laying, right-edge padding, reveal
  walker, or box-width math.
