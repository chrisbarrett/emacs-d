## Why

Now that link previews fire under plain `gfm-pretty-mode`, three
rendering issues are visible in everyday markdown buffers:

1. Previews under a list-item marker (`- [foo](path#L1-L2)`) render
   the box top after the `- ` but every body line and the bottom
   border start at column 0 — the box is jagged.
2. Previews inside a blockquote line (`> [foo](path#L1-L2)`) render
   the `gfm-pretty-blockquotes` left rail _and_ the preview box's
   left edge — two left borders.
3. Diff previews show plain text with no syntax colouring; `+`/`-`
   hunks are indistinguishable from context.  RET on a diff-range
   link outside `gfm-present-mode` falls through to `markdown-mode`
   which tries to follow `diff:base...head[#path]` as a relative
   file path and fails.

Goal: previews render and behave correctly anywhere
`gfm-pretty-mode` is on — not just inside a presentation slide.

## What Changes

- Box display indents body lines + bottom border to align under the
  box top when the preview's link is preceded by a list-item marker
  or a blockquote marker on the same line.  The marker glyph stays
  visible at the original column; the box hangs to its right.
- Suppress the `gfm-pretty-blockquotes` rail on lines covered by a
  `link-previews` overlay so the preview box owns the left edge on
  blockquoted preview lines.  Plain `>` lines outside a preview
  keep their rail unchanged.
- Run the diff body through `diff-mode` font-lock (same shape as
  source-range fontification) so added/removed/context lines pick
  up `diff-added` / `diff-removed` / `diff-context` faces.  The
  generated overlay display string carries those `face` properties.
- Add a `gfm-pretty-link-previews-follow-link` command bound to RET
  on every preview overlay's `keymap` text property.  Dispatch:
  source-range links open the target file at the start line via
  `find-file` (and pulse the range when `pulsar` is available);
  diff-range links open the underlying diff via `magit-diff-range`
  when available, otherwise a `*Diff*` buffer via `vc-diff`.  This
  routing lifts `gfm-present--follow-source-link` /
  `--follow-diff-link` behaviour onto the decorator.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: the `link-previews` decorator gains marker-aware
  indentation, blockquote-rail suppression, diff-mode fontification,
  and a RET dispatch contract.

## Impact

- `lisp/gfm/gfm-pretty-link-previews.el`: marker-aware indent in
  `--box-display` / `--apply-block`; diff fontification helper
  modelled on `--fontify-source`; overlay carries `keymap` with
  RET bound to a new follow command.
- `lisp/gfm/gfm-pretty-blockquotes.el`: skip the rail (anchor +
  display) on source lines fully covered by a `link-previews`
  overlay.  No requirement change for plain blockquotes.
- `lisp/gfm/gfm-pretty-link-previews-tests.el`: regression tests
  for list-item indent, blockquote-rail suppression, diff-body face
  properties, and RET dispatch to find-file / magit-diff-range
  stubs.
- `lisp/gfm/gfm-present.el`: no behavioural change.  The follow
  command is now usable outside presentation mode; presentation's
  own RET binding stays.
- No external dependencies added.  `magit-diff-range` lookup is
  best-effort via `fboundp`.
