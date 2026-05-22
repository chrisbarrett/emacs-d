## Why

Source-range and diff-range link previews in `gfm-present-mode` render as a
plain header-and-body string. They sit next to authored fenced code blocks
that have rounded box-drawing borders, language icons, and bordered footers
— and the visual mismatch reads as two unrelated kinds of content. Authors
referencing live source on a slide expect the preview to look like the code
block it conceptually *is*.

## What Changes

- Source-range previews (`<path>#L<a>-L<b>`) render inside a box-drawing
  border that matches authored fenced code blocks. The abbreviated source
  path and line range live in the top border; a truncation notice lives in
  the bottom border when the requested range exceeds the line cap.
- Diff-range previews (`diff:<base>...<head>[#<path>]`) get the same box
  treatment via a shared rendering helper. The diff body uses
  LHS-margin mode (first column is the `+`/`-`/` ` indicator, no left
  padding inside the box) so it visually matches authored ` ```diff `
  fenced blocks.
- Preview decoration triggers only on **standalone** links — links that
  occupy a whole line, optionally inside a list item or blockquote.
  Inline links inside prose are left undecorated. (Currently every
  source / diff link in the slide gets a preview, including ones mid-
  sentence — visually breaks prose.)
- The markdown label (`[label]` text) is dropped from the preview
  surface. The bordered path identifies the source unambiguously.
- File-not-found and invalid-range states render as a bare
  shadow-faced sentinel (no box), distinct from healthy previews.
- The path shown in the top border is project-relative when the source
  is inside a project (resolved against the source file's directory,
  not the presentation buffer's), with fallback to `~/`-abbreviation
  and leading-ellipsis truncation to fit the box.
- Box width follows the same formula as authored fences:
  `min(window-width, max(80, longest-body-line + decoration))`.
- Body lines exceeding the content budget are truncated with an
  ellipsis. The preview is a preview — full content is one RET away.

## Capabilities

### Modified Capabilities

- `gfm-present`: rewrites the rendering shape of the source-range and
  diff-range preview overlays, adds a standalone-link gating
  requirement, drops the `[label]` from preview output, and changes
  error-state rendering to a bare sentinel.

## Impact

- `lisp/gfm/gfm-present.el`:
  `gfm-present--source-preview-display`,
  `gfm-present--diff-preview-fence`, and
  `gfm-present--render-link-previews` rewritten.
  New shared helper `gfm-present--box-display` (or similar) extracted.
  New path-abbreviation helper.
  Standalone-link predicate.
- `lisp/gfm/gfm-present-tests.el`: existing tests against
  `gfm-present--source-preview-display` and
  `gfm-present--diff-preview-fence` output shape are rewritten to
  match new structure. New tests cover the standalone gating, path
  abbreviation, ellipsis truncation, and bare-sentinel error states.
- `gfm-pretty-border-face` (from `gfm-pretty-borders`) reused for
  border styling; no new face introduced.
- No buffer-text changes; rendering remains entirely overlay-based.
