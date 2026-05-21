## Why

The fences decorator's per-line right-edge mask (a stretch glyph in
`default` face inside each body overlay's `after-string`) was introduced
to stop `:extend t` text-property faces (e.g. `diff-added`,
`diff-removed`) from leaking past the box's right border. It works for
that, but it also masks every other `:extend t` face — including the
`region` face used by vanilla selection and evil's visual state. The
result: a `V`-line selection over text inside a fenced block visibly
clips at the last column of text instead of extending to the window's
right edge, breaking the "whole line is selected" reading.

## What Changes

- Region highlight inside fenced, indented, and YAML-helmet code
  blocks SHALL extend to the window's right edge on selected body
  lines, matching the behaviour outside src blocks.
- Diff-bg clipping past the box's right border SHALL be preserved for
  unselected lines.
- Add a buffer-local `post-command-hook` (on fences enable) that swaps
  each visible body overlay's `after-string` between a "masked"
  variant (current behaviour) and a "bare" variant (no past-EOL
  paint) based on whether the line overlaps the active selection.
- Detect selection bounds from both vanilla (`use-region-p` +
  `region-beginning` / `region-end`) and evil's linewise/charwise
  visual state (`evil-visual-beginning` / `evil-visual-end`).
  Visual-block selection is unaffected and uses the masked variant.
- Body overlay creation in `--apply-bordered-display` and
  `--apply-indent-display` SHALL precompute and stash both
  after-string variants on the overlay so the swap works after
  rebuilds.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: revise the "Code-fence body background fill" requirement
  to permit region-driven past-EOL fill on selected body lines while
  retaining the diff-bg clipping behaviour for unselected lines.

## Impact

- Code: `lisp/gfm/gfm-pretty-fences.el` (new helper, hook wiring, body
  overlay creation in both fenced and indent paths).
- Tests: `lisp/gfm/gfm-pretty-tests.el` — loosen the existing
  "after-string ends with default tail" assertions to cover only the
  unselected path; add coverage for the selected path.
- No public-API changes. No new dependencies. Evil is optional —
  detection uses `bound-and-true-p`.
- Out of scope: `gfm-pretty-callouts.el` has the same mask shape but
  is left untouched in this change to keep scope small. A separate
  follow-up can mirror the fix there if the same visual artifact
  shows up inside callout bodies.
