## Why

When stepping through a slide deck in `gfm-present-mode`, there is no
visual cue for where the user is in the document.  Counting H1s by hand
breaks the flow of a walkthrough.  A persistent `n/m` header turns the
narrowed slide into a self-locating frame.

Separately, `C-x n w` (`widen`) currently leaves the user in
presentation mode with a widened buffer — an inconsistent state where
the slide frame is lost but the keymap is still active.  Remapping
`widen` to disable the mode makes the gesture do what the user means.

## What Changes

- Add a buffer-local header line to `gfm-present-mode` buffers that
  shows `<current-slide>/<total-slides>` (1-based, left-aligned).
- Refresh the counter at known transition points only: mode enable,
  `gfm-present-next-slide`, `gfm-present-previous-slide`,
  `gfm-present-follow-link` slug branch, anchor-jump subscription,
  and revert restore.
- Hide the header (`header-line-format` stays nil) when the document
  has no H1s or point is before the first H1.
- Add `[remap widen]` to `gfm-present-mode-map` bound to a command
  that disables `gfm-present-mode` (the disable hook widens).
- Mirror the remap into the evil overriding-map plumbing so it works
  from evil normal/motion/visual states.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-present`: Adds a slide-count header line requirement
  (behaviour-facing); modifies the existing keymap requirement to
  include the `widen` remap.

## Impact

- `lisp/gfm/gfm-present.el`: new buffer-local var for the counter,
  refresh helper called from existing transition points, header-line
  format string, remap entry in `gfm-present-mode-map`, mirrored
  evil binding.
- `lisp/gfm/gfm-present-tests.el`: scenarios for header content,
  refresh on each transition, no-H1 hiding, widen remap behaviour.
- `openspec/specs/gfm-present/spec.md`: gains one requirement, one
  requirement modified (on archive).
