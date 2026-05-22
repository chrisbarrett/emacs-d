## Why

RET on a `gfm-pretty-links` anchor link fails with `user-error "No heading
matches anchor: #<slug>"` whenever the buffer is narrowed and the target
heading lives outside the visible region. This breaks every TOC link in
`gfm-present-mode` (which narrows to one slide at a time) and any plain
`M-x narrow-to-region` flow.

## What Changes

- `gfm-pretty-links--jump-to-anchor` widens the buffer for the heading
  search so anchors resolve regardless of current narrowing.
- After a successful jump, the function widens the buffer, moves point to
  the target heading, then runs a new abnormal hook
  `gfm-pretty-links-after-anchor-jump-functions` with the target position.
  Other modes hook in to restore their preferred narrowing.
- `gfm-present-mode` registers a buffer-local hook function that re-narrows
  to the H1 region containing the target heading, preserving slide framing
  after the jump.
- The slug branch of `gfm-present-follow-link` remains as a fallback for
  buffers where `gfm-pretty-links` is not active.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `gfm-pretty`: `RET follows the link when point is on the decoration`
  gains a narrowing-safety invariant and a public abnormal-hook extension
  point fired after a successful anchor jump.
- `gfm-present`: heading-text in-doc link follow gains a scenario covering
  the pretty-links hook path — when both modes are on, the user lands at
  the target heading narrowed to its slide region.

## Impact

- `lisp/gfm/gfm-pretty-links.el` — `gfm-pretty-links--jump-to-anchor`
  widens for search and goto; new public hook variable.
- `lisp/gfm/gfm-present.el` — mode enable adds the buffer-local hook
  function; mode disable removes it.
- `lisp/gfm/gfm-pretty-links-tests.el` — new ERT test for narrowing-safe
  jump and hook invocation.
- `lisp/gfm/gfm-present-tests.el` — new ERT test for the present-mode
  re-narrow behaviour.
- No new dependencies. No user-facing key changes. Behaviour change is
  strictly additive — pre-fix behaviour under widened buffers is preserved.
