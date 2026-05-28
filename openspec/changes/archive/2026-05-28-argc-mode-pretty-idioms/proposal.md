## Why

`argc-mode`'s box overlay predates the `gfm-pretty` design language and
shows its age: borders are sized once buffer-wide rather than per window,
soft-wrapped continuation visual rows lose the left rail and right
border, the border face leaks italic/underline styling from underlying
prose font-lock, and sibling overlays with `:extend t` (`region`,
`hl-line`, `diff-added`) bleed past the right border. Cherry-picking
four idioms from `gfm-pretty` fixes all four issues without taking on
the engine, the anchor/display split, reveal, or selection variants.

## What Changes

- Border sizing uses `(space :align-to right)` / `(- right N)` so each
  displaying window renders the box flush to its own right edge,
  replacing the fixed `max(80, max-line-length + 4)` buffer-wide width.
- Per-line overlays gain a `wrap-prefix` property carrying the left
  rail (`│ `) so soft-wrapped visual continuation rows continue the
  rail instead of losing it.
- The right-edge after-string gains a trailing `(space :align-to right)`
  segment painted in the `default` face, masking past-EOL `:extend t`
  leaks from `region`, `hl-line`, `diff-added`, etc.
- `argc-box-face` is rendered through a normalised face spec that
  clears `:slant`, `:underline`, `:overline`, `:strike-through`, `:box`
  and pins `:background "unspecified-bg"`, so border glyphs no longer
  inherit prose styling or pick up text-property backgrounds.

Out of scope (explicitly): the `gfm-pretty` engine, decorator registry,
anchor/display split, per-window display overlays, scoped rebuilds,
reveal-on-cursor, and selection-aware variant swap. `argc-mode` keeps
its existing lifecycle: `after-change-functions` + 0.2 s idle-timer
debounce + full rebuild.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `argc-mode`: the box-rendering requirement gains per-window width via
  `:align-to right`, the `wrap-prefix` continuation rail, the past-EOL
  mask tail, and the normalised border face. The buffer-wide
  `max(80, max-line-length + 4)` width rule is removed.

## Impact

- `lisp/argc-mode/argc-mode.el`: rewrite `argc--make-border` and
  `argc--apply-box-overlays`; add normalised-face helper.
- `lisp/argc-mode/argc-mode-tests.el`: update box-width scenarios; add
  wrap-prefix / past-EOL-mask / normalised-face assertions.
- No new dependencies. `argc-mode` does NOT gain a dependency on the
  `gfm-pretty` libraries — the four idioms are inlined.
