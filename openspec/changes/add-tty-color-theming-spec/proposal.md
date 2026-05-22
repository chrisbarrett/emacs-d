## Why

`modules/tty/` carries non-obvious colour-handling logic — the
`"unspecified-bg"` sentinel pattern, the `+theme-default-background`
cache, and per-package overrides that prevent third-party colour
arithmetic from feeding the sentinel into colour resolvers. None of
this is captured as a spec, so contributors (and future-me) cannot
tell whether changes regress the contract.

A concrete leak motivates the work: switching between `*Messages*` and
any markdown buffer reliably writes `Unable to load color
"unspecified-bg" [N times]` into the messages log. The string is
stamped onto `fringe` / `window-divider*` `:foreground` by
`early-init.el`, and the always-alive macOS daemon frame's
`ns_defined_color` (unlike `tty_defined_color`) has no special case
for the sentinel — every pulse-animation redisplay forces re-realise
across all alive frames and surfaces the warning.

## What Changes

- **New axis** — adds `tty` capability spec covering colour-theming
  invariants on tty frames: the `default` bg sentinel pattern, the
  cache contract, the resolve-original advice protocol for 3rd-party
  colour callers, and the "never as `:foreground`" rule.
- **Bug fix** — `early-init.el +sync-frame-parameters` reads
  `+theme-default-background` (or no-ops when the cache holds an
  `"unspecified"` value) rather than the raw `(face-attribute 'default
  :background)`. Removes the leak path that paints "unspecified-bg"
  onto fringe / window-divider faces.
- **Regression test** — asserts that pulsing on a tty frame whose
  daemon also has an NS frame emits no `Unable to load color` line.

## Capabilities

### New Capabilities

- `tty`: terminal-frame compatibility for `modules/tty/` — clipboard,
  mouse, box-drawing, and the colour-theming sentinel + cache
  contract that lets third-party blending code interoperate with
  `"unspecified-bg"`-pinned default faces.

### Modified Capabilities

<!-- none -->

## Impact

- `early-init.el` — one-line read source swap in
  `+sync-frame-parameters`.
- `modules/tty/init.el` — no behavioural change; the spec ratifies
  existing invariants and the package-specific advice attached there.
- `modules/tty/tests.el` — new regression test exercising the
  pulse-on-tty path with a live graphic frame in the same daemon.
- New spec file `openspec/specs/tty/spec.md`.
