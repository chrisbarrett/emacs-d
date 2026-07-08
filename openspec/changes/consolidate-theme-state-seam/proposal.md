## Why

"The active theme's effective background" has no owner. The change hook
(`+theme-changed-hook`) lives in `modules/theme/lib.el`, but the cached
background (`+theme-default-background`) is defined and refreshed in
`modules/tty/init.el` — and `lisp/gfm/gfm-pretty-callouts.el`, a
nominally standalone library, reads that config-module variable through
a `boundp` guard and hooks `+theme-changed-hook` directly. Theme-state
knowledge is smeared across three modules and one library; a change to
theme internals can break tty, pulsar, and gfm independently.

## What Changes

- **theme module owns theme state**: `+theme-default-background` (the
  defvar and its refresh-on-change hook, at a depth earlier than tty's
  pass-through clear) moves from `modules/tty/init.el` to
  `modules/theme/lib.el`, alongside `+theme-changed-hook`. tty keeps
  only its own concerns: the sentinel clear and frame ornamentation.
- **gfm decouples from the config**: `gfm-pretty` gains a
  `gfm-pretty-background-function` defcustom (default: query the
  `default` face) and a library-owned `gfm-pretty-theme-change-hook`.
  Callouts reads the background via the function and registers its face
  refresh on the library hook; the direct `+theme-changed-hook` add-hook
  calls and `+theme-default-background` boundp-reads leave `lisp/gfm/`.
- **config bridges the seam once**: the theme module wires
  `+theme-changed-hook` to run `gfm-pretty-theme-change-hook` and sets
  `gfm-pretty-background-function` to read the cache. One bridge, in
  config, at the seam.
- Consumers inside config (`modules/ui/config/+pulsar.el`, tty's
  ornamentation painting) keep reading `+theme-default-background`;
  only its home moves.

## Capabilities

### New Capabilities

- `theme`: theme-change notification and effective-background cache —
  ownership of `+theme-changed-hook`, `+theme-default-background`, and
  the bridge to library-owned theme hooks. (Recognised axis; module
  exists at `modules/theme/`.)

### Modified Capabilities

- `tty`: the "theme background is cached for colour arithmetic"
  requirement is removed (ownership migrates to the `theme` axis; the
  ordering constraint — cache refresh before sentinel clear — is
  preserved in the theme spec).
- `gfm-pretty`: the "Theme change responsiveness" requirement changes —
  callouts registers its refresh on the library-owned hook and derives
  colours via `gfm-pretty-background-function` instead of the config's
  hook and variable.

## Impact

- `modules/theme/lib.el` — gains the cache defvar, refresh hook, and
  gfm bridge.
- `modules/tty/init.el` — loses the defvar and save hook; keeps clear
  hook and ornamentation.
- `lisp/gfm/gfm-pretty-callouts.el` — background reads and hook
  registrations swap to library-owned indirection; gfm becomes loadable
  outside this config with sane defaults.
- `modules/ui/config/+pulsar.el` — unchanged behaviour; hook it uses
  is unchanged.
- `openspec/specs/` — new `theme` spec; deltas on `tty` and
  `gfm-pretty`.
