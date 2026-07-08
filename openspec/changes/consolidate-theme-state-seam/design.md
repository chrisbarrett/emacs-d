# Design: consolidate-theme-state-seam

## Context

Theme-change state is split across three owners: `modules/theme/lib.el`
defines and runs `+theme-changed-hook`; `modules/tty/init.el` defines
`+theme-default-background` and refreshes it from that hook (depth -95,
ahead of tty's own sentinel clear at default depth);
`lisp/gfm/gfm-pretty-callouts.el` reads the variable behind `boundp`
guards (lines ~107, ~830) and registers two functions directly on
`+theme-changed-hook` (lines ~880, ~1002). `modules/ui/config/+pulsar.el`
also hooks `+theme-changed-hook` — legitimate config-consumer usage.

The gfm coupling contradicts the library's standalone posture (it has
its own specs, tests, and no other config dependencies), and the
tty-owns-theme-cache arrangement means theme knowledge lives one module
away from the hook that drives it.

## Goals / Non-Goals

**Goals**

- One owner (theme module) for theme-change notification and the
  effective-background cache.
- `lisp/gfm/` free of config symbols; loadable standalone.
- Exactly one bridge point between config theme state and gfm.

**Non-Goals**

- Changing pulsar's or tty's consumption of the hook — hooking
  `+theme-changed-hook` from config modules is the intended extension
  point.
- Renaming `+theme-default-background` or `+theme-changed-hook`.
- Generalising the bridge for other libraries before a second consumer
  exists (one adapter = hypothetical seam).

## Decisions

### Decision: new axis `theme`

The recognised-axes list includes `theme` and `modules/theme/` exists,
but no stable spec does. This change creates it, covering: hook
ownership, cache ownership (including the depth ordering constraint
inherited from the tty spec), and the gfm bridge.

### Decision: move the defvar + save hook verbatim; keep tty's clear hook in tty

`+theme-default-background` and the depth-`-95` refresh function move
to `modules/theme/lib.el` unchanged in name and semantics. tty keeps
`+tty-clear-bg-h` — the sentinel clear is a tty concern; the cache is
not. The ordering contract (refresh before clear) is now expressed
across two specs and carried by the same hook-depth mechanism as
today.

### Decision: gfm indirection = one hook + one function defcustom

`gfm-pretty-theme-change-hook` (library-owned normal hook) replaces
direct `+theme-changed-hook` registration; the two existing add-hook
sites in callouts (face refresh, blockquote-face bg strip) move onto
it. `gfm-pretty-background-function` (defaulting to a `default`-face
query) replaces `boundp`-guarded reads of `+theme-default-background`.

Alternative considered: a `gfm-pretty-background` *variable* the config
sets on each theme change. Rejected — a function pulls current state on
demand, so ordering against the config's refresh hook cannot produce a
stale colour.

Alternative considered: gfm advertises `run-hooks` integration via
`defvar gfm-pretty-theme-hook-name` that it attaches to when bound.
Rejected — inverted dependency in disguise; the library would still
know about embedding conventions.

### Decision: bridge lives in the theme module

`modules/theme/` wires `+theme-changed-hook →
gfm-pretty-theme-change-hook` and sets
`gfm-pretty-background-function`. Placing the bridge in
`lang-markdown` was considered (it owns gfm activation) but the bridge
is about theme state, and theme already runs at startup; a
`with-eval-after-load 'gfm-pretty` guard keeps it lazy.

## Risks / Trade-offs

- [Hook-depth ordering silently breaks when the refresh moves modules]
  → the theme spec pins depth `≤ -95`; the tty spec's existing "cache
  refresh precedes clear" scenario is retained in the theme spec and
  exercised by tty tests.
- [gfm face refresh stops firing if the bridge is forgotten] → theme
  spec scenario asserts the bridge; gfm keeps a working default
  (face-background query) so standalone behaviour degrades gracefully
  rather than erroring.
- [Load-order: theme module sets `gfm-pretty-background-function`
  before gfm loads] → defcustom setting via `setopt` after
  `with-eval-after-load` avoids clobbering; verified in tasks.

## Migration Plan

Config-internal move; no user-facing migration. Single commit sequence:
theme gains the state, tty drops it, gfm swaps indirection, bridge
added — each step keeps `make test` green. Rollback is a revert.

## Open Questions

- None.
