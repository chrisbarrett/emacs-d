## Context

`modules/tty/init.el` does three jobs: clipboard (clipetty),
mouse (xterm-mouse), and colour theming. The colour-theming bits are
the load-bearing ones for this change:

1. On every tty frame, `default`'s `:background` is set to the
   sentinel `"unspecified-bg"` so the terminal emulator's own bg
   shows through (`+tty-clear-bg-h`).
2. The pre-clear theme bg is cached in `+theme-default-background`
   and kept in sync via `+theme-changed-hook` (`+tty-save-default-bg-h`,
   depth `-95` so it runs before the clear).
3. Third-party colour callers (`pulse-momentary-highlight-overlay`,
   `winpulse-window`) read `(face-background 'default …)` for colour
   arithmetic. On tty that returns the sentinel and propagates as
   garbage. An `:around` advice
   (`+face-background-resolve-original-value-a`) cl-letfs
   `face-background` for those callers so `'default` resolves to
   `+theme-default-background` instead.
4. `pulsar` brings its own cl-letf (`pulsar.el:449`) that returns
   `pulsar-tty-color` for any face inside its pulse path, which the
   `+pulsar--update-bg-h` hook keeps fresh.

That contract is real but undocumented, and a related contract is
silently violated: `early-init.el +sync-frame-parameters` reads
`(face-attribute 'default :background)` and paints the result onto
`fringe` / `window-divider*` `:foreground`. On tty boot that string is
`"unspecified-bg"`. Per `src/xfaces.c:1163-1166`, `tty_defined_color`
short-circuits the sentinel; per `src/nsterm.m:2326-2348`,
`ns_defined_color` does not. The macOS daemon keeps a live (invisible)
NS frame, so any redisplay-triggering operation that forces
face_change=true — e.g. `set-face-background 'pulse-highlight-face …`
running 5× per pulse — re-realises every face on every alive frame.
Realising `fringe` on the NS frame fails colour lookup and
`load_color2` adds `Unable to load color "unspecified-bg"` to
`*Messages*` (`src/xfaces.c:1310`).

The fix is small; the spec is the larger payoff.

## Goals / Non-Goals

**Goals:**

- Document the colour-theming contract that `modules/tty/` already
  enforces so future changes can be evaluated against it.
- Stop `"unspecified-bg"` from appearing as `:foreground` on any face
  — currently `fringe`, `window-divider`,
  `window-divider-first-pixel`, `window-divider-last-pixel`.
- Keep the existing user-visible behaviour: terminal bg pass-through
  on tty frames, pulse / winpulse animation working in tty without
  garbage colour output.

**Non-Goals:**

- Touching clipetty / xterm-mouse — orthogonal to this change.
- Upstreaming the `ns_defined_color` asymmetry (out of scope; we
  work around it).
- Re-engineering the cache/advice pattern — the design ratifies
  what's already in `modules/tty/init.el`.
- Hardening every elisp colour caller across elpaca — the existing
  resolve-advice covers the active triggers (pulse, winpulse); other
  callers either filter "unspecified-bg" themselves (e.g.
  `indent-bars:573`) or only fire on triggers (corfu popups, magit
  diff) that the user can address as they appear.

## Decisions

### Decision: new axis `tty`

`modules/tty/` is an existing module that pre-dates the spec layer.
The spec-conventions axis rule says "Axis name = module directory
name where a corresponding module exists" (`openspec/specs/spec-
conventions/spec.md`). `tty` qualifies; no existing axis contains
this material.

The `theme` module already exists and `modules/theme/lib.el` houses
`+theme-default-background`'s consumers (`+theme-dark-p`,
`+theme-update`, `+theme-changed-hook`). I considered folding the
colour-theming requirements into a `theme` spec, but `theme/` covers
*which* theme to load, not *how* tty frames work around colour
sentinels. The `+theme-default-background` cache lives in
`modules/tty/init.el`, not `modules/theme/`. Keeping them separate
matches the existing source layout.

**Alternatives considered:**

- *Fold into a `theme` spec.* Rejected — `theme/` and `tty/` are
  separate modules with separate `init.el`s; conflating them would
  hide the tty-specific contract that 3rd-party packages must
  cooperate with.
- *Internals-facing only.* Considered marking the spec
  internals-facing because the contract is mostly invisible to the
  user. Rejected: clipetty (clipboard) and xterm-mouse are
  user-observable, so the `tty` axis spec is mixed. The colour
  requirements are framed as observable invariants
  ("`*Messages*` contains no `Unable to load color` line during
  pulse animation") which is testable.

### Decision: fix the producer, not the consumer

The bug is `early-init.el` writing `"unspecified-bg"` onto fringe
`:foreground`. Three plausible fixes:

1. *Filter in writer.* Change `+sync-frame-parameters` to source bg
   from `+theme-default-background` (or skip when the cache holds
   `"unspecified"…`). One line.
2. *Filter on read.* Advise `set-face-foreground` globally to drop
   `"unspecified-bg"` arguments.
3. *Advise `ns_defined_color`.* Not callable from elisp.

(1) is the smallest blast radius and matches the rest of the
module: the cache already exists for exactly this purpose. (2)
would create a hidden global rule that's surprising for the next
reader. (3) isn't feasible.

### Decision: regression test under sandbox daemon

The failure only manifests when both a tty frame AND a graphic NS
frame are alive in the same daemon. `make test`'s default `emacs
--batch` is neither. The regression test SHALL spin up a sandbox
daemon per the CLAUDE.md convention
(`emacs-claude-sandbox-<unique>`), open a tty client in tmux, force
the daemon to also create an NS frame (or detect the existing one
in --bg-daemon mode), pulse, and grep `*Messages*` for the
warning. If the test harness can't easily reproduce the dual-frame
condition, fall back to asserting the *producer* invariant: no
face in `(face-list)` has `:foreground "unspecified-bg"` after
`+sync-frame-parameters` runs. That's a strictly stronger
assertion than the smoke test and doesn't depend on NS frame
liveness.

## Risks / Trade-offs

- *Fringes/dividers no longer "blend" if the cache isn't populated
  early enough.* → `+sync-frame-parameters` runs on `after-init-hook`
  and `+theme-changed-hook`, both of which fire after
  `modules/tty/init.el` has set up the cache. The defensive guard
  (skip when `"unspecified"…`) is a no-op in that case.
- *Theme bg drifts away from the actual rendered bg if a later hook
  re-clears default.* → `+tty-save-default-bg-h` at depth `-95` runs
  before any default-bg clearing on `+theme-changed-hook`, so the
  cache observes the theme value, not the sentinel. This is the
  current contract; the spec just pins it.
- *Other 3rd-party callers we haven't found yet may still feed the
  sentinel into NS colour resolution.* → Out of scope; the spec
  documents the protocol so future work can fix them incrementally
  using the same advice pattern.
