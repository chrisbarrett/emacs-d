# tty Specification

## Purpose

Terminal-frame compatibility for `modules/tty/` — covers the
colour-theming invariants that let Emacs interoperate cleanly with
the terminal emulator's own background and with third-party colour
arithmetic. Specifically: the `"unspecified-bg"` pass-through
sentinel on `default :background` for tty frames, the
`+theme-default-background` cache that preserves the real theme
colour for blending code, the "sentinels never cross-stamped onto
the wrong attribute" rule, and the per-package shadowing /
resolve-original advice protocol used by pulse-style animation
packages. Clipboard, mouse, and box-drawing concerns owned by
`modules/tty/` are out of scope of this spec and may be addressed
by future requirements.

## Requirements

### Requirement: `default` background is the terminal pass-through sentinel on tty frames

The realised value of `default`'s `:background` SHALL be the string
`"unspecified-bg"` on every tty frame whenever the user's
configuration is fully loaded. This is the Emacs-blessed sentinel
that suppresses Emacs's own background paint so the terminal
emulator's background shows through.

The clear SHALL be applied:

- on each tty frame at creation time
  (`+after-make-tty-frame-functions`); and
- on every tty frame when the colour theme changes
  (`+theme-changed-hook`).

The clear SHALL NOT touch graphic frames.

#### Scenario: tty frame default bg is the sentinel

- **GIVEN** Emacs running as a daemon
- **WHEN** a tty client connects and `+modules-load-all` has run
- **THEN** `(face-attribute 'default :background tty-frame)` is
  `"unspecified-bg"`

#### Scenario: graphic frame default bg untouched

- **GIVEN** the same daemon also has a live graphic frame
- **WHEN** the user inspects `(face-attribute 'default :background
  graphic-frame)`
- **THEN** the value is the colour the active theme set, never
  `"unspecified-bg"`

#### Scenario: theme change re-clears existing tty frames

- **WHEN** the user invokes `+theme-light` or `+theme-dark` while one
  or more tty frames are live
- **THEN** every tty frame's default `:background` is restored to
  `"unspecified-bg"` after the new theme finishes loading

### Requirement: theme background is cached for colour arithmetic

The system SHALL maintain a variable `+theme-default-background`
whose value is the active theme's `default` `:background` BEFORE the
tty pass-through clear is applied. The cache SHALL be a real colour
string (e.g. `"#fdf6e3"`), never the sentinel `"unspecified-bg"`.

The cache SHALL be refreshed on every theme change via a hook on
`+theme-changed-hook` that runs at a depth earlier than any default-
bg clear (depth `-95` or lower). On a graphic-only session the cache
SHALL track the theme bg without becoming the sentinel.

Consumers that need the real bg for colour blending, gradient
endpoints, or pulse arithmetic SHALL read `+theme-default-background`
directly, not `(face-background 'default …)`.

#### Scenario: cache holds real bg after theme load

- **WHEN** a theme has finished loading
- **THEN** `+theme-default-background` is a non-empty string
- **AND** it does not start with `"unspecified"`

#### Scenario: cache refresh precedes clear

- **GIVEN** the theme changes from light to dark
- **WHEN** `+theme-changed-hook` is run
- **THEN** `+tty-save-default-bg-h` records the new theme's bg before
  `+tty-clear-bg-h` overwrites the live frames' default `:background`
  with the sentinel

### Requirement: sentinels never cross-stamped onto the wrong attribute

The string `"unspecified-bg"` SHALL NOT appear as the `:foreground`
attribute value of any face. Symmetrically, `"unspecified-fg"` SHALL
NOT appear as the `:background` attribute value of any face.

The two sentinel strings are meaningful only on the matching
attribute of the `default` face: `"unspecified-bg"` on `default
:background` (tty pass-through), and `"unspecified-fg"` on `default
:foreground` (Emacs's own initial value). Any other placement is a
cross-stamp.

Rationale: each sentinel is silently filtered by Emacs's tty colour
resolver (`tty_defined_color` in `src/xfaces.c`) but NOT by every
graphic colour resolver — notably `ns_defined_color` in
`src/nsterm.m` returns failure. A daemon hosting both kinds of
frames will emit `Unable to load color "unspecified-bg"` into
`*Messages*` on every face re-realisation if the sentinel is
cross-stamped, because face_change=true forces realisation across
all alive frames.

Code paths that paint frame ornamentation faces (e.g. fringe,
window-divider) from the default background SHALL consult
`+theme-default-background` (the cached real value), and SHALL skip
the paint if the cached value is missing or starts with
`"unspecified"`.

#### Scenario: no face has bg-sentinel as foreground

- **WHEN** the user runs `(face-list)` and inspects every face's
  `:foreground` attribute
- **THEN** no face's `:foreground` is `"unspecified-bg"`

#### Scenario: no face has fg-sentinel as background

- **WHEN** the user runs `(face-list)` and inspects every face's
  `:background` attribute
- **THEN** no face's `:background` is `"unspecified-fg"`

#### Scenario: pulse animation emits no colour-load warning

- **GIVEN** a daemon with a live tty frame AND a live graphic frame
- **AND** the colour theme has loaded and the tty default-bg is the
  sentinel
- **WHEN** the user calls `(pulsar-pulse-line)` in the tty frame
- **THEN** the `*Messages*` buffer contains no new line matching
  `Unable to load color`

### Requirement: resolve-original advice for third-party colour callers

The system SHALL advise third-party entry points that read
`(face-background 'default …)` to seed colour arithmetic (gradients,
blends, pulse interpolation) so that, while the entry point runs,
`face-background` returns `+theme-default-background` for the
`'default` face instead of the sentinel. The shared advice helper
SHALL be `+face-background-resolve-original-value-a`, applied with
`:around`.

The following entry points SHALL carry the advice:

- `pulse-momentary-highlight-overlay`
- `winpulse-window`

The advice SHALL forward all other arguments and the function's
original behaviour for non-`'default` faces unchanged.

#### Scenario: pulse path sees real bg via advice

- **GIVEN** a tty frame whose `default` `:background` is
  `"unspecified-bg"`
- **WHEN** any caller inside `pulse-momentary-highlight-overlay` or
  `winpulse-window` evaluates `(face-background 'default nil t)`
- **THEN** the returned value is `+theme-default-background`, not
  the sentinel

#### Scenario: advice does not affect other faces

- **WHEN** code inside an advised entry point evaluates
  `(face-background 'some-other-face nil t)`
- **THEN** the value returned is whatever the original
  `face-background` would have returned for that face

### Requirement: pulse animation handled via per-package shadowing in tty

Pulse-style animation packages SHALL run in tty frames without
emitting colour-resolution warnings. The repository ships:

- `pulsar`'s own `cl-letf` of `face-background` to
  `pulsar-tty-color` inside `pulsar--create-pulse`; the system
  SHALL keep `pulsar-tty-color` synchronised to the
  `pulsar-generic` face's `:background` via the
  `+pulsar--update-bg-h` hook on `+theme-changed-hook` and
  `+after-make-tty-frame-functions`.
- The resolve-original advice on `pulse-momentary-highlight-overlay`
  and `winpulse-window` (see prior Requirement) for the cases
  `pulsar` does not cover.

These shall together suffice for the active animation triggers
(`pulsar-pulse-on-window-change`, `pulsar-pulse-line`,
`winpulse-mode`).

#### Scenario: pulsar-tty-color tracks theme

- **WHEN** the colour theme changes
- **THEN** `pulsar-tty-color` equals
  `(face-background 'pulsar-generic nil t)` evaluated under the new
  theme

#### Scenario: winpulse animates without colour warning

- **GIVEN** a tty frame and `winpulse-mode` enabled
- **WHEN** the user switches between two windows and triggers
  `winpulse-window`
- **THEN** `*Messages*` contains no new `Unable to load color` line

### Requirement: tty frames opt out of GUI-only ornamentation

`early-init.el`'s frame-parameter sync SHALL NOT propagate
`(face-attribute 'default :background)` directly onto the
`:foreground` of `fringe`, `window-divider`,
`window-divider-first-pixel`, or `window-divider-last-pixel`. The
paint SHALL be sourced from `+theme-default-background` (the cached
real value) and SHALL be skipped entirely when the cache is missing
or starts with `"unspecified"`.

#### Scenario: fringe foreground after init

- **WHEN** `after-init-hook` has run
- **THEN** the `:foreground` attribute of each of `fringe`,
  `window-divider`, `window-divider-first-pixel`, and
  `window-divider-last-pixel` is either `'unspecified` (the symbol,
  meaning unset) or equals `+theme-default-background`, never
  `"unspecified-bg"`

#### Scenario: skipped when cache unavailable

- **GIVEN** an edge case where `+theme-default-background` is unbound
  or holds an `"unspecified"`-prefixed value at sync time
- **WHEN** `+sync-frame-parameters` runs
- **THEN** the fringe / window-divider paint step is skipped
- **AND** no face has its `:foreground` written to that value
