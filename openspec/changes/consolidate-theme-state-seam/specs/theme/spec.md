# theme Delta

## ADDED Requirements

### Requirement: theme module owns change notification

The theme module (`modules/theme/`) SHALL define `+theme-changed-hook`
and SHALL run it after every theme load, theme switch, and theme
disable. No other module SHALL define or run this hook.

#### Scenario: hook fires on theme switch

- **WHEN** the user switches themes via the theme module's commands
- **THEN** `+theme-changed-hook` runs exactly once after the new theme
  is active

### Requirement: theme module owns the effective-background cache

The theme module SHALL define `+theme-default-background`, a cache of
the active theme's `default` `:background` captured BEFORE any tty
pass-through clear is applied. The cache SHALL always be a real colour
string (e.g. `"#fdf6e3"`), never a sentinel beginning with
`"unspecified"`.

The cache SHALL be refreshed via a function on `+theme-changed-hook`
registered at depth `-95` or lower, so it runs earlier than any
consumer — in particular earlier than tty's default-background sentinel
clear. Config-internal consumers needing the real background for colour
blending or pulse arithmetic SHALL read `+theme-default-background`,
not `(face-background 'default …)`.

#### Scenario: cache holds real bg after theme load

- **WHEN** a theme has finished loading
- **THEN** `+theme-default-background` is a non-empty string
- **AND** it does not start with `"unspecified"`

#### Scenario: cache refresh precedes tty clear

- **GIVEN** the theme changes from light to dark in a tty session
- **WHEN** `+theme-changed-hook` is run
- **THEN** the theme module's cache refresh records the new theme's
  background before tty's clear hook overwrites the live frames'
  default `:background` with the sentinel

### Requirement: theme module bridges library theme hooks

The theme module SHALL contain the config's only coupling between
`+theme-changed-hook` and library-owned theme indirection points: it
SHALL run `gfm-pretty-theme-change-hook` from `+theme-changed-hook`
and SHALL set `gfm-pretty-background-function` to a function returning
`+theme-default-background`. Libraries under `lisp/` SHALL NOT
reference `+theme-changed-hook` or `+theme-default-background`
directly.

#### Scenario: gfm faces refresh on theme switch via the bridge

- **GIVEN** a buffer with `gfm-pretty-mode` and a rendered callout
- **WHEN** the user switches theme
- **THEN** `+theme-changed-hook` runs `gfm-pretty-theme-change-hook`
- **AND** the callout tint faces recompute from the new
  `+theme-default-background`

#### Scenario: lisp/ carries no config theme symbols

- **WHEN** `lisp/` is searched for `+theme-changed-hook` and
  `+theme-default-background` (excluding the generated
  `+autoloads.el`)
- **THEN** no source file under `lisp/` references either symbol
