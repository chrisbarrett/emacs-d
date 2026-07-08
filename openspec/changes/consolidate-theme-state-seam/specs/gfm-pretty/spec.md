# gfm-pretty Delta

## MODIFIED Requirements

### Requirement: Theme change responsiveness

The library SHALL own its theme indirection points: a normal hook
`gfm-pretty-theme-change-hook`, run by the embedding configuration
whenever the colour theme changes, and a defcustom
`gfm-pretty-background-function`, a nullary function returning the
effective `default` background colour string (defaulting to querying
the `default` face). The library SHALL NOT reference the embedding
configuration's hooks or variables (`+theme-changed-hook`,
`+theme-default-background`) directly.

The callouts decorator's `:on-enable-fn` SHALL add a face-refresh
helper to `gfm-pretty-theme-change-hook` that recomputes per-type
tints from the background reported by
`gfm-pretty-background-function`.  `:on-disable-fn` SHALL remove it.

The helper SHALL, in one pass per type, recompute the 10%-toward-bg
blend once and set `:background` on BOTH the body face (e.g.
`gfm-pretty-callouts-note-body-face`) AND the tint face (e.g.
`gfm-pretty-callouts-note-tint-face`) for that type.  Sharing the
single blend call between body and tint faces SHALL prevent drift
between buffer-char tinting (body face, applied via font-lock) and
overlay-cell tinting (tint face, referenced via `:inherit` from
overlay face specs).

Because overlay face specs reference the tint face by name (not by
baked colour), existing rendered overlays SHALL pick up the new
`:background` at the next redisplay without a decorator rebuild.

#### Scenario: Theme switch — body and tint faces refresh together

- **WHEN** the embedding configuration runs
  `gfm-pretty-theme-change-hook` after a light-to-dark theme switch
- **THEN** each `gfm-pretty-callouts-*-body-face` background is
  recomputed from `gfm-pretty-background-function`
- **AND** each `gfm-pretty-callouts-*-tint-face` background is
  recomputed in the same pass
- **AND** the next redisplay shows correctly tinted body backgrounds

#### Scenario: Theme switch propagates to already-rendered overlays

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and at least one
  callout rendered
- **WHEN** the theme changes and `gfm-pretty-theme-change-hook` runs
- **AND** `gfm-pretty-mode` is NOT toggled off and on
- **THEN** the existing overlays SHALL re-resolve their face specs at
  the next redisplay
- **AND** the rendered callout panel SHALL show the new theme's tint
  colour, not the prior theme's

#### Scenario: Library loads standalone with default background source

- **GIVEN** an Emacs session without this configuration's theme module
- **WHEN** `gfm-pretty-callouts` is loaded and enabled
- **THEN** tint faces derive from `(face-background 'default)` via the
  default `gfm-pretty-background-function`
- **AND** no void-variable or void-function error references config
  symbols
