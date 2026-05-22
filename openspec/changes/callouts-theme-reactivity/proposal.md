## Why

`gfm-pretty-callouts` computes per-type tints at render time via
`gfm-pretty-callouts--tinted-bg` (blending the type face's foreground
10% toward `+theme-default-background`) and bakes the resulting hex
colour into every anchor / display / after-string overlay's face
spec.  When the active theme changes, the type faces update but the
already-laid overlays keep their stale tint until a buffer-wide
rebuild.  The user noticed this after fixing the same class of bug
on `gfm-pretty-blockquotes` — they expect callouts to track theme
the same way (overlay specs refer to faces by name; Emacs re-resolves
on every redisplay).

## What Changes

- Move the per-type tint computation off the overlay-spec hot path
  and onto a derived face per callout type:
  `gfm-pretty-callouts-<type>-tint-face` (one per NOTE / TIP /
  IMPORTANT / WARNING / CAUTION).  The face's `:background` is set
  by a `+theme-changed-hook` handler that calls the existing blend
  helper.
- Switch every callouts overlay face spec from
  `(:inherit <border-face> :slant normal :background "#hex")` to
  `(:inherit (<tint-face> <border-face>) :slant normal)`.  No baked
  colour string anywhere in the overlay specs.
- Refresh the new tint faces in `gfm-pretty-callouts--refresh-body-
  faces` (which already runs on theme change) so all tint state is
  recomputed in one pass.
- No behavioural change when theme is steady — the visual output is
  identical to the current implementation pre-theme-change.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: the callouts decorator's rendering contract gains a
  theme-reactivity guarantee — overlay face specs MUST reference
  faces by name, never baked colour strings — and adds the new
  per-type tint faces refreshed on theme change.

## Impact

- `lisp/gfm/gfm-pretty-callouts.el`: add per-type tint face
  defaces; extend `--refresh-body-faces` (or split it) to also set
  the tint faces' `:background` from the blend; rewrite
  `--apply-block-anchors` and `--apply-block-display` to use
  `:inherit` references instead of `(gfm-pretty-callouts--tinted-bg
  border-face)` calls baked into face specs.
- `lisp/gfm/gfm-pretty-tests.el`: assertions covering the old baked
  colour shape need updating; add a regression asserting overlay
  face specs carry `:inherit <tint-face>` (not `:background <hex>`).
- `openspec/specs/gfm-pretty/spec.md`: delta updates the callouts
  rendering requirements to mandate face references and to add the
  theme-reactivity invariant.
- No engine changes; no decorator-protocol changes.
