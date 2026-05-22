## MODIFIED Requirements

### Requirement: Code-fence body background fill

The fences decorator SHALL paint a background fill on the body region
of every fenced/YAML/indented block using `gfm-code-fences--face-extend-bg`
to lift the underlying default-bg cleanly under `:extend t` text-property
faces (e.g. `diff-added`).

The right-edge `after-string` of each body line overlay has two
variants: a "masked" variant ending in a stretch glyph painted in
`default` face (clips past-EOL `:extend t` bg) and a "bare" variant
that contributes no past-EOL paint. The decorator SHALL select
between the two variants per body overlay based on whether the
overlay's range overlaps the active selection — bare for selected
lines, masked otherwise. Selection bounds are derived from evil's
linewise/charwise visual state (when active) or vanilla
`use-region-p`; evil visual-block is treated as unselected.

#### Scenario: Body line with diff face

- **GIVEN** a fenced block one of whose lines has a `diff-added`
  text-property face
- **AND** no active selection covers that line
- **THEN** the body-background fill is visible up to the right border
- **AND** the diff face's background does not leak past the right
  border

#### Scenario: Selected body line extends to window edge

- **GIVEN** a fenced block whose body lines are covered by an active
  vanilla `mark-active` selection or an evil linewise/charwise visual
  selection
- **THEN** each selected body line's `region` face background extends
  from the last buffer column through the right border to the
  window's right edge
- **AND** the right border `│` is not visible on selected lines

#### Scenario: Selection toggles after-string variant

- **GIVEN** a fenced body line with both `gfm-pretty-fences-after-masked`
  and `gfm-pretty-fences-after-bare` stashed on its overlay
- **WHEN** the active selection starts overlapping that line
- **THEN** the overlay's `after-string` is set to the bare variant
- **AND** when the selection no longer overlaps the line, `after-string`
  is restored to the masked variant

#### Scenario: Rebuild during active selection

- **GIVEN** a `V`-line selection covering one or more body lines of a
  fenced block
- **WHEN** the fences decorator rebuilds the block's overlays (e.g.
  after a buffer edit or window-configuration change)
- **THEN** the new body overlays for selected lines are created with
  `after-string` set to the bare variant from the start, without a
  one-frame masked render before a post-command-hook sync

#### Scenario: Visual-block selection retains mask

- **GIVEN** an evil visual-block selection (`Ctrl-V`) over a fenced
  block
- **THEN** body overlays use the masked `after-string` variant
- **AND** diff-bg clipping past the right border is preserved
