## ADDED Requirements

### Requirement: Decorator phase declaration and iteration order

The engine SHALL classify every registered decorator into one of three
phases:

- `atoms` — decorators contributing `display` overlays or text-props
  that change the visual cell width of in-line content relative to
  the underlying buffer characters.
- `containers` — decorators measuring per-line visual width to size
  block-level borders, fills, or wrap-aware after-strings.
- `overlays` — decorators that neither change nor measure in-line
  visual width.

The engine SHALL iterate decorators in the order
`atoms → containers → overlays` at every dispatch site, including
(but not limited to) the scheduled rebuild, the
`after-change-functions` dirty-region propagation, the window
reconciler, lifecycle enable / disable, and the enabled-decorator
enumeration. Within a phase, iteration order SHALL be unspecified.

A decorator that does not declare a phase SHALL default to
`containers`.

#### Scenario: Atoms render before containers measure on a fresh enable

- **GIVEN** an atom-phase decorator A and a container-phase decorator C
- **AND** a buffer where A's overlays would change the visual width
  of a line that C measures
- **WHEN** `gfm-pretty-mode` enables and the engine drives a full
  rebuild
- **THEN** A's `:apply-block-fn` SHALL run before C's `:apply-block-fn`
- **AND** C's measurement SHALL observe A's overlays as already in
  place

#### Scenario: Toggling a decorator preserves phase order

- **GIVEN** decorators registered across the three phases
- **WHEN** the user calls `gfm-pretty-toggle-decorator` on one of them
- **AND** a rebuild is dispatched
- **THEN** iteration in the rebuild SHALL still respect
  `atoms → containers → overlays`

#### Scenario: Unspecified phase defaults to containers

- **GIVEN** a decorator registered without a `:phase` argument
- **THEN** the engine SHALL treat its phase as `containers`

### Requirement: Visual-width measurement primitives

The engine SHALL expose `gfm-pretty-visual-line-width LBEG LEND` and
`gfm-pretty-visual-max-line-width BEG END &optional INDENT` as public
primitives for container-phase decorators.

`gfm-pretty-visual-line-width` SHALL return the visual cell count of
`[LBEG, LEND)` honouring overlay `display` and text-property `display`
substitutions and overlay / text-property `invisible`:

- An invisible chunk contributes 0.
- A `display` chunk whose value is a string contributes the
  `string-width` of that string and SHALL be advanced past in one
  step (no double-counting).
- A `display` chunk whose value is a stretch-glyph spec
  (`(space :align-to N)` / `(space :width N)`) contributes the
  cell count derivable from the spec when determinable; otherwise
  it falls back to `string-width` of the underlying buffer chars.
- Plain chunks contribute the `string-width` of the underlying
  buffer chars.

`gfm-pretty-visual-max-line-width` SHALL iterate lines from BEG to
END and return the maximum per-line visual width minus INDENT (default
0). It SHALL NOT cross a newline within a single line's measurement.

#### Scenario: Display overlay shrinks visual width

- **GIVEN** a line whose buffer text is `[label](/very/long/path)`
- **AND** an overlay over that range whose `display` is a 10-cell string
- **THEN** `gfm-pretty-visual-line-width` for the line SHALL return 10

#### Scenario: Invisible region contributes zero

- **GIVEN** a line with a 5-cell `invisible` overlay
- **THEN** `gfm-pretty-visual-line-width` SHALL return the visible
  remainder, not the raw character count

#### Scenario: Unknown stretch-glyph spec falls back to string-width

- **GIVEN** a line carrying a `display` spec the helper cannot
  evaluate cell-wise
- **THEN** the helper SHALL fall back to the underlying chars'
  `string-width` and SHALL NOT raise

### Requirement: Atomic-span marker honoured by wrap simulation

Atom-phase decorators SHALL tag the `display` strings (or
`display`-bearing text properties) they install with a non-nil
`gfm-pretty-atomic` text property over the full extent of the
visually-atomic span.

`gfm-pretty-simulate-wrap` SHALL treat space characters falling inside
a `gfm-pretty-atomic` span as non-wrap-points. The simulator MAY wrap
at the span boundary; it SHALL NOT wrap inside the span.

The engine SHALL expose a helper `gfm-pretty-visualised-string
LBEG LEND` that returns a propertised string composed by walking
`[LBEG, LEND)` analogously to `gfm-pretty-visual-line-width` and
copying the `gfm-pretty-atomic` property forward from contributing
overlays / text-props. Container-phase decorators SHALL use this
helper to build the input to `gfm-pretty-simulate-wrap` when
simulating wrap over buffer content that may contain atom-decorator
substitutions.

#### Scenario: Wrap does not split inside an atomic display string

- **GIVEN** an atom display string `"icon label phrase"` over a
  buffer range, with `gfm-pretty-atomic t` on the full string
- **AND** the string is passed through `gfm-pretty-visualised-string`
  and into `gfm-pretty-simulate-wrap` with a width that lands inside
  the string
- **THEN** the resulting wrap position SHALL be at the start (or
  past the end) of the atomic span, never within it

## MODIFIED Requirements

### Requirement: Decorator registration via `gfm-pretty-define-decorator`

The system SHALL expose `gfm-pretty-define-decorator` as the public
registration entry point. A decorator SHALL declare itself by name and
contribute pure functions; the engine SHALL own the lifecycle hooks
(`after-change-functions`, `window-configuration-change-hook`,
`post-command-hook`, the idle rebuild timer, the dirty-region tracker,
the window-state snapshot, the scoped-rebuild routing, and the
generic rebuild stats wrapper).

A registration form SHALL accept:

- `NAME` (symbol) — decorator identifier.
- `:phase PHASE` — one of `(atoms containers overlays)`; defaults to
  `containers` when omitted. Governs the engine's iteration order
  per the "Decorator phase declaration and iteration order"
  requirement.
- `:registry REGISTRY` — `gfm-pretty--registry` value identifying the
  decorator's overlay tag. The engine SHALL derive every overlay
  property name (anchor, display, revealable, saved-display) from this
  registry's `tag`.
- `:collect-fn FN` — returns the buffer's list of blocks (under
  widening).
- `:range-fn FN` — returns `(BEG . END)` for a block.
- `:apply-block-fn FN` — `(block window)`-arity function that applies
  every overlay required to render BLOCK in WINDOW. Decorators that
  share width-independent state across windows MAY call
  `gfm-pretty-borders--apply-with-anchors` to factor the
  anchors-once-per-block / display-once-per-window split internally.
- `:rebuild-fn FN` (optional) — full rebuild override. When omitted,
  the engine SHALL perform generic teardown + reapply by calling
  `:collect-fn` and `:apply-block-fn` once per (block, window) pair.
- `:full-rebuild-required-p FN` (optional, takes the dirty region) —
  returns non-nil when the dirty region forces a full rebuild
  (structural-line edit, adjacency-gated edit, or any other
  decorator-specific reason). When omitted, the engine SHALL fall
  through to block-containment scoping.
- `:reveal-fn FN` (optional) — bespoke reveal handler (used by links
  to group whole-link overlays by id).
- `:on-enable-fn FN` (optional) — decorator extras to install when
  the umbrella mode (or `gfm-pretty-toggle-decorator`) enables. A
  decorator MAY install bespoke buffer-local hooks here (e.g. tables
  installs its own `window-configuration-change-hook` handler for its
  custom reconciler).
- `:on-disable-fn FN` (optional) — symmetric teardown.

The registration protocol SHALL NOT include keys for `:apply-anchors`,
`:apply-display`, `:structural-line-ranges`, `:edit-adjacency`,
`:revealable-prop`, `:saved-display-prop`, `:revealable-p`,
`:reconcile-windows`, `:block-at-point`, or `:edit-at-point`. The
behaviours those keys previously named SHALL be implemented as
follows:

- Anchor / display split → call
  `gfm-pretty-borders--apply-with-anchors` from inside the
  decorator's `:apply-block-fn`.
- Structural-line ranges + edit-adjacency → fold into the decorator's
  `:full-rebuild-required-p`.
- Revealable property + saved-display property → derived from the
  decorator's `:registry` (`<tag>-revealable`, `<tag>-saved-display`).
- Custom revealable predicate → no decorator uses this; the engine's
  default property-presence check is the only path.
- Per-decorator window reconciler → wired by the decorator's
  `:on-enable-fn` via a buffer-local `window-configuration-change-hook`
  handler (tables only).
- `block-at-point` / `edit-at-point` → exported by the decorator
  under the naming convention `<name>--block-at-point` /
  `<name>--edit-at-point`. The public commands in `gfm-pretty.el`
  SHALL consult any decorator that exports these functions; only
  `tables` does so today.

#### Scenario: Registering a new decorator

- **WHEN** a new visual-decoration feature is added
- **THEN** it SHALL declare itself via
  `(gfm-pretty-define-decorator 'NAME …)` with the contributions it
  needs
- **AND** the engine SHALL invoke its functions during rebuild, reveal,
  and reconcile without further wiring

#### Scenario: Decorator declines to override scoped-rebuild policy

- **GIVEN** a decorator registers `:apply-block-fn`, `:collect-fn`,
  `:range-fn`
- **AND** does NOT register `:full-rebuild-required-p`
- **WHEN** the user edits inside one block's body
- **THEN** the engine SHALL fall through to block-containment scoping
- **AND** rebuild only that block

#### Scenario: Anchor/display split is opt-in via the borders helper

- **GIVEN** a decorator (e.g. `callouts` or `fences`) wants width-
  independent overlays shared across windows
- **WHEN** its `:apply-block-fn` runs for a given (block, window) pair
- **THEN** the decorator SHALL call
  `gfm-pretty-borders--apply-with-anchors` with separate
  anchor-application and display-application thunks
- **AND** the helper SHALL apply the anchors at most once per
  (block, current rebuild pass) regardless of how many windows are
  displaying the buffer

#### Scenario: Decorator declares its phase

- **GIVEN** an atom-phase decorator (e.g. `links`)
- **WHEN** the decorator's `gfm-pretty-define-decorator` form is
  evaluated
- **THEN** it SHALL include `:phase 'atoms`
- **AND** the engine SHALL place it in the atoms phase for all
  subsequent dispatch

### Requirement: Rendering primitives are public

The system SHALL expose the following primitives for decorator authors:

- `gfm-pretty-available-width &optional WINDOW` — char width
  primitive.
- `gfm-pretty-visual-line-width LBEG LEND` — visual cell count for a
  single line range, honouring `display` and `invisible`.
- `gfm-pretty-visual-max-line-width BEG END &optional INDENT` —
  visual cell count of the widest line in a multi-line range, minus
  INDENT.
- `gfm-pretty-visualised-string LBEG LEND` — propertised string
  composed from `[LBEG, LEND)` with `display` substitutions inlined
  and `gfm-pretty-atomic` carried forward; intended for input to
  `gfm-pretty-simulate-wrap`.
- `gfm-pretty-top-border WIDTH FACE BUFFER-WIDTH &optional ICON` —
  top-border (LEADING . TRAILING) split.
- `gfm-pretty-bottom-border WIDTH FACE BUFFER-WIDTH` — bottom-border
  split.
- `gfm-pretty-right-after BOX-WIDTH FACE &optional BG` — right-edge
  after-string.
- `gfm-pretty-simulate-wrap TEXT WIDTH &optional CONT-PREFIX-W` —
  wrap-position simulator. TEXT MAY carry the `gfm-pretty-atomic`
  text-property over arbitrary substrings; the simulator SHALL NOT
  break inside an atomic span.
- `gfm-pretty-make-anchor BEG END &rest PROPS` — anchor-overlay
  factory bound to the calling decorator's registry.
- `gfm-pretty-make-display BEG END WINDOW &rest PROPS` —
  display-overlay factory restricted to WINDOW when non-nil.

Every other engine symbol SHALL be `gfm-pretty--` private. In
particular, a raw-character `max-line-width` helper SHALL NOT be
exposed; container-phase decorators SHALL measure via
`gfm-pretty-visual-max-line-width`.

#### Scenario: Decorator builds a bordered block

- **GIVEN** a decorator's `:apply-block-fn` callback
- **WHEN** it constructs a border for the current window
- **THEN** it calls `gfm-pretty-available-width` and
  `gfm-pretty-top-border` / `gfm-pretty-bottom-border` /
  `gfm-pretty-right-after`
- **AND** it does NOT reference `gfm-pretty--*` internal helpers

#### Scenario: Container measures longest body-line visual width

- **GIVEN** a container-phase decorator sizing a box around a block
- **WHEN** it computes the longest body line's visual extent
- **THEN** it SHALL call `gfm-pretty-visual-max-line-width` over the
  body range
- **AND** SHALL NOT measure raw character counts

### Requirement: Callout box width sizing

Box width SHALL be `min(text-width, max(80, max-content-width + 4))`
per window, where `text-width` is `gfm-pretty-available-width` for the
window and `max-content-width` is the longest body line's
**visual** width — measured via `gfm-pretty-visual-max-line-width` —
minus the 2-char `> ` prefix. The measurement SHALL observe overlays
laid by atom-phase decorators; the engine's phase-ordered dispatch
guarantees those overlays are in place before callouts measures.

#### Scenario: Wide content, narrow window

- **GIVEN** a callout whose longest body line is 60 visual cells
- **AND** a window 100 cells wide
- **THEN** the box width SHALL be `min(100, max(80, 64)) = 80`

#### Scenario: Narrow content, wide window

- **GIVEN** a callout whose longest body line is 20 visual cells
- **AND** a window 100 cells wide
- **THEN** the box width SHALL be `min(100, max(80, 24)) = 80`

#### Scenario: Wide content, very wide window

- **GIVEN** a callout whose longest body line is 200 visual cells
- **AND** a window 250 cells wide
- **THEN** the box width SHALL be `min(250, max(80, 204)) = 204`

#### Scenario: Body line dominated by a prettified inline link

- **GIVEN** a callout body line whose raw buffer text is ~75 chars
  but whose visible rendering after `links`-decorator substitution
  is ~15 cells
- **AND** a window 100 cells wide
- **THEN** the box width SHALL be `min(100, max(80, 15 + 4)) = 80`
- **AND** the right-edge `│` on that body line SHALL be placed using
  the 15-cell visual extent, not the 75-char raw extent

### Requirement: Callout wrapped right-edge alignment

For each callout body line, the engine SHALL determine whether the
line overflows the content budget (`box-width - 4`) by comparing the
line's **visual** width against the budget. Visual width is computed
via `gfm-pretty-visual-line-width`.

- When the line's visual width fits in the content budget, the
  right-edge `│` SHALL be placed at `align-to (box-width - 2)` with no
  wrap simulation.
- When the line overflows, the right-edge `│` SHALL be padded by
  simulating wrap via `gfm-pretty-simulate-wrap` over the line's
  visualised string (composed via `gfm-pretty-visualised-string`) so
  the border lands at the box width on the final visual row.

#### Scenario: Visual width fits — non-overflow path

- **GIVEN** a body line whose visual width is 15 cells
- **AND** a content budget of 76 cells
- **THEN** the right-edge `│` SHALL be placed at
  `align-to (box-width - 2)` without invoking the wrap simulator

#### Scenario: Long body line wraps mid-line

- **GIVEN** a 200-cell visual body line in an 80-col window
- **WHEN** the right-edge after-string is computed
- **THEN** the wrap simulator SHALL run over the visualised string
- **AND** the `│` lands at column 80 on the final wrapped visual row
- **AND** the simulator SHALL NOT break inside any atomic span the
  visualised string carries
