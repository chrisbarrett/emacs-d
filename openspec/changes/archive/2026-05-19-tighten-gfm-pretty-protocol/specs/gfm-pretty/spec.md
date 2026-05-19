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

### Requirement: Scoped post-edit rebuild routing

The engine SHALL own the routing decision for scoped post-edit
rebuilds. After an `after-change-functions` burst settles, the engine's
idle timer SHALL invoke a per-decorator routing function that, given
the decorator's accumulated dirty region:

1. Calls the decorator's optional `:full-rebuild-required-p` with the
   dirty region. If non-nil, the engine SHALL perform a full rebuild
   via `:rebuild-fn` (or generic teardown + reapply when
   `:rebuild-fn` is absent).
2. Otherwise calls the decorator's `:collect-fn` (cache-hit normal)
   and matches the dirty region against each block's `:range-fn`
   result. If exactly one block fully contains the dirty region, the
   engine SHALL rebuild that single block via `:apply-block-fn` per
   currently-displayed window. Otherwise the engine SHALL perform a
   full rebuild.

A decorator that does not register `:full-rebuild-required-p` SHALL
still receive correct scoped rebuilds via the block-containment
fallback (step 2).

#### Scenario: Edit inside one block body triggers single-block rebuild

- **GIVEN** a buffer with two callouts and no other decorators
- **WHEN** the user types a character inside callout #1's body
- **AND** the idle timer fires
- **THEN** the engine SHALL match the dirty region against the
  collected blocks
- **AND** rebuild only callout #1 via its `:apply-block-fn` per
  displayed window
- **AND** callout #2's overlays SHALL be untouched

#### Scenario: Structural-line edit forces full rebuild

- **GIVEN** a buffer with two fenced code blocks
- **WHEN** the user inserts a backtick on block #1's opening fence
  line
- **AND** the idle timer fires
- **THEN** the fences decorator's `:full-rebuild-required-p` SHALL
  return non-nil for the dirty region
- **AND** the engine SHALL invoke the fences decorator's
  `:rebuild-fn` (full rebuild)

#### Scenario: Adjacency edit invalidates indent block

- **GIVEN** a buffer with an indented code block preceded by a blank
  line
- **WHEN** the user types on the blank line directly above the indent
  block
- **AND** the idle timer fires
- **THEN** the fences decorator's `:full-rebuild-required-p` SHALL
  return non-nil for the dirty region
- **AND** the engine SHALL invoke the fences decorator's
  `:rebuild-fn` (full rebuild)

### Requirement: Per-window cursor reveal

The engine SHALL install one `post-command-hook` handler that walks
every enabled decorator's revealable overlays in the selected window.
The engine SHALL derive each decorator's revealable + saved-display
overlay property names from its registered `:registry` value
(`<tag>-revealable` / `<tag>-saved-display`); decorators SHALL NOT
register these property names separately.

When point lies inside a revealable overlay whose `window` property
is nil or equals the selected window, the engine SHALL save the
overlay's `display` property under the `<tag>-saved-display` property
and set `display` to nil — exposing the source. When point leaves the
overlay, the engine SHALL restore `display` from the saved property.
Reveal SHALL be scoped to the selected window: an overlay restricted
to a non-selected window SHALL NOT have its source revealed
regardless of where point is.

A decorator MAY register a `:reveal-fn` (bespoke reveal handler) to
take over reveal entirely; otherwise the engine's default walker
runs. A decorator's overlays are revealable iff they carry the
`<tag>-revealable` property; a decorator that manages its own cursor
model (e.g. `tables` for cell highlighting) simply never sets that
property on its overlays.

#### Scenario: Point on a fence marker reveals the source in selected window only

- **GIVEN** a fenced code block decorated in W1 (selected) and W2
- **WHEN** point moves onto the opening fence marker in W1
- **THEN** W1 SHALL show the raw `\`\`\`lang` line
- **AND** W2 SHALL continue to show the rendered box

#### Scenario: Engine derives revealable property per decorator

- **GIVEN** the callouts decorator registers
  `:registry gfm-pretty-callouts--registry` whose tag is
  `'gfm-pretty-callouts`
- **AND** the fences decorator registers
  `:registry gfm-pretty-fences--registry` whose tag is
  `'gfm-pretty-fences`
- **WHEN** point enters a callout overlay carrying
  `gfm-pretty-callouts-revealable`
- **THEN** the engine SHALL reveal it
- **AND** SHALL NOT touch overlays carrying
  `gfm-pretty-fences-revealable` outside the callout

#### Scenario: Tables decorator skipped by reveal loop

- **GIVEN** the tables decorator's overlays carry no
  `gfm-pretty-tables-revealable` property
- **WHEN** point enters a rendered table cell
- **THEN** the engine's reveal loop SHALL NOT modify any table overlay
- **AND** the tables decorator's `:on-enable-fn`-installed cursor
  handler SHALL manage cell highlighting independently

### Requirement: Per-window decorator rendering

The engine SHALL invoke each enabled decorator's `:apply-block-fn`
once per (block, window) pair currently displaying the buffer.
Width-dependent overlay construction SHALL be the decorator's
responsibility; the engine SHALL pass the current window so the
decorator may compute width via `window-max-chars-per-line` or via
`gfm-pretty--available-width`.

Decorators that share width-independent overlay state across windows
SHALL call `gfm-pretty-borders--apply-with-anchors` from inside their
`:apply-block-fn` to lay anchor overlays at most once per (block,
current rebuild pass) while continuing to apply display overlays per
window.

A decorator's overlays SHALL be removed by tag (via
`gfm-pretty--remove-overlays` consulting the registry) when its
enable bit flips off or when the umbrella mode is disabled.

#### Scenario: Per-window display overlays in a split

- **GIVEN** a decorated buffer in W1 (80 cols) and W2 (120 cols)
- **WHEN** the engine performs a full rebuild
- **THEN** each block SHALL have one set of display overlays in W1
  sized to 80 cols
- **AND** another set in W2 sized to 120 cols
- **AND** width-independent overlays (anchors) created via
  `gfm-pretty-borders--apply-with-anchors` SHALL exist only once per
  block, not once per (block, window)

## ADDED Requirements

### Requirement: Engine owns per-decorator overlay state in `gfm-pretty--state`

Internals-facing. The engine SHALL store every decorator's overlay
list and revealable-hidden list inside `gfm-pretty--state`, keyed by
decorator name, under the `'overlays` and `'hidden-ovs` slots
respectively. No decorator SHALL declare module-local `defvar-local`
symbols for its overlay or hidden-overlay lists.

The `gfm-pretty--registry` struct SHALL NOT carry `overlays-symbol`
or `hidden-ovs-symbol` slots. Registry-aware primitives
(`gfm-pretty--register`, `gfm-pretty--remove-overlays`,
`gfm-pretty--prune-dead-overlays`, etc.) SHALL accept the decorator
name (or derive it from the registry's `tag`) and read/write through
the state alist.

#### Scenario: Adding a decorator's overlay updates engine state

- **GIVEN** the callouts decorator's `:apply-block-fn` runs for one
  block in one window
- **WHEN** it creates a display overlay via the engine's
  registry-aware overlay primitives
- **THEN** the overlay SHALL appear in
  `(gfm-pretty--state-get 'callouts 'overlays)`
- **AND** no symbol named `gfm-pretty-callouts--overlays` SHALL
  exist as a buffer-local variable

#### Scenario: Disabling a decorator clears its state slots

- **GIVEN** the fences decorator is enabled and has overlays
- **WHEN** `gfm-pretty-toggle-decorator 'fences` flips it off
- **THEN** `(gfm-pretty--state-get 'fences 'overlays)` SHALL be nil
- **AND** `(gfm-pretty--state-get 'fences 'hidden-ovs)` SHALL be nil
- **AND** every overlay tagged with the fences registry's `tag`
  SHALL have been removed from the buffer
