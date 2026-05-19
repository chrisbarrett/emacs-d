## ADDED Requirements

### Requirement: Scoped post-edit rebuild routing

The engine SHALL own the routing decision for scoped post-edit
rebuilds. After an `after-change-functions` burst settles, the engine's
idle timer SHALL invoke a per-decorator routing function that, given
the decorator's accumulated dirty region:

1. Calls the decorator's optional `:structural-line-ranges-fn` and, if
   the dirty region overlaps any returned `(BEG . END)` range, performs
   a full rebuild via `:rebuild-fn`.
2. Calls the decorator's optional `:edit-adjacency-fn` with the dirty
   region; if non-nil, performs a full rebuild.
3. Otherwise calls the decorator's `:collect-fn` (cache-hit normal)
   and matches the dirty region against each block's `:range-fn`
   result. If exactly one block fully contains the dirty region, the
   engine SHALL rebuild that single block via its `:apply-anchors-fn`
   plus `:apply-display-fn` (per current window). Otherwise the engine
   SHALL perform a full rebuild.

A decorator that does not register `:structural-line-ranges-fn` or
`:edit-adjacency-fn` SHALL still receive correct scoped rebuilds via
the block-containment fallback (step 3).

#### Scenario: Edit inside one block body triggers single-block rebuild

- **GIVEN** a buffer with two callouts and no other decorators
- **WHEN** the user types a character inside callout #1's body
- **AND** the idle timer fires
- **THEN** the engine SHALL match the dirty region against the
  collected blocks
- **AND** rebuild only callout #1 via its `:apply-anchors` /
  `:apply-display` callbacks
- **AND** callout #2's overlays SHALL be untouched

#### Scenario: Edit on a structural line forces full rebuild

- **GIVEN** a buffer with two fenced code blocks
- **WHEN** the user inserts a backtick on block #1's opening fence
  line
- **AND** the idle timer fires
- **THEN** the engine SHALL detect the dirty region overlaps a
  structural-line range from the fences decorator
- **AND** invoke the fences decorator's `:rebuild-fn` (full rebuild)

#### Scenario: Adjacency edit invalidates indent block

- **GIVEN** a buffer with an indented code block preceded by a blank
  line
- **WHEN** the user types on the blank line directly above the indent
  block
- **AND** the idle timer fires
- **THEN** the engine SHALL detect adjacency via the fences
  decorator's `:edit-adjacency-fn`
- **AND** invoke the fences decorator's `:rebuild-fn` (full rebuild)

### Requirement: Engine-owned generic rebuild stats

The engine SHALL track per-decorator rebuild stats (rebuild count,
total wall-time, last duration, max duration) in
`gfm-pretty--state`. The engine SHALL wrap every `:rebuild-fn`
invocation and every single-block rebuild path with a timer and
update the per-decorator slot. When a single rebuild exceeds
`gfm-pretty-slow-rebuild-threshold` (defcustom, default 0.05 s) the
engine SHALL emit `display-warning` of severity `:warning`.

`gfm-pretty-stats &optional DECORATOR` SHALL be an interactive
command. When DECORATOR is nil it SHALL prompt via
`completing-read` over registered decorators; with one symbol arg it
displays that decorator's stats. The output SHALL include any
phase-level totals the decorator has chosen to publish via an
optional `gfm-pretty--state` `phase-totals` slot.

Decorator-local phase instrumentation (e.g. `find-fenced`,
`compose-borders`, `compose-overflow`, `apply` in fences;
`find`, `compose`, `apply` in tables) SHALL remain owned by the
decorator that knows the phase boundaries; the decorator SHALL
write its phase totals into its own state slot for `gfm-pretty-stats`
to read.

#### Scenario: Engine times a rebuild

- **GIVEN** `gfm-pretty-mode` is enabled with the fences decorator
- **WHEN** the engine invokes the fences decorator's `:rebuild-fn`
- **THEN** the engine SHALL record the rebuild's wall-time into
  `gfm-pretty--state`'s `rebuild-stats` slot for `'fences`
- **AND** `(gfm-pretty-stats 'fences)` SHALL include `rebuild-count`,
  `total-time`, `last-time`, and `max-time`

#### Scenario: Slow rebuild logged via engine threshold

- **GIVEN** `gfm-pretty-slow-rebuild-threshold` is 0.05
- **WHEN** the tables decorator's `:rebuild-fn` takes 0.08 s
- **THEN** the engine SHALL emit a `display-warning` of severity
  `:warning`

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
- `:collect FN` — returns the buffer's list of blocks (under widening).
- `:range FN` — returns `(BEG . END)` for a block.
- `:apply-anchors FN` — applies a block's width-independent overlays.
- `:apply-display FN` — applies a block's width-dependent per-window
  overlays.
- `:rebuild FN` (optional) — full rebuild override. When omitted, the
  engine performs generic teardown + reapply using `:collect`,
  `:apply-anchors`, `:apply-display`.
- `:structural-line-ranges FN` (optional, no-arg) — returns
  `((BEG . END) ...)` ranges whose intersection with a dirty region
  forces a full rebuild (e.g. fence opening / closing lines, callout
  marker lines, table header / delimiter / trailing-body lines).
- `:edit-adjacency FN` (optional, takes the dirty region) — returns
  non-nil when the dirty region overlaps a line whose edit could
  create or destroy an adjacency-gated block (e.g. blank line above /
  below an indent code block; line above / below a callout).
- `:revealable-p FN` (optional) — predicate for cursor reveal.
- `:revealable-prop SYM` / `:saved-display-prop SYM` (optional) —
  overlay property tags used by the engine's standard reveal walker.
- `:reveal FN` (optional) — bespoke reveal handler (used by links to
  group whole-link overlays by id).
- `:reconcile-windows FN` (optional) — bespoke window-state reconciler
  (used by tables).
- `:block-at-point FN` (optional) — predicate for the public block
  introspection API.
- `:edit-at-point FN` (optional) — command for the public edit
  dispatcher.
- `:on-enable FN` (optional) — decorator extras to install when the
  umbrella mode (or `gfm-pretty-toggle-decorator`) enables.
- `:on-disable FN` (optional) — symmetric teardown.

The registration protocol SHALL NOT include a `:scoped-rebuild` key.
Scoped rebuild policy is engine-owned (see "Scoped post-edit rebuild
routing").

#### Scenario: Registering a new decorator

- **WHEN** a new visual-decoration feature is added
- **THEN** it SHALL declare itself via
  `(gfm-pretty-define-decorator 'NAME …)` with the contributions it
  needs
- **AND** the engine SHALL invoke its functions during rebuild, reveal,
  and reconcile without further wiring

#### Scenario: Decorator declines to override scoped-rebuild policy

- **GIVEN** a decorator registers `:apply-anchors`, `:apply-display`,
  `:collect`, `:range`
- **AND** does NOT register `:structural-line-ranges` or
  `:edit-adjacency`
- **WHEN** the user edits inside one block's body
- **THEN** the engine SHALL fall through to block-containment scoping
- **AND** rebuild only that block

### Requirement: Callout scoped post-edit rebuild

The callouts decorator SHALL register `:structural-line-ranges`
returning the `> [!TYPE]` marker line ranges, and `:edit-adjacency`
returning non-nil when the dirty region overlaps a line directly
above or below an existing callout. The engine's routing (see
"Scoped post-edit rebuild routing") SHALL use these to choose between
full rebuild, adjacency-driven full rebuild, and single-block
scoped rebuild.

When the dirty region intersects a callout body line (not the marker
and not adjacent to another callout), the engine SHALL rebuild only
the containing callout via that block's `:apply-anchors` and
`:apply-display` callbacks. When the dirty region does not overlap
any callout block range or contribution from
`:structural-line-ranges` / `:edit-adjacency`, the callouts decorator
SHALL NOT contribute work to the rebuild iteration.

#### Scenario: Edit inside one callout body

- **GIVEN** two non-adjacent callouts
- **WHEN** the user edits inside callout #1's body
- **THEN** only callout #1 is rebuilt by the engine
- **AND** callout #2's overlays are untouched

#### Scenario: Edit on a callout marker forces full rebuild

- **GIVEN** two callouts in a buffer
- **WHEN** the user edits callout #1's `> [!NOTE]` marker line
- **THEN** the engine detects the structural-line intersection
- **AND** invokes the callouts decorator's `:rebuild` (full rebuild)

### Requirement: Code-fence scoped post-edit rebuild

The fences decorator SHALL register `:structural-line-ranges`
returning every fence opening line, every fence closing line, and
the YAML helmet's two `---` marker lines. It SHALL register
`:edit-adjacency` returning non-nil when the dirty region overlaps
a blank line directly above or below an indent code block (where
discovery is blank-line-gated).

The engine's routing (see "Scoped post-edit rebuild routing")
SHALL use these so an edit on a fence line or a blank line adjacent
to an indent block forces a full fences rebuild, an edit fully
contained in one block scopes to that block, and edits outside
every fences range produce no fences work.

#### Scenario: Edit inside fenced body

- **GIVEN** two fenced blocks
- **WHEN** the user edits inside block #1's body (not a marker)
- **THEN** only block #1 is rebuilt by the engine via the fences
  decorator's apply callbacks

#### Scenario: Blank line above an indent block becomes non-blank

- **GIVEN** an indent code block preceded by a blank line
- **WHEN** the user types on that preceding blank line
- **THEN** the engine detects adjacency
- **AND** invokes the fences decorator's `:rebuild` (full rebuild)

### Requirement: Code-fence performance instrumentation

The fences decorator SHALL contribute *phase-level* timing (`find-fenced`,
`find-yaml`, `find-indent`, `compose-borders`, `compose-overflow`,
`apply`) to its own state's `phase-totals` slot during rebuilds.
Rebuild count, total / last / max wall-time, and the slow-rebuild
warning are owned by the engine (see "Engine-owned generic rebuild
stats") and apply uniformly to every decorator.

`gfm-pretty-fences-stats` SHALL remain an interactive command that
displays the fences decorator's full stats; it MAY be implemented as
a thin call to `(gfm-pretty-stats 'fences)`.

#### Scenario: Slow rebuild logged

- **GIVEN** a buffer where rendering a single fence takes 0.1 s
- **WHEN** that rebuild completes
- **THEN** the engine SHALL emit `display-warning` of severity
  `:warning` (engine-level threshold)
- **AND** `gfm-pretty-fences-stats` SHALL show the elapsed time plus
  the per-phase breakdown

### Requirement: Table performance instrumentation

The tables decorator SHALL contribute phase-level totals (`find`,
`compose`, `apply`) to its own state's `phase-totals` slot during
rebuilds. Rebuild count, total / last / max wall-time, and the
slow-rebuild warning are owned by the engine (see "Engine-owned
generic rebuild stats").

`gfm-pretty-tables-stats` SHALL remain an interactive command that
displays the tables decorator's full stats; it MAY be implemented as
a thin call to `(gfm-pretty-stats 'tables)`.

#### Scenario: Slow rebuild

- **GIVEN** a rebuild taking 0.08 s
- **THEN** the engine SHALL emit a `display-warning`
- **AND** `gfm-pretty-tables-stats` SHALL show the elapsed time plus
  the per-phase breakdown
