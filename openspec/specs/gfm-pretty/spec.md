# gfm-pretty Specification

## Purpose

Lives at `lisp/gfm/gfm-pretty.el` (library axis). Behaviour-facing:
defines the umbrella minor mode (`gfm-pretty-mode`), the decorator
registration protocol (`gfm-pretty-define-decorator`), the public
block-introspection API (`gfm-pretty-block-at-point`,
`gfm-pretty-edit-block-at-point`), per-decorator toggling
(`gfm-pretty-toggle-decorator`), and the rendering / scheduling
contracts of the six built-in decorators (callouts, blockquotes,
fences, tables, hrule, links).

Cross-references the `gfm-present` axis for the slide walkthrough mode
that operates over `gfm-pretty`-decorated buffers.

---

## Requirements
<!-- ── Engine: umbrella mode and registration ─────────────────────── -->

### Requirement: Umbrella minor mode

The system SHALL provide a buffer-local minor mode `gfm-pretty-mode`
that enables every registered decorator with a non-nil enable bit and
removes every overlay it created when disabled.

`gfm-pretty-mode` SHALL be enabled in every `gfm-mode` buffer via
`gfm-mode-hook` (added by the `lang-markdown` composition module,
which `(require 'gfm-pretty)` and enables the mode).

#### Scenario: Enabling the umbrella mode in a markdown buffer

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing GFM
  callouts, fenced code blocks, tables, hrules, and links
- **THEN** every registered decorator's anchor and display overlays
  are applied
- **AND** the callout font-lock keywords are installed
- **AND** the buffer renders with all visual decorations

#### Scenario: Disabling the umbrella mode

- **WHEN** `gfm-pretty-mode` is disabled in a decorated buffer
- **THEN** every overlay created by every decorator is removed
- **AND** every decorator's `:on-disable` callback runs (e.g. removing
  the links xref backend and eldoc function)
- **AND** the buffer visually matches its raw markdown source

### Requirement: TAB-key dispatch in `gfm-pretty-mode`

`gfm-pretty-mode` SHALL bind `TAB` in its own keymap
(`gfm-pretty-mode-map`) to a command that dispatches by point context,
and SHALL NOT fall through to `indent-for-tab-command` /
`markdown-indent-line` in non-list contexts.

The dispatch SHALL be:

- On a heading line (`markdown-on-heading-p`) — invoke
  `markdown-cycle` interactively so heading visibility cycling
  (including its `last-command` state machine) keeps working.
- Inside a table (`markdown-table-at-point-p`) — invoke
  `markdown-table-forward-cell`.
- On a list-item prefix slot — invoke `markdown-indent-line` once.
  The "prefix slot" predicate SHALL match
  `^\s-*\(?:[-*+]\|[0-9]+[.)]\)\s-+` and require point ≤
  `(match-end 0)`; this branch SHALL additionally require
  `(and (bound-and-true-p evil-mode) (evil-insert-state-p))`.
- Everywhere else — no-op. No whitespace SHALL be inserted into the
  buffer.

Disabling `gfm-pretty-mode` SHALL restore the prior `TAB` binding
(`markdown-cycle`) by deactivating the buffer-local keymap.

#### Scenario: TAB on a callout marker line is a no-op

- **GIVEN** a buffer with `> [!NOTE]` at BOL and `gfm-pretty-mode`
  enabled
- **WHEN** point is at column 0 of the marker line and the user
  presses `TAB`
- **THEN** the buffer SHALL be unchanged (no spaces inserted)
- **AND** the callout block SHALL still parse and decorate correctly

#### Scenario: TAB on a heading cycles visibility

- **GIVEN** a buffer with `## Section` and `gfm-pretty-mode` enabled
- **WHEN** point is on the heading line and the user presses `TAB`
- **THEN** the engine SHALL invoke `markdown-cycle` interactively
- **AND** the heading's subtree visibility state SHALL advance

#### Scenario: TAB inside a table moves to the next cell

- **GIVEN** a GFM table with point inside a cell and
  `gfm-pretty-mode` enabled
- **WHEN** the user presses `TAB`
- **THEN** point SHALL move to the next cell via
  `markdown-table-forward-cell`

#### Scenario: TAB at list-item prefix slot indents in evil insert state

- **GIVEN** a buffer with `- item` at BOL, `gfm-pretty-mode` enabled,
  and `evil-insert-state-p` returning non-nil
- **WHEN** point is between BOL and the start of `item` and the user
  presses `TAB`
- **THEN** the engine SHALL invoke `markdown-indent-line` once
- **AND** the list-item line SHALL be indented one step

#### Scenario: TAB at list-item prefix slot outside evil insert state is a no-op

- **GIVEN** a buffer with `- item` at BOL, `gfm-pretty-mode` enabled,
  and `evil-insert-state-p` returning nil
- **WHEN** point is between BOL and the start of `item` and the user
  presses `TAB`
- **THEN** the buffer SHALL be unchanged

#### Scenario: TAB in paragraph body is a no-op

- **GIVEN** a buffer with a plain paragraph line and
  `gfm-pretty-mode` enabled
- **WHEN** point is mid-paragraph and the user presses `TAB`
- **THEN** the buffer SHALL be unchanged

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
   currently-displayed window. If no collected block overlaps the
   dirty region (the edit destroyed the block whose overlays the
   decorator owned), the engine SHALL perform a full rebuild via
   `:rebuild-fn` (or generic teardown + reapply when `:rebuild-fn` is
   absent) so stale overlays are evacuated. Otherwise (multiple
   overlapping blocks) the engine SHALL also perform a full rebuild.

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

#### Scenario: Edit destroys a tracked block, leaving no overlap

- **GIVEN** a buffer containing a single callout `> [!NOTE]` with
  the callouts decorator enabled
- **WHEN** an edit inserts a leading space at BOL of the marker line
  (e.g. via a fallback indent path), so the marker no longer parses
  as a callout
- **AND** the idle timer fires
- **THEN** `:collect-fn` SHALL return no callout block overlapping
  the dirty region
- **AND** the engine SHALL perform a full rebuild of the callouts
  decorator
- **AND** no overlays tagged with the callouts decorator's registry
  SHALL remain in the now-destroyed block's former range

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

### Requirement: Per-decorator toggle via `gfm-pretty-toggle-decorator`

The system SHALL expose `gfm-pretty-toggle-decorator NAME` (interactive)
which flips a buffer-local enable bit for the named decorator. When the
bit is nil, the engine SHALL skip that decorator during rebuild,
reveal, and reconcile, and the decorator's overlays SHALL be removed.
When toggled back on, the decorator's overlays SHALL be rebuilt.

#### Scenario: Toggling the tables decorator off

- **GIVEN** `gfm-pretty-mode` is enabled with the tables decorator
  active
- **WHEN** the user calls `(gfm-pretty-toggle-decorator 'tables)`
- **THEN** every table overlay is removed
- **AND** other decorators' overlays remain untouched
- **AND** the leader binding `SPC m t` (configured to call this toggle)
  reports the new state

### Requirement: Public block introspection

The system SHALL expose `gfm-pretty-block-at-point` and
`gfm-pretty-edit-block-at-point` as the only public block-aware
entry points. `gfm-pretty-block-at-point` SHALL return
`(DECORATOR-NAME . BLOCK)` for the first enabled decorator whose
`gfm-pretty-<name>--block-at-point` function returns non-nil, or nil
if none matches. `gfm-pretty-edit-block-at-point` SHALL dispatch to
the matched decorator's `gfm-pretty-<name>--edit-at-point` function.
Participation in this dispatch is a naming convention — a decorator
opts in by exporting both functions, with no registry slot involved.

No `gfm-pretty--*` private symbol SHALL be relied upon by code outside
the `gfm-pretty` module.

#### Scenario: Leader binding queries the public API

- **GIVEN** `modules/leader/init.el` provides the local-leader `t`
  binding for "edit block at point"
- **WHEN** point is inside a GFM table
- **THEN** the binding calls `gfm-pretty-edit-block-at-point`
- **AND** `gfm-pretty-tables--edit-at-point` is invoked
- **AND** no private `gfm-pretty--*` symbol is read

### Requirement: Rendering primitives are public

The system SHALL expose the following primitives for decorator authors:

- `gfm-pretty-available-width &optional WINDOW` — char width
  primitive.
- `gfm-pretty-top-border WIDTH FACE BUFFER-WIDTH &optional ICON` —
  top-border (LEADING . TRAILING) split.
- `gfm-pretty-bottom-border WIDTH FACE BUFFER-WIDTH` — bottom-border
  split.
- `gfm-pretty-right-after BOX-WIDTH FACE &optional BG` — right-edge
  after-string.
- `gfm-pretty-simulate-wrap TEXT WIDTH &optional CONT-PREFIX-W` —
  wrap-position simulator.
- `gfm-pretty-make-anchor BEG END &rest PROPS` — anchor-overlay
  factory bound to the calling decorator's registry.
- `gfm-pretty-make-display BEG END WINDOW &rest PROPS` —
  display-overlay factory restricted to WINDOW when non-nil.

Every other engine symbol SHALL be `gfm-pretty--` private.

#### Scenario: Decorator builds a bordered block

- **GIVEN** a decorator's `:apply-block-fn` callback
- **WHEN** it constructs a border for the current window
- **THEN** it calls `gfm-pretty-available-width` and
  `gfm-pretty-top-border` / `gfm-pretty-bottom-border` /
  `gfm-pretty-right-after`
- **AND** it does NOT reference `gfm-pretty--*` internal helpers

<!-- ── Engine: cross-cutting contracts ────────────────────────────── -->

### Requirement: Debounced rebuild scheduler

The engine SHALL install exactly one of each lifecycle hook per
buffer when `gfm-pretty-mode` enables: one `after-change-functions`
handler, one `window-configuration-change-hook` handler, one
`post-command-hook` reveal handler, and one idle rebuild timer.

The timer SHALL fire after a 0.2 second idle delay following a
buffer modification, iterate every enabled decorator, and rebuild
within the buffer's accumulated dirty region. The engine SHALL
cancel and re-arm the timer on every modification so a burst of
edits produces one rebuild after the burst ends.

Decorators SHALL NOT install their own `after-change-functions`
or `post-command-hook` handlers for scheduling purposes. Decorators
MAY use `:on-enable-fn` / `:on-disable-fn` to install
decorator-specific hooks — for example, the tables decorator
installs its own buffer-local `window-configuration-change-hook`
handler for its bespoke per-window reconciler, and the links
decorator installs an xref backend.

#### Scenario: Burst of edits causes one rebuild

- **GIVEN** `gfm-pretty-mode` is enabled
- **WHEN** the user types ten characters in rapid succession
- **THEN** the engine SHALL cancel and re-arm the timer on each edit
- **AND** the rebuild SHALL run once, 0.2 s after the last edit
- **AND** every decorator's `:apply-block-fn` SHALL be invoked over
  the dirty region

#### Scenario: One handler per hook regardless of decorator count

- **GIVEN** a buffer with five enabled decorators
- **WHEN** the engine installs lifecycle hooks
- **THEN** `(length after-change-functions)` SHALL increase by one
  (engine handler only)
- **AND** `(length post-command-hook)` SHALL increase by one
- **AND** at most one buffer-local idle timer SHALL be live

### Requirement: Block discovery memoisation

The engine SHALL memoise each registered decorator's `:collect-fn`
result by `buffer-chars-modified-tick` per decorator per buffer.
Repeated calls within an unmodified buffer SHALL NOT rescan; the
engine returns the cached block list. The first call after a buffer
modification SHALL rescan and refresh the cache.

The engine SHALL invoke `:collect-fn` under `save-restriction` plus
`widen` so a narrowed buffer (e.g. under `gfm-present-mode`) still
sees the full block set. Decorators register their *uncached, widened*
discovery function; the engine wraps it.

This subsumes the previous per-decorator block-discovery cache
requirements.

#### Scenario: Repeated reveal calls hit cache

- **GIVEN** a buffer with 20 callouts and `gfm-pretty-mode` enabled
- **WHEN** `post-command-hook` invokes reveal twice between edits
- **THEN** the callouts decorator's `:collect-fn` SHALL run once
- **AND** the second reveal SHALL use the cached block list

#### Scenario: Cache invalidates on buffer modification

- **GIVEN** a cached block list for the fences decorator
- **WHEN** any buffer modification advances `buffer-chars-modified-tick`
- **THEN** the next collect call SHALL rescan and replace the cache

#### Scenario: Narrowed buffer sees the full block set

- **GIVEN** a buffer with three fenced blocks narrowed to the second
- **WHEN** the engine collects for the fences decorator
- **THEN** the result SHALL include all three blocks
- **AND** the buffer's narrowing SHALL be preserved after the call

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

### Requirement: Selective per-window reconciliation

When `window-configuration-change-hook` fires, the engine SHALL compare
the prior `(window . width)` snapshot against the current set. Removed
windows SHALL have their display overlays deleted synchronously.
Added or resized windows SHALL be rebuilt on the next idle tick,
visible blocks first, off-screen blocks after, one block per idle tick.
Unchanged windows SHALL keep their overlays.

#### Scenario: Splitting the window

- **GIVEN** a decorated buffer in W1
- **WHEN** the user runs `C-x 3` producing W2 of the same width
- **THEN** the engine SHALL pace W2's rebuild one block per idle tick
- **AND** W1's overlays SHALL remain untouched

### Requirement: Visible-first prioritised rebuild

When a width change triggers a per-window rebuild, the engine SHALL
render blocks intersecting the window's `(window-start . window-end)`
range first, then off-screen blocks one per idle tick, until the queue
is exhausted.

#### Scenario: Large file with many blocks

- **GIVEN** a 1000-line markdown file with 30 fenced blocks
- **WHEN** the window width changes
- **THEN** the blocks visible in the current window SHALL be rendered
  in the first idle ticks
- **AND** off-screen blocks SHALL render in later ticks

### Requirement: Wrap simulation always terminates

`gfm-pretty-simulate-wrap` SHALL terminate for every input. When the
effective per-line width is less than or equal to the continuation
wrap-prefix width, the simulator SHALL clamp the line width to at
least 1 so the position counter advances on every loop iteration.

This guards against tiny-window transients (e.g. `C-x 3` splits where
one window briefly drops below the wrap-prefix width).

#### Scenario: Three-column window

- **GIVEN** a callout body line of 200 characters
- **WHEN** the window is 3 columns wide (less than the 2-char
  wrap-prefix)
- **THEN** `gfm-pretty-simulate-wrap` SHALL return a result
- **AND** the function SHALL NOT loop forever

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

### Requirement: Engine per-decorator overlay-freshness tick

The engine SHALL track a per-decorator `last-rebuild-tick` value in
`gfm-pretty--state`, set to `(buffer-chars-modified-tick)` at the end of every
successful `gfm-pretty--rebuild`. A predicate
`gfm-pretty--stale-p (DECORATOR-NAME)` SHALL return non-nil when the slot is
nil or strictly less than the current buffer tick; nil otherwise.

The tick is buffer-local (via the existing `defvar-local` of
`gfm-pretty--state`) and survives across window-configuration changes. It is
cleared to nil when the decorator is disabled or when the engine performs a
full state reset (e.g. `before-revert-hook` teardown).

Decorators MAY consult `gfm-pretty--stale-p` from any context — motion
commands, post-command hooks, etc. — to gate behaviour that reads stale
overlay-derived data (integer positions, fixnum offset vectors, hash keys)
that does not auto-adjust to buffer edits.

#### Scenario: Tick set on rebuild

- **WHEN** `gfm-pretty--rebuild` completes for a decorator in a buffer with
  tick `T`
- **THEN** `gfm-pretty--stale-p` for that decorator returns nil
- **AND** the next buffer modification advances the tick beyond `T`
- **AND** `gfm-pretty--stale-p` returns non-nil until the next rebuild
  completes

#### Scenario: Tick cleared on disable

- **WHEN** a decorator is disabled via
  `gfm-pretty--disable-decorator`
- **THEN** its `last-rebuild-tick` slot is nil
- **AND** `gfm-pretty--stale-p` returns non-nil

### Requirement: Engine revert lifecycle

The engine SHALL install `before-revert-hook` and `after-revert-hook`
handlers buffer-locally in every buffer where `gfm-pretty-mode` is active.

`before-revert-hook` SHALL, for each enabled decorator: remove every
registered overlay via the decorator's registry and clear the decorator's
`dirty-region`, `last-window-state`, `hidden-ovs`, `anchors-laid`, and
`last-rebuild-tick` state slots. The pending engine idle rebuild timer
SHALL be cancelled.

`after-revert-hook` SHALL synchronously run the engine's scheduled-rebuild
routine for each enabled decorator (full rebuild, since `dirty-region` is
nil), producing a fresh overlay set before control returns to the user.

The handlers SHALL be removed by `gfm-pretty--remove-engine-hooks`
together with the existing engine hooks.

#### Scenario: Overlay state cleared before revert

- **WHEN** `before-revert-hook` runs in a `gfm-pretty-mode` buffer with an
  enabled decorator
- **THEN** that decorator's `overlays` state slot is empty
- **AND** every position-bearing state slot (`dirty-region`,
  `last-window-state`, `hidden-ovs`, `anchors-laid`,
  `last-rebuild-tick`) is nil

#### Scenario: Synchronous rebuild after revert

- **GIVEN** a `gfm-pretty-mode` buffer with the tables decorator enabled
- **WHEN** `revert-buffer t t` completes
- **THEN** `after-revert-hook` has run a full rebuild for tables
- **AND** the decorator's `last-rebuild-tick` equals the buffer's current
  `buffer-chars-modified-tick`
- **AND** the first subsequent user command sees a fresh overlay set
  before executing

<!-- ── Callouts decorator ─────────────────────────────────────────── -->

### Requirement: Callouts decorator registration and toggle

The system SHALL register a decorator named `callouts` that applies a
bordered-callout overlay decoration plus a font-lock keyword
contribution colouring `> [!TYPE]` markers and bodies.

`(gfm-pretty-toggle-decorator 'callouts)` SHALL flip both layers
together — overlays and font-lock keywords.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer with callouts
- **THEN** each recognised callout (NOTE, TIP, IMPORTANT, WARNING,
  CAUTION, CRITICAL) is decorated with a curved-border box, a
  type-coloured top label, and a tinted background
- **AND** the `> [!TYPE]` marker text is fontified by the callouts
  font-lock keywords

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'callouts)` flips the enable
  bit off
- **THEN** every callout overlay is removed
- **AND** the callout font-lock keywords are removed
- **AND** the marker text reverts to default markdown-mode
  fontification

### Requirement: Callout block discovery

The callouts decorator's `:collect` SHALL recognise a callout block as
a marker line of the form `> [!TYPE]` where `TYPE` is one of `NOTE`,
`TIP`, `IMPORTANT`, `WARNING`, `CAUTION`, `CRITICAL`, followed by zero
or more `>`-prefixed continuation lines. The block range SHALL run
from the marker line's BOL to the last continuation line's EOL.

Discovery SHALL widen before scanning so a narrowed buffer (e.g. under
`gfm-present-mode`) still sees the full block boundaries.

#### Scenario: Single-line marker

- **GIVEN** a buffer line `> [!NOTE]`
- **WHEN** `:collect` runs
- **THEN** the returned block range covers the line only

#### Scenario: Multi-line callout

- **GIVEN** a marker `> [!IMPORTANT]` followed by `> body line 1` and
  `> body line 2`
- **THEN** the block range covers all three lines

### Requirement: Callout overlay tint faces

The system SHALL define one derived tint face per callout type:

- `gfm-pretty-callouts-note-tint-face`
- `gfm-pretty-callouts-tip-tint-face`
- `gfm-pretty-callouts-important-tint-face`
- `gfm-pretty-callouts-warning-tint-face`
- `gfm-pretty-callouts-caution-tint-face`

Each face's default spec MUST be empty (`'((t))`) — the `:background`
is set dynamically from the per-type blend at theme-change time, not
specified statically.  CAUTION and CRITICAL callouts SHALL share the
caution tint face (mirroring the existing type-face mapping).

The callouts decorator's `:apply-block-fn` SHALL reference these
faces by name (via `:inherit`) in every overlay face spec it
constructs; it MUST NOT bake `(face-background <tint-face>)` or
`(gfm-pretty-callouts--tinted-bg <border-face>)` as a literal
`:background "#hex"` string into any overlay's face plist.

#### Scenario: Tint faces exist with empty default specs

- **GIVEN** `gfm-pretty-callouts` is loaded
- **THEN** each of the five tint faces SHALL be defined
- **AND** `face-attribute <tint-face> :background nil 'default` SHALL
  return `unspecified` (no static colour in the defface)

#### Scenario: Overlay face specs reference tint faces

- **GIVEN** a NOTE callout rendered in a buffer with `gfm-pretty-mode`
  enabled
- **WHEN** the anchor overlay's `face` property is inspected
- **THEN** the face spec SHALL carry
  `:inherit gfm-pretty-callouts-note-tint-face`
- **AND** the face spec SHALL NOT carry a `:background "#hex"`
  literal colour string

### Requirement: Callout bordered-block rendering

The callouts decorator's `:apply-block-fn` SHALL render a bordered
callout box with:

- A top border using the type's coloured face and the type label as
  the upper-right caption.
- A `│ ` substitution for the body-line blockquote prefix on each
  body line, using the border face. The substitution SHALL cover
  both the two-char `> ` prefix and the one-char bare `>` form (a
  blockquote continuation line with no trailing space, used in
  practice for blank rows between body paragraphs).
- A right-edge `│` painted via `gfm-pretty-right-after` (or its
  overflow variant on wrapped body lines) so the right border
  aligns to the box width regardless of body-line wrapping.
- A bottom border using the type's coloured face.
- A tinted body background sourced from the type's tint face (e.g.
  `gfm-pretty-callouts-note-tint-face`). Every overlay face spec
  SHALL reference the tint face by name via `:inherit`; the
  decorator SHALL NOT bake a `:background "#hex"` literal into any
  overlay face plist (so theme changes propagate at the next
  redisplay without a decorator rebuild).

The decorator's `:apply-block-fn` SHALL call
`gfm-pretty-borders--apply-with-anchors` so anchor overlays
(carrying the body-face property and a `wrap-prefix` of `│ `) are
laid at most once per (block, rebuild pass) while per-window
display overlays (borders and right-edge after-strings) apply once
per window.

#### Scenario: NOTE callout

- **GIVEN** `> [!NOTE]\n> hello`
- **WHEN** the decorator renders
- **THEN** the top border shows `┌── NOTE ──┐` (right-aligned label)
- **AND** the body line shows `│ hello` with a blue-tinted background
- **AND** the bottom border shows `└──────────┘`

#### Scenario: Bare-`>` body line

- **GIVEN** `> [!IMPORTANT]\n> first paragraph\n>\n> second paragraph`
  (note the middle body line is a single `>` with no trailing space)
- **WHEN** the decorator renders
- **THEN** the middle body line shows `│` at the left edge (display
  string `│ `) with no raw `>` character visible
- **AND** the right-edge `│` lands on the box-width column on that
  line, matching the surrounding body rows
- **AND** the per-window body-prefix overlay over the bare `>`
  carries `gfm-pretty-callouts-kind 'body-prefix` and the
  `gfm-pretty-callouts-revealable` property so the reveal walker
  exposes the raw `>` when point sits on the line in the selected
  window

#### Scenario: Overlay face specs carry no baked colour strings

- **GIVEN** a TIP callout rendered in a buffer
- **WHEN** any anchor, prefix-display, or rhs-after-string overlay
  for the block is inspected
- **THEN** none of those overlays' `face` specs SHALL contain a
  `:background` plist key whose value is a string (colour literal)
- **AND** each spec SHALL reference `gfm-pretty-callouts-tip-tint-face`
  via `:inherit`

### Requirement: Callout box width sizing

Box width SHALL be `min(text-width, max(80, max-content-width + 4))`
per window, where `text-width` is `gfm-pretty-available-width` for the
window and `max-content-width` is the longest body line's visible
width minus the 2-char `> ` prefix.

#### Scenario: Wide content, narrow window

- **GIVEN** a callout whose longest body line is 60 cells
- **AND** a window 100 cells wide
- **THEN** the box width SHALL be `min(100, max(80, 64)) = 80`

#### Scenario: Narrow content, wide window

- **GIVEN** a callout whose longest body line is 20 cells
- **AND** a window 100 cells wide
- **THEN** the box width SHALL be `min(100, max(80, 24)) = 80`

#### Scenario: Wide content, very wide window

- **GIVEN** a callout whose longest body line is 200 cells
- **AND** a window 250 cells wide
- **THEN** the box width SHALL be `min(250, max(80, 204)) = 204`

### Requirement: Callout wrapped right-edge alignment

For a body line wrapping in the window, the right-edge `│` SHALL be
padded by simulating wrap via `gfm-pretty-simulate-wrap` so the
border lands at the box width on the final visual row, regardless of
how the line wraps.

#### Scenario: Long body line wraps mid-line

- **GIVEN** a 200-char body line in an 80-col window
- **WHEN** the right-edge after-string is computed
- **THEN** the `│` lands at column 80 on the final wrapped visual row

### Requirement: Callout marker line and body prefix reveal

The callouts decorator SHALL carry the engine's revealable property
(`gfm-pretty-callouts-revealable`) on the marker-line top and on
each per-line `> ` → `│ ` body-prefix display overlay. The engine's
reveal walker SHALL hide those overlays in the selected window when
point lies on them, exposing the raw `> [!TYPE]` or `> ` source.
The property name SHALL be derived from the callouts registry's
`tag`; the decorator SHALL NOT register it separately. Reveal SHALL
be scoped to the selected window.

#### Scenario: Point on marker

- **GIVEN** a NOTE callout with point on the marker line in W1
  (selected)
- **THEN** W1 shows the raw `> [!NOTE]` text
- **AND** the top-border decoration is visible
- **AND** other windows showing the buffer continue to show the
  marker decoration

### Requirement: Callout narrowing-resilient discovery and teardown

`:collect` SHALL widen the buffer for the duration of its scan. Overlay
teardown via the engine's bulk-cleanup helper SHALL also widen so the
registry list and on-buffer overlay set stay in lockstep regardless of
any current narrowing.

When the engine rebuilds within a sub-region (e.g. under
`gfm-present-mode` narrowing), the decorator's overlay set SHALL
converge with the same overlays that would be present in a clean
widened rebuild — the suite
`(narrow → rebuild → widen → rebuild)` SHALL produce a steady state.

#### Scenario: Narrowed rebuild then widen

- **GIVEN** a buffer with three callouts narrowed to the second
- **WHEN** `gfm-pretty--rebuild` runs, then the buffer is widened, then
  rebuilt again
- **THEN** the final overlay set SHALL match what a fresh widened
  rebuild produces
- **AND** no `args-out-of-range` SHALL signal

### Requirement: Callout scoped post-edit rebuild

The callouts decorator SHALL register
`:full-rebuild-required-p` that returns non-nil when the dirty
region overlaps a `> [!TYPE]` marker line (structural-line case) or
overlaps a line directly above or below an existing callout
(adjacency case). The two conditions are OR-combined inside the
predicate. The engine's routing (see "Scoped post-edit rebuild
routing") SHALL use the predicate to choose between full rebuild
and single-block scoped rebuild.

When the dirty region intersects a callout body line (not the
marker and not adjacent to another callout), the engine SHALL
rebuild only the containing callout via its `:apply-block-fn`
(per displayed window). When the dirty region does not overlap any
callout block range and does not trigger the predicate, the
callouts decorator SHALL NOT contribute work to the rebuild
iteration.

#### Scenario: Edit inside one callout body

- **GIVEN** two non-adjacent callouts
- **WHEN** the user edits inside callout #1's body
- **THEN** only callout #1 is rebuilt by the engine
- **AND** callout #2's overlays are untouched

#### Scenario: Edit on a callout marker forces full rebuild

- **GIVEN** two callouts in a buffer
- **WHEN** the user edits callout #1's `> [!NOTE]` marker line
- **THEN** `:full-rebuild-required-p` returns non-nil for the dirty
  region
- **AND** the engine invokes the callouts decorator's `:rebuild-fn`
  (full rebuild)

### Requirement: Theme change responsiveness

The callouts decorator's `:on-enable-fn` SHALL add a face-refresh
helper to `+theme-changed-hook` that recomputes per-type tints from
the current theme background.  `:on-disable-fn` SHALL remove it.

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

- **WHEN** the user switches from a light to a dark theme
- **THEN** `+theme-changed-hook` fires
- **AND** each `gfm-pretty-callouts-*-body-face` background is
  recomputed
- **AND** each `gfm-pretty-callouts-*-tint-face` background is
  recomputed in the same pass
- **AND** the next redisplay shows correctly tinted body backgrounds

#### Scenario: Theme switch propagates to already-rendered overlays

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and at least one
  callout rendered
- **WHEN** the user switches theme
- **AND** `gfm-pretty-mode` is NOT toggled off and on
- **THEN** the existing overlays SHALL re-resolve their face specs at
  the next redisplay
- **AND** the rendered callout panel SHALL show the new theme's tint
  colour, not the prior theme's

<!-- ── Blockquotes decorator ──────────────────────────────────────── -->

### Requirement: Blockquotes decorator registration and toggle

The system SHALL register a decorator named `blockquotes` that applies a left-rail overlay decoration on plain GFM blockquote blocks (runs of `^>`-prefixed lines that do NOT belong to a callout block).

The decorator SHALL render the rail (`▌ `) at a configurable column inset (default `tab-width`).  The inset columns to the left of the rail render in the default face (no tint).  No top, right, or bottom decoration glyph is painted.

`(gfm-pretty-toggle-decorator 'blockquotes)` SHALL flip the decorator on and off.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing a plain blockquote
- **THEN** each plain blockquote line shows a `▌` rail glyph at column `gfm-pretty-blockquotes-inset-cols`
- **AND** soft-wrapped continuation visual rows show the same inset gutter and `▌ ` rail prefix in `gfm-pretty-blockquotes-rail-face`

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'blockquotes)` flips the enable bit off
- **THEN** every blockquote overlay (anchor, prefix display) is removed
- **AND** the raw `>` characters and `markdown-mode`'s default `wrap-prefix "> "` reappear on continuation rows

### Requirement: Blockquote block discovery

The blockquotes decorator's `:collect` SHALL recognise a blockquote block as a maximal run of consecutive lines whose first character is `>`. The block range SHALL run from the first line's BOL to the last line's EOL.

Runs that contain a `> [!TYPE]` marker line (where `TYPE` is one of `NOTE`, `TIP`, `IMPORTANT`, `WARNING`, `CAUTION`, `CRITICAL`) SHALL be excluded so the callouts decorator retains ownership of those blocks.

Adjacent `>`-prefixed runs separated by one or more blank lines SHALL be returned as distinct blocks.

Discovery SHALL widen before scanning so a narrowed buffer (e.g. under `gfm-present-mode`) still sees the full block boundaries.

#### Scenario: Single-line blockquote

- **GIVEN** a buffer line `> Pain: something`
- **WHEN** `:collect` runs
- **THEN** the returned block range covers the line only

#### Scenario: Multi-line blockquote

- **GIVEN** three consecutive lines `> a`, `> b`, `> c`
- **THEN** the block range covers all three lines as one block

#### Scenario: Two blockquotes separated by blank line

- **GIVEN** `> a\n\n> b`
- **THEN** `:collect` returns two distinct blocks

#### Scenario: Callout excluded

- **GIVEN** a callout `> [!NOTE]\n> body` adjacent to a plain blockquote `> other`
- **WHEN** the two are separated by a blank line
- **THEN** `:collect` returns one block (the plain `> other`) and does not return the callout

### Requirement: Blockquote left-rail rendering

The blockquotes decorator's `:apply-block-fn` SHALL render the rail with an inset gutter:

- A per-line anchor overlay whose `before-string` is `<inset-spaces>` in the default face, painting the leading gutter.  The before-string MUST live on the anchor (not on the per-window prefix display) so reveal exposing the raw `> ` source does NOT also drop the gutter — without this, point-on-line would visually unshift the body text by `inset-cols` columns.
- A per-line anchor overlay whose `wrap-prefix` is the propertised string `<inset-spaces>▌<space>` (inset in default face, `▌` in `gfm-pretty-blockquotes-rail-face`, trailing space unfaced) so soft-wrapped visual continuation rows show the inset gutter and rail.  Continuation visual rows have no buffer char at column 0 to host a before-string, so the inset is baked into the wrap-prefix.
- A per-window display overlay substituting the two-char `> ` prefix on each blockquote line with `▌<space>` (rail in `gfm-pretty-blockquotes-rail-face`, trailing space unfaced).
- A per-window display overlay substituting the one-char bare `>` form with `▌` (no trailing space — matches the 1-char source).

**Coexistence with `link-previews`.**  When a `>` line contains a standalone source-range or diff-range link, the blockquotes decorator SHALL still lay its rail overlays (anchor + display) on that line as for any other `>` line — the rail covers the leading `> ` chars and the preview overlay covers the link span; they paint disjoint ranges on the same buffer line.  The preview decorator is responsible for emitting a matching rail on its own body / bottom-border visual rows so a multi-line blockquote with prose before / after the preview reads as one continuous rail.

The decorator SHALL NOT paint a tinted background on the blockquote rectangle, a top / bottom border, or a right-edge terminator after-string.  An earlier draft applied a tinted background spanning each line's content + an after-string padding the bg out to the window margin — it was removed after the user found that Emacs' `:extend t` paints past EOL only on the visual row containing EOL, so a word-wrapped blockquote's intermediate continuation rows were left with default-bg cells between content and the intended right edge (a ragged stepped appearance).  Without per-visual-row padding mechanics (which Emacs does not expose), no clean rectangle is reachable; the rail-only treatment side-steps the issue.

The decorator's `:apply-block-fn` SHALL call `gfm-pretty-borders--apply-with-anchors` (or an equivalent engine seam) so anchor overlays are laid at most once per (block, rebuild pass) while per-window display overlays apply once per window.

#### Scenario: Plain blockquote

- **GIVEN** `> Pain: clutter` with `gfm-pretty-blockquotes-inset-cols` = 4
- **WHEN** the decorator renders
- **THEN** the line shows four columns of leading gutter (default face), then `▌ Pain: clutter` with the rail in `gfm-pretty-blockquotes-rail-face`
- **AND** no raw `>` is visible on that line

#### Scenario: Bare-`>` line in middle of block

- **GIVEN** `> first paragraph\n>\n> second paragraph`
- **WHEN** the decorator renders
- **THEN** the middle line shows the inset gutter followed by `▌` at column `inset-cols` with no raw `>` visible

#### Scenario: Soft-wrapped long line

- **GIVEN** a single source line `> ` followed by 200 characters of text, displayed in a window with `visual-line-mode` enabled
- **WHEN** the decorator renders
- **THEN** visual row 1 shows the inset gutter then `▌ ` then the first chunk of text
- **AND** continuation visual rows show the inset gutter then `▌ ` then the wrapped continuation (the `wrap-prefix` overlay wins over `markdown-mode`'s `wrap-prefix "> "` text property)

#### Scenario: blockquote line containing a preview keeps the rail

- **GIVEN** a line `> [fn](modules/auth.rs#L1-L3)` with both `gfm-pretty-blockquotes` and `gfm-pretty-link-previews` enabled
- **WHEN** the decorators render
- **THEN** the `gfm-pretty-blockquotes` rail overlays (anchor + display) are present on that line — the rail covers the leading `> ` chars
- **AND** the preview's box-top renders to the right of the rail on the same visual row
- **AND** the preview's body and bottom-border rows carry their own `<inset>▌<space>` prefix so the rail visually continues through the box

### Requirement: Blockquote rail face

The system SHALL define a face `gfm-pretty-blockquotes-rail-face` whose default spec inherits from `font-lock-constant-face`.

The rail glyph in the display overlay and the `▌` cell of the `wrap-prefix` on the anchor overlay SHALL both use this face.

#### Scenario: Face inheritance

- **GIVEN** the default theme
- **WHEN** `face-attribute` reads `gfm-pretty-blockquotes-rail-face :inherit`
- **THEN** it returns `font-lock-constant-face`

### Requirement: Blockquote source reveal

The blockquotes decorator SHALL carry the engine's revealable property (`gfm-pretty-blockquotes-revealable`) on each per-line prefix display overlay.

The engine's reveal walker SHALL hide those overlays in the selected window when point lies on them, exposing the raw `>` (or `> `) source.  The property name SHALL be derived from the blockquotes registry's `tag`; the decorator SHALL NOT register it separately.  Reveal SHALL be scoped to the selected window.

Each prefix display overlay SHALL carry paired `gfm-pretty-display-masked` (the rail string) and `gfm-pretty-display-bare` (the raw source with `gfm-pretty--str-with-region-bg` applied) properties so the variant walker flips the `display` between them based on selection state.

The anchor overlay's `before-string` and `wrap-prefix` SHALL remain in place when the display overlay is revealed; reveal toggles only the per-window prefix display.

#### Scenario: Point on blockquote line

- **GIVEN** a plain blockquote with point on its first line in window W1 (selected)
- **THEN** W1 shows the raw `> ` text in place of the rail prefix
- **AND** the inset gutter remains visible to the left of the raw source (the anchor's `before-string`), so body content does NOT shift horizontally between masked and revealed states
- **AND** other windows showing the buffer continue to show the rail

#### Scenario: Region overlapping blockquote

- **GIVEN** an active region spanning two lines of a plain blockquote in W1 (selected)
- **THEN** every covered line in W1 shows the raw `> ` source with `region` bg painted through
- **AND** other windows continue to show the masked rail

### Requirement: Blockquote narrowing-resilient discovery and teardown

`:collect` SHALL widen the buffer for the duration of its scan. Overlay teardown via the engine's bulk-cleanup helper SHALL also widen so the registry list and on-buffer overlay set stay in lockstep regardless of any current narrowing.

When the engine rebuilds within a sub-region (e.g. under `gfm-present-mode` narrowing), the decorator's overlay set SHALL converge with the same overlays that would be present in a clean widened rebuild — the suite `(narrow → rebuild → widen → rebuild)` SHALL produce a steady state.

#### Scenario: Narrowed rebuild then widen

- **GIVEN** a buffer with three blockquotes narrowed to the second
- **WHEN** `gfm-pretty--rebuild` runs, then the buffer is widened, then rebuilt again
- **THEN** the final overlay set SHALL match what a fresh widened rebuild produces
- **AND** no `args-out-of-range` SHALL signal

### Requirement: Blockquote scoped post-edit rebuild

The blockquotes decorator SHALL register `:full-rebuild-required-p` that returns non-nil when the dirty region:

- overlaps the first character of any blockquote line (structural-line case — the `>` character is the partition decider), or
- overlaps a line directly above or below an existing blockquote block (adjacency case — edits there can grow or shrink the block), or
- overlaps a `> [!TYPE]` marker line (so a callout-to-plain conversion routes through full rebuild and the partition shifts to blockquotes).

The three conditions are OR-combined inside the predicate. The engine's routing SHALL use the predicate to choose between full rebuild and single-block scoped rebuild.

When the dirty region intersects a blockquote body line (not adjacent to a block boundary and not changing the leading character), the engine SHALL rebuild only the containing blockquote via its `:apply-block-fn` (per displayed window). When the dirty region does not overlap any blockquote block range and does not trigger the predicate, the blockquotes decorator SHALL NOT contribute work to the rebuild iteration.

#### Scenario: Edit inside one blockquote body

- **GIVEN** two blockquotes A and B with point inside A's text (not changing the leading `>`)
- **WHEN** the user types into A
- **THEN** only A's overlays are rebuilt
- **AND** B's overlays are untouched

#### Scenario: New `>` line typed at start of paragraph

- **GIVEN** a plain paragraph immediately above a blockquote
- **WHEN** the user types `> ` at the start of that paragraph's line
- **THEN** the predicate fires (adjacency case)
- **AND** a full rebuild reassigns the line into the blockquote block

#### Scenario: Callout marker typed into plain blockquote

- **GIVEN** a plain blockquote `> Pain: ...`
- **WHEN** the user edits the line to `> [!IMPORTANT] Pain: ...`
- **THEN** the predicate fires (structural-line case for callouts)
- **AND** the rebuild releases the line from blockquotes and assigns it to the callouts decorator

### Requirement: Blockquote inset gutter customisation

The system SHALL define a defcustom `gfm-pretty-blockquotes-inset-cols` whose default is the symbol `tab-width` (evaluated at render time so the inset tracks the buffer-local `tab-width` unless the user pins a different value).

The decorator SHALL read this value once per `:apply-block-fn` pass and use it as the width of the leading gutter on every blockquote line and every wrap-prefix.

#### Scenario: Default tracks `tab-width`

- **GIVEN** a buffer with `tab-width` = 4 and `gfm-pretty-blockquotes-inset-cols` at its default
- **WHEN** the decorator renders a blockquote
- **THEN** the inset gutter is 4 columns wide

#### Scenario: User overrides defcustom

- **GIVEN** `(setq gfm-pretty-blockquotes-inset-cols 2)` in a buffer with `tab-width` = 4
- **WHEN** the decorator renders a blockquote
- **THEN** the inset gutter is 2 columns wide regardless of `tab-width`

<!-- ── Fences decorator ───────────────────────────────────────────── -->

### Requirement: Fences decorator registration and toggle

The system SHALL register a decorator named `fences` that renders GFM
fenced code blocks, YAML front-matter helmets, and tab-/4-space
indented code blocks with a curved border, a language icon, and
reveal-on-cursor for fence markers.

`(gfm-pretty-toggle-decorator 'fences)` SHALL flip the decorator on
and off.

#### Scenario: Toggling on

- **WHEN** the fences decorator is enabled in a buffer with a fenced
  code block
- **THEN** the block is rendered inside a curved border `┌ … ┐ … └ … ┘`
- **AND** the language icon appears at the upper-right of the top
  border when `nerd-icons` is available

### Requirement: Fenced block discovery

The fences decorator's `:collect` SHALL discover opening fences of the
form `^\s*```+\s*({\s*)?(LANG)?\s*…$` and matching closing fences
`^\s*```+\s*$`. The block range covers BOL of the opener to EOL of the
closer. The lang capture extends to icon resolution.

#### Scenario: Standard fenced block

- **GIVEN** `\`\`\`bash\necho hi\n\`\`\``
- **THEN** `:collect` returns the range BOL of the opener to EOL of
  the closer
- **AND** the language is `bash`

### Requirement: YAML helmet discovery

The fences decorator's `:collect` SHALL recognise leading YAML
front-matter delimited by `^---\s*$` on the first non-blank line and
another `^---\s*$` line below it. Helmets SHALL be treated as
code-fence blocks for rendering purposes.

#### Scenario: YAML frontmatter

- **GIVEN** a buffer starting with `---\ntitle: Hello\n---\n# body`
- **THEN** the YAML helmet between the two `---` lines SHALL be
  collected as a block

### Requirement: Indented block discovery

The fences decorator's `:collect` SHALL recognise 4-space or
tab-indented code blocks preceded and followed by blank lines, except
where they fall inside a fenced block's range.

#### Scenario: Indented block between paragraphs

- **GIVEN** a paragraph, a blank line, four-space-indented code, a
  blank line, and another paragraph
- **THEN** the indented region SHALL be collected as a block

### Requirement: Mutual exclusion of indent inside fence

`:collect` SHALL NOT emit an indented-block entry whose range is
already covered by a fenced or YAML block.

#### Scenario: Indented lines inside a fence

- **GIVEN** a fenced block whose body happens to be 4-space indented
- **THEN** only the fenced block SHALL be returned

### Requirement: Code-fence bordered-block rendering

The fences decorator's `:apply-block-fn` SHALL render a
curved-border box with:

- A top border using the border face and (when resolvable) a
  language icon at the upper-right.
- A `│ ` body-line *before-string* substituted in for the opener's
  indent on each body line, plus a continuation glyph as the
  `wrap-prefix` so word-wrapped visual rows are inset under the
  body content rather than starting at column 0.
- A right-edge `│` so wrapped lines align to the box width.
- A bottom border.

The decorator's `:apply-block-fn` SHALL call
`gfm-pretty-borders--apply-with-anchors` so anchor overlays
(width-independent props such as the wrap-prefix and body
background fill) are laid at most once per (block, rebuild pass)
while per-window display overlays (borders and right-edge
after-strings, restricted to `WINDOW`) apply once per window.

The wrap-prefix glyph is a styling choice owned by the implementation
(`gfm-pretty--wrap-prefix` in `lisp/gfm/gfm-pretty-borders.el`); the
requirement constrains *that* a continuation glyph appears, not which
glyph.

#### Scenario: Fence with language

- **GIVEN** `\`\`\`bash\necho hi\n\`\`\``
- **THEN** the top border renders `┌──── …  ┐` with the bash icon
  right-aligned
- **AND** body line renders `│ echo hi` with a default-bg fill behind
  it
- **AND** bottom border `└──── …  ┘`

#### Scenario: Wrapped body line shows continuation glyph

- **GIVEN** a fenced block whose body line is wider than the window
- **WHEN** the line wraps across two or more visual rows
- **THEN** each continuation visual row begins with the decorator's
  continuation glyph (the `wrap-prefix`), not with `│ `
- **AND** the right-edge `│` lands at the box width on the final
  wrapped row

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

### Requirement: Code-fence box width sizing

Box width SHALL be `gfm-pretty-available-width` for the rendering
window. Wrapping is handled via the body wrap-prefix and the right-edge
overflow after-string.

#### Scenario: Narrow window

- **GIVEN** a fenced block in a 60-cell window
- **THEN** the box width is 60

### Requirement: Code-fence marker line reveal

The fences decorator SHALL carry the engine's revealable property
(`gfm-pretty-fences-revealable`) on the opening and closing fence
marker display overlays. When point lies on a marker, the engine's
reveal walker SHALL hide those overlays in the selected window so
the raw `\`\`\`lang` shows. The property name SHALL be derived
from the fences registry's `tag`; the decorator SHALL NOT register
it separately.

#### Scenario: Point on opening fence

- **GIVEN** point on `\`\`\`bash`
- **THEN** the selected window shows `\`\`\`bash` (raw)
- **AND** other windows continue to show the top-border decoration

### Requirement: Language icon resolution

When `nerd-icons` is loaded, the fences decorator SHALL resolve the
language tag against `markdown-code-lang-modes` (lang → mode → icon)
and render the icon as the top border's right-aligned glyph. When
unresolved or `nerd-icons` is absent, the top border SHALL render
without an icon.

#### Scenario: Known language

- **GIVEN** a `\`\`\`go` fence and `nerd-icons` available
- **THEN** the top border ends with the Go icon glyph

#### Scenario: Unknown language

- **GIVEN** `\`\`\`flubber` for which no entry exists in
  `markdown-code-lang-modes`
- **THEN** the top border renders without an icon

### Requirement: YAML body fontification

The fences decorator SHALL fontify the body of a YAML helmet using
`yaml-ts-mode`'s font-lock (when its tree-sitter grammar is loadable)
so keys and values colourise inside the helmet's border.

#### Scenario: YAML helmet fontification

- **GIVEN** a YAML helmet with `title: Hello`
- **AND** `yaml-ts-mode` grammar is available
- **THEN** `title` renders with a key face and `Hello` with a value
  face

### Requirement: Code-fence narrowing-resilient discovery and teardown

`:collect` SHALL widen for the duration of its scan; bulk-cleanup
widens for its deletion sweep. `(narrow → rebuild → widen → rebuild)`
SHALL converge.

#### Scenario: Narrowed rebuild then widen

- **GIVEN** a buffer with three fenced blocks narrowed to the second
- **WHEN** the engine rebuilds, then widens, then rebuilds
- **THEN** the final overlay set matches a fresh widened rebuild
- **AND** no `args-out-of-range` signals

### Requirement: Code-fence scoped post-edit rebuild

The fences decorator SHALL register `:full-rebuild-required-p`
that returns non-nil when the dirty region overlaps any of:

- a fence opening line,
- a fence closing line,
- a YAML helmet's `---` marker line, or
- a blank line directly above or below an indent code block (where
  discovery is blank-line-gated).

The four conditions are OR-combined inside the predicate. The
engine's routing (see "Scoped post-edit rebuild routing") SHALL
use the predicate so an edit on any of those lines forces a full
fences rebuild; an edit fully contained in one block scopes to that
block; and edits outside every fences range produce no fences work.

#### Scenario: Edit inside fenced body

- **GIVEN** two fenced blocks
- **WHEN** the user edits inside block #1's body (not a marker)
- **THEN** only block #1 is rebuilt by the engine via the fences
  decorator's `:apply-block-fn`

#### Scenario: Blank line above an indent block becomes non-blank

- **GIVEN** an indent code block preceded by a blank line
- **WHEN** the user types on that preceding blank line
- **THEN** `:full-rebuild-required-p` returns non-nil for the dirty
  region
- **AND** the engine invokes the fences decorator's `:rebuild-fn`
  (full rebuild)

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

<!-- ── Tables decorator ───────────────────────────────────────────── -->

### Requirement: Tables decorator registration and toggle

The system SHALL register a decorator named `tables` that renders GFM
pipe tables as bordered, zebra-striped grids with cursor-aware cell
highlighting and indirect-edit commands.

`(gfm-pretty-toggle-decorator 'tables)` flips the decorator on/off.
The local-leader binding `t` SHALL call this toggle.

#### Scenario: Toggling

- **WHEN** the tables decorator is enabled in a buffer with a table
- **THEN** the table renders with `┌ … ┐`, body rows, and a closing
  `└ … ┘` border
- **AND** alternating body rows have a striped background

### Requirement: Table block discovery

`:collect` SHALL recognise a GFM table as a header line followed by an
alignment line (`|---|---|`) followed by zero or more body rows, each
delimited by `|`.

#### Scenario: Two-column table

- **GIVEN** `| a | b |\n|---|---|\n| 1 | 2 |`
- **THEN** the block range covers all three lines

### Requirement: Cell parser

The tables decorator SHALL parse each row into cells delimited by
`|`. Leading and trailing `|` (exterior pipes) SHALL be optional. A
literal `\|` SHALL be preserved as cell content, not a delimiter.

#### Scenario: Escaped pipe

- **GIVEN** a cell `a \| b`
- **THEN** the cell contains the text `a | b`

### Requirement: Column width normalisation

Column widths SHALL be normalised to the maximum visible width per
column across header + body, then padded with spaces in each cell.

#### Scenario: Variable cell widths

- **GIVEN** column 1 cells `a`, `bb`, `ccc`
- **THEN** all rendered cells in column 1 are width 3

### Requirement: Auto-composition does not skew column widths

Cell width measurements SHALL use `string-width` over the source text,
ignoring composed glyphs. Italicised letters, inline-link replacements,
and other composition products SHALL NOT inflate measured widths
beyond the underlying source character count.

#### Scenario: Italic cell

- **GIVEN** a cell `*hello*`
- **THEN** the cell width measurement uses `string-width` over the
  source string, not the rendered glyph composition

### Requirement: Window-fitted column widths

The tables decorator SHALL cap column widths proportionally so the
rendered table fits within the window's `gfm-pretty-available-width`
without horizontal scroll, when the natural total table width would
otherwise exceed it.

#### Scenario: Narrow window

- **GIVEN** a four-column table with natural width 200 cells
- **AND** a 100-cell window
- **THEN** column widths are capped so total width ≤ 100

### Requirement: Cell wrapping for capped columns

Cell content SHALL wrap inside its cell when the column is capped
below the cell's natural width, padded so subsequent rows remain
aligned to the column boundaries.

#### Scenario: Long cell in capped column

- **GIVEN** a cell `the quick brown fox` capped to 10 cells
- **THEN** the cell renders across multiple visual lines, each padded
  to column width

### Requirement: Cell-edit commit preserves the row

After an indirect-edit commit, the underlying source row SHALL be
re-built with the new cell content, preserving the row's column
boundaries and other cells.

#### Scenario: Commit edited cell

- **GIVEN** indirect edit replaces cell 2 with `xyz`
- **WHEN** the user commits
- **THEN** the source row's cell 2 reads `xyz`
- **AND** other cells are byte-identical to their pre-edit content

### Requirement: Table border and rule decoration

The tables decorator SHALL render:

- A top border `┌────────┐` above the header row.
- A mid rule `├────────┤` replacing the alignment line.
- A bottom border `└────────┘` below the last body row.

#### Scenario: Rendered borders

- **GIVEN** a one-row body table
- **THEN** the rendering shows top border, header, mid rule, body
  row, and bottom border in that order

### Requirement: Exterior pipe rendering

Outer `|` characters in each row SHALL render as `│`, painting the
left and right edges of the box. Optional exterior pipes in source
SHALL render identically to the explicit case.

#### Scenario: No exterior pipes in source

- **GIVEN** source `a | b\n---|---\n1 | 2`
- **THEN** the rendered rows show `│ a │ b │` with the left and right
  `│` synthesised

### Requirement: Interior column gap rendering

Interior `|` separators SHALL render with a one-cell default-bg gap on
either side and a `│` border between cells, distinct from the row
background fill.

#### Scenario: Two body rows side-by-side

- **GIVEN** a two-column table
- **THEN** the column gap renders as ` │ ` between adjacent cells
- **AND** the gap colour is default-bg, not the row's stripe colour

### Requirement: Header emphasis

The header row SHALL render with a header-style face emphasising its
text relative to body rows.

#### Scenario: Header row weight

- **GIVEN** a table
- **THEN** the header row's face renders the text at a stronger weight
  than body rows

### Requirement: Body row zebra striping

Alternating body rows SHALL render with a stripe-coloured background.
Odd-indexed rows (1-based, header excluded) use the stripe face;
even-indexed rows use the default-bg.

#### Scenario: Four-row body

- **GIVEN** four body rows
- **THEN** rows 1 and 3 render with the stripe face background
- **AND** rows 2 and 4 render with default-bg

### Requirement: Stripe face

The tables decorator SHALL expose `gfm-pretty-tables-row-alt-face` and
`gfm-pretty-tables-row-alt-cap-face` as the stripe and stripe-cap
faces. The cap face's `:foreground` SHALL mirror the stripe face's
`:background` so half-block caps (`▐` / `▌`) extend the stripe to the
header box's vertical edges.

#### Scenario: Custom stripe colour

- **WHEN** the user customises `gfm-pretty-tables-row-alt-face` to a
  different background
- **THEN** the next rebuild renders stripes in the new colour
- **AND** the stripe-cap face's foreground updates to match

### Requirement: Active-cell highlight

The cell containing point SHALL render with a blue-tinted background
(`gfm-pretty-tables-active-cell-face`). The highlight SHALL track
point movement within and between cells.

#### Scenario: Moving between cells

- **WHEN** point moves from cell (row 1, col 1) to cell (row 1, col 2)
- **THEN** the highlight shifts to the new cell on the next redisplay

### Requirement: Cursor anchoring inside cells

Cursor motion within a cell SHALL be confined to the cell's visible
character range: cursor SHALL NOT land on the rendered `│` separator
or the column-gap padding.

#### Scenario: Right-arrow past cell end

- **WHEN** point is at the last character of cell 1 and user presses
  right-arrow
- **THEN** point jumps to the first character of cell 2 (skipping the
  separator)

### Requirement: Cell-entry key hints

When point enters a cell, the echo-area SHALL display a hint with the
key bindings for cell motion and edit commands.

#### Scenario: Hint on cell entry

- **WHEN** point moves into a cell
- **THEN** the echo area shows the hint string within one
  post-command tick

### Requirement: Structural cell motion

The tables decorator SHALL bind cell-motion commands (next-cell,
previous-cell, next-row-same-column, previous-row-same-column) on its
mode keymap.

Cell-aware motion commands SHALL consult `gfm-pretty--stale-p` for the
`tables` decorator before reading `gfm-pretty-tables-cell-bounds` or
`gfm-pretty-tables-display-cell-bounds`. When stale, the command SHALL
fall through to the underlying evil command (or the equivalent generic
motion outside evil) exactly as if point were outside any table, without
calling `goto-char` on any cached integer offset.

#### Scenario: Tab moves to next cell

- **WHEN** point is in cell 1 and user presses TAB
- **THEN** point moves to the first character of cell 2

#### Scenario: j after burst edit falls through to evil-next-line

- **GIVEN** point inside a table cell at tick `T0`
- **AND** a buffer modification advances the tick to `T1` before the
  engine's idle rebuild fires
- **WHEN** the user presses `j`
- **THEN** the cell-aware shim takes the fall-through path
- **AND** point moves as `evil-next-line` would move it
- **AND** no `goto-char` is issued against a `cell-bounds` value laid
  down at tick `T0`

### Requirement: Snap-to-cell on row entry

Point SHALL snap to the first cell's first character on row entry from
outside the table (vertical motion), rather than landing on a separator
or column gap.

The snap SHALL be skipped when `gfm-pretty--stale-p` returns non-nil for
the `tables` decorator. In that case point remains where the underlying
motion command left it; the next rebuild restores correct snap
behaviour.

#### Scenario: Down-arrow into table

- **WHEN** point is on the line above a table and user presses
  down-arrow
- **THEN** point lands on the first character of the header row's
  first cell

#### Scenario: Stale snap is skipped

- **GIVEN** the `tables` decorator's `last-rebuild-tick` is older than
  the current buffer tick
- **WHEN** `gfm-pretty-tables--maybe-snap-to-cell` is invoked
- **THEN** point is not moved

### Requirement: In-place edit commands divert to indirect editor

The tables decorator SHALL open the current cell in an indirect buffer
and replay the keystroke there, whenever the user invokes a destructive
editing command (insert, delete, yank) on a rendered cell.

#### Scenario: Insert key in cell

- **WHEN** point is in cell `abc` and user presses `i` (evil insert)
- **THEN** an indirect-edit popup opens with the cell content
- **AND** subsequent keys edit the indirect buffer

### Requirement: Cell-only indirect edit

A user command SHALL be available to open just the current cell in an
indirect editor, exiting via `C-c C-c` to commit or `C-c C-k` to
abort.

#### Scenario: Commit

- **GIVEN** an open cell indirect editor with content `xyz`
- **WHEN** the user presses `C-c C-c`
- **THEN** the source row's cell is replaced with `xyz`
- **AND** the indirect buffer closes

### Requirement: Whole-table indirect edit

A user command SHALL be available to open the entire table in an
indirect editor for free-form pipe-syntax editing, with commit /
abort bindings.

#### Scenario: Whole-table edit

- **WHEN** the user invokes the whole-table edit command
- **THEN** an indirect buffer opens with the table's raw pipe-syntax
  source
- **AND** commit replaces the source range with the edited content

### Requirement: Header column swap

A user command SHALL move a column left or right (swapping with its
neighbour). The swap SHALL update header, alignment line, and every
body row consistently.

#### Scenario: Move column right

- **GIVEN** a three-column table, point in column 2
- **WHEN** the user invokes "move column right"
- **THEN** columns 2 and 3 are swapped in every row of the source

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

### Requirement: Table narrowing-resilient discovery and teardown

`:collect` SHALL widen for its scan. Bulk-cleanup widens. The
`(narrow → rebuild → widen → rebuild)` sequence SHALL converge.

#### Scenario: Narrowed rebuild

- **GIVEN** a buffer with two tables narrowed to the second
- **WHEN** the engine rebuilds, then widens, then rebuilds
- **THEN** the final overlay set matches a fresh widened rebuild
- **AND** no zombie overlays remain in the formerly-narrowed range

### Requirement: Table rebuilds are idempotent

The tables decorator's cell parse SHALL ignore its own display overlays.
`gfm-pretty-tables--transcribe-source-overlays` SHALL skip every overlay
carrying the `gfm-pretty-tables-display` property when computing
width-affecting splices, so the parser never bakes prior render output
into cell text.

The property holds at every rebuild entry point — the full-clear
`--rebuild`, the engine-driven `--apply-block`, the per-window
`--rebuild-block-for-window`, and the visible-first
`--rebuild-window-prioritised`. Foreign width-affecting overlays
(e.g. links decorator display strings) MUST continue to splice into
cell text so column widths reflect the decorated content.

Internals-facing.

#### Scenario: Repeated rebuild converges

- **GIVEN** a GFM buffer with `gfm-pretty-mode` enabled and at least
  one table whose rows have already been decorated
- **WHEN** the tables decorator's rebuild path runs a second time
  without an intervening edit
- **THEN** the per-row display overlay's `display` string equals the
  string produced by the first rebuild, character-for-character
- **AND** no display string contains the substring `│ │ │ │ │ ` (five
  consecutive border chars — the signature of a self-fed parse)

#### Scenario: Per-window rebuild after narrowing

- **GIVEN** a buffer with a GFM table whose source spans the visible
  region, with display overlays already applied
- **WHEN** the buffer is narrowed to a region containing the table and
  the per-window rebuild path (`--reconcile-windows` →
  `--rebuild-window-prioritised` → `--rebuild-block-for-window`) runs
- **THEN** the resulting row display strings match a fresh
  full-rebuild on a widened buffer with no prior overlays
- **AND** the table renders correctly under `gfm-present-mode` slide
  navigation, which churns `window-configuration-change-hook`

#### Scenario: Foreign overlay decoration still splices

- **GIVEN** a table cell whose source is `[name](https://example.com)`
  and the links decorator has placed a `display`-carrying overlay over
  the link's URL
- **WHEN** the tables decorator parses the cell
- **THEN** the parser splices the links overlay's display string into
  the cell text (column-width measurement honours the decorated width)
- **AND** the parser does NOT splice any `gfm-pretty-tables-display`
  overlay text into the cell, even if such an overlay also covers the
  region

<!-- ── Hrule decorator ────────────────────────────────────────────── -->

### Requirement: Hrule decorator registration and toggle

The system SHALL register a decorator named `hrule` that replaces
qualifying GFM dash-form thematic break lines (`---`, `----`, …) with
a single window-width unicode horizontal bar.

`(gfm-pretty-toggle-decorator 'hrule)` flips on/off.

#### Scenario: Toggling

- **WHEN** the hrule decorator is enabled in a buffer with `---`
- **THEN** the line renders as a window-wide unicode bar `────…`

### Requirement: HR block discovery

`:collect` SHALL read ranges with the `markdown-hr` text property set
by `markdown-syntax-propertize-hrs`, filter to those whose
first non-whitespace character is `-` (dash form), and exclude lines
whose first non-whitespace character is `>` (blockquote-nested
HR-like text). `***` and `___` forms SHALL pass through to markdown's
font-lock unchanged (not collected here).

The `:collect` function SHALL ensure `markdown-syntax-propertize-hrs`
has been applied over the scanned region by calling
`syntax-propertize` before reading the property.

#### Scenario: Dash-form HR

- **GIVEN** a `---` line outside any code block
- **THEN** `:collect` returns the line's range

#### Scenario: Star-form HR

- **GIVEN** a `***` line
- **THEN** `:collect` returns nothing for it
- **AND** markdown-mode's font-lock decoration renders it normally

#### Scenario: Setext heading underline excluded

- **GIVEN** a `Title\n---` setext-2 heading
- **THEN** `:collect` SHALL NOT return the `---` line as an HR

### Requirement: HR rendering

The hrule decorator's `:apply-block-fn` SHALL replace each
collected HR line's display with `(make-string WIDTH ?─)`
propertised with the hrule face. WIDTH comes from
`gfm-pretty-available-width` for the rendering window. The
overlay is per-window so two windows of different widths each see
a bar sized to their own width.

#### Scenario: HR in 100-cell window

- **GIVEN** an HR line in a 100-cell window
- **THEN** the display string is 100 `─` characters

### Requirement: HR cursor reveal

The hrule decorator SHALL carry the engine's revealable property
(`gfm-pretty-hrule-revealable`) on the HR display overlay. When
point lies on the HR line, the engine's reveal walker SHALL hide
the overlay so the raw `---` source shows in the selected window
only. The property name SHALL be derived from the hrule registry's
`tag`; the decorator SHALL NOT register it separately.

#### Scenario: Point on HR

- **WHEN** point moves to the HR line
- **THEN** the selected window shows `---` (raw)
- **AND** the unicode bar reappears when point leaves

### Requirement: HR narrowing-resilient discovery and teardown

`:collect` SHALL widen for its scan; bulk-cleanup widens. The
`(narrow → rebuild → widen → rebuild)` sequence SHALL converge.

#### Scenario: Narrowed rebuild

- **GIVEN** a buffer with three HRs narrowed to the second
- **WHEN** the engine rebuilds, then widens, then rebuilds
- **THEN** the final overlay set matches a fresh widened rebuild

<!-- ── Links decorator ────────────────────────────────────────────── -->

### Requirement: Links decorator registration and toggle

The system SHALL register a decorator named `links` that decorates
Markdown / GFM links with per-window overlays, replacing the bracket
scaffolding with a title label and a host-aware icon. The decorator
SHALL be enabled iff `markdown-hide-urls` is non-nil, and SHALL
auto-toggle when `markdown-hide-urls` changes.

#### Scenario: hide-urls on

- **GIVEN** `(setq-local markdown-hide-urls t)`
- **WHEN** `gfm-pretty-mode` enables
- **THEN** the links decorator's enable bit is t
- **AND** link overlays render

#### Scenario: hide-urls off

- **GIVEN** `(setq-local markdown-hide-urls nil)`
- **THEN** the links decorator's enable bit is nil
- **AND** link source renders raw

### Requirement: Link shape discovery

`:collect` SHALL recognise inline links `[title](url)`, reference
links `[title][label]`, autolinks `<url>`, bare URLs, and
wiki-style links. Image links (`![alt](url)`), reference definition
lines (`[label]: url`), and footnote markers (`[^id]`) SHALL be
excluded.

#### Scenario: Inline link

- **GIVEN** `[Anthropic](https://anthropic.com)`
- **THEN** `:collect` returns one link with title `Anthropic` and url
  `https://anthropic.com`

#### Scenario: Image link excluded

- **GIVEN** `![alt](pic.png)`
- **THEN** `:collect` does not emit a link for it

### Requirement: Link decoration skips code regions

The links decorator SHALL NOT collect or render a link whose source
starts inside a markdown code region. Code regions include:

- fenced code blocks (``` and `~~~`)
- indented (pre) code blocks
- inline code spans (`` `code` ``)

Detection SHALL delegate to markdown-mode's own helpers:
`markdown-code-block-at-pos` for fenced and indented blocks,
`markdown-inline-code-at-pos-p` for inline code. The check uses the
link record's start position (the title-side opening bracket, or
for autolinks / bare URLs the first character of the matched span).

The exclusion mirrors the existing table-cell and
reference-definition-line exclusions: code-region text is governed
by its own rendering layer, and overlaying link decoration on top
would misrepresent the source.

#### Scenario: Link inside fenced code block

- **GIVEN** a fenced block containing the literal `[foo](bar)`
- **THEN** the links decorator does NOT decorate that span
- **AND** the code block renders its content unchanged

#### Scenario: Link inside indented code block

- **GIVEN** a four-space-indented line containing `[foo](bar)`
- **THEN** the links decorator does NOT decorate that span

#### Scenario: Link inside inline code

- **GIVEN** an inline code span `` `[foo](bar)` ``
- **THEN** the links decorator does NOT decorate the bracketed
  text
- **AND** the inline code renders its content unchanged

#### Scenario: Link adjacent to code does decorate

- **GIVEN** the line `See [foo](bar) and \`some-code\``
- **THEN** the `[foo](bar)` link decorates normally
- **AND** the inline code renders its content unchanged

### Requirement: Standalone span predicate

The engine SHALL expose a public helper `gfm-pretty-standalone-span-p
(beg end)` that returns non-nil iff the line containing `[beg, end)`,
with the span between `beg` and `end` removed, matches the shape:

```
^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$
```

The helper is a pure function of buffer text between `(line-beginning
-position)` of `beg` and `(line-end-position)` of `end`. It does not
modify match-data observable to callers. It is the single source of
truth for "this span occupies a standalone line, optionally inside a
list-item or blockquote marker".

Decorators that need standalone-line gating SHALL delegate to this
helper rather than re-implement the regex.

#### Scenario: Whole-line span is standalone

- **GIVEN** a buffer line whose only content is a span `S`
- **WHEN** `gfm-pretty-standalone-span-p` is called with `S`'s range
- **THEN** it returns non-nil

#### Scenario: List-item-only span is standalone

- **GIVEN** a buffer line `- S` where `S` is the span
- **WHEN** the helper is called with `S`'s range
- **THEN** it returns non-nil

#### Scenario: Blockquote-marker-only span is standalone

- **GIVEN** a buffer line `> S` where `S` is the span
- **WHEN** the helper is called with `S`'s range
- **THEN** it returns non-nil

#### Scenario: Span embedded in prose is not standalone

- **GIVEN** a buffer line `See S for details.`
- **WHEN** the helper is called with `S`'s range
- **THEN** it returns nil

### Requirement: Link decoration defers source-range and diff URL forms to link-previews

The links decorator SHALL defer rendering to the `link-previews`
decorator for spans that `link-previews` will actually claim. The
deferral SHALL match `link-previews`' own claim rules so spans that
neither decorator would render do not silently disappear.

Concretely, the links decorator SHALL NOT collect or render a link
when ALL of the following hold:

1. The link's `kind` is `inline` (a `[label](url)` shape — not
   reference, shortcut, autolink, wiki, or bare-URL).
2. The resolved URL matches the source-range shape
   `<path>#L<digits>` optionally followed by `-L<digits>`.
3. `gfm-pretty-standalone-span-p` returns non-nil for the link's
   full `[label](url)` source span.

Independently, the links decorator SHALL NOT collect or render any
link whose resolved URL matches the diff shape
`diff:<base>...<head>` optionally followed by `#<path>`. Diff URLs
are unconditionally deferred regardless of `kind` or standalone
status.

| Form          | Shape                                          | Example                       |
| :------------ | :--------------------------------------------- | :---------------------------- |
| source-range  | `<path>#L<digits>` optionally `-L<digits>`     | `/repo/foo.yml#L13-L22`       |
| diff          | `diff:<base>...<head>` optionally `#<path>`    | `diff:main...HEAD#README.md`  |

The check SHALL be applied during the discovery filter pass alongside
the existing code-region exclusion. Skipped links produce no
title-side overlay, no URL-side overlay, and no overlay keymap.

All other source-range links — inline source-range links embedded in
prose, reference-style source-range links (`[label][src]` with
`[src]: /p#L1`), shortcut links, autolinks, and bare-URL forms —
SHALL be collected and decorated using the normal classification
path. Source-range URLs starting with `/`, `./`, `../`, `~/`, or
`file:` classify as `file`; other source-range URLs classify by the
usual rules. Title-side, URL-side icon, and RET behaviour follow the
resolved class.

The deferral exists because `link-previews` owns rendering and RET
dispatch for spans it claims via its own preview overlays and
overlay-keymap. Pretty-links overlays on a claimed span would either
stack incompatibly (display garbling) or shadow `link-previews`' RET
via overlay-keymap precedence. Spans that `link-previews` does not
claim have no ownership conflict.

#### Scenario: Standalone inline source-range link is deferred

- **GIVEN** a buffer line whose only content is
  `[snippet](/repo/foo.yml#L13-L22)`
- **THEN** the links decorator does NOT create any overlay for that
  span
- **AND** the buffer text renders raw (whatever `link-previews` does
  with it)

#### Scenario: Standalone inline source-range link with single line is deferred

- **GIVEN** a buffer line whose only content is `[line](/repo/x.el#L42)`
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Inline source-range link in prose is decorated as file

- **GIVEN** a buffer line `See [snippet](/repo/foo.yml#L13-L22) for context.`
- **THEN** the links decorator creates a title-side overlay with the
  `file` class
- **AND** a URL-side overlay covers the `(/repo/foo.yml#L13-L22)`
  span with the file icon (or `""` when `nerd-icons` is unavailable)

#### Scenario: List-item-only inline source-range link is deferred

- **GIVEN** a buffer line `- [snippet](/repo/foo.yml#L13-L22)`
- **THEN** the links decorator does NOT create any overlay
- **AND** `link-previews` is the only decorator that may render the
  span

#### Scenario: Reference link to source-range URL is decorated as file

- **GIVEN** a buffer containing `[snippet][src]` with a definition line
  `[src]: /repo/foo.yml#L13-L22`
- **THEN** the links decorator creates its usual title-side overlay
  with the `file` class
- **AND** the URL-side overlay (when the reference label appears
  inline) is created per the standard reference-link rules

#### Scenario: Inline diff link is deferred regardless of standalone

- **GIVEN** `[changed](diff:main...feature)` either inline in prose
  or on a line by itself
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Inline diff link with file scope is deferred

- **GIVEN** `[changed](diff:main...feature#path/to/file.el)` either
  inline in prose or on a line by itself
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Plain file link without line range unaffected

- **GIVEN** `[ops](./scripts/x.sh)` (no `#L...` suffix)
- **THEN** the links decorator creates its usual title-side overlay
  with the `file` class

#### Scenario: Plain anchor link unaffected

- **GIVEN** `[Setup](#setup)`
- **THEN** the links decorator creates its usual title-side overlay
  with the `anchor` class

### Requirement: URL target classification

The links decorator SHALL classify every link's *resolved* URL into
one of three target classes: `web`, `anchor`, or `file`. The
classification SHALL drive title-side face selection, url-side
overlay presence, and RET dispatch.

Classification rules, applied in order:

| Prefix on resolved URL                  | Class    |
| :-------------------------------------- | :------- |
| `#`                                     | `anchor` |
| `./`, `../`, `/`, or `file:`            | `file`   |
| any other scheme (`http://`, `https://`, …) or any other content | `web`    |

Reference-style links (full, collapsed, shortcut) SHALL classify via
the URL recorded in their resolved `[label]:` definition, not via
the source shape. Autolinks and bare URLs SHALL classify via their
extracted target.

The classifier SHALL be a pure function of the URL string; the
resulting class SHALL be recorded on both the title-side and (when
present) url-side overlays as an overlay property so consumers
(RET, eldoc) can read it without re-classifying.

#### Scenario: Inline anchor link

- **GIVEN** `[Setup](#setup)`
- **THEN** the link classifies as `anchor`

#### Scenario: Reference link to a relative path

- **GIVEN** `[ops][tg-auth-sh]` with a definition line
  `[tg-auth-sh]: ./_scripts/tg-auth.sh`
- **THEN** the link classifies as `file`

#### Scenario: Reference link to a web URL

- **GIVEN** `[catalog][cat]` with `[cat]: https://github.com/x/y`
- **THEN** the link classifies as `web`

#### Scenario: Absolute path

- **GIVEN** `[etc](/etc/hostname)`
- **THEN** the link classifies as `file`

### Requirement: Local-link face customisation points

The links decorator SHALL expose two faces for local target classes:
`gfm-pretty-links-anchor-face` and `gfm-pretty-links-file-face`. Both
SHALL default to inheriting `markdown-link-face` with `:underline
nil` so local link titles are distinguishable from external
underlined links. Themes MAY override either independently.

#### Scenario: Default faces strip underline

- **WHEN** the decorator loads with no theme overrides
- **THEN** `gfm-pretty-links-anchor-face` and
  `gfm-pretty-links-file-face` resolve to no underline

### Requirement: Title-side overlay rendering

The links decorator's `:apply-block-fn` SHALL replace the
`[title]` span (brackets included) with the title text in a face
chosen by the link's target class:

| Class    | Face                            |
| :------- | :------------------------------ |
| `web`    | `gfm-pretty-links-title-face`   |
| `anchor` | `gfm-pretty-links-anchor-face`  |
| `file`   | `gfm-pretty-links-file-face`    |

`gfm-pretty-links-title-face` SHALL continue to default to
`markdown-link-face`. The overlay is per-window.

When the label is fully wrapped in a single pair of backticks (e.g.
`` `pretty` ``), the title-side overlay SHALL strip the wrapping
backticks from the displayed string. A pair is considered "wrapping"
only when the label both begins and ends with `` ` `` AND there are
no further backticks between them. Labels with interior backticks
(`` `code` and prose ``) SHALL display unchanged. The overlay's
`gfm-pretty-links-label` metadata SHALL retain the original
unstripped label so eldoc and xref see the source representation.

#### Scenario: Web link uses title face

- **GIVEN** `[Anthropic](https://anthropic.com)`
- **THEN** the title-side overlay displays `Anthropic` in
  `gfm-pretty-links-title-face`

#### Scenario: Anchor link uses anchor face

- **GIVEN** `[Setup](#setup)`
- **THEN** the title-side overlay displays `Setup` in
  `gfm-pretty-links-anchor-face`

#### Scenario: File link uses file face

- **GIVEN** `[ops](./scripts/x.sh)`
- **THEN** the title-side overlay displays `ops` in
  `gfm-pretty-links-file-face`

#### Scenario: Fully backtick-wrapped label has backticks stripped

- **GIVEN** ``[`pretty`](./x.hcl)``
- **THEN** the title-side overlay's `display` is `pretty` (no
  backticks) under `gfm-pretty-links-file-face`
- **AND** the overlay's `gfm-pretty-links-label` metadata is
  `` `pretty` `` (backticks retained)

#### Scenario: Interior backticks leave display unchanged

- **GIVEN** ``[say `hi` world](./x.md)``
- **THEN** the title-side overlay's `display` is the full label
  including backticks: `` say `hi` world ``

### Requirement: URL-side icon rendering

The links decorator's `:apply-block-fn` SHALL replace the URL span
(`(url)`, `[label]`, the autolink span, …) with a single
nerd-icons glyph for `web` and `file` classes: the glyph is resolved
from the target host or scheme for `web`, and from the URL's basename
via `nerd-icons-icon-for-file` for `file`.

For `file`-class URLs the icon resolver SHALL strip any `#…`
fragment from the URL before computing the basename, so a URL of the
form `<path>#L<n>[-L<n>]` resolves the glyph for the underlying
file extension (e.g. `foo.el#L42` resolves the elisp glyph rather
than falling back to a generic glyph on the synthetic extension
`el#L42`). Fragment-stripping applies only to the file-resolution
branches; `web`-class URLs and `anchor` classification are
unaffected.

For the `anchor` class the URL-side overlay SHALL be created with its
`display` property set to the empty string, hiding the `(#slug)` span
from view while keeping the overlay's metadata
(`gfm-pretty-links-class`, `gfm-pretty-links-url`, etc.) available to
RET dispatch, eldoc, and xref.

When `nerd-icons` is unavailable, the URL-side overlay SHALL be
omitted for `web` links (URL shows raw). For `file` links the
overlay SHALL still be created with `display` = `""` (path hidden,
no icon). Anchor-class behaviour is unaffected.

#### Scenario: Github URL

- **GIVEN** `[code](https://github.com/user/repo)` and `nerd-icons`
  available
- **THEN** the URL-side renders as the GitHub icon

#### Scenario: Unknown host

- **GIVEN** a link to an unrecognised host (web class)
- **THEN** the URL-side renders as a generic web icon

#### Scenario: Anchor link hides URL span

- **GIVEN** `[Setup](#setup)`
- **THEN** a URL-side overlay covers the `(#setup)` span
- **AND** its `display` property is the empty string
- **AND** the overlay carries `gfm-pretty-links-class` = `anchor` and
  `gfm-pretty-links-url` = `#setup`

#### Scenario: File link renders icon for URL span

- **GIVEN** `[ops](./scripts/x.sh)` and `nerd-icons` available
- **THEN** a URL-side overlay covers the `(./scripts/x.sh)` span
- **AND** its `display` property is the nerd-icons glyph resolved by
  `nerd-icons-icon-for-file` on `x.sh`
- **AND** the overlay carries `gfm-pretty-links-class` = `file` and
  `gfm-pretty-links-url` = `./scripts/x.sh`

#### Scenario: File link with source-range fragment resolves by extension

- **GIVEN** an inline-in-prose link `[snippet](/path/foo.el#L42-L48)`
  and `nerd-icons` available
- **THEN** a URL-side overlay covers the `(/path/foo.el#L42-L48)`
  span
- **AND** its `display` property is the nerd-icons glyph that
  `nerd-icons-icon-for-file` returns for basename `foo.el`
- **AND** the overlay carries `gfm-pretty-links-class` = `file` and
  `gfm-pretty-links-url` = `/path/foo.el#L42-L48`

#### Scenario: Parent-relative file link with code-styled label hides URL span

- **GIVEN** ``[`dev/global/iam-roles/`](../../dev/global/iam-roles/terragrunt.stack.hcl)``
- **THEN** the title-side overlay displays `dev/global/iam-roles/`
  (wrapping backticks stripped) under `gfm-pretty-links-file-face`
- **AND** a URL-side overlay covers the parenthesised path with a
  non-nil `display` (an icon when `nerd-icons` resolves one, `""`
  otherwise)

#### Scenario: File link without `nerd-icons` still hides path

- **GIVEN** a file-class link and `nerd-icons` unavailable
- **THEN** a URL-side overlay covers the path span
- **AND** its `display` property is the empty string
- **AND** the overlay carries `gfm-pretty-links-class` = `file`

### Requirement: Reference link resolution

Reference links SHALL resolve through a buffer-local definition alist
keyed on the lower-cased label. Unresolved labels SHALL fall back to
rendering the title side only (no icon).

#### Scenario: Resolved reference

- **GIVEN** `[name][label]\n\n[label]: https://example.com`
- **THEN** the URL-side resolves via the alist and renders the
  example.com icon

### Requirement: Suppression of built-in URL composition

The links decorator's `:on-enable` SHALL install `:around` advice on
`markdown-fontify-inline-links` and `markdown-fontify-reference-links`
suppressing markdown-mode's `markdown-hide-urls` compose-region URL
collapse. The advice SHALL be gated on the decorator's enable bit so
disabling the decorator (without disabling `gfm-pretty-mode`) restores
markdown-mode's collapse.

#### Scenario: Advice gated on decorator

- **GIVEN** `markdown-hide-urls` is t but the links decorator's enable
  bit is nil
- **THEN** markdown-mode's compose-region collapse runs unchanged

### Requirement: RET follows the link when point is on the decoration

The links decorator SHALL install an overlay keymap binding `RET` to
a class-dispatched follow handler. The handler SHALL read the link's
target class and URL from the overlay under point and act per class:

| Class    | Action                                                                  |
| :------- | :---------------------------------------------------------------------- |
| `web`    | call `markdown--browse-url` on the URL                                  |
| `anchor` | jump to the heading whose generated slug matches the anchor portion    |
| `file`   | `find-file` on the URL's path component (URL with any trailing `#L<n>[-L<n>]` fragment stripped), expanded relative to `buffer-file-name`'s directory (or `default-directory` when the buffer has no file); when the URL carried an `#L<n>[-L<n>]` source-range fragment, the handler SHALL then jump point to line `<n>` (range-end is ignored) |

The binding SHALL fire when point is on the rendered overlay.

For the `anchor` class the handler SHALL search the entire buffer for
the matching heading regardless of the current narrowing, and on
match SHALL widen the buffer before moving point to the heading.
After a successful jump the handler SHALL run the abnormal hook
`gfm-pretty-links-after-anchor-jump-functions` with the target buffer
position as its single argument. Subscribers can use the hook to
restore their preferred narrowing or apply additional decoration.

#### Scenario: RET on web link

- **WHEN** point is on a rendered `web` link's title and user presses
  `RET`
- **THEN** `markdown--browse-url` opens the URL

#### Scenario: RET on anchor link

- **GIVEN** a heading `## Setup Steps` in the buffer
- **AND** point is on a rendered `[go](#setup-steps)` link
- **WHEN** user presses `RET`
- **THEN** point moves to the `Setup Steps` heading

#### Scenario: RET on anchor link in narrowed buffer

- **GIVEN** a buffer narrowed to a region that does not include the
  `## Setup Steps` heading
- **AND** point is on a rendered `[go](#setup-steps)` link inside the
  narrowed region
- **WHEN** user presses `RET`
- **THEN** the buffer is widened
- **AND** point moves to the `Setup Steps` heading

#### Scenario: anchor jump hook fires with target position

- **GIVEN** a function `F` registered on
  `gfm-pretty-links-after-anchor-jump-functions`
- **WHEN** RET on an anchor link successfully jumps to a heading at
  buffer position `P`
- **THEN** `F` is called once with `P` as its sole argument
- **AND** `F` is called after point has moved and the buffer has been
  widened

#### Scenario: RET on anchor with no matching heading

- **GIVEN** a rendered `[go](#missing)` link
- **AND** no heading in the buffer has slug `missing`
- **WHEN** user presses `RET`
- **THEN** the handler raises `user-error` with a message naming the
  missing anchor
- **AND** `gfm-pretty-links-after-anchor-jump-functions` is not run

#### Scenario: RET on file link

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer
  whose file lives at `/repo/README.md`
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/repo/scripts/x.sh`

#### Scenario: RET on file link in fileless buffer

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer with
  no `buffer-file-name`
- **WHEN** user presses `RET`
- **THEN** `find-file` resolves the path against `default-directory`

#### Scenario: RET on inline source-range file link jumps to line

- **GIVEN** a rendered inline-in-prose link
  `[snippet](/path/foo.el#L42-L48)` in a buffer
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/path/foo.el`
- **AND** point lands on line 42 of that buffer

#### Scenario: RET on inline source-range file link with single line jumps to line

- **GIVEN** a rendered inline-in-prose link
  `[line](/path/foo.el#L42)` in a buffer
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/path/foo.el`
- **AND** point lands on line 42

### Requirement: Reference goto-definition via xref

The links decorator's `:on-enable` SHALL register an xref backend on
`xref-backend-functions` that resolves a reference label at point to
its `[label]:` definition line.

#### Scenario: M-. on reference label

- **GIVEN** `[name][label]` and a definition line `[label]: url`
- **WHEN** point is on the rendered reference link and user presses
  `M-.`
- **THEN** xref jumps to the definition line

### Requirement: Eldoc link exposure

The links decorator's `:on-enable` SHALL install an eldoc
documentation function that surfaces the *formatted source* of the
link under point — not the bare URL. The returned string SHALL be a
propertised reconstruction of the link's raw markdown source, with:

- `shadow` face on scaffolding characters (`[`, `]`, `(`, `)`)
- the class-appropriate title face on the title span (`web` →
  `gfm-pretty-links-title-face`, `anchor` →
  `gfm-pretty-links-anchor-face`, `file` →
  `gfm-pretty-links-file-face`)
- `markdown-url-face` on the URL span

For reference-shape links, the reconstruction SHALL be `[title][label]`
(or `[label]` for shortcut references) with `shadow` on brackets and
the class face on both title and label spans. Inline title
attributes (`"Anthropic Home"`) SHALL render after the URL, in
italics.

The function SHALL return nil off any decorated link so other eldoc
providers are not blocked.

#### Scenario: Eldoc on inline web link

- **GIVEN** point on a rendered `[Anthropic](https://anthropic.com)`
  link
- **THEN** eldoc displays `[Anthropic](https://anthropic.com)` with
  `shadow` on the brackets and parens,
  `gfm-pretty-links-title-face` on `Anthropic`, and
  `markdown-url-face` on `https://anthropic.com`

#### Scenario: Eldoc on reference file link

- **GIVEN** point on `[ops][tg-auth-sh]` with definition
  `[tg-auth-sh]: ./scripts/tg-auth.sh`
- **THEN** eldoc displays `[ops][tg-auth-sh]` with `shadow` on the
  brackets and `gfm-pretty-links-file-face` on `ops` and
  `tg-auth-sh`

### Requirement: Overlay decoration does not skew column widths

The links decorator SHALL NOT collect or render a link whose source
falls inside a GFM table cell. When the tables decorator's
`:collect` includes a cell containing what looks like a link, the
link decorator SHALL defer — the table cell's natural text content
governs column width.

#### Scenario: Link inside table cell

- **GIVEN** a table cell `[name](url)`
- **THEN** the links decorator does NOT decorate that link
- **AND** the cell's column-width measurement uses the raw text
  width

### Requirement: Link previews decorator registration and toggle

The system SHALL register a decorator named `link-previews` that renders box-bordered previews for standalone source-range (`<path>#L<a>-L<b>`) and diff-range (`diff:<base>...<head>[#<path>]`) references in `gfm-pretty-mode` buffers.  Both markdown link references — `[label](url)` — and bare-line references — the URL written alone on a line — SHALL be recognised.

`(gfm-pretty-toggle-decorator 'link-previews)` SHALL flip the decorator on and off.  The default-on state matches the other decorators registered under `gfm-pretty`.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing a standalone source-range link
- **THEN** a preview overlay covers the link's region with a box-bordered rendered preview

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'link-previews)` flips the enable bit off
- **THEN** every preview overlay is removed
- **AND** the raw `[label](url)` markdown text is visible

#### Scenario: bare-line source reference gets a preview

- **GIVEN** a line whose only content is `modules/auth.rs#L42-L48` (with no surrounding `[...](...)` syntax)
- **WHEN** previews are rendered
- **THEN** a preview overlay covers the bare reference's region with a box-bordered rendered preview

### Requirement: Link previews — standalone link gating

The `link-previews` decorator SHALL decorate only standalone references — references that occupy a whole line, optionally inside a single list-item or blockquote marker.  This gating applies uniformly to bracketed `[label](url)` references and to bare-line URL-only references.

A `[label](url)` link SHALL be considered standalone when the line containing the link, with the `[label](url)` token removed, matches:

```
^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$
```

A bare-line reference SHALL be considered standalone when the line containing the URL token, with the URL token removed, matches the same regex.  The URL token is the contiguous non-whitespace run that parses as a source-range URL (`<path>#L<a>[-L<b>]`) or a diff-range URL (`diff:<base>...<head>[#<path>]`).  For bare lines, the path component of a source-range URL MUST contain at least one `/` — basename-only tokens (e.g. `auth.rs#L1-L5`) SHALL NOT match, to suppress false positives on agent-generated prose.

The surrounding line content is whitespace and at most one of: an unordered-list marker (`- `, `* `, `+ `), an ordered-list marker (`<n>. `), or a blockquote marker (`> `).

References failing this check SHALL be left undecorated.  This gating SHALL apply to both source-range and diff-range previews; inline-classification SHALL NOT depend on URL form.

#### Scenario: whole-line link gets a preview

- **GIVEN** a line whose only content is `[fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: list-item-only link gets a preview

- **GIVEN** a line `- [fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: blockquote link gets a preview

- **GIVEN** a line `> [fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: inline link in prose is left undecorated

- **GIVEN** a line `See [fn](modules/auth.rs#L42-L48) for details.`
- **WHEN** previews are rendered
- **THEN** no preview overlay covers the link's region
- **AND** the original markdown text `[fn](modules/auth.rs#L42-L48)` is visible (possibly via the `gfm-pretty-links` decorator's title-side display, but no source body content is shown)

#### Scenario: bare-line list-item reference gets a preview

- **GIVEN** a line `- modules/auth.rs#L42-L48`
- **WHEN** previews are rendered
- **THEN** an overlay covers the bare URL token's region with a rendered preview

#### Scenario: bare-line blockquote reference gets a preview

- **GIVEN** a line `> modules/auth.rs#L42-L48`
- **WHEN** previews are rendered
- **THEN** an overlay covers the bare URL token's region with a rendered preview

#### Scenario: bare reference embedded in prose is left undecorated

- **GIVEN** a line `See modules/auth.rs#L42-L48 for details.`
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that reference

#### Scenario: bare basename-only reference is left undecorated

- **GIVEN** a line whose only content is `auth.rs#L1-L5` (no `/` in the path)
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that reference

### Requirement: Link previews — bare-line reference discovery

The `link-previews` decorator SHALL discover bare-line references by scanning the widened buffer for lines whose sole significant content (after stripping any leading whitespace and at most one list-item or blockquote marker) is a URL token parsable as a source-range URL (`<path>#L<a>[-L<b>]`) or a diff-range URL (`diff:<base>...<head>[#<path>]`).

For bare-line source-range references, the `<path>` component MUST contain at least one `/`.  Absolute paths (`/…`), tilde-prefixed paths (`~/…`), and relative paths containing a `/` SHALL all be accepted; tilde-prefixed paths SHALL be expanded via `expand-file-name`; relative paths SHALL be resolved against `default-directory` (matching the existing behaviour for bracketed source links).

A bare-line reference's overlay range SHALL cover the URL token only (not the leading whitespace or marker prefix), so the marker continues to render in its native form to the left of the box.  The payload, rendering pipeline, RET-follow dispatch, and broken-preview sentinel behaviour SHALL be identical to the bracketed form once the block is collected.

When a line could match both a bracketed `[label](url)` pattern and a bare-line pattern (theoretically impossible given the brackets, but stated for clarity), the bracketed form SHALL take precedence; bare-line matching SHALL skip any span already claimed by a bracketed match.

#### Scenario: absolute bare path is recognised

- **GIVEN** a line whose only content is `/Users/chris/src/foo/main.rs#L10-L20`
- **WHEN** previews are rendered
- **THEN** an overlay covers the URL token with a rendered preview whose top border embeds the abbreviated path and range

#### Scenario: tilde-prefixed bare path is expanded and recognised

- **GIVEN** a line whose only content is `~/src/foo/main.rs#L10-L20`
- **WHEN** previews are rendered
- **THEN** an overlay covers the URL token with a rendered preview reading from the expanded absolute path

#### Scenario: bare diff-range reference is recognised

- **GIVEN** a line whose only content is `diff:abc1234...def5678#main.tf`
- **WHEN** previews are rendered
- **THEN** an overlay covers the URL token with a diff-range preview rendered exactly as a bracketed `[…](diff:abc1234...def5678#main.tf)` would render

#### Scenario: overlay covers token only, not marker

- **GIVEN** a line `  - /abs/path/main.rs#L1-L3`
- **WHEN** previews are rendered
- **THEN** the overlay's range starts at the `/` of the URL token and ends at the trailing digit `3`
- **AND** the leading `  - ` continues to render unchanged

### Requirement: Link previews — preformatted-context exclusion for bare references

Bare-line references SHALL be excluded from preview decoration when they fall inside any of the following preformatted contexts:

1. **Fenced code blocks** — lines between an opening triple-backtick (or longer-than-three-backtick) fence and its matching closing fence, inclusive of the fence lines themselves.  The fence pattern matches `gfm-pretty-fences`' opening (`gfm-pretty-fences--open-re`) and closing (`gfm-pretty-fences--close-re`) shape; the decorator MAY recompute these ranges locally rather than depending on `gfm-pretty-fences' enabled state.
2. **GFM indented code blocks** — lines whose leading-whitespace indent is at least 4 columns, when not preceded on a contiguous prior line by a list-item marker whose continuation indent would naturally absorb the 4-space indent.  An approximation that treats any line with ≥4 leading spaces as preformatted (regardless of list-item context) SHALL be acceptable, given that bare-line references inside list-item bodies normally use ≤2 spaces of indent in agent-generated prose.
3. **Inline-code wrap on its own line** — lines whose entire significant content is a single inline-code span (`` `<token>` ``), with optional leading marker.  Treating these as preformatted preserves the writer's intent of suppressing decoration.

These exclusion rules apply only to bare-line references; bracketed `[label](url)` references are already gated by markdown's existing handling of code spans and SHALL remain unaffected.

#### Scenario: bare reference inside a fenced code block is left undecorated

- **GIVEN** a buffer containing
  ````
  ```
  /abs/path/main.rs#L1-L3
  ```
  ````
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for the line inside the fence

#### Scenario: bare reference in 4-space indented block is left undecorated

- **GIVEN** a line beginning with 4 leading spaces followed by `/abs/path/main.rs#L1-L3`
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that line

#### Scenario: bare reference inside inline-code wrap is left undecorated

- **GIVEN** a line whose only content is `` `/abs/path/main.rs#L1-L3` ``
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that line

#### Scenario: bare reference inside a fence with 4-backtick delimiter is left undecorated

- **GIVEN** a buffer containing a `~~~~`-delimited or 4-backtick-delimited fence whose body contains a bare source-range line
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for the line inside the fence (the recogniser MAY restrict matching to triple-backtick fences; in that case 4-backtick fences are still treated as preformatted because the decorator's opening-fence regex matches a run of ≥3 backticks)

### Requirement: Source-range link preview overlay rendering

For each standalone link whose URL matches `<path>#L<start>` or `<path>#L<start>-L<end>`, the `link-previews` decorator SHALL place an overlay on the link's full markdown expression (`[label](url)`) whose `display` property is a propertised multi-line string composed of:

1. A **top border** in `gfm-pretty-border-face` of the form `┌─ <abbrev-path>:<start>-<end> ──…──┐`.  The abbreviated path is computed by:
   - If the absolute source path is inside a project (resolved via `project-current` with `default-directory` bound to the source file's directory), the path SHALL be made relative to the project root.
   - Otherwise, the path SHALL be passed through `abbreviate-file-name` (`~/`-style replacement).
   - If the resulting label still does not fit the box's top border, the path SHALL be left-truncated with a leading ellipsis `…/` preserving the trailing basename.
   The label is right-padded with the `─` border character to fill the top edge.
2. A **body** of up to 10 lines from `<path>`, starting at `<start>`, each prefixed with `│ ` and suffixed with ` │` in `gfm-pretty-border-face`.  Body lines SHALL be fontified by the major-mode resolved for `<path>` via `auto-mode-alist`.  Fontification uses `font-lock-ensure` in a temp buffer; the temp buffer SHALL have `buffer-file-name` set to `<path>` before activating the major-mode (so tree-sitter modes that key off the file name activate).  The temp buffer SHALL NOT trigger any file-saving side effects.
3. Body-line truncation: if a body line's `string-width` exceeds the box's interior width, the line SHALL be truncated to the interior width minus one cell and suffixed with `…`.  Truncation is measured in cells via `string-width`, not characters.
4. A **bottom border** in `gfm-pretty-border-face`.
   - When the requested range exceeds the 10-line cap, the bottom border SHALL embed `+N more lines` (where N is `(end - start + 1) - 10`).
   - Otherwise, the bottom border SHALL be a bare `└──…──┘`.

Box width SHALL be `min(available-width, max(80, longest-body-line + decoration-w))`, where `decoration-w` is 4 (matching `│ ` + ` │`) and `available-width` is the selected window's `gfm-pretty--available-width` (falling back to `fill-column` or 80).

The markdown label (`[label]` portion of the link) SHALL NOT appear anywhere in the rendered preview surface.

When `<end>` is omitted, the range SHALL be `<start>` to `<start>` (single line).

When the file does not exist or the range is invalid, the overlay SHALL render as a bare single-line sentinel — no box, no border — propertised with `shadow` face, of the form `[broken preview] <abbrev-path>:<start>-<end> — <reason>` where `<reason>` is `file not found` or `invalid range`.

The major-mode resolution SHALL be a pure function of `<path>` via `auto-mode-alist`; when no entry matches, fontification SHALL fall back to `fundamental-mode`.

The underlying buffer text SHALL NOT be modified — overlays use `display` properties only.

**Marker-aware continuation prefix.**  When the link span begins at a buffer position whose preceding characters on the same line are non-empty (a list-item marker `- ` / `* ` / `+ ` / `<n>. `, or a blockquote marker `> ` before the link), every line of the display string EXCEPT the first SHALL be prefixed with a `continuation-prefix` string whose visual width equals the column at which the first display line begins.  The first display line is left bare — it inherits its column from the overlay's buffer position.

The continuation prefix is chosen to mirror what surrounds the link's line:

- **Blockquote context** (the line begins with `>` AND the `blockquotes` decorator is enabled): the prefix SHALL equal the rail's wrap-prefix string, i.e. `(make-string inset-cols ?\s)` followed by `▌` (in `gfm-pretty-blockquotes-rail-face`) followed by a space.  This makes the rail flow continuously through the preview's body and bottom rows so a multi-line blockquote containing prose before/after the preview reads as one block.
- **List-item context** (or any other non-empty leading run): the prefix SHALL be `(make-string indent ?\s)` in the default face.

Lines whose link span starts at the line's beginning (`indent = 0`) SHALL render without any continuation prefix.

The blockquote rail decorator SHALL continue to render its rail overlay on a `> ` line containing a preview — i.e., the blockquote decorator MUST NOT suppress its per-line rail on preview lines.  The preview's continuation prefix takes care of the rail on the box's body rows.

#### Scenario: small range renders fontified body inside a box

- **GIVEN** a standalone link `[fn](modules/auth.rs#L42-L48)` on its own line
- **AND** lines 42-48 of `modules/auth.rs` are 7 lines of Rust code
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region
- **AND** the overlay's `display` string contains the 7 source lines
- **AND** the display string begins with `┌` and ends with `┘`
- **AND** at least one character in the body carries a `face` text property assigned by `rust-mode` (or `rust-ts-mode` per `auto-mode-alist`)

#### Scenario: file not found renders bare sentinel

- **GIVEN** a standalone link `[x](does-not-exist.rs#L1-L5)`
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string is a single line
- **AND** contains `[broken preview]`
- **AND** contains `file not found`

#### Scenario: oversize range renders with bottom-border footer

- **GIVEN** a standalone link `[x](file.rs#L1-L40)` whose file has at least 40 lines
- **WHEN** previews are rendered
- **THEN** the overlay's body contains the first 10 lines of the file
- **AND** the bottom border embeds `+30 more lines`

#### Scenario: list-item preview aligns under marker

- **GIVEN** a line `- [fn](modules/auth.rs#L1-L3)`
- **WHEN** previews are rendered
- **THEN** the overlay's first display line starts with `┌` (no leading indent — the overlay's display position already puts `┌` at the visual column of `[')
- **AND** every body line starts with two leading spaces before `│`
- **AND** the bottom border starts with two leading spaces before `└`

#### Scenario: blockquote preview keeps the rail flowing through the box

- **GIVEN** a line `> [fn](modules/auth.rs#L1-L3)` with the `blockquotes` decorator enabled and default inset
- **WHEN** previews are rendered
- **THEN** the overlay's first display line starts with `┌` (no leading prefix — its visual column comes from the buffer position of `[`)
- **AND** every body line is prefixed with `<inset-spaces>▌<space>` so the `▌` rail glyph is visible at the same column as adjacent `> ` lines outside the preview
- **AND** the bottom border line carries the same `<inset-spaces>▌<space>` prefix

### Requirement: Diff-range link preview overlay rendering

For each standalone link whose URL matches `diff:<base>...<head>` (with optional `#<path>` fragment), the `link-previews` decorator SHALL place an overlay on the link's full markdown expression whose `display` property is a propertised multi-line string composed of:

1. A **top border** in `gfm-pretty-border-face` of the form `┌─ <base>...<head> ──…──┐` when the URL has no path qualifier, or `┌─ <base>...<head> — <path> ──…──┐` when path-scoped.  Refs matching `(rx bos (= 40 hex) eos)` SHALL be shortened to their first 7 characters before being embedded in the label; branch names and tags pass through unchanged.
2. A **body** of up to 10 lines from `git diff <base>...<head> [-- <path>]` executed from the buffer's worktree.  Body lines SHALL render in **LHS-margin mode**: each line is prefixed with a single `│` (no left padding) and suffixed with `│`, so the first body column is the `+`/`-`/` ` diff indicator.  Box interior decoration width is 2 cols (matching authored ` ```diff ` fences via `gfm-pretty-fences--lhs-margin-langs`).  Body lines SHALL be fontified by activating `diff-mode` in a temp buffer, inserting the joined lines, running `font-lock-ensure`, and capturing the resulting `face` text properties.  Added lines SHALL carry `diff-added` (or its mode-resolved family), removed lines `diff-removed`, hunk headers `diff-hunk-header`, file headers `diff-file-header`, and context lines `diff-context` — the `diff-mode` defaults.
3. Body-line truncation as for source previews, applied to the interior width minus 1 cell with a trailing `…`.
4. A **bottom border** in `gfm-pretty-border-face`.
   - When the diff exceeds the 10-line cap, the bottom border SHALL embed `+N more lines`.
   - Otherwise, the bottom border SHALL be a bare `└──…──┘`.

When `git diff` produces no output, the overlay SHALL render as a bare single-line sentinel — no box — propertised with `shadow` face, of the form `[broken preview] <base>...<head>[<path>] — no changes`.

When `git` exits non-zero, the overlay SHALL render as a bare single-line sentinel of the form `[broken preview] <base>...<head>[<path>] — git error: <first-error-line>`.

The markdown label SHALL NOT appear anywhere in the rendered preview surface.

The box width formula is identical to source-range previews, substituting `decoration-w` 2 for LHS-margin mode.

**Marker-aware indent** applies identically to diff previews: when the link span follows a list-item or blockquote marker on the same line, the top border, every body line, and the bottom border SHALL be prefixed with `(make-string indent ?\s)`.

#### Scenario: diff link renders box-bordered preview

- **GIVEN** a standalone link `[change](diff:HEAD~1...HEAD#auth.rs)`
- **AND** `git diff HEAD~1...HEAD -- auth.rs` produces 6 lines of output
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region
- **AND** the overlay's `display` string begins with `┌` and ends with `┘`
- **AND** the display contains those 6 diff lines
- **AND** the body lines begin directly with `│+`, `│-`, or `│ ` (no left padding between `│` and the indicator)

#### Scenario: SHA refs shortened to 7 chars

- **GIVEN** a standalone link with two 40-char hex SHAs as base and head
- **WHEN** previews are rendered
- **THEN** the overlay's top border contains the first 7 chars of each SHA
- **AND** does not contain either 40-char SHA in full

#### Scenario: empty diff renders bare sentinel, no box

- **WHEN** a standalone diff link's `git diff` invocation produces no output
- **THEN** the overlay's `display` string is a single line
- **AND** contains `[broken preview]` and `no changes`

#### Scenario: diff body carries diff-mode faces

- **GIVEN** a standalone diff link whose `git diff` output contains at least one `+`-prefixed line and one `-`-prefixed line
- **WHEN** previews are rendered
- **THEN** at least one position inside an added line carries a `face` text property derived from `diff-added`
- **AND** at least one position inside a removed line carries a `face` text property derived from `diff-removed`

### Requirement: Link previews refresh on edit and window-configuration change

The `link-previews` decorator SHALL participate in the engine's standard rebuild lifecycle:

- `after-change-functions` triggers `:full-rebuild-required-p`; the decorator returns non-nil for any edit (link detection is buffer-position-sensitive enough that incremental scoping isn't worth the bookkeeping).
- `window-configuration-change-hook` triggers per-window display overlay re-rendering so box widths track the active window's `gfm-pretty--available-width`.

The decorator SHALL NOT install file-system watchers, idle timers (beyond the engine's existing rebuild debounce), or any other passive refresh mechanism.

#### Scenario: edit triggers preview rebuild

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone source-range link rendered
- **WHEN** the user edits the link's URL to point at a different file
- **THEN** the engine schedules a rebuild
- **AND** the next redisplay shows the updated preview

#### Scenario: window resize triggers preview re-render

- **GIVEN** a rendered preview overlay in window W of width N
- **WHEN** W's width changes to M
- **THEN** the preview overlay's box width re-clamps to `min(M, max(80, longest-body-line + decoration-w))`

### Requirement: Link previews — RET dispatch

Every `link-previews` decorator overlay SHALL carry a `keymap` text property binding `RET` (and `<return>`) to `gfm-pretty-link-previews-follow-link-at-point`.

`gfm-pretty-link-previews-follow-link-at-point` SHALL:

- Identify the preview overlay at point via the `gfm-pretty-link-previews-display` overlay property; signal a `user-error` when none is found.
- For source-range overlays (`gfm-pretty-link-previews-kind` is `source`): resolve the path against the buffer's `default-directory` if relative, then `find-file` the target, narrow nothing, move point to the start line, and pulse the requested range when `pulsar-highlight-pulse` is `fboundp`.
- For diff-range overlays (`gfm-pretty-link-previews-kind` is `diff`): `(require 'magit nil t)`; when `magit-diff-range` is `fboundp` call it with `<base>...<head>` (no extra args) and, when the parsed URL has a `:path`, pass `(list path)` as the files argument.  When `magit-diff-range` is absent, fall back to populating a `*Diff*` buffer with `git -C <worktree> diff <base>...<head> [-- <path>]` output and `pop-to-buffer` it in `diff-mode`.
- Before navigating, push the current point onto the local mark ring via `push-mark`.

The overlay-borne keymap SHALL be active only while point is inside the overlay; outside the overlay, the buffer's normal RET binding (typically `markdown-follow-link-at-point`) SHALL run unmodified.

`gfm-present-follow-link` (presentation-mode's RET) MAY delegate to `gfm-pretty-link-previews-follow-link-at-point` when the link at point is a preview-eligible standalone source-range or diff-range link, OR continue to use its own dispatch — both routes SHALL produce the same observable outcome.

#### Scenario: RET on source preview opens target

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone source-range preview overlay rendered
- **WHEN** the user presses RET with point inside the overlay's span
- **THEN** the target file is opened in another window
- **AND** point in the target buffer is at the start of the requested start line
- **AND** the start position is pushed onto the originating buffer's mark ring

#### Scenario: RET on diff preview dispatches to magit when available

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone diff-range preview overlay rendered
- **AND** `magit-diff-range` is `fboundp`
- **WHEN** the user presses RET with point inside the overlay's span
- **THEN** `magit-diff-range` is called with `<base>...<head>`
- **AND** when the link has a `:path`, the call includes `(list <path>)` as the files argument

#### Scenario: RET on diff preview falls back to `*Diff*` buffer when magit is absent

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone diff-range preview overlay rendered
- **AND** `magit-diff-range` is NOT `fboundp`
- **WHEN** the user presses RET with point inside the overlay's span
- **THEN** a `*Diff*` buffer is populated from `git diff <base>...<head> [-- <path>]` run in the worktree
- **AND** the buffer is shown in `diff-mode`

#### Scenario: RET outside any preview overlay falls through

- **GIVEN** a buffer with `gfm-pretty-mode` enabled, a preview overlay rendered, and point on a non-overlay link in the same buffer
- **WHEN** the user presses RET
- **THEN** the preview decorator's keymap does not fire
- **AND** the buffer's normal RET binding handles the link
