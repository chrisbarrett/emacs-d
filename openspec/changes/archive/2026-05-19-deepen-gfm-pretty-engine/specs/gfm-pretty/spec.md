## ADDED Requirements

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


## MODIFIED Requirements

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

Decorators SHALL NOT install their own `after-change-functions`,
`window-configuration-change-hook`, or `post-command-hook` handlers
for scheduling purposes. Decorators MAY use `:on-enable` /
`:on-disable` to install decorator-specific hooks (e.g. the tables
decorator's cursor handler, the links decorator's xref backend).

#### Scenario: Burst of edits causes one rebuild

- **GIVEN** `gfm-pretty-mode` is enabled
- **WHEN** the user types ten characters in rapid succession
- **THEN** the engine SHALL cancel and re-arm the timer on each edit
- **AND** the rebuild SHALL run once, 0.2 s after the last edit
- **AND** every decorator's `:apply-anchors` / `:apply-display`
  contributions SHALL be invoked over the dirty region

#### Scenario: One handler per hook regardless of decorator count

- **GIVEN** a buffer with five enabled decorators
- **WHEN** the engine installs lifecycle hooks
- **THEN** `(length after-change-functions)` SHALL increase by one
  (engine handler only)
- **AND** `(length window-configuration-change-hook)` SHALL increase
  by one
- **AND** `(length post-command-hook)` SHALL increase by one
- **AND** at most one buffer-local idle timer SHALL be live

### Requirement: Per-window cursor reveal

The engine SHALL install one `post-command-hook` handler that walks
every enabled decorator's revealable overlays in the selected window.
Decorators that participate in reveal SHALL register a unique overlay
property symbol via `:revealable-prop` at registration time;
the engine reads this property to identify revealable overlays
created by that decorator.

When point lies inside a revealable overlay whose `window` property
is nil or equals the selected window, the engine SHALL save the
overlay's `display` property under the decorator's
`saved-display-prop` and set `display` to nil — exposing the source.
When point leaves the overlay, the engine SHALL restore `display`
from `saved-display-prop`. Reveal SHALL be scoped to the selected
window: an overlay restricted to a non-selected window SHALL NOT
have its source revealed regardless of where point is.

A decorator MAY register a custom `:revealable-p` predicate that
takes an overlay and returns non-nil iff the engine should treat it
as revealable at the current point; this overrides the default
property-presence check.

A decorator MAY omit `:revealable-prop` if it manages its own
cursor model (e.g. the tables decorator's cell highlighting); the
engine's reveal loop skips such decorators.

#### Scenario: Point on a fence marker reveals the source in selected window only

- **GIVEN** a fenced code block decorated in W1 (selected) and W2
- **WHEN** point moves onto the opening fence marker in W1
- **THEN** W1 SHALL show the raw `\`\`\`lang` line
- **AND** W2 SHALL continue to show the rendered box

#### Scenario: Engine reads `:revealable-prop` per decorator

- **GIVEN** the callouts decorator registers
  `:revealable-prop 'gfm-pretty-callouts-revealable`
- **AND** the fences decorator registers
  `:revealable-prop 'gfm-pretty-fences-revealable`
- **WHEN** point enters a callout overlay carrying
  `gfm-pretty-callouts-revealable`
- **THEN** the engine SHALL reveal it
- **AND** SHALL NOT touch overlays carrying
  `gfm-pretty-fences-revealable` outside the callout

#### Scenario: Tables decorator skipped by reveal loop

- **GIVEN** the tables decorator registers without `:revealable-prop`
- **WHEN** point enters a rendered table cell
- **THEN** the engine's reveal loop SHALL NOT modify any table overlay
- **AND** the tables decorator's `:on-enable`-installed cursor handler
  SHALL manage cell highlighting independently

### Requirement: Theme change responsiveness

The callouts decorator's `:on-enable` SHALL add
`gfm-pretty-callouts--refresh-body-faces` to `+theme-changed-hook`.
`:on-disable` SHALL remove it. The refresh function SHALL recompute
each callout body face's `:background` from the current theme by
tinting 10% toward the theme background.

#### Scenario: Theme switch

- **WHEN** the user switches from a light to a dark theme
- **THEN** `+theme-changed-hook` fires
- **AND** each `gfm-pretty-callouts-*-body-face` background is
  recomputed
- **AND** the next redisplay shows correctly tinted body backgrounds


## REMOVED Requirements

### Requirement: Callout block-discovery cache

**Reason:** Subsumed by the engine-level "Block discovery memoisation"
requirement, which applies uniformly to every registered decorator.
Per-decorator caches are no longer the right granularity once the
engine owns the lifecycle.
**Migration:** See `gfm-pretty/spec.md` requirement "Block discovery
memoisation". The callouts decorator now registers its uncached,
widened `:collect-fn`; the engine memoises the result.

### Requirement: Code-fence block-discovery cache

**Reason:** Subsumed by the engine-level "Block discovery memoisation"
requirement.
**Migration:** See `gfm-pretty/spec.md` requirement "Block discovery
memoisation". The fences decorator now registers its uncached,
widened `:collect-fn`; the engine memoises the result.
