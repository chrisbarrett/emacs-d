## Why

The `gfm-pretty` engine is well-deepened (single timer, single
overlay-registry abstraction, single reveal hook), but the
engine↔decorator seam is leaking duplicate infrastructure into every
decorator: each of `fences`, `callouts`, `tables` reimplements the
same scoped-rebuild routing algorithm, the same per-buffer
rebuild-stats wrapper, and each ships a parallel "compat" surface
(named minor-mode, mirrored buffer-locals, 0-arg shims) that exists
only so legacy test fixtures can probe engine state through the
decorator's namespace. Tests testing past the decorator's real
interface is the canonical signal that the seam is the wrong shape.
Three near-identical scoped-rebuild implementations and a parallel
shadow API are now load-bearing on touch — every new decorator
inherits the boilerplate.

## What Changes

- Engine owns scoped-rebuild routing. Decorators register
  `:structural-line-ranges-fn` and `:edit-adjacency-fn` (both
  optional); the engine performs the dirty-region intersect /
  adjacency / fully-contained-block check and dispatches to the
  decorator's per-block apply functions or its full `:rebuild-fn`.
  `:scoped-rebuild-fn` is removed from the registration protocol.
- **BREAKING** Decorator-named `define-minor-mode` shims
  (`gfm-pretty-fences-mode`, `gfm-pretty-callouts-mode`,
  `gfm-pretty-tables-mode`, `gfm-pretty-hrule-mode`,
  `gfm-pretty-links-mode`), the mirrored buffer-locals
  (`--dirty-region`, `--last-window-state`, `--rebuild-timer`), the
  0-arg `--rebuild-scoped` / `--schedule-full-rebuild` /
  `--rebuild-block-for-window` shims, and the engine's
  `gfm-pretty--set-compat-mode-var` are deleted. Existing tests are
  rewritten to drive the engine API directly.
- Engine owns generic rebuild stats (count, total / last / max time,
  slow-rebuild warning, `slow-rebuild-threshold` defcustom),
  per-decorator via `gfm-pretty--state`. Decorator-local
  phase-breakdown timing (e.g. `find-fenced`, `compose-overflow`,
  `apply`) stays where it provides irreplaceable signal.
  `gfm-pretty-stats DECORATOR` becomes the unified introspection
  command. Per-decorator `gfm-pretty-fences-stats` /
  `gfm-pretty-tables-stats` remain as thin wrappers.
- `gfm-pretty-borders.el` is split. Box-drawing primitives (top /
  bottom builders, right-edge after-string, wrap simulator, border
  face) keep that name. Width helpers, range / overlap predicates,
  and `gfm-pretty--display-windows` move to `gfm-pretty-engine.el`
  where they live with the engine that uses them.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: replaces "decorator registration" /
  "debounced rebuild scheduler" / "scoped post-edit rebuild" /
  "performance instrumentation" requirements to reflect engine
  ownership of routing and stats; removes the decorator-named
  `*-mode` and 0-arg `--rebuild-scoped` shim surfaces from the
  public contract.

## Impact

- `lisp/gfm/gfm-pretty-engine.el` — gains scoped-rebuild router,
  generic stats, width / range helpers.
- `lisp/gfm/gfm-pretty-fences.el`, `gfm-pretty-callouts.el`,
  `gfm-pretty-tables.el` — lose `--rebuild-scoped` (and adjacency
  helpers), `--rebuild-block`, `--rebuild-blocks`, `--stats` &
  `--record-stats` & `--init-stats`, the legacy compat
  buffer-locals, the `--schedule-full-rebuild` /
  `--rebuild-block-for-window` shims, and their compat
  `define-minor-mode` form. Register
  `:structural-line-ranges-fn` and `:edit-adjacency-fn` instead.
- `lisp/gfm/gfm-pretty-hrule.el`, `gfm-pretty-links.el` — lose
  their compat `define-minor-mode` form.
- `lisp/gfm/gfm-pretty-borders.el` — keeps only border builders /
  wrap / face normalisation.
- `lisp/gfm/gfm-pretty-tests.el` — `--dirty-region` /
  `--last-window-state` / `--rebuild-timer` probes and
  `*-mode` toggle assertions migrated to engine API.
- No user-visible behaviour change. `gfm-pretty-mode` and
  `gfm-pretty-toggle-decorator` remain the only user entry points.
