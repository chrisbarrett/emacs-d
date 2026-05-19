## Why

`gfm-pretty` ships an umbrella minor mode plus a registry of five
decorators, but each decorator still owns its own lifecycle: its own
`define-minor-mode` body, its own scheduler, its own dirty-region tracker,
its own `--reveal`, its own block-discovery cache, its own registry and
reconciler instance. The engine is half-built — the toolkit lives in
`gfm-pretty-borders.el` (registry struct, reconciler struct, scheduler
primitives, window-state) but decorators thread their own state symbols
into it and recreate the same lifecycle skeleton five times.

Concrete duplication today:

| Per decorator (×5)                 | Should be (×1 in engine)        |
|------------------------------------|----------------------------------|
| `--schedule-rebuild` on `after-change-functions` | one engine handler |
| `--schedule-full-rebuild` on `window-configuration-change-hook` | one engine handler |
| `--reveal` on `post-command-hook`  | one engine handler              |
| `--rebuild-timer` idle timer       | one engine timer                |
| `--dirty-region`, `--last-window-state` defvar-locals | one per registered decorator inside the engine |
| `--blocks-cache` (TICK . BLOCKS) memoisation | engine memoises `collect-fn` |
| `define-minor-mode gfm-pretty-<name>-mode` | umbrella mode only        |

The umbrella `gfm-pretty-mode` is shallow — it iterates the registry and
toggles each decorator's own mode. The registry holds 5 callbacks
(`:enable-fn`, `:disable-fn`, `:enabled-p-fn`, `:block-at-point-fn`,
`:edit-at-point-fn`) — purely dispatch — and knows nothing about
lifecycle. Adding a sixth decorator requires writing ~250 lines of
duplicated scaffold before any actual decorator logic.

The stable spec at `openspec/specs/gfm-pretty/spec.md` already describes
the deepened shape (lines 49–84 of `Decorator registration via
gfm-pretty-define-decorator`: "the engine SHALL own the lifecycle
hooks (`after-change-functions`,
`window-configuration-change-hook`, `post-command-hook`, the idle
rebuild timer, the dirty-region tracker, the window-state snapshot)"
and the registration form lists ten kwargs including `:collect`,
`:apply-anchors`, `:apply-display`, `:revealable-p`, etc). The
implementation diverged: the registration form today accepts only the
five dispatch callbacks, and the engine never installs hooks.

This change finishes the engine to match the spec, deletes ~250 LOC of
duplicated lifecycle scaffold, splits the toolkit-vs-engine concerns
that currently share one file, and cleans up the last `+markdown-`
face-name leak from the previous module rename.

## What Changes

- **Split** `lisp/gfm/gfm-pretty-borders.el` into two files:
  - `gfm-pretty-borders.el` — graphics toolkit only (top/bottom border
    strings, `right-after`, wrap simulation, normalised border face,
    width helpers, `:available-width` primitive). Decorator-neutral.
  - `gfm-pretty-engine.el` — registry, reconciler, scheduler, window-state,
    block-cache, reveal dispatch. The lifecycle owner.
- **Deepen** `gfm-pretty-define-decorator` registration. Existing 5
  callbacks expand to the full set already documented in the spec:
  `:collect`, `:range`, `:apply-anchors`, `:apply-display`,
  optional `:font-lock`, `:revealable-prop` (the overlay-property
  symbol marking revealable overlays), `:revealable-p`,
  `:block-at-point`, `:edit-at-point`, `:on-enable`, `:on-disable`.
  Engine maintains per-decorator state (overlay list, dirty region,
  window-state snapshot, blocks-cache, registry struct) keyed by
  decorator name.
- **Engine owns lifecycle hooks**, installed once per buffer when
  `gfm-pretty-mode` enables. One `after-change-functions` handler, one
  `window-configuration-change-hook` handler, one `post-command-hook`
  reveal handler, one idle rebuild timer. Each iterates registered
  decorators with non-nil enable bits.
- **Engine memoises** `collect-fn` results by
  `buffer-chars-modified-tick`. Per-decorator `--blocks-cache`
  defvar-locals deleted.
- **Engine drives reveal**. Decorators register their
  `revealable-prop` overlay property symbol once; the engine's reveal
  loop walks every decorator's revealable overlays in the selected
  window. Per-decorator `--reveal` functions deleted (4 of 5
  decorators; tables uses a different cursor model).
- **Per-decorator `define-minor-mode` bodies deleted**: callouts,
  fences, tables, hrule, links no longer have their own minor mode.
  Their enable bits live as registry-tracked buffer-locals.
  `gfm-pretty-toggle-decorator NAME` flips the bit and runs
  `:on-enable` / `:on-disable`.
- **Rename internal callout faces** from `+markdown-gfm-callout-*`
  (leftover from `modules/lang-markdown/`) to `gfm-pretty-callouts-*`.
  Refresh function `+markdown-gfm-callout-refresh-body-faces` renames
  to `gfm-pretty-callouts--refresh-body-faces`. Cleans up the last
  `+markdown-` leak in `lisp/gfm/`.
- **No behaviour change.** Same overlays, same scheduling cadence,
  same reveal scoping, same per-window rendering, same narrowing
  safety. The narrowing-regression suite passes after each pass.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: Internals refactor — file split, lifecycle collapse,
  cache and reveal absorbed into the engine. Spec touches:
  - **ADDED**: "Block discovery memoisation" (engine-level requirement
    making the cache contract explicit and decorator-agnostic).
  - **MODIFIED**: "Debounced rebuild scheduler" — sharpens "at most
    one timer per buffer" to "one timer, one after-change handler,
    one wcc handler, one post-command handler per buffer, all
    engine-owned".
  - **MODIFIED**: "Per-window cursor reveal" — adds the per-decorator
    `revealable-prop` registration mechanism; the engine walks each
    decorator's overlays.
  - **MODIFIED**: "Theme change responsiveness" — renames
    `+markdown-gfm-callout-refresh-body-faces` →
    `gfm-pretty-callouts--refresh-body-faces` and the
    `+markdown-gfm-callout-*-body-face` references in the scenario.
  - **REMOVED**: "Callout block-discovery cache" — subsumed by the
    engine-level "Block discovery memoisation" requirement.
  - **REMOVED**: "Code-fence block-discovery cache" — subsumed.

## Impact

- **Code**: `lisp/gfm/gfm-pretty-borders.el` splits into two files.
  `lisp/gfm/gfm-pretty.el` grows (engine moves out of borders into
  here, then `gfm-pretty-engine.el`). Each decorator file
  (`gfm-pretty-callouts.el`, `gfm-pretty-fences.el`,
  `gfm-pretty-tables.el`, `gfm-pretty-hrule.el`,
  `gfm-pretty-links.el`) drops ~30–60 lines of duplicated lifecycle
  scaffold, depending on decorator. Net change: ~−200 LOC across the
  five decorator files, ~+150 LOC in the engine. Net ≈ −50 to −100
  LOC, organised in one place instead of five.
- **Public API**: Unchanged. `gfm-pretty-mode`,
  `gfm-pretty-toggle-decorator`, `gfm-pretty-block-at-point`,
  `gfm-pretty-edit-block-at-point`, `gfm-pretty-define-decorator`
  retain their names. `gfm-pretty-define-decorator` accepts more
  kwargs but old call-sites still work (the dispatch-only callbacks
  are still recognised; per-decorator modes remain registered
  until pass 2 collapses them).
- **Private API removed**: `gfm-pretty-<name>-mode` (×5),
  `gfm-pretty-<name>--schedule-rebuild` (×5),
  `gfm-pretty-<name>--schedule-full-rebuild` (×5),
  `gfm-pretty-<name>--reveal` (×4),
  `gfm-pretty-<name>--blocks-cache` (×5 — some have multiple cache
  defvars per decorator), `gfm-pretty-<name>--dirty-region` (×5),
  `gfm-pretty-<name>--rebuild-timer` (×5),
  `gfm-pretty-<name>--last-window-state` (×5),
  `gfm-pretty-<name>--registry` (×5),
  `gfm-pretty-<name>--reconciler` (×5), assorted thin shim
  functions. `+markdown-gfm-callout-*-face` and
  `+markdown-gfm-callout-refresh-body-faces` rename.
- **Tests**: `lisp/gfm/gfm-pretty-tests.el` — the per-decorator
  narrowing-regression suites stay; tests targeting
  `gfm-pretty-callouts-mode` toggle adapt to
  `(gfm-pretty-toggle-decorator 'callouts)`. New tests added for
  engine-owned cache memoisation and reveal dispatch.
- **Specs**: Single delta against `openspec/specs/gfm-pretty/spec.md`
  (one ADDED, three MODIFIED, two REMOVED). No other axis affected.
- **External callers**: `modules/leader/init.el` uses the public API
  (`gfm-pretty-toggle-decorator`, `gfm-pretty-edit-block-at-point`) —
  unchanged. `modules/lang-markdown/init.el`'s `gfm-mode-hook`
  enables `gfm-pretty-mode` — unchanged.
- **Performance**: Net win. Today, opening a markdown buffer runs
  five `after-change-functions` handlers, five
  `window-configuration-change-hook` handlers, five
  `post-command-hook` reveals, and arms five idle timers per edit
  burst. After: one of each. The functions inside still iterate
  decorators, but lookup is once-per-event, not five-times-per-event.
- **Sequencing**: Five passes — file split, lifecycle collapse,
  cache absorption, reveal absorption, face rename. Each pass passes
  the narrowing-regression suite before the next begins. Failures
  localise to one pass.
