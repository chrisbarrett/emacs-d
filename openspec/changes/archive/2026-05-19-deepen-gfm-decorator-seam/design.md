## Context

`lisp/gfm/gfm-pretty-engine.el` owns one set of buffer-local
lifecycle hooks (`after-change-functions`, `window-configuration-change-hook`,
`post-command-hook`) and one idle rebuild timer. Each decorator
registers via `gfm-pretty-define-decorator`. The engine drives
collect / apply-anchors / apply-display through registered callbacks
and offers a per-decorator `:scoped-rebuild-fn` escape hatch for
post-edit work.

Today three decorators (`fences`, `callouts`, `tables`) each
implement `:scoped-rebuild-fn` as the same algorithm:

1. If the dirty region overlaps a *structural line* (fence delim,
   callout marker, table delim) — full rebuild.
2. If the dirty region overlaps a line *adjacent* to a block whose
   discovery is gated by adjacency (blank-line gating for indent
   fences; line above / below a callout) — full rebuild.
3. Otherwise collect the decorator's blocks. If exactly one block's
   range fully contains the dirty region — rebuild just that block.
   Else — full rebuild.

See `gfm-pretty-fences.el:930-956`, `gfm-pretty-callouts.el:574-599`,
`gfm-pretty-tables.el:1404-1432`. Each decorator also ships its own
`--rebuild-block` and `--rebuild-blocks` helpers that the engine
already provides (`gfm-pretty--rebuild-block-for-window`,
`gfm-pretty-fences.el:798-809` for the full-block teardown idiom).

Each of those three decorators also ships:

- `--stats` alist + `--init-stats` + `--record-stats` + an
  `slow-rebuild-threshold` defcustom. Wrapper is identical (count,
  total-time, last-time, max-time, slow-warning); only the phase
  keys differ.
- `--dirty-region`, `--last-window-state`, `--rebuild-timer`
  buffer-locals that mirror engine state purely so tests can
  inspect them. Engine even ships `gfm-pretty--set-compat-mode-var`
  (`gfm-pretty-engine.el:621-629`) for the same reason.
- A `define-minor-mode gfm-pretty-NAME-mode` shim that just calls
  `gfm-pretty--enable-decorator` / `--disable-decorator`. Two
  decorators (`hrule`, `links`) ship this too despite not having
  the buffer-local mirrors.

`gfm-pretty-borders.el` carries border-drawing AND width helpers
(`--available-width`, `--text-width`, `--max-line-width`), range /
overlap predicates (`--in-ranges-p`, `--region-overlaps-p`), and
the window-list helper (`--display-windows`, which is also defined
in the engine). The name "borders" no longer matches what is in
the file.

## Goals / Non-Goals

**Goals:**

- Engine owns scoped-rebuild routing. Decorator's contribution
  shrinks to *structural-line ranges* and *edit-adjacency ranges*.
- Engine owns rebuild stats (count / total / last / max / slow
  warning). Phase-level breakdowns stay decorator-local.
- Delete the parallel engine-shadow API used only by tests
  (decorator-named `*-mode`, mirrored buffer-locals, 0-arg
  scoped-rebuild, `--schedule-full-rebuild`,
  `--rebuild-block-for-window`).
- `gfm-pretty-borders.el` contains only border-drawing concerns.
- No user-visible behaviour change. `gfm-pretty-mode` and
  `gfm-pretty-toggle-decorator` remain the user entry points.

**Non-Goals:**

- No change to anchor / display split, per-window reconciler,
  reveal semantics, or overlay-registry encoding.
- No change to discovery regexps or per-decorator rendering
  geometry (top / bottom builders, right-edge alignment, etc.).
- `gfm-present.el` is out of scope.

## Decisions

### Decision: Replace `:scoped-rebuild-fn` with two predicate registrations

The engine adds two optional registration keys:

- `:structural-line-ranges-fn` — `()` → list of `(BEG . END)` ranges
  whose edits force a full rebuild. For fences: the open + close
  fence lines AND YAML helmet marker lines. For callouts: the
  `> [!TYPE]` marker lines. For tables: header + delimiter +
  trailing body line (per current implementation).
- `:edit-adjacency-fn` — `(REGION)` → non-nil when REGION overlaps
  any line whose edit could create or destroy a block (blank-line
  adjacency for indent fences; line above / below a callout).
  Decorators without adjacency-gated discovery (tables, hrule,
  links) omit this key.

The engine's `gfm-pretty--scheduled-rebuild` (currently
`gfm-pretty-engine.el:524-536`) becomes:

```
(dolist (decorator (gfm-pretty--enabled-decorators))
  (let* ((name (gfm-pretty--decorator-name decorator))
         (dirty (gfm-pretty--state-get name 'dirty-region)))
    (gfm-pretty--state-set name 'dirty-region nil)
    (cond
     ((null dirty) (gfm-pretty--rebuild decorator))
     ((gfm-pretty--dirty-forces-full-rebuild-p decorator dirty)
      (gfm-pretty--rebuild decorator))
     (t (gfm-pretty--rebuild-scoped-by-block decorator dirty)))))
```

Where `--rebuild-scoped-by-block` calls
`gfm-pretty--collect`, finds matching blocks via
`gfm-pretty--region-overlaps-p` against each `:range-fn` result,
and rebuilds the single-fully-contained block via
`gfm-pretty--rebuild-block` (extracted from current decorator
`--rebuild-block` bodies). Fallback to full rebuild on multiple
matches or partial containment.

**Why not keep `:scoped-rebuild-fn` as an escape hatch?** No
decorator actually needs an escape hatch — every existing impl is
the same routing. Removing it makes the engine the single owner of
the policy. A future decorator with truly novel scoped logic can
re-introduce the hook.

**Alternative rejected — `:can-scope-edit-p` predicate per
(dirty, block).** Cleaner-looking but loses the early structural-line
exit, forcing every dirty region to scan every block.

### Decision: Engine-owned generic stats; decorator-local phase stats

Engine adds `gfm-pretty--state` slot `rebuild-stats`:
`(rebuild-count total-time last-time max-time)`. Engine times each
`:rebuild-fn` and `--rebuild-block` invocation and updates the slot.
Slow-rebuild warning uses a new defcustom
`gfm-pretty-slow-rebuild-threshold` (default 0.05 s) and emits via
`display-warning`. Per-decorator overrides (`gfm-pretty-fences-slow-rebuild-threshold`,
`gfm-pretty-callouts-slow-rebuild-threshold`,
`gfm-pretty-tables-slow-rebuild-threshold`) are replaced by the
single engine-level customisation.

`gfm-pretty-stats` becomes an interactive command:
`(gfm-pretty-stats &optional DECORATOR)` — defaulting to a
completing-read over registered decorators, returning the engine
stats plus any decorator-published phase totals. Phase
instrumentation (`find-fenced`, `compose-borders`,
`compose-overflow`, `apply` in fences;
`find` / `compose` / `apply` in tables) stays in the decorator —
the phase keys differ and the call sites are inside decorator
internals.

`gfm-pretty-fences-stats` and `gfm-pretty-tables-stats` remain as
thin shims that call `(gfm-pretty-stats 'fences)` etc.

**Alternative rejected — push phase timing into engine too.**
Phase boundaries are decorator-internal (mid-discovery, mid-compose);
the engine has no insight into them. Hoisting them costs more
coupling than it saves.

### Decision: Delete decorator-named `*-mode` shims and mirrored buffer-locals

`gfm-pretty-fences-mode`, `gfm-pretty-callouts-mode`,
`gfm-pretty-tables-mode`, `gfm-pretty-hrule-mode`,
`gfm-pretty-links-mode` are deleted along with
`gfm-pretty--set-compat-mode-var` and every decorator's
`--dirty-region`, `--last-window-state`, `--rebuild-timer`
buffer-locals (engine carries the same in
`gfm-pretty--state` / `gfm-pretty--rebuild-timer`).

The compat shims `--schedule-full-rebuild`,
`--rebuild-block-for-window`, the 0-arg `--rebuild-scoped`, and
the `--init-stats` / `--record-stats` / `--stats` family are
deleted from each decorator.

Tests in `lisp/gfm/gfm-pretty-tests.el` that drive these are
rewritten to call the engine directly:

- `(gfm-pretty--state-get 'fences 'dirty-region)` in place of
  `gfm-pretty-fences--dirty-region`.
- `(gfm-pretty--arm-engine-timer)` in place of
  `gfm-pretty-fences--schedule-full-rebuild`.
- `(gfm-pretty--rebuild-block-for-window (gfm-pretty--get 'fences)
  block window)` in place of
  `gfm-pretty-fences--rebuild-block-for-window`.
- `(gfm-pretty--scheduled-rebuild)` driving the engine timer
  callback directly, with the dirty region set via state-set.
- Toggling decorators in tests uses
  `gfm-pretty--enable-decorator` / `--disable-decorator` directly.

**Alternative rejected — keep the shims behind a feature flag.**
A flag would freeze the parallel surface forever; the shims have
no end users beyond the tests.

**Alternative rejected — make the shims `(declare (obsolete ...))`
first.** This is a personal config repo, not a public package;
there are no third-party callers. Direct deletion is correct.

### Decision: Split `gfm-pretty-borders.el`

Move out of `gfm-pretty-borders.el` into `gfm-pretty-engine.el`:

- `gfm-pretty--in-ranges-p`, `gfm-pretty--region-overlaps-p` —
  used by engine routing.
- `gfm-pretty--available-width`, `gfm-pretty--text-width`,
  `gfm-pretty--max-line-width` — width primitives used by render
  paths but lifecycle-adjacent.
- `gfm-pretty--display-windows` — already defined in engine; drop
  the duplicate.
- `gfm-pretty--wrap-prefix-w` constant.

What stays in `gfm-pretty-borders.el`:

- `gfm-pretty-border-face` and
  `gfm-pretty--normalised-border-face`.
- `gfm-pretty--top-strings`, `--bottom-strings`,
  `--right-after`, `--right-after-overflow`, `--wrap-prefix`.
- `gfm-pretty--simulate-wrap`, `--last-visual-col` (wrap sim is
  part of right-edge alignment).
- `gfm-pretty--icon-gui-nudge` defcustom.

**Alternative rejected — rename `gfm-pretty-borders.el` to
`gfm-pretty-graphics.el`.** Renaming a file in a personal config
costs every consumer a require update for cosmetic gain. Split
the file's contents in place; keep its name.

### Decision: Keep `gfm-pretty-mode` and `gfm-pretty-toggle-decorator` as the only user entry points

Already the documented public API. Tests for "enable
fences in a buffer" become:

```
(gfm-pretty-mode 1)            ;; or
(gfm-pretty-toggle-decorator 'fences)
```

instead of `(gfm-pretty-fences-mode 1)`. No coverage is lost
because every existing `*-mode` does only:

```
(gfm-pretty--install-engine-hooks)
(gfm-pretty--enable-decorator (gfm-pretty--get 'NAME))
```

with the inverse on disable.

## Risks / Trade-offs

- **[Risk] Test rewrite mis-models the engine timer.** Several
  tests synchronously poke `--rebuild-timer` after an edit and
  assert the timer fires. Mitigation: tests cancel + invoke
  `gfm-pretty--scheduled-rebuild` directly, matching how the
  timer callback runs; assertions on the timer existence move to
  `gfm-pretty--rebuild-timer` (the engine's single timer
  var).

- **[Risk] An external CLAUDE.md / config caller binds
  `gfm-pretty-fences-mode` directly.** Search for `gfm-pretty-fences-mode`
  / `-callouts-mode` etc. across the repo before deletion; replace
  with `gfm-pretty-mode` + `gfm-pretty-toggle-decorator`. If
  `gfm-pretty-mode` is the only enabling site (the umbrella
  already enables every decorator), no caller change is needed.

- **[Risk] Slow-rebuild threshold becomes global.** Some
  decorators currently use the same default (0.05 s) but a
  user-set `gfm-pretty-fences-slow-rebuild-threshold` would no
  longer apply just to fences. Mitigation: this is a personal
  config; nobody has overridden these defaults. Engine-level
  threshold replaces all three with one defcustom.

- **[Trade-off] Removing `:scoped-rebuild-fn` from the protocol
  means a future decorator with bespoke routing must reintroduce
  the hook.** Acceptable — none of the five current decorators
  needs it, and the protocol stays smaller for the common case.

- **[Trade-off] Width helpers and range predicates move to
  `gfm-pretty-engine.el`, which already weighs 656 LOC.** Engine
  grows by ~30 LOC; decorator graphics file shrinks by the same.
  Engine becomes the canonical home for "things every decorator
  needs to do positional math"; borders file becomes specifically
  about drawing boxes. The split is by concern, not size.
