## Context

`lisp/gfm/` ships an umbrella minor mode plus five decorators
(callouts, fences, tables, hrule, links). The previous unification
change relocated everything into one library and stood up a thin
registry, but stopped before collapsing the duplicated lifecycle —
each decorator still owns its own minor mode, scheduler, dirty-region
tracker, reveal handler, and block cache.

```
                            What's deep, what's shallow today
                            ──────────────────────────────────

  gfm-pretty.el (161 LOC)
    gfm-pretty-mode          shallow — loops over registry, toggles 5 modes
    gfm-pretty-toggle-decorator   shallow — flips one decorator's own mode
    gfm-pretty-block-at-point     deep enough — single public API, dispatch
    gfm-pretty-define-decorator   shallow — accepts 5 dispatch callbacks,
                                   no lifecycle contract
  gfm-pretty-borders.el (611 LOC)
    border-drawing toolkit   deep — primitives reused across decorators
    registry struct          deep — abstracts overlay tagging
    reconciler struct        half-built — parameterised by SYMBOL NAMES
                              the decorator threads in; engine doesn't own
                              the symbols
    scheduler primitives     half-built — arm-rebuild-timer takes
                              callback + timer-symbol + mode-symbol
                              (decorator owns the timer)

  gfm-pretty-callouts.el (1026 LOC)
  gfm-pretty-fences.el (1196 LOC)
  gfm-pretty-tables.el (2362 LOC)
  gfm-pretty-hrule.el (339 LOC)
  gfm-pretty-links.el (673 LOC)

    Each owns ~30–60 LOC of scaffold:
      defvar-local --overlays, --hidden-ovs
      defvar-local --last-window-state, --dirty-region, --rebuild-timer
      defconst --registry, --reconciler
      defun --schedule-rebuild, --schedule-full-rebuild
      defun --reveal (4 of 5)
      defun --rebuild, --rebuild-blocks, --rebuild-block-for-window
      defun --reconcile-windows, --rebuild-scoped
      define-minor-mode gfm-pretty-<name>-mode
      with-eval-after-load 'gfm-pretty registers 5-callback shim
```

The spec at `openspec/specs/gfm-pretty/spec.md` describes a deepened
shape that the implementation never reached:

- Requirement "Decorator registration via gfm-pretty-define-decorator"
  (line 49) lists 10 kwargs and asserts "the engine SHALL own the
  lifecycle hooks (after-change-functions, window-configuration-
  change-hook, post-command-hook, the idle rebuild timer, the
  dirty-region tracker, the window-state snapshot)." Today: registration
  accepts 5 kwargs, engine owns no hooks.
- Requirement "Debounced rebuild scheduler" (line 173) says "at most
  one idle rebuild timer per buffer." Today: 5 timers per buffer.
- Requirement "Per-window cursor reveal" (line 190) says "the engine
  SHALL install one post-command-hook handler that consults every
  enabled decorator's :revealable-p predicate." Today: each decorator
  adds its own handler.

The implementation passes the existing tests because the per-decorator
copies of the lifecycle achieve the same observable outcome through
duplication. The spec is correct; the implementation is incomplete.

The narrowing-regression suite (`:tags '(narrowing-regression)` in
`lisp/gfm/gfm-pretty-tests.el`) is load-bearing: it catches
`args-out-of-range` and zombie overlays when discovery / teardown
mishandle narrowing. The suite runs against each decorator's
behaviour, so it covers the deepened engine equally — same
observable outcomes, smaller surface.

## Goals / Non-Goals

**Goals:**

- Engine owns lifecycle. One `after-change-functions` handler, one
  `window-configuration-change-hook` handler, one `post-command-hook`
  reveal, one idle rebuild timer per buffer.
- Decorators register intent: discovery, range extraction, anchor
  application, display application, revealable-prop, optional
  font-lock, optional `:on-enable` / `:on-disable` for
  decorator-specific extras (xref backend, cursor-intangible-mode,
  keymap install). They no longer own their own mode.
- Engine memoises `:collect` by `buffer-chars-modified-tick` once.
  Per-decorator `--blocks-cache` defvar-locals deleted.
- Engine dispatches reveal across every registered decorator's
  `:revealable-prop`. Per-decorator `--reveal` handlers deleted.
- `gfm-pretty-borders.el` becomes a graphics toolkit only; the
  lifecycle code moves to `gfm-pretty-engine.el`.
- Internal callout faces lose the `+markdown-` prefix.

**Non-Goals:**

- No public-API change. Existing call-sites
  (`gfm-pretty-mode`, `gfm-pretty-toggle-decorator`,
  `gfm-pretty-block-at-point`, `gfm-pretty-edit-block-at-point`,
  `gfm-pretty-define-decorator`) keep their names. The umbrella mode
  enables / disables the same decorators in the same order.
- No behaviour change visible to the buffer. Same overlays, same
  faces, same reveal cadence, same scheduling, same per-window
  rendering, same narrowing safety.
- No new decorator. The five built-ins remain. Adding a sixth
  becomes cheaper but is out of scope.
- No removal of the tables decorator's bespoke cursor model. Tables
  uses cursor-aware highlighting that does not fit the standard
  reveal protocol; it stays separate and is invoked from the
  decorator's `:on-enable` / `:on-disable`.

## Decisions

### Decision: Engine owns lifecycle hooks once per buffer

When `gfm-pretty-mode` enables, the engine installs:

- one `after-change-functions` handler →
  `gfm-pretty--after-change BEG END LEN`. Records BEG..END in the
  per-decorator dirty-region (only for decorators with non-nil enable
  bit), arms the single idle timer.
- one `window-configuration-change-hook` handler →
  `gfm-pretty--wcc`. Iterates decorators, calls the engine reconciler
  per decorator.
- one `post-command-hook` handler →
  `gfm-pretty--reveal`. Iterates decorators, walks each one's
  revealable overlays in the selected window.

Engine state lives in `gfm-pretty--decorator` struct, extended with
buffer-local slots stored on a single buffer-local alist:

```elisp
(defvar-local gfm-pretty--state nil
  "Alist (NAME . (overlays hidden-ovs dirty-region window-state
                  blocks-cache rebuild-timer)).")
```

Lookups: `(gfm-pretty--state-get name 'overlays)` etc. One buffer-local
variable instead of 5×6 buffer-locals.

Disable removes the three hooks and cancels the timer; iterates
decorators in reverse order calling `:on-disable`.

**Alternatives considered:**

- _Keep per-decorator buffer-locals, just unify the hook handlers._
  Rejected — the duplication of state is most of the LOC; collapsing
  the hooks alone leaves ~200 LOC of `defvar-local` declarations.
- _Pass `gfm-pretty-define-decorator` the buffer-local symbol names
  to allocate (like today's reconciler struct)._ Rejected — that's
  the *current* half-built shape; it leaks engine bookkeeping into
  every decorator file.

### Decision: Engine memoises `:collect-fn`

The engine wraps each decorator's `:collect-fn`:

```elisp
(defun gfm-pretty--collect (decorator)
  (let* ((cache (gfm-pretty--state-get (gfm-pretty--decorator-name decorator)
                                       'blocks-cache))
         (tick (buffer-chars-modified-tick)))
    (if (eq tick (car cache))
        (cdr cache)
      (let ((blocks (save-restriction
                      (widen)
                      (funcall (gfm-pretty--decorator-collect-fn decorator)))))
        (gfm-pretty--state-set name 'blocks-cache (cons tick blocks))
        blocks))))
```

Decorators register their *uncached, widened-scan* discovery function.
The `save-restriction` + `widen` wrapper guarantees the narrowing-safety
invariant for every decorator without repeating the discipline in five
files.

**Alternatives considered:**

- _Per-decorator caches, no engine memoisation._ Rejected — that's
  today's shape and the source of 5× copy-paste.
- _Memoise by `(point-min)` / `(point-max)` instead of tick._
  Rejected — narrowing changes those without invalidating content.
  Tick is the correct cache key (already proven in current
  per-decorator caches).

### Decision: Engine drives reveal via registered `:revealable-prop`

Each decorator that participates in reveal registers a unique overlay
property symbol (the existing `gfm-pretty-callouts-revealable`,
`gfm-pretty-fences-revealable`, …). The engine's
`post-command-hook` handler:

```elisp
(defun gfm-pretty--reveal ()
  (dolist (entry gfm-pretty--decorators)
    (let* ((d (cdr entry))
           (prop (gfm-pretty--decorator-revealable-prop d))
           (saved (gfm-pretty--decorator-saved-display-prop d)))
      (when prop
        (gfm-pretty--reveal-for d prop saved)))))
```

`gfm-pretty--reveal-for` is the existing algorithm (loop over
`hidden-ovs`, restore those point left; loop over `overlays-in pos
(1+ pos)`, hide those at point in the selected window). One copy.

Tables uses `:on-enable` to install its own cursor handler — it
doesn't register `:revealable-prop` and the reveal loop skips it.

**Alternatives considered:**

- _Decorators register a `:reveal-fn`._ Rejected — every implementation
  is the same loop with different property names; better to share the
  loop and pass property names than five copies of the loop.
- _Make tables fit the protocol too._ Rejected — tables tracks
  current cell highlighting, not source reveal. Different concern,
  doesn't fit the protocol. Out of scope.

### Decision: Split `gfm-pretty-borders.el` into borders + engine

```
Before                                  After
──────                                  ─────
gfm-pretty-borders.el (611 LOC)         gfm-pretty-borders.el (~280 LOC)
  graphics toolkit                        top-strings, bottom-strings,
  registry + reconciler                   right-after, right-after-overflow,
  scheduler                               simulate-wrap, wrap-prefix,
  window-state                            normalised-border-face,
                                          available-width, max-line-width
                                          (decorator-neutral)

                                        gfm-pretty-engine.el (~330 LOC pre-collapse,
                                          ~480 LOC after absorbing lifecycle from
                                          decorators)
                                          registry, decorator struct,
                                          buffer-local state, hooks owner,
                                          scheduler, reveal, cache,
                                          reconciler, dispatcher
```

Hrule and links decorators currently `(require 'gfm-pretty-borders)` to
reach the registry; after the split they `(require 'gfm-pretty-engine)`
and the borders requirement disappears for them.

Done in pass 1 — gives the deepened engine a clean home before pass 2
moves lifecycle into it.

**Alternatives considered:**

- _Keep one file._ Rejected — two unrelated modules under one filename
  forces every decorator to require all of borders + engine even when
  it only needs one.

### Decision: Sequence — 5 passes; narrowing-regression suite gates each

```
Pass 1   Split gfm-pretty-borders.el → borders + engine
         No behaviour change. Lifecycle still lives in decorators.
         Decorators now (require 'gfm-pretty-engine) for registry +
         reconciler + scheduler, (require 'gfm-pretty-borders) only
         when they draw boxes.

Pass 2   Move lifecycle hooks into the engine.
         gfm-pretty-define-decorator accepts the full kwarg set.
         Engine installs after-change / wcc / post-command / timer
         once per buffer when gfm-pretty-mode enables.
         Per-decorator state moves to gfm-pretty--state alist.
         Per-decorator define-minor-mode bodies and schedule-rebuild
         / schedule-full-rebuild helpers deleted.
         gfm-pretty-toggle-decorator now flips the engine-tracked
         enable bit instead of toggling a child minor mode.

Pass 3   Engine memoises collect-fn. Delete each decorator's
         --blocks-cache defvar-local and --find-blocks (cached
         wrapper). Decorators register --find-blocks-1 (uncached,
         widened).

Pass 4   Engine drives reveal via :revealable-prop.
         Delete callouts/fences/hrule/links --reveal functions.
         Each decorator registers its revealable overlay-property
         symbol once.

Pass 5   Rename +markdown-gfm-callout-*-face →
         gfm-pretty-callouts-*-face. Rename
         +markdown-gfm-callout-refresh-body-faces →
         gfm-pretty-callouts--refresh-body-faces. Update spec
         scenario at openspec/specs/gfm-pretty/spec.md line 446.
         Migration risk: themes that reference the old face names
         lose their customisation. Mitigated by `define-obsolete-
         face-alias` for one release cycle (none currently
         configured outside this repo, but the alias is cheap
         insurance).
```

Each pass byte-compiles and passes `make test` (full ERT suite
including `narrowing-regression`) before the next begins. A failure
in pass N localises to that pass.

**Alternatives considered:**

- _One mega-pass._ Rejected — narrowing failures would be ambiguous
  between cache wiring, hook collapse, reveal dispatch.
- _Land pass 5 first (lowest risk)._ Accepted as a possible
  reordering if the rename surfaces no surprises; not load-bearing
  on ordering.

### Decision: `define-obsolete-face-alias` for `+markdown-` face renames

`+markdown-gfm-callout-note-face` → `gfm-pretty-callouts-note-face`,
similarly for `tip`, `important`, `warning`, `caution`, `header`,
`note-body`, `tip-body`, `important-body`, `warning-body`,
`caution-body`. Same for `+markdown-prettier-ignore-comment-face` and
`+markdown-overlay-border-face` and `+markdown-gfm-hrule-face`.

The faces are not referenced outside `lisp/gfm/` in this repo. But
`define-obsolete-face-alias` is one line per face and preserves any
external customisation if it ever exists, so it's cheap to add and
will be removed once external usage is confirmed absent.

**Alternatives considered:**

- _Hard rename, no alias._ Acceptable in this personal config (no
  external consumers) but the alias is cheap insurance and
  documents the migration explicitly. Pick this if alias clutter
  feels worse than the risk.

## Risks / Trade-offs

[**Lifecycle migration touches every decorator**] → Two-pass
discipline (1 then 2) localises file-split issues vs lifecycle
issues. Each pass runs `make test` before the next.

[**`gfm-pretty-mode` toggle ordering changes**] → Today's umbrella
toggles each decorator's mode, which has side effects via its body.
After deepening, the engine installs hooks once and calls each
decorator's `:on-enable` in registration order. Side-effect ordering
preserved via the registration-order iteration; verified by the
existing `gfm-pretty-mode` enable/disable round-trip test.

[**Per-decorator mode commands disappear**] → No user docs reference
them; `modules/leader/init.el` uses the public toggle. Any
`M-x gfm-pretty-callouts-mode` muscle memory dies. Acceptable —
`(gfm-pretty-toggle-decorator 'callouts)` is the documented entry
point. Could add `define-obsolete-function-alias` for one release
if needed; defer until requested.

[**Engine state alist grows hot**] → 5 decorators × 6 slots = 30
alist entries per buffer. `alist-get` is fine at this size; if it
becomes hot, switch to a `hash-table` keyed on decorator name —
internal change.

[**Engine memoising widens the buffer for every collect**] →
Already happens in every per-decorator cache today (the
`save-restriction` + `widen` is in each `--find-blocks-1`).
Centralising means it happens once-per-tick rather than once per
decorator per tick. Mild perf win.

[**Reveal dispatch order matters for overlapping overlays**] →
Today each decorator independently reveals its own. After: the
engine walks decorators in registration order. Overlapping
revealable overlays (none exist in practice — different decorators
target different block types) would be revealed in registration
order. Document the order; add a test if any overlap shows up.

[**`with-eval-after-load 'gfm-pretty` registration shim**] → Each
decorator file currently wraps its `gfm-pretty-define-decorator`
call in `with-eval-after-load 'gfm-pretty`. After the split this
becomes `with-eval-after-load 'gfm-pretty-engine`. Mechanical.

[**Tables decorator does not fit the protocol cleanly**] → Has
its own cursor-aware highlighting model, indirect-edit machinery,
keymap. Solution: `:on-enable` installs the keymap + advice +
cursor handler; `:on-disable` reverses. No `:revealable-prop`
registered; engine's reveal loop skips it. Verified: existing
table-cursor scenarios in `gfm-pretty-tests.el` are unaffected by
the engine's reveal loop because they operate on different overlay
properties.

[**Narrowing-resilient teardown invariant**] → Currently asserted
per decorator. After deepening, the engine's
`gfm-pretty--remove-overlays` is the only caller of the widened
bulk-cleanup. Easier to reason about; one place to audit.

[**Spec divergence between MODIFIED reqs and migrated content**] →
The MODIFIED `Debounced rebuild scheduler` and `Per-window cursor
reveal` requirements must reflect the engine-owned reality. Authors
of the MODIFIED entries copy the FULL existing requirement body
(per OpenSpec rules) and edit; partial-MODIFIED loses content at
archive.

[**Face alias clutter**] → If 11 `define-obsolete-face-alias`
calls feel like noise, drop them — no external consumer exists.
Worth keeping for one release as a defensive measure.
