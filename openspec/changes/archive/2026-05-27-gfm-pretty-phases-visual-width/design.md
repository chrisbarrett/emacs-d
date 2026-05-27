## Context

`gfm-pretty` is a decorator engine that lays overlays on Markdown
buffers. Multiple decorators run concurrently on the same buffer:

- **Width-changing** decorators install `display` props that shrink or
  expand the visual extent of in-line content relative to raw buffer
  characters. Today only `links` is in this class
  (`lisp/gfm/gfm-pretty-links.el`); it collapses a long URL like
  `[label](/very/long/path.md#L1-L36)` into an icon + short label.
- **Width-measuring** decorators read line widths to size borders and
  pad right edges. Today: `callouts`, `fences`, `tables`.
- **Other** decorators (`blockquotes`, `hrule`, `link-previews`)
  neither change nor measure visual width.

Two engine properties combine to make width-measuring decorators
miscount:

1. `gfm-pretty--decorators` is a flat alist (`lisp/gfm/gfm-pretty-engine.el:178`)
   iterated by `dolist` in every dispatch path
   (`gfm-pretty--scheduled-rebuild` at line 751;
   `gfm-pretty--after-change` at 722; `gfm-pretty--enabled-decorators`
   at 716). Order = `setf`-insertion order = load order. Today
   `callouts` loads before `links` (alphabetically, and via the
   `(require 'gfm-pretty-callouts)` in `gfm-pretty-blockquotes.el:22`),
   so on a fresh enable callouts measures before links has laid any
   display overlays.
2. `gfm-pretty--max-line-width` (`lisp/gfm/gfm-pretty-engine.el:106`)
   measures `(- lend lbeg)` — raw buffer chars, ignoring overlays and
   `display` text-props.
   `gfm-pretty--simulate-wrap`
   (`lisp/gfm/gfm-pretty-borders.el:157`) and its caller
   `gfm-pretty--last-visual-col` consume `buffer-substring-no-properties`,
   stripping the very display props that determine visual width.

Concrete victim: a callout whose body lines are dominated by
prettified inline links. `gfm-pretty-callouts--apply-block-display`
(`lisp/gfm/gfm-pretty-callouts.el:339`) computes:

- `max-content` via `gfm-pretty--max-line-width body-beg-pos end 2`
  (line 367) — raw char count.
- `line-content-w (max 0 (- (- lend lbeg) 2))` (line 416) — raw
  char count.
- `right-after-overflow` calls `gfm-pretty--last-visual-col` on
  `buffer-substring-no-properties` (line 418, 422–425) — raw chars.

If the raw text width fits in the content budget (`box-width - 4`),
the non-overflow path lands `│` at `align-to (box-width - 2)` — far
right of where the visual content ends. If the raw text exceeds the
budget (common with un-prettified-yet long URLs), the overflow path
simulates a wrap that does not happen in the rendered buffer and the
`│` lands on a phantom wrapped row.

The bug class is engine-wide. Fences and tables share the same width
helpers and the same decorator-iteration race; their visible
failure modes are dormant only because their typical content
(monospaced code; numeric / short-string cells) rarely triggers
width-changing overlays today.

## Goals / Non-Goals

**Goals:**

- Make decorator iteration order an engine invariant rather than a
  load-order artefact, structured around the width-change /
  width-measurement dependency.
- Replace raw-character width math with overlay-aware visual-width
  math throughout the engine's public surface.
- Make it impossible by construction for a future width-measuring
  decorator to read raw-char widths — i.e. delete the old helpers
  rather than mark them obsolete.
- Eliminate spurious wrap-simulation inside atomic display tokens
  (link icons, future prettify-symbols, etc).

**Non-Goals:**

- Per-pixel ground-truth alignment via `posn-at-point` /
  `window-text-pixel-size`. The overlay-walk approximation is
  sufficient for monospaced cell-aligned borders and avoids forcing
  redisplay.
- `composition` text-prop / ligature awareness. Markdown content
  rarely uses these; the visual-width helper documents the gap.
- Fixing the pre-existing nested-decorator case (e.g. fenced block
  inside a callout — both draw borders in overlapping ranges). Out
  of scope; a separate change.
- A general dependency graph between decorators. Three coarse
  phases are sufficient for current and foreseeable decorators.

## Decisions

### Decision: Three-phase iteration enum

Phases: `(atoms containers overlays)`. Engine guarantees iteration
order in every dispatch path (`--rebuild`, `--rebuild-scoped-by-block`,
`--rebuild-window-prioritised`, `--scheduled-rebuild`, lifecycle
enable/disable, `--enabled-decorators`).

- `atoms` — decorators producing visual-width-changing in-line
  overlays. `links` today.
- `containers` — decorators measuring visual width to draw block
  borders. `callouts`, `fences`, `tables`.
- `overlays` — neither changes nor measures visual width.
  `blockquotes`, `hrule`, `link-previews`.

Default phase when unspecified: `containers`. Containers are the most
common decorator shape; atoms are the rare, dependency-anchoring
case that must be declared explicitly.

**Alternatives considered:**

- Two phases (`producers / consumers`). Rejected: no place to put
  width-irrelevant decorators without making the phase axis lie.
- Pairwise `:depends-on` lists. Rejected: invites tentacular graphs
  where phase ordering captures the only dependency that actually
  exists today.
- A "sort decorators by class at register-time" tweak with no
  schema change. Rejected: invisible invariant; future decorators
  silently land in the wrong class.

### Decision: Visual-width primitive walks overlays + text-props

`gfm-pretty--visual-line-width LBEG LEND` walks `[LBEG, LEND)` using
`next-overlay-change` and `next-property-change`. For each chunk:

- If any overlay in `(overlays-at pos)` carries an `invisible` prop,
  or `(get-text-property pos 'invisible)` is non-nil, contribute 0.
- Else if any overlay carries a `display` prop whose value is a
  string, contribute `(string-width display-string)` and advance to
  the overlay's end (do not double-count).
- Else if `(get-text-property pos 'display)` is a string, contribute
  `(string-width display-string)`.
- Else contribute `(string-width (buffer-substring-no-properties pos
  next))`.

Display values that are stretch-glyph specs (`(space :align-to N)`,
`(space :width N)`) are honoured by computing their cell count from
the spec where determinable; unknown specs fall back to the
display-region's raw `string-width`. The common atom case in this
codebase (link icon + label) uses plain strings.

`gfm-pretty--visual-max-line-width BEG END &optional INDENT`
iterates lines from BEG to END, computes per-line visual width minus
INDENT, returns the max.

**Alternatives considered:**

- `window-text-pixel-size` / `posn-at-point`. Rejected: requires a
  live window and a completed redisplay cycle; expensive per line;
  pulls in font-metric concerns out of scope for cell-aligned
  borders.
- A textsubst pre-pass that builds a "visualised string" then runs
  the existing helpers on it. Rejected: doubles the buffer allocation
  per measurement and bakes the assumption that visual width is a
  pure function of buffer-substring + immediate overlays (it isn't,
  once invisibility enters).

### Decision: Atomic-span marker for wrap simulation

Atom-phase decorators tag their visual-width-contributing
overlays/text-props with `gfm-pretty-atomic t` on the display string
itself.

`gfm-pretty--simulate-wrap` accepts a string that may carry
`gfm-pretty-atomic` as a text property over arbitrary substrings.
Inside an atomic span, `?\s` is not treated as a wrap point. This
mirrors Emacs's actual redisplay: a `display` string is one
indivisible unit at wrap time.

To run the simulator over a buffer range with display substitutions,
the engine provides `gfm-pretty--visualised-string LBEG LEND` that
walks the same way as the visual-width primitive but builds a
propertised string instead of a width counter.

**Alternatives considered:**

- Pass a separate "atomic ranges" list alongside the text. Rejected:
  two-argument coupling invites mismatches; text-properties travel
  with substrings naturally.
- Replace inner spaces with ` ` in the visualised string.
  Rejected: hack that loses information; surface for future bugs
  if a real ` ` appears in buffer text.

### Decision: Delete `gfm-pretty--max-line-width` outright

No external callers (private `--` prefix; this repo holds every
caller). An obsolete shim would allow future decorators to keep
reaching for the raw-char measurement and reintroduce the bug class.
Outright deletion forces every container-shape decorator through the
visual-width helper.

`gfm-pretty-simulate-wrap` keeps its name but the documented contract
changes: it now requires that any visually-atomic span in the input
text carries the `gfm-pretty-atomic` text-property. Spec delta
records the change.

### Decision: Phase enforced at every dispatch site, not at register-time only

A naïve implementation could sort `gfm-pretty--decorators` once at
register-time. Rejected because:

- Decorators may be enabled / disabled at runtime via
  `gfm-pretty-toggle-decorator`; `--enabled-decorators` is the
  iteration source of truth.
- Dispatch helpers (`--rebuild-blocks`, `--rebuild-block`,
  `--rebuild-block-for-window`) take a single decorator; only the
  callers that fan out (`--scheduled-rebuild`, etc.) need to
  honour phase order. Sorting at the iteration site keeps the
  invariant local to where it matters.

Implementation: a single helper `gfm-pretty--decorators-by-phase`
returns the phase-ordered list, used everywhere a `dolist` over
decorators happens.

### Decision: Short-circuit non-overflow path on visual fit

In callouts (and equivalently fences/tables where the same shape
applies), once the visual line width is in hand, the per-body-line
overflow check becomes:

```elisp
(setq overflow-p (> visual-line-width content-budget))
```

In the non-overflow case the right-edge `│` lands at
`align-to (box-width - 2)` with no wrap simulation. This is the hot
path for prettified-link callouts and is now correct by construction.
The overflow path still calls `simulate-wrap`, now over the
visualised string with atomic spans honoured.

## Risks / Trade-offs

- **Risk**: Existing tests assume decorator iteration order
  (load-dependent today). → Mitigation: audit
  `lisp/gfm/gfm-pretty-tests.el`, `gfm-pretty-callouts` tests in
  `modules/lang-markdown/tests.el`, and decorator-level test files
  for order assumptions; update them to drive phase order
  explicitly.
- **Risk**: Decorators that toggle dynamically (`gfm-pretty-toggle-
  decorator`) re-enter the dispatch path. → Mitigation: phase
  ordering lives in the iteration site, not in cached state, so a
  newly enabled decorator slots into its phase on the next pass.
- **Risk**: Visual-width helper undercounts when an atom decorator's
  display value is a stretch-glyph spec (`(space :align-to ...)`).
  → Mitigation: links uses plain strings; document the gap; add a
  test that covers stretch-glyph fallback (visual width =
  `string-width` of the underlying buffer chars).
- **Risk**: `prettify-symbols-mode` composition props are not
  visited by the walker. → Mitigation: out of scope (non-goal);
  noted in the helper docstring so a future change can extend.
- **Trade-off**: Phase ordering is global to the engine, not per-
  rebuild-pass. A buffer where only `containers` are dirty still
  runs `atoms` for inspection (no-op if their dirty region is
  empty). Cost is negligible; benefit is invariant simplicity.
- **Trade-off**: Deleting `gfm-pretty--max-line-width` is a breaking
  internal change. → Mitigation: scope is one repo; the proposal
  enumerates every caller; `git grep` is the validator.

## Migration Plan

1. Land engine schema additions (phase slot, default `containers`,
   visual-width primitives) before touching any decorator.
2. Switch atom-phase decorator (`links`) to declare its phase and
   tag its display contributions atomic.
3. Switch container-phase decorators (`callouts`, `fences`,
   `tables`) one at a time: declare phase, swap width math, run
   that decorator's tests.
4. Switch overlay-phase decorators (`blockquotes`, `hrule`,
   `link-previews`) — declaration only, no width-math changes.
5. Delete `gfm-pretty--max-line-width` last; remaining callers (if
   any) surface as byte-compile errors at this point.
6. Spec delta committed alongside step 1; updated as later steps
   introduce additional observable behaviour.

Rollback: revert the change branch; no persisted state changes.
