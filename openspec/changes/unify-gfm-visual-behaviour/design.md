## Context

`modules/lang-markdown/` today hosts both the markdown integration layer
(file associations, `major-mode-remap-alist`, `markdown-code-lang-modes`,
`apheleia` formatter wiring, `+markdown-tab-dwim`, two `markdown-mode`
advices) and a 5900-line overlay-based visual-decoration engine
(`lib/+gfm-block-borders.el` plus five decorator libs).

The decorator libs are near-identical in lifecycle shape:

```
modules/lang-markdown/lib/
├── +gfm-block-borders.el   shared engine (registry, reconciler, scheduler,
│                           border builders, wrap simulator, window-state)
├── +gfm-callouts.el        gfm-callouts-mode
├── +gfm-code-fences.el     gfm-code-fences-mode
├── +gfm-tables.el          gfm-tables-mode
├── +gfm-hrule.el           gfm-hrule-mode
└── +gfm-links.el           gfm-links-mode
```

Each `define-minor-mode` adds `after-change-functions`,
`window-configuration-change-hook`, `post-command-hook`, an idle timer, a
dirty-region tracker, and a window-state snapshot. Five copies; same shape.

External coupling: `modules/leader/init.el:85-87` reaches into the private
`gfm-tables--block-at-point` and `gfm-tables-edit-table-at-point` symbols
because no public block-introspection API exists.

`openspec/specs/lang-markdown/spec.md` is 2343 lines covering all five
decorators alongside the markdown integration layer. Specs governed by
`spec-conventions/spec.md` enforce one-spec-per-axis and
axis-matches-module-name rules.

`modules/presentation/` has no code references to `lang-markdown/lib/`;
its `init.el` is an 11-line `(require '+autoloads)` shim, and the
substantive code is one ~724-line lib in `lib.el` plus tests.

The existing `lisp/` directory holds config infrastructure
(`+core-paths.el`, `+corelib.el`, `+hooks.el`, `+modules.el`,
`+autoloads.el`) plus one focused lib (`evil-tty-cursor.el`). Self-
contained libraries belong here under the convention "`lisp/` for
libraries, `modules/` for composition" — formalised in this change.

## Goals / Non-Goals

**Goals:**

- One umbrella minor mode (`gfm-pretty-mode`) replacing five decorator
  modes. Per-decorator toggles route through
  `gfm-pretty-toggle-decorator`.
- An extension protocol (`gfm-pretty-define-decorator`) that lets a
  decorator declare `:collect`, `:range`, `:apply-anchors`,
  `:apply-display`, optional `:font-lock`, `:revealable-p`,
  `:block-at-point`, `:edit-at-point`, `:on-enable`, `:on-disable`. The
  engine owns the lifecycle.
- A minimal public surface (`gfm-pretty-mode`,
  `gfm-pretty-define-decorator`, `gfm-pretty-toggle-decorator`,
  `gfm-pretty-block-at-point`, `gfm-pretty-edit-block-at-point`, a small
  set of rendering primitives). Everything else `gfm-pretty--` private.
- All visual-behaviour modification for markdown buffers (overlays,
  font-lock callout fontifier, callout faces, body-face theme refresh,
  header-face weight styling, `markdown-blockquote-face` neutralisation)
  lives in one library and has spec coverage and unit tests.
- `modules/leader/init.el` migrates off private `gfm-tables--*` symbols
  onto the public block-introspection API.
- New library axis `gfm-pretty` at `lisp/gfm/`; presentation code
  relocates to `lisp/gfm/gfm-present.el` under a new library axis
  `gfm-present`. The `lang-markdown` module axis is retained but
  shrunk.
- `spec-conventions` Requirement "One spec per axis…" reworded to allow
  axes that correspond to a `lisp/<lib>/` library OR a
  `modules/<name>/` composition unit, formalising the placement
  convention.

**Non-Goals:**

- No behaviour change. Every overlay, every face, every reveal, every
  scoped rebuild renders identically to today's modes.
- No hoisting of the narrowed-source renderer or focus-highlight from
  `gfm-present` into `gfm-pretty`. Today there is no second consumer;
  hoisting is deferred until one materialises.
- No stable public API beyond this config. External Emacs packages that
  happen to load this code are not promised compatibility — this is
  personal config.
- No new external dependencies. `markdown-mode` and `nerd-icons` (soft)
  remain the only required packages.
- No performance optimisation beyond what falls out naturally from
  collapsing five copies of the lifecycle into one (single idle timer,
  single after-change handler, single reveal post-command).

## Decisions

### Decision: `lisp/` for libraries, `modules/` for composition

`modules/` historically housed every feature unit in this config —
including units that are purely library code (a buffer-local minor
mode with no init-time wiring) packaged alongside an
`init.el` / `lib.el` / `tests.el` shell. The line between "library"
and "config" had not been drawn explicitly.

This change draws it:

- `lisp/<family>/<lib>.el` is the home for self-contained, well-
  developed elisp libraries. Existing libs in `lisp/`
  (`+core-paths.el`, `+corelib.el`, `+hooks.el`, `+modules.el`,
  `evil-tty-cursor.el`) already follow the pattern.
- `modules/<name>/` is the home for *composition* — code that wires
  libraries into the running config via `init.el`, `gfm-mode-hook`
  installation, `auto-mode-alist` entries, formatter registration,
  leader-key bindings, etc.

`gfm-pretty` and `gfm-present` are both substantial library units
with no composition concerns of their own (no `init.el` to run at
config load, no hooks to install — just a public API and a minor mode
the user / a composer can enable). They go to `lisp/gfm/`. The
`gfm-mode-hook` that turns `gfm-pretty-mode` on lives in
`modules/lang-markdown/init.el` — the composition layer.

Layout:

```
lisp/gfm/
├── gfm-pretty.el            public entry, define-decorator,
│                            umbrella mode, engine
├── gfm-pretty-borders.el    primitives (private)
├── gfm-pretty-callouts.el   decorator
├── gfm-pretty-fences.el     decorator
├── gfm-pretty-tables.el     decorator
├── gfm-pretty-hrule.el      decorator
├── gfm-pretty-links.el      decorator
├── gfm-pretty-tests.el      ERT suite (incl. narrowing-regression)
├── gfm-present.el           slide-walkthrough mode
└── gfm-present-tests.el     ERT suite
```

Flat under `lisp/gfm/`, family namespace `gfm-`. Matches the existing
`lisp/` shape (everything flat, prefix tells you the family).
Sub-files of `gfm-pretty` prefix themselves with `gfm-pretty-` so
`grep`, `imenu`, and `find-file` group them naturally.

`modules/presentation/` is deleted outright (not renamed). Its
`init.el` is an 11-line `(require '+autoloads)` shim and contributes
nothing once `gfm-present` autoloads from `lisp/gfm/`. There is no
composition step to retain.

**Alternatives considered:**

- _Both libs as modules under `modules/gfm-pretty/` and
  `modules/gfm-present/`._ Rejected after explicit user direction:
  modules are reserved for composition. Library code (even substantial)
  belongs in `lisp/`.
- _Sub-directory per lib: `lisp/gfm-pretty/gfm-pretty.el`
  + `lisp/gfm-present/gfm-present.el`._ Rejected. Two siblings with no
  shared contents create two top-level `lisp/` entries for one
  concern (GFM tooling). The shared `lisp/gfm/` parent groups them.
- _Flat in `lisp/` (no `gfm/` parent)._ Rejected. Twelve `gfm-*.el`
  files at `lisp/` top-level would clutter the directory; the family
  parent is one layer of containment that pays for itself with two
  libs and grows naturally if more GFM tooling lands.

### Decision: New axis `gfm-pretty` (library axis under `lisp/gfm/`)

The current `lang-markdown` axis contains two concerns whose
dependencies, lifecycle, and tests do not overlap: (a) the markdown
integration layer (file associations, `major-mode-remap-alist`,
formatters, lang-mode advices) — the composition layer — and (b) the
overlay-and-font-lock visual engine (registry, reconciler, scheduler,
per-decorator narrowing-safety contract, ~50 requirement-level
invariants) — a library. The split is already visible in the file
layout (`lib/+gfm-*.el` vs `lib.el` / `init.el`) but is not yet
expressed at axis level.

Promoting `gfm-pretty` to its own library axis aligns the spec
boundary with the lib boundary this change creates and lets the
engine, decorators, narrowing-safety contract, and rebuild semantics
be specified as one cohesive unit. The new axis is wholly
behaviour-facing; the underlying engine's invariants surface through
observable contracts (per-window sizing, scoped post-edit rebuild,
narrowing-resilient discovery).

**Alternatives considered:**

- _Keep one axis `lang-markdown`._ Rejected. The spec is already 2343
  lines and mixes user-facing markdown integration with overlay-engine
  internals. Adding the extension protocol on top makes it worse.
- _Split into `gfm-engine` + per-decorator axes._ Rejected. The engine
  is private; decorators are tightly coupled to it via the extension
  protocol. Splitting fragments the public API contract without payoff.

### Decision: Axis rename `presentation` → `gfm-present` (library axis)

`modules/presentation/` is a markdown-specific slide-walkthrough mode —
its inputs are `gfm-mode` buffers, its source-range previews assume
markdown rendering, its `+presentation-focus-face` is keyed to text-
glyph bounds. The lib is pure code (a buffer-local minor mode); no
composition concerns. It relocates to `lisp/gfm/gfm-present.el`
alongside `gfm-pretty`.

The rename is mechanical at the requirement level: same content,
renamed under the `gfm-present` axis. Symbol renames
(`+presentation-*` → `gfm-present-*`) apply uniformly across lib and
tests. Filesystem move is handled in tasks.md per the
`spec-conventions` rule that capability renames are filesystem moves
driven by tasks, not paired ADDED+REMOVED delta blocks.

`modules/presentation/` is deleted; the 11-line `init.el` shim is
redundant once the lib autoloads from `lisp/gfm/`.

**Alternatives considered:**

- _Keep `presentation`._ Rejected. The bare name collides ambiguously
  with the `presentation` skill at `~/.claude/skills/presentation/`
  (a different concern: demo-mode authoring guidance).
- _Fold into `gfm-pretty` as a decorator._ Rejected. Presentation is
  buffer-level slideshow behaviour over heading-narrowed slides, not
  per-block overlay rendering. The engine's per-block lifecycle does
  not fit; conflating them would force one or the other into the
  wrong abstraction.
- _Keep at `modules/gfm-present/`._ Rejected. Pure library code; no
  composition. Library code belongs in `lisp/`.

### Decision: Umbrella mode + decorator registry

`gfm-pretty-mode` is the only user-facing toggle. The engine maintains a
buffer-local registry of active decorators, each registered via:

```elisp
(gfm-pretty-define-decorator NAME
  :collect          FN     ; → (list of blocks)
  :range            FN     ; (block) → (BEG . END)
  :apply-anchors    FN     ; (block) — width-independent overlays
  :apply-display    FN     ; (block window) — width-dependent overlays
  :font-lock        KW     ; optional font-lock keywords contribution
  :revealable-p     FN     ; optional; default checks anchor-prop
  :block-at-point   FN     ; optional; for public block introspection
  :edit-at-point    FN     ; optional; for public dispatch
  :on-enable        FN     ; optional; extras at gfm-pretty-mode enable
  :on-disable       FN)    ; optional; extras at gfm-pretty-mode disable
```

Engine owns: `after-change-functions`,
`window-configuration-change-hook`, `post-command-hook`, idle rebuild
timer, dirty-region tracker, window-state snapshot, per-decorator
overlay registries, visible-first prioritised rebuild, scoped post-edit
rebuild. Decorators contribute pure functions; the engine drives them.

Per-decorator toggling (`gfm-pretty-toggle-decorator NAME`) flips a
buffer-local enable bit consulted by the engine when iterating
decorators. Used by `leader/init.el` for the `t` binding currently bound
to `gfm-tables-mode`.

**Alternatives considered:**

- _Keep five minor modes._ Rejected per user direction; the user wants
  one umbrella.
- _Hard-code each decorator inline in the engine._ Rejected. Loses the
  extension-point property; future visual tweaks become engine edits.
- _Generic `minor-mode-list` aggregation._ Rejected. Dirty-region and
  window-state coordination across decorators requires engine ownership;
  five independent modes share neither.

### Decision: Callout font-lock folded into the callouts decorator

`+markdown-fontify-gfm-callouts` (in
`modules/lang-markdown/lib.el:266-290`) is currently invoked from
`gfm-mode-hook` independently of `gfm-callouts-mode`. The text colouring
of `> [!NOTE]` markers, the body-face merge, and the
`font-lock-extend-region-functions` hook all cooperate with the overlay
box; conceptually they are one feature.

Folding into the callouts decorator means:

- Callout decorator's `:font-lock` key contributes the keywords.
- Engine installs / uninstalls them with the decorator's enable bit.
- The body-face theme-refresh hook
  (`+markdown-gfm-callout-refresh-body-faces`) and the
  `+theme-changed-hook` wire-up move into the decorator's `:on-enable` /
  `:on-disable`.
- Toggling the callouts decorator off turns off both layers cleanly;
  enabling the umbrella mode turns both on.

**Alternatives considered:**

- _Keep font-lock independent in `lang-markdown`._ Rejected. Users would
  see half a decorator turn off when toggling, and the font-lock layer
  has no separate use-case in this config.

### Decision: Public block introspection replaces internal-name reach

`modules/leader/init.el:85-87` calls `gfm-tables--block-at-point` and
`gfm-tables-edit-table-at-point` — both private `--` symbols. The new
public API:

```elisp
(gfm-pretty-block-at-point)         ; → (DECORATOR-NAME . BLOCK) or nil
(gfm-pretty-edit-block-at-point)    ; dispatch via decorator's :edit-at-point
```

Engine consults each enabled decorator's `:block-at-point` and returns
the first match. `gfm-pretty-edit-block-at-point` calls the matching
decorator's `:edit-at-point`. Today only `tables` provides an editor;
others return nil.

**Alternatives considered:**

- _Per-decorator public symbols (`gfm-pretty-tables-block-at-point`)._
  Rejected. Public surface grows with every decorator; consumers repeat
  the dispatch logic the engine should own.
- _Keep `gfm-tables--*` private and live with the leakage._ Rejected.
  This is the leakage the change is meant to fix.

### Decision: Two-pass implementation

Pass 1 — _Relocate + rename._ Move files into `modules/gfm-pretty/lib/`,
rename namespaces (`gfm-callouts-*` → `gfm-pretty-callouts-*`, etc.),
keep the five `define-minor-mode` bodies in place under new names
(`gfm-pretty-callouts-mode`, …) wired off the umbrella. Tests pass
unchanged.

Pass 2 — _Collapse lifecycle._ Replace the five `define-minor-mode`
bodies with one umbrella + per-decorator registrations through
`gfm-pretty-define-decorator`. Engine drives shared scheduler / reveal /
reconcile / dirty-region. Tests pass unchanged.

**Rationale:** A regression in pass 1 is unambiguously a namespace
error; a regression in pass 2 is unambiguously an engine refactor error.
The narrowing-regression suite (`:tags '(narrowing-regression)`) catches
both, but separating the passes makes diagnosis instant.

**Alternatives considered:**

- _Single pass._ Rejected. Failure-class ambiguity. With the engine and
  the namespace both moving, a test failure could mean either.
- _Three passes (relocate, rename, collapse)._ Rejected.
  Relocate-without-rename leaves stale `+gfm-*` filenames coupled to
  fresh `gfm-pretty-*` requires; the half-state would not byte-compile.

### Decision: `gfm-present` declares load-order dep only on `gfm-pretty`

`modules/presentation/lib.el` has zero code references to any `gfm-*`
symbol today. The dep is conceptual (presentation operates on
GFM-decorated buffers). For this change, `lisp/gfm/gfm-present.el`
declares `(require 'gfm-pretty)` at the top of the file as a
load-order dep. No call-sites change.

If a second consumer of the narrowed-source renderer
(`presentation/spec.md` §"Reusable narrowed-source renderer") or focus
highlighting emerges, hoist the primitive into `gfm-pretty` at that
point — not speculatively.

**Alternatives considered:**

- _Hoist narrowed-source renderer + focus-face into `gfm-pretty` now._
  Rejected. Widens this change's blast radius without producing a
  second caller. Confirmed with user.
- _No dep at all._ Rejected. Both libs live in `lisp/gfm/` as a
  family; `gfm-present` makes more sense alongside `gfm-pretty` than
  alone. The explicit `(require 'gfm-pretty)` documents that intent
  and means a future caller of `gfm-present-mode` outside `gfm-mode`
  contexts gets a deterministic load order.

### Decision: spec-conventions axis rule reworded for `lisp/` libraries

`spec-conventions/spec.md` Requirement "One spec per axis; spec name
matches module name" currently reads as if every axis corresponds to
a `modules/<name>/` directory. The "where a corresponding module
exists" clause already permits exceptions, but the recognised-axes
list and example all reference modules, making the lib/composition
split implicit.

This change reworders the requirement to make the split explicit: an
axis SHALL correspond to either a `lisp/<lib>/` library directory OR
a `modules/<name>/` composition directory. The spec directory name
SHALL equal that lib or module directory name. The recognised-axes
list is updated to:

- mark each axis as **(lib)** or **(module)**,
- replace `presentation` with `gfm-present (lib)`,
- add `gfm-pretty (lib)`,
- narrow `lang-markdown (module)` description.

This delta lands in the same change so the axis-validity check stays
consistent at merge time. The change therefore touches the
`contributor-internals` axis via the spec-conventions delta. Pure
metadata; no behaviour change.

**Alternatives considered:**

- _Leave the spec rule unchanged and rely on the "where a
  corresponding module exists" carve-out._ Rejected. The carve-out is
  buried; future contributors reading the rule top-down would assume
  modules-only and miss the lib placement.
- _Convention change in a separate proposal._ Rejected per user
  direction (Option A): the placement decision is part of the
  unification rationale and belongs alongside it.

## Risks / Trade-offs

[**Migration size ~6400 LOC in one change**] → Two-pass execution (above)
localises failures to one pass. Tests relocate verbatim before any code
edit; pass 1 proves the relocation alone holds the narrowing-regression
suite.

[**Narrowing-regression suite is load-bearing**
(`modules/lang-markdown/tests.el :tags '(narrowing-regression)`)] →
Suite relocates to `lisp/gfm/gfm-pretty-tests.el` byte-identical
except for `require` lines. Pass 1 runs it; pass 2 runs it; archive
runs it.

[**External callers of `gfm-<name>-mode` would break**] → None exist
outside this config. Only known internal caller (`leader/init.el`)
migrates in this same change.

[**Spec size: `gfm-pretty/spec.md` will be ~2300 lines**] → Inherent.
The per-decorator narrowing-safety, scoped-rebuild, per-window-sizing,
and reveal-protocol contracts each require dedicated requirements
with scenarios. Internal organisation (## Requirements grouped per
decorator, then engine-level cross-cutting) keeps it navigable.

[**Module / lib load order**] → `lang-markdown/init.el`'s
`gfm-mode-hook` does `(require 'gfm-pretty)` then enables
`gfm-pretty-mode`, so `gfm-pretty` autoloads on first markdown buffer
without explicit module ordering. `leader/init.el` references
`gfm-pretty-block-at-point` / `gfm-pretty-edit-block-at-point` — both
autoloaded from `lisp/gfm/`; no module ordering required.
`gfm-present` `(require 'gfm-pretty)` at its top covers its own
ordering. tasks.md verifies the autoload generator scans `lisp/gfm/`.

[**`apheleia` formatter wiring in `lang-markdown/init.el` references
`gfm-mode-local-vars`**] → Stays in `lang-markdown` (composition).
The `gfm-mode-hook` → `gfm-pretty-mode` registration adds alongside;
no ordering coupling.

[**`markdown-code-lang-modes` is consumed by both layers** (markdown
native fontification AND `gfm-pretty`'s code-fence icon lookup)] →
Keep the `defcustom` value in `lang-markdown` (it is upstream
`markdown-mode`'s variable; just our table); `gfm-pretty` reads it
at runtime. No relocation.

[**`+theme-changed-hook` wire-up for callout body faces**] → Moves to
callouts decorator's `:on-enable` / `:on-disable`. Behaviour
identical; hook adds/removes scope to mode lifetime instead of init
time. Net win — removes a global hook installed at load time.

[**Byte-compile order**] → `gfm-pretty-borders.el` compiles before
the decorator files (which `(require 'gfm-pretty-borders)`).
`gfm-pretty.el` compiles before `gfm-present.el` and before
`modules/leader/init.el` consumes the public API. The byte-compile
order is determined by `(require …)` graph, not file order; tasks.md
verifies that pass 1 byte-compiles cleanly before pass 2 starts.

[**ELN cache invalidation**] → File relocations change the load path
of every `gfm-*` symbol. First post-change Emacs start re-compiles.
No permanent cost.

[**Autoload generation**] → `+autoloads.el` must surface
`gfm-pretty-mode`, `gfm-pretty-block-at-point`,
`gfm-pretty-edit-block-at-point`, `gfm-pretty-toggle-decorator`,
`gfm-pretty-define-decorator`, `gfm-present-mode`,
`gfm-present-markdown`, and the decorator-toggle entry points. tasks.md
includes a verification step (start Emacs without explicit `require`,
trigger each entry point).
