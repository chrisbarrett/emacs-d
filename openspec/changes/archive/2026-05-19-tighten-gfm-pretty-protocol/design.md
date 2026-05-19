## Context

The `gfm-pretty` engine in `lisp/gfm/gfm-pretty-engine.el` exposes a
17-keyword registration protocol (`gfm-pretty-define-decorator`) through
which five decorators (`callouts`, `fences`, `tables`, `hrule`, `links`)
plug into a shared lifecycle (one set of buffer-local hooks +
idle-timer per buffer). The protocol surface has accreted in three
overlapping ways:

- **Single-adapter slots.** `:block-at-point-fn`, `:edit-at-point-fn`,
  `:reconcile-windows-fn` are used by `tables` only (see
  `lisp/gfm/gfm-pretty-tables.el:2106–2114`). `:revealable-p-fn` is
  declared on the struct (`gfm-pretty-engine.el:120`) but no decorator
  registers it — the default property-presence branch is always taken
  (`gfm-pretty-engine.el:772–774`).
- **Duplicated overlay-tag naming.** `gfm-pretty--registry-for`
  (`gfm-pretty-engine.el:223–236`) already derives the revealable /
  saved-display property symbols from the registry's `tag`, yet every
  decorator also passes `:revealable-prop` / `:saved-display-prop`
  explicitly with the matching symbols (e.g. `callouts.el:789–790`,
  `fences.el:775–776`, `links.el:684–685`).
- **Split-home overlay state.** `gfm-pretty--state`'s docstring
  (`gfm-pretty-engine.el:86–96`) advertises `overlays` and `hidden-ovs`
  slots, but no code reads or writes them there. Overlays actually live
  in module-local `defvar-local` symbols (`*--overlays`,
  `*--hidden-ovs`) referenced by the registry's `overlays-symbol` /
  `hidden-ovs-symbol` slots.

In addition, the anchor/display split (`:apply-anchors-fn` +
`:apply-display-fn`) is enforced as a protocol obligation, but only
`callouts` (`callouts.el:783–786`) and `fences` (`fences.el:769–772`)
actually use it. `hrule` (`hrule.el:104–105`) and `links`
(`links.el:484–485`) declare no-op `:apply-anchors-fn`s; `tables`
bypasses the split entirely with its own `:rebuild-fn`. Finally, the
"does this dirty region need a full rebuild?" question is answered
through two separate hooks (`:structural-line-ranges-fn` and
`:edit-adjacency-fn`) that the engine ORs together in
`gfm-pretty--dirty-forces-full-rebuild-p`
(`gfm-pretty-engine.el:642–653`).

The recent `2026-05-19-deepen-gfm-pretty-engine` change consolidated
the lifecycle hooks (`after-change-functions`, `wcc`, `post-command`)
and the idle timer into the engine. This change is the next pass: the
*protocol surface* and *state ownership*.

## Goals / Non-Goals

**Goals:**

- Reduce `gfm-pretty-define-decorator`'s keyword count by ≥6 by
  removing slots with 0 or 1 adapters.
- Make overlay-tag property names single-sourced from the registry
  struct.
- Make `gfm-pretty--state` the sole owner of per-decorator overlay
  lists, eliminating module-local `defvar-local *--overlays` /
  `*--hidden-ovs`.
- Replace the anchor/display protocol pair with a single
  `:apply-block-fn`. Preserve the callouts/fences split as an internal
  helper in `gfm-pretty-borders.el`.
- Replace `:structural-line-ranges-fn` + `:edit-adjacency-fn` with
  one `:full-rebuild-required-p` predicate.
- Keep every behaviour-facing scenario from the existing
  `openspec/specs/gfm-pretty/spec.md` passing (visual output, reveal,
  scoped rebuild, RET-to-follow, etc.).

**Non-Goals:**

- No change to the engine's lifecycle-hook ownership (the
  consolidation from the prior change stays as-is).
- No change to the user-facing `gfm-pretty-mode` umbrella, its
  decorator-toggle command, or the public introspection commands
  (`gfm-pretty-block-at-point`, `gfm-pretty-edit-block-at-point`).
- No reshuffling of *which* decorators exist or *what they render*.
  Tables continues to do its own thing; this change does not
  attempt to fold tables into the generic rebuild path.
- No changes to `gfm-present`.

## Decisions

### Decision 1: Drop 1-adapter and 0-adapter slots from the registration protocol

`:block-at-point-fn`, `:edit-at-point-fn`, `:reconcile-windows-fn`, and
`:revealable-p-fn` come off the keyword list.

The tables-specific `block-at-point` and `edit-at-point` callbacks
move to `gfm-pretty-tables.el` and are wired through *direct
references* in `gfm-pretty.el`'s public commands rather than via the
registry. The umbrella file (`gfm-pretty.el`) already has to
`(require 'gfm-pretty-tables)` lazily via `gfm-pretty--require-all`;
the dispatch in `gfm-pretty-block-at-point` /
`gfm-pretty-edit-block-at-point` becomes "ask tables first, then any
future decorator that exposes the same pair of named functions". The
shape is a *naming convention* (each decorator that wants to
participate exports `<name>--block-at-point` /
`<name>--edit-at-point`), not a registry slot. Today only tables
participates.

`:reconcile-windows-fn` was tables-only; tables also registers its own
`:rebuild-fn`, so the engine effectively delegates the whole rebuild
+ reconcile pair to the decorator. Moving the bespoke
`reconcile-windows` callback inside tables's own setup (called by
tables's `:on-enable-fn` to add an extra `wcc` handler) preserves
behaviour without inflating the protocol.

`:revealable-p-fn` has zero adapters; the default-only branch in
`gfm-pretty--reveal-for` becomes the unconditional branch.

**Alternatives considered:**

- *Keep all slots, treat them as documentation*. Rejected — the
  struct's accessor functions are dead code for those slots, and the
  protocol's surface area is the thing future readers (and
  hypothetical new decorators) measure against. A wide protocol
  encourages over-extension at the seam.
- *Promote tables to its own separate module outside the registry*.
  Rejected as overkill — tables shares the engine's overlay registry,
  reveal walker (negative: it opts out), state plist, and stats
  wrapper. The right shrink is the protocol, not the module
  boundary.

### Decision 2: Source overlay-tag property names from the registry struct alone

`gfm-pretty--registry-for` already derives `<tag>-revealable` and
`<tag>-saved-display`. The engine's reveal walker reads the property
name from the decorator's struct (`gfm-pretty--decorator-revealable-prop`
/ `gfm-pretty--decorator-saved-display-prop`). Change: have the
engine read from
`(gfm-pretty--registry-revealable
   (gfm-pretty--decorator-registry decorator))`
instead. Drop the two slots from
`gfm-pretty-define-decorator`.

**Alternatives considered:**

- *Keep both, document one as authoritative*. Rejected — current
  duplicated state is bug-prone exactly because *neither* is named as
  authoritative.
- *Drop derivation from the registry and keep explicit slots*.
  Rejected — derivation gives a single naming convention
  (`<decorator>-revealable`, `<decorator>-saved-display`) that's
  predictable in overlay inspectors (`describe-text-properties` shows
  the same shape across every decorator).

### Decision 3: Move overlay lists into `gfm-pretty--state`

Each decorator's overlay list (`*--overlays`) and revealable-hidden
list (`*--hidden-ovs`) move into
`(gfm-pretty--state-get NAME 'overlays)` and
`(gfm-pretty--state-get NAME 'hidden-ovs)`. The registry's
`overlays-symbol` and `hidden-ovs-symbol` slots are removed; the
registry-aware primitives (`gfm-pretty--register`,
`gfm-pretty--remove-overlays`, etc.) take the decorator NAME (or the
registry) and read/write through the state alist.

**Alternatives considered:**

- *Leave the lists in module-local symbols and update the docstring*.
  Rejected — a state plist that the engine owns is the natural home;
  pulling everything into one place makes "show me everything this
  decorator is tracking" a one-line accessor.
- *Use a hash table keyed by decorator name*. Rejected — `alist-get`
  on a 5-entry alist is faster than a hash-table lookup for this
  size, and the rest of the engine already uses an alist.

### Decision 4: Collapse `:apply-anchors-fn` + `:apply-display-fn` into `:apply-block-fn (block window)`

`:apply-block-fn` is responsible for everything the previous pair did,
for one (block, window) pair. Decorators with the
anchor-once-per-block / display-per-window split (`callouts`,
`fences`) call a shared helper exported by `gfm-pretty-borders.el`:

```elisp
(gfm-pretty-borders--apply-with-anchors block window
  :registry REGISTRY
  :anchors-fn ANCHORS-FN     ; runs at most once per (block, tick)
  :display-fn DISPLAY-FN)    ; runs once per (block, window)
```

The helper tracks which (block, tick) pairs have had their anchors
laid down (via the engine's existing `blocks-cache` slot or a new
sibling slot) and skips the anchors call on subsequent windows in the
same tick. The engine's generic rebuild loop (
`gfm-pretty--rebuild` and `gfm-pretty--rebuild-blocks`) simplifies to
"for each (block, window), call `:apply-block-fn`".

`hrule`, `links`, and `tables` implement `:apply-block-fn` directly
without anchors; they already do this in spirit (their
`:apply-anchors-fn` is no-op or absent).

**Alternatives considered:**

- *Add a third `:has-anchors` boolean to the struct*. Rejected — adds
  a slot to avoid removing two; net no improvement, and the actual
  knowledge ("does this decorator share state across windows?") is
  better expressed by which helper the decorator chooses to call.
- *Make the engine introspect on the body of `:apply-block-fn`*.
  Rejected — no Emacs-Lisp introspection idiom fits, and the helper
  approach is more direct.

### Decision 5: Replace structural + adjacency hooks with `:full-rebuild-required-p (dirty)`

The engine's `gfm-pretty--dirty-forces-full-rebuild-p` becomes a
single function-call: if the decorator registered
`:full-rebuild-required-p`, call it with the dirty region;
treat its return value as the answer. The
two helpers the decorator might want (line-range intersection,
adjacency-line check) remain available as
`gfm-pretty--in-ranges-p` / `gfm-pretty--region-overlaps-p` /
new `gfm-pretty--region-adjacent-to-any-p`.

`callouts` and `fences` define their own
`:full-rebuild-required-p` that composes the same OR they used to
get implicitly. The composition moves from the engine into the
decorator that owns the reason.

**Alternatives considered:**

- *Keep both hooks, mark one as the "primary"*. Rejected — the
  engine OR-combines them; collapsing the join into the decorator
  flattens the abstraction layer without losing expressivity.
- *Push the predicate into a method-table dispatched on a "kind" tag
  like fences does for blocks*. Rejected — only two decorators need
  the slot at all; method-table is heavier than a single optional
  function pointer.

## Risks / Trade-offs

- **Risk:** Scoped-rebuild semantics for `callouts` / `fences` drift
  when `:full-rebuild-required-p` composes its inputs differently
  than the engine's prior OR.
  **Mitigation:** Existing narrowing-regression suite under
  `modules/lang-markdown/tests.el` (`:tags '(narrowing-regression)`)
  exercises the scoped-rebuild path on every decorator; preserve it.
  Add an ERT test per decorator that asserts the dirty regions which
  used to force a full rebuild (structural marker edits, indent-block
  blank-line adjacency) still do so.
- **Risk:** Moving overlay lists into the state plist breaks
  `cl-typep` / `seqp` ergonomics that current debugging code may
  rely on (e.g. inspecting `gfm-pretty-callouts--overlays` directly
  from `M-:`).
  **Mitigation:** Document the migration in the spec delta; provide
  a thin `(gfm-pretty--overlays-for NAME)` accessor for ad-hoc
  inspection. No external callers in the repo grep-positive.
- **Risk:** The shared anchors helper in `gfm-pretty-borders.el`
  introduces a cross-block-per-tick coordination problem (don't lay
  anchors twice within one rebuild pass).
  **Mitigation:** The engine's existing tick-keyed
  `blocks-cache` slot already gives a "did we hit this tick yet?"
  signal; reuse it as the anchors-laid sentinel keyed by `(name .
  block-id)`, or maintain a small set on the state plist for the
  duration of one rebuild call.
- **Trade-off:** The `block-at-point` / `edit-at-point` convention
  becomes a *naming* contract rather than a registry slot. This
  trades a typed slot for a discoverability hit (a future contributor
  has to read `gfm-pretty.el` to learn the convention). The win is
  protocol surface — and since only `tables` participates today, the
  hit is small.

## Migration Plan

1. **Engine internals**: update `gfm-pretty--state` docstring + slot
   accessors; add `'overlays` / `'hidden-ovs` slot population to the
   registry-aware primitives; remove `overlays-symbol` /
   `hidden-ovs-symbol` from the registry struct.
2. **Engine protocol**: rewrite `gfm-pretty-define-decorator` to drop
   the six removed keys; update `gfm-pretty--decorator` struct;
   update `gfm-pretty--reveal-for`,
   `gfm-pretty--dirty-forces-full-rebuild-p`,
   `gfm-pretty--rebuild`, `gfm-pretty--reconcile-windows`,
   `gfm-pretty--rebuild-blocks` to use the new accessors.
3. **Borders helper**: export
   `gfm-pretty-borders--apply-with-anchors` (or similar) from
   `gfm-pretty-borders.el`.
4. **Per-decorator updates** (callouts, fences, tables, hrule, links):
   replace `:apply-anchors-fn` + `:apply-display-fn` with
   `:apply-block-fn`; remove `:revealable-prop` /
   `:saved-display-prop` / `:revealable-p-fn`; drop module-local
   `*--overlays` / `*--hidden-ovs` `defvar-local`s; tables wires its
   own `wcc` handler from `:on-enable-fn`; callouts + fences register
   `:full-rebuild-required-p`.
5. **Tests**: update `lisp/gfm/gfm-pretty-tests.el` to assert through
   the engine's state accessors rather than module-local symbols.
   Preserve the narrowing-regression suite verbatim.
6. **Smoke test**: open a representative markdown buffer (with
   callouts, fences, indent code blocks, a YAML helmet, tables,
   hrules, links), `C-x 3` split, narrow, widen, toggle each
   decorator. No overlay leaks, no slow-rebuild warnings.

Rollback is `git revert`; no on-disk state changes.
