# Design: extract-blockquote-decorator-core

## Context

`gfm-pretty-callouts.el` and `gfm-pretty-blockquotes.el` grew in
parallel from the same shape: scan for marker lines, group into blocks,
lay width-independent anchor overlays, decide when an edit forces a
full rebuild, and hook enable/disable lifecycle. Ten private functions
share names and structure across the two files; only marker regexes,
faces, and the display path differ. The display paths genuinely
diverge — callouts render a bordered box via `gfm-pretty-borders` plus
tint font-lock; blockquotes render a left rail with an inset gutter —
so the duplication is confined to the width-independent machinery.

Constraints:

- The decorator registration protocol
  (`gfm-pretty-define-decorator`) is spec'd and must not change.
- The narrowing-regression suite in `modules/lang-markdown/tests.el`
  pins overlay convergence across `narrow → rebuild → widen → rebuild`
  for both decorators and must pass unmodified.
- All behaviour-facing requirements in `openspec/specs/gfm-pretty/`
  (discovery, rendering, reveal, scoped rebuild, narrowing resilience)
  stay verbatim.

## Goals / Non-Goals

**Goals**

- One implementation of quote-block discovery, collection, anchor
  application, rebuild policy, adjacency gating, and lifecycle.
- Callouts and blockquotes reduced to parameters + rendering.
- Direct unit coverage of the shared core, once.

**Non-Goals**

- Unifying the display paths (box vs rail) — they are different by
  design.
- Touching the other decorators (fences, tables, links, hrule).
- Changing any rendered output, face, toggle, or public API.
- Generalising the core for hypothetical third quote-like decorators;
  two real adapters define the seam.

## Decisions

### Decision: new file `gfm-pretty-quote-base.el`

The core lives in a new `lisp/gfm/gfm-pretty-quote-base.el`, sibling to
`gfm-pretty-borders.el`. Folding it into the engine was rejected: the
engine is decorator-agnostic and already 1140 lines; quote-block
semantics (marker lines, `>` prefixes) are decorator-family knowledge,
not engine knowledge.

### Decision: parameterisation via a `cl-defstruct` spec object

Each decorator constructs a `gfm-pretty-quote-spec` struct (marker
regex, block-qualifier predicate, face symbols, overlay-key prefix,
render function) at load time and passes it to core functions. A struct
over a plist: slot access is compile-checked and mistyped keys fail
loudly. The render function slot is the seam between shared machinery
and decorator-specific display — the core calls it per (block, window);
callouts passes its box renderer, blockquotes its rail renderer.

### Decision: extraction order is discovery → policy → anchors → lifecycle

Incremental extraction, one function family at a time, keeping both
decorators green after each step. Discovery first because it is pure
(no overlays) and has the strongest existing test signal; lifecycle
last because it composes the rest.

### Decision: callouts font-lock stays in `gfm-pretty-callouts.el`

The ~300 lines of tint font-lock (including
`+theme-changed-hook`-driven face refresh) are callout-only and remain
untouched. The core knows nothing about font-lock.

## Risks / Trade-offs

- [Narrowing edge cases regress during extraction] → run
  `:tags '(narrowing-regression)` after every extraction step; the
  suite predates this change and encodes the known failure modes
  (`args-out-of-range`, zombie overlays).
- [Subtle behavioural drift between the two decorators is actually
  load-bearing] → diff the paired functions before merging each into
  the core; any intentional divergence becomes a spec-struct parameter,
  not a silent unification.
- [Struct indirection costs on hot rebuild paths] → discovery is
  already memoised at the engine level; slot access is an aref. Accept.

## Migration Plan

Pure internal refactor within `lisp/gfm/`; no user-visible migration.
Rollback is reverting the commits — the decorator files remain valid
adapters at every extraction step.

## Open Questions

- Whether `region-adjacent-to-{callout,block}-p` (differently named,
  near-identical bodies) unify cleanly or hide an intentional
  difference in adjacency semantics — resolve by diffing during the
  policy extraction step.
