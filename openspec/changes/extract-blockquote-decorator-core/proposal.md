## Why

The callouts (`gfm-pretty-callouts.el`, ~1019 lines) and blockquotes
(`gfm-pretty-blockquotes.el`, ~346 lines) decorators implement the same
quote-block machinery twice: ten identically-named private functions
(`find-blocks`, `collect-blocks`, `apply-block`, `apply-block-anchors`,
`apply-block-display`, `block-visible-p`, `full-rebuild-required-p`,
`marker-line-ranges`, `on-enable`, `on-disable`) differ only in marker
regex, faces, and rendering style. Every discovery, rebuild-policy, or
narrowing bug must be found and fixed twice, and the
narrowing-regression suite duplicates its coverage across both.

## What Changes

- **Shared quote-block core**: a new file under `lisp/gfm/` owns the
  width-independent machinery common to both decorators — marker-line
  scanning/discovery, block collection, anchor-overlay application,
  full-rebuild policy for structural marker-line edits, edit-adjacency
  gating, and enable/disable lifecycle — parameterised per decorator by
  marker pattern, faces, and overlay keys.
- **Callouts and blockquotes become thin adapters**: each keeps only its
  decorator registration, its marker parameters, and its
  decorator-specific rendering (callouts: bordered box + tint font-lock;
  blockquotes: left rail + inset gutter).
- **No behaviour change**: every existing behaviour-facing requirement
  for callouts and blockquotes (discovery, rendering, reveal, narrowing
  resilience, scoped rebuild) is preserved verbatim.
- **Tests consolidate**: shared-core behaviour gains direct unit
  coverage once; decorator tests keep only adapter-specific assertions.
  The narrowing-regression suite continues to run against both
  decorators unchanged.

## Capabilities

### New Capabilities

<!-- none — this deepens an existing axis -->

### Modified Capabilities

- `gfm-pretty`: gains an internals requirement that the two
  blockquote-family decorators route discovery, anchor application,
  rebuild policy, and lifecycle through a single shared core. Existing
  behaviour-facing requirements are unchanged.

## Impact

- `lisp/gfm/` — new shared-core file; `gfm-pretty-callouts.el` and
  `gfm-pretty-blockquotes.el` shrink to parameterisation + rendering.
- `lisp/gfm/gfm-pretty-tests.el` — shared-core unit tests added;
  duplicated per-decorator machinery tests collapse.
- `modules/lang-markdown/tests.el` — narrowing-regression suite
  unchanged (guards the refactor).
- `openspec/specs/gfm-pretty/spec.md` — one ADDED internals
  requirement.
- No change to the decorator registration protocol, public
  introspection API, or any rendered output.
