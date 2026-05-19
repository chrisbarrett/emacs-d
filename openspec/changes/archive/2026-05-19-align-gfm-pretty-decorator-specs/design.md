## Context

`openspec/specs/gfm-pretty/spec.md` carries two layers of requirements:

1. **Engine layer** (umbrella mode, registration, scoped rebuild, reveal,
   per-window rendering, overlay state ownership). This layer was rewritten
   in lockstep with the `tighten-gfm-pretty-protocol` change and matches the
   current implementation (see `lisp/gfm/gfm-pretty-engine.el:153-184`).
2. **Decorator layer** (per-decorator requirements for Callouts, Fences,
   Tables, Hrule, Links + the public introspection commands + the engine
   stats + the debounced scheduler). This layer pre-dates the protocol
   tightening and still names dropped keys.

A grep across the file confirms the second layer is stale:

```
lisp/gfm/gfm-pretty-tables.el — no `:apply-anchors-fn`, no `:apply-display-fn`,
  no `:reconcile-windows-fn`, no `:block-at-point-fn`, no `:edit-at-point-fn`,
  no `:structural-line-ranges-fn`, no `:edit-adjacency-fn`,
  no `:revealable-prop`, no `:saved-display-prop`.
spec.md — every one of those keys mentioned in a requirement body.
```

The decorator implementations have moved on; the spec hasn't.

## Goals / Non-Goals

**Goals:**

- Restate every decorator-layer requirement in terms of the surviving
  protocol keys (`:apply-block-fn`, `:full-rebuild-required-p`,
  `:rebuild-fn`, registry-derived prop names).
- Replace `:block-at-point` / `:edit-at-point` registry references with the
  naming-convention pair (`gfm-pretty-<name>--block-at-point` /
  `gfm-pretty-<name>--edit-at-point`).
- Where a section reaches for `:apply-anchors` + `:apply-display` as a
  protocol obligation, rewrite to describe a single `:apply-block-fn` that
  internally calls `gfm-pretty-borders--apply-with-anchors` when the
  decorator wants the anchor / display split.
- Where a section names `:structural-line-ranges` + `:edit-adjacency`,
  collapse to a single `:full-rebuild-required-p` predicate.

**Non-Goals:**

- No new requirement is added or removed; only the wording inside existing
  requirement bodies and scenarios changes.
- No code, test, or behaviour change.
- The engine-layer requirements that already match the protocol are NOT
  re-touched.
- The Callouts / Fences / Tables / Hrule / Links *behavioural* outcomes
  (what each decorator renders, what reveal exposes, etc.) stay verbatim;
  only the protocol-key names inside the rationale change.

## Decisions

### Decision 1: Edit the stable spec body in place via a delta

`openspec-customisation` requires normative wording changes to flow through
a delta spec rather than direct edits to `openspec/specs/`. Per the project
rule "Pure structural / hygiene edits ... that touch no `### Requirement:`
body go in tasks.md, not as spec deltas" — but here the edits *do* touch
requirement bodies, so a delta is the right shape. The delta will use
`## MODIFIED Requirements` blocks that reproduce each affected requirement
with the corrected wording.

**Alternatives considered:**

- *Edit `openspec/specs/gfm-pretty/spec.md` directly without a delta*.
  Rejected — bypasses the audit trail that the archive flow gives us.
- *Open one delta per section (Callouts, Fences, …)*. Rejected — they share
  the same root cause and the archive collapses to one entry anyway;
  splitting only adds churn.

### Decision 2: Keep the wording surgical, not editorial

A single change touches ~9 requirements. The temptation is to also fix
prose drift, reorder scenarios, etc. Resist that: every line touched is a
line a reviewer has to re-read. Replace dropped key names verbatim, leave
the rest alone.

**Alternatives considered:**

- *Take a broader editorial pass while we're in the file*. Rejected — see
  `simplify`-style guidance; widen scope only when the prose is actively
  misleading, not just imperfect.

## Risks / Trade-offs

- **Risk:** A delta that touches 9 requirements is large enough that the
  archive's sync step might surface a merge conflict with a parallel
  change.
  **Mitigation:** No parallel change is in flight against this spec
  (`openspec list --json` shows zero active changes after archive of
  `tighten-gfm-pretty-protocol`).
- **Risk:** The wording change inadvertently alters the *meaning* of a
  requirement (e.g. dropping a constraint a dropped key implied).
  **Mitigation:** For each rewrite, cross-check the surviving requirement
  body against the engine code that satisfies it. Specifically:
  - `:apply-anchors` + `:apply-display` → behaviour is now bundled in
    `:apply-block-fn`; the per-window contract is unchanged
    (`gfm-pretty-engine.el:608-615` iterates blocks × windows).
  - `:structural-line-ranges` + `:edit-adjacency` → both are now read by
    `:full-rebuild-required-p` (see `gfm-pretty-callouts.el:441` and
    `gfm-pretty-fences.el:665`); the OR-of-two-hooks semantics is
    preserved by the decorator composing it internally.
  - `:block-at-point` / `:edit-at-point` → the dispatch is by naming
    convention (`lisp/gfm/gfm-pretty.el:74-91`); only `tables`
    participates, matching the previous behaviour.
