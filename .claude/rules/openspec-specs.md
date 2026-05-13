---
paths:
  - "openspec/specs/**"
  - "openspec/changes/**"
---

Authority: `openspec/specs/spec-conventions/spec.md`. Read first.

Spec-touching edit checklist:

1. Stable spec uses `# X Specification` / `## Purpose` / `## Requirements` / `### Requirement: ...` / `#### Scenario: ...`. Delta headings (`## ADDED/MODIFIED/REMOVED Requirements`) only under `openspec/changes/*/specs/`.
2. Spec covers exactly one axis; directory name matches module name where a module exists. Recognised axes: lang-markdown, presentation, eglot, evil, org, vcs, completion, theme, contributor-internals. New axis → "Decision: new axis X" in design.md.
3. Behaviour-facing and internals-facing concerns do not mix. Internals specs declare `Internals-facing.` in Purpose.
4. New specs meet granularity threshold (≥2 reqs or ≥40 lines) or justify standalone in Purpose. Overlap → cross-reference, not restatement.
5. No `TBD` in Purpose. Archive-generated stubs filled before merge.

Existing violations: don't fix inline; open a hygiene change proposal citing `spec-conventions`.
