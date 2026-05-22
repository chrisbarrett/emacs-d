## Context

`gfm-pretty-links--decorate-link` (lisp/gfm/gfm-pretty-links.el:488)
runs after `:collect` returns link records. Records carry the
resolved URL string. Classification
(`gfm-pretty-links--classify-url`, gfm-pretty-links.el:115-124) is a
pure function of that string and returns one of `web`, `anchor`,
`file`. There is no fourth class for "do not decorate".

The closest precedent for an *exclusion* is the code-region skip
(gfm-pretty/spec.md:1590-1632), implemented by
`gfm-pretty-links--in-code-p` (gfm-pretty-links.el:242-249) and
consulted inside `gfm-pretty-links--blocks-in-range` so excluded
records are dropped before overlay construction.

Two URL forms collide with `gfm-present-mode`:

- **Source-range:** `path#L<n>` or `path#L<n>-L<n>`. Examples:
  `/repo/foo.yml#L13-L22`, `./scripts/x.sh#L5`. Already parsed by
  `gfm-present--source-link-rx` (gfm-present.el:271-276).
- **Diff:** `diff:<base>...<head>[#<path>]`. Already parsed by
  `gfm-present--parse-diff-link` (gfm-present.el:388-401).

## Goals / Non-Goals

**Goals:**

- Pretty-links produces zero overlays for links whose URL matches
  either form, in any link kind (inline, reference, autolink, bare,
  shortcut, wiki).
- Detection is a pure function of the URL string — same as
  classification.
- Other consumers (gfm-present) become the de-facto owners of these
  URL forms without explicit handoff or coordination.

**Non-Goals:**

- Adding a public protocol for external modules to register
  additional skip predicates. Two URL forms, two regexes — protocol
  premature.
- Changing the existing three classes (`web`, `anchor`, `file`).
  These are still applied to non-skipped links.
- Touching RET dispatch in pretty-links. The skip removes the
  overlay keymap entirely; no follow-handler change needed.

## Decisions

### Decision: predicate filter, mirror code-region exclusion

Add `gfm-pretty-links--skip-url-p` taking the URL string. Returns
non-nil when URL matches source-range or diff regex. The
`gfm-pretty-links--blocks-in-range` (gfm-pretty-links.el:427-453)
pass already drops records via `unless (or …)`; extend the `or` arm
to include `(gfm-pretty-links--skip-url-p (record-url ...))`.

Alternatives considered:

- **Fourth class `skip` from `classify-url`.** Rejected: the
  classification concept exists to drive face/icon/RET selection
  for *decorated* links. Adding a non-decorating class muddles its
  contract.
- **Per-scan-helper early-return.** Rejected: each of the five scan
  helpers (`scan-inline`, `scan-reference`, `scan-autolinks`,
  `scan-bare-urls`, `scan-shortcut`) would need the check. Single
  filter pass keeps it DRY.
- **Public hook for skip predicates.** Rejected (Non-Goals): two
  forms, no other consumers in sight.

### Decision: regex sourced from gfm-present, not duplicated

Pretty-links does not depend on gfm-present. Duplicating the
regexes risks drift. Two options:

1. Define the regexes in pretty-links (source of truth lives here).
   Gfm-present continues to use its own parsers; both modules agree
   on what these URL shapes look like as a matter of code-level
   convention.
2. Promote the regexes to a shared elisp library (`gfm-link-shapes`?)
   that both modules depend on.

Pick **1** for this change. Two regexes, well-known shapes, low drift
risk. Option 2 becomes worthwhile only if a third consumer appears.

### Decision: skip applies regardless of link kind

Inline links, reference links (resolved to one of these URLs),
autolinks, bare URLs, and shortcut references all carry a `:url`
slot post-resolution. The filter is class-of-URL not class-of-kind
— check uniformly.

## Risks / Trade-offs

- **Risk:** A user writes a regular file link that happens to end in
  `#L<digits>` (anchor convention collision).
  → Mitigation: such URLs are vanishingly rare in markdown; the
  `#L<n>` pattern is overwhelmingly source-range semantics. Worst-
  case impact is loss of pretty-links decoration on those links —
  markdown-mode's raw rendering takes over. Acceptable.

- **Risk:** A `diff:` URL appears outside a presentation buffer
  where gfm-present is not active. Skipping leaves it undecorated
  but pretty-links never knew how to render it anyway (no `diff:`
  icon). Net effect: same raw text as before, just without
  pretty-links' `file`-face misclassification.
  → No mitigation needed; this is strictly better than current
  behaviour.

- **Trade-off:** Pretty-links gains two regexes it cannot meaningfully
  test in isolation (no observable behaviour change without
  gfm-present rendering its own preview). Tests assert
  "no pretty-links overlay produced" — sufficient to lock the
  contract.
