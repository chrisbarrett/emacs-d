# spec-conventions Specification

## Purpose

Contributor-only. Documents the conventions for authoring specs under
`openspec/specs/`: canonical heading structure, bucket assignment, axis
granularity, the TBD ban, and the pre-merge checklist every spec-touching
change applies.

## Requirements

### Requirement: Stable specs use the canonical heading structure

A file under `openspec/specs/<capability>/spec.md` SHALL begin with a
level-one heading of the form `# <Capability> Specification`, followed by a
`## Purpose` section containing one or two sentences naming what the
capability covers, followed by a `## Requirements` section containing one or
more `### Requirement: …` blocks. Each `### Requirement: …` block SHALL
state its rules using `SHALL` or `MUST` and SHALL be followed by one or more
`#### Scenario: …` blocks giving GIVEN/WHEN/THEN bullets.

Delta-style headings — `## ADDED Requirements`, `## MODIFIED Requirements`,
`## REMOVED Requirements` — SHALL appear only in
`openspec/changes/<change>/specs/<capability>/spec.md` files. They describe
the delta a change applies and have no meaning in stable specs.

#### Scenario: Stable spec uses canonical headings

- **GIVEN** a file `openspec/specs/lang-markdown/spec.md`
- **WHEN** the file is opened
- **THEN** it begins with `# lang-markdown Specification`
- **AND** the next heading is `## Purpose` with prose, not "TBD"
- **AND** the next heading is `## Requirements`

#### Scenario: Delta headings flagged in stable specs

- **GIVEN** a stable spec under `openspec/specs/` that contains
  `## ADDED Requirements` at top level
- **WHEN** the spec is reviewed against this requirement
- **THEN** the review SHALL flag the heading as non-conforming and require
  it be replaced with the canonical structure before any further edit lands

### Requirement: One spec per axis; spec name matches module name

Every spec under `openspec/specs/` SHALL cover exactly one configuration
axis. An axis is a self-contained config domain — typically one directory
under `modules/`. The spec directory name SHALL equal the module directory
name where a corresponding module exists (e.g. `lang-markdown`, not
`gfm-rendering` or `markdown`). A spec that crosses multiple axes SHALL be
split at the axis boundary before merge; a spec that sub-divides a single
axis SHALL be folded into the axis spec.

The recognised axes are:

- **lang-markdown** — GFM rendering, callouts, code fences, tables,
  overlays, narrowing safety
- **presentation** — presentation mode, slide navigation, document narrowing
- **eglot** — LSP integration, diagnostics
- **evil** — vi emulation, keybindings
- **org** — org-mode, agenda, capture
- **vcs** — git / magit integration
- **completion** — completion-at-point, corfu, cape
- **theme** — visual theming, faces, colours
- **contributor-internals** — spec conventions, test infrastructure,
  code-quality rules; not user-observable behaviour

A new axis SHALL be justified in the spec's Purpose before merge; no silent
expansion of this list.

#### Scenario: Three GFM sub-specs fold into lang-markdown

- **GIVEN** specs `gfm-callout-rendering`, `gfm-code-fence-rendering`,
  `gfm-table-rendering` exist under `openspec/specs/`
- **WHEN** the one-spec-per-axis rule is applied
- **THEN** all three SHALL be consolidated into `lang-markdown/spec.md`
- **AND** the three directories SHALL be removed

#### Scenario: New spec for a recognised axis uses module name

- **GIVEN** a proposal adding a spec for git integration
- **WHEN** the bucket assignment is reviewed
- **THEN** the spec SHALL be filed as `openspec/specs/vcs/spec.md`
- **AND** no new axis SHALL be created

#### Scenario: New spec proposes a new axis

- **GIVEN** a proposal adding a spec whose coverage does not fit any
  recognised axis
- **WHEN** the change is opened
- **THEN** the spec's Purpose section SHALL explain why no existing axis
  covers it
- **AND** the design.md SHALL include a "Decision: new axis X" entry

### Requirement: Behaviour and internals specs do not mix scope

A spec SHALL be either *behaviour-facing* (covering observable Emacs
behaviour: minor modes, keybindings, faces, overlay rendering, window
layout, user commands) or *internals-facing* (covering implementation
invariants: overlay lifecycle, narrowing safety, rebuild correctness,
performance contracts). A spec that mixes both scopes SHALL be split at the
boundary or assigned to whichever scope dominates, with the minority concerns
demoted to implementation notes.

Internals-facing specs SHALL state their scope in Purpose
(`Internals-facing.`) and MAY use implementation vocabulary freely (overlay,
narrowing, buffer, point, marker). Behaviour-facing specs SHOULD use
user-observable terms (mode, command, face, key, buffer appearance).

The meta-spec `spec-conventions` is exempt from scope assignment.

#### Scenario: Behaviour spec covers user-observable outcome

- **GIVEN** a requirement about how GFM tables are rendered in a buffer
- **WHEN** the spec is reviewed
- **THEN** the requirement refers to faces, column widths, and overlay
  placement — not to the data structure used to track cell spans

#### Scenario: Internals spec declares its scope

- **GIVEN** a spec covering overlay rebuild correctness under narrowing
- **WHEN** the spec is opened
- **THEN** the Purpose section begins `Internals-facing.`

### Requirement: Specs meet a granularity threshold or fold into a parent

A spec under `openspec/specs/` SHOULD contain at least two requirements OR
span at least ~40 lines of substantive content (excluding heading and
Purpose). Specs below this threshold SHALL either fold into the axis spec or
document in their Purpose why standalone status is justified (typically: a
small invariant that other specs depend on by reference).

A new spec MUST cross-reference any existing spec covering related behaviour
rather than restating overlapping requirements. Silent overlap is a defect;
explicit cross-reference is correct.

#### Scenario: Thin spec folds into axis spec

- **GIVEN** a proposal adding a single-requirement spec for a minor
  rendering edge case
- **WHEN** the granularity rule is applied
- **THEN** the requirement SHALL be folded into the relevant axis spec
- **AND** no new top-level spec SHALL be created

#### Scenario: Justified thin spec stands alone

- **GIVEN** a proposal adding a 15-line spec stating a single narrowing
  invariant on which rebuild, overlay, and display all depend
- **WHEN** the granularity rule is applied
- **THEN** the spec MAY stand alone provided its Purpose explains why and
  lists the dependent specs

### Requirement: TBD is forbidden in the Purpose section

A spec's `## Purpose` section SHALL NOT contain the literal text "TBD" or
the phrase "Update Purpose after archive". Auto-archive of a change proposal
SHALL NOT create a stable spec with a TBD Purpose; the author SHALL write a
proper Purpose as part of the change before archiving.

If a Purpose cannot be written, the spec is not ready to merge.

#### Scenario: Archive blocked on TBD

- **GIVEN** a change whose stable-spec output would have
  `Purpose: TBD - created by archiving change …`
- **WHEN** archive is attempted
- **THEN** the archive SHALL be rejected until the Purpose is written

#### Scenario: Existing TBD purposes flagged

- **GIVEN** a stable spec already in tree with `TBD` in its Purpose
- **WHEN** any change touches that spec
- **THEN** the change SHALL replace the TBD with a written Purpose before
  landing

### Requirement: Pre-merge checklist applies to every spec-touching change

A change proposal under `openspec/changes/` that creates or modifies a file
under `specs/` SHALL pass the following checklist before merge.

1. Each new or modified stable spec uses canonical headings; no delta-style
   headings appear under `openspec/specs/`.
2. Each new spec covers exactly one axis; the spec directory name matches the
   corresponding module name where one exists.
3. Behaviour-facing and internals-facing concerns do not mix; internals specs
   declare `Internals-facing.` in Purpose.
4. New specs meet the granularity threshold or justify standalone status in
   Purpose.
5. No Purpose section contains `TBD`.
6. Cross-references to overlapping specs are explicit; silent restatement is
   removed.

#### Scenario: Checklist applied by agent

- **WHEN** an agent reads or edits a file under `openspec/specs/**` or
  `openspec/changes/**`
- **THEN** the agent has loaded `.claude/rules/openspec-specs.md`
- **AND** the rule directs the agent to this spec
- **AND** the agent applies the six-point checklist before completing the
  edit

#### Scenario: Checklist failure blocks merge

- **GIVEN** a change proposal that adds a stable spec with
  `## ADDED Requirements` at top level
- **WHEN** the pre-merge checklist runs
- **THEN** item (1) flags the spec
- **AND** the change SHALL NOT merge until the headings are rewritten

### Requirement: Capability renames are filesystem moves driven by tasks.md

A capability rename SHALL be applied as a filesystem move plus a
cross-reference sweep driven by the change's `tasks.md`, rather than as
paired `## ADDED Requirements` and `## REMOVED Requirements` blocks for
every moved requirement. The rename SHALL preserve every `### Requirement:`
body, every `#### Scenario:` child byte-identical except for the spec's
top-level title heading.

#### Scenario: Rename change uses tasks-driven move

- **GIVEN** a proposal renaming `gfm-callout-rendering` to `lang-markdown`
- **WHEN** the change is reviewed
- **THEN** the change SHALL drive the rename through `tasks.md` filesystem
  operations
- **AND** the change SHALL NOT contain `## ADDED Requirements` or
  `## REMOVED Requirements` blocks for the moved requirements

#### Scenario: Rename with content change separates the two intents

- **GIVEN** a proposal that renames a capability AND modifies a requirement
  body
- **WHEN** the change is reviewed
- **THEN** the body modification SHALL be expressed as a normal
  `## MODIFIED Requirements` delta in the new capability directory
- **AND** the rename portion SHALL still be driven by `tasks.md`
