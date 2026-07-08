# gfm-pretty Delta

## ADDED Requirements

### Requirement: Shared quote-block decorator core

The callouts and blockquotes decorators SHALL route their
width-independent machinery through a single shared quote-block core in
`lisp/gfm/`: marker-line discovery, block collection, anchor-overlay
application, full-rebuild policy for structural marker-line edits,
edit-adjacency gating, and enable/disable lifecycle. Each decorator
SHALL supply only its parameters (marker pattern, faces, overlay keys)
and its decorator-specific rendering (callouts: bordered box and tint
font-lock; blockquotes: left rail and inset gutter).

The core SHALL NOT be a registered decorator itself; exactly the two
existing decorators (`callouts`, `blockquotes`) remain registered via
`gfm-pretty-define-decorator`, and their public toggles, faces, and
rendered output SHALL be unchanged by the consolidation.

#### Scenario: discovery is a single implementation

- **WHEN** the callouts and blockquotes decorators are both enabled in
  a buffer
- **THEN** both decorators' block discovery executes the shared core's
  scan function, each invocation parameterised by that decorator's
  marker pattern
- **AND** neither decorator file defines its own private scan loop

#### Scenario: rebuild policy is shared

- **GIVEN** a buffer with a callout block and a plain blockquote block
- **WHEN** a structural marker line of either block is edited
- **THEN** the full-rebuild-required decision for both decorators is
  computed by the same shared-core policy function

#### Scenario: consolidation preserves rendered behaviour

- **GIVEN** the narrowing-regression suite in
  `modules/lang-markdown/tests.el`
- **WHEN** the suite runs against the consolidated decorators
- **THEN** every existing callout and blockquote scenario passes
  without modification to the test expectations
