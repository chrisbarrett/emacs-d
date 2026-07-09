## Why

Several module test suites assert on source text instead of behaviour:
`modules/evil/tests.el` reads `init.el` with `insert-file-contents` and
`search-forward`s for exact strings like `"(evil-mode +1)"` and
`"evil-undo-system 'undo-redo"`; `modules/org-capture/tests.el` and
`modules/vulpea/tests.el` do the same. These tests break on any
formatting or structural refactor of init files even when the resulting
Emacs state is identical — they pin the implementation, not the
interface. The interface of a config module is its post-load state
(modes enabled, variables set, hooks registered, commands bound);
that is what tests should exercise.

## What Changes

- **Convention codified**: module tests SHALL assert post-load
  observable state or invoke public commands, and SHALL NOT read a
  module's source files as text to assert on their contents.
- **Offending suites rewritten**: the source-text assertions in evil,
  org-capture, and vulpea tests are replaced with state assertions
  (e.g. `evil-mode` is enabled; `evil-undo-system` equals
  `undo-redo`; cursor variables have the configured shapes).
- **Audit of remaining suites**: the other test files using
  `search-forward` are audited; searches over test fixtures/buffers
  are fine and stay; searches over module source are rewritten.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `contributor-internals`: gains a requirement that module test suites
  exercise post-load interfaces rather than module source text.

## Impact

- `modules/evil/tests.el`, `modules/org-capture/tests.el`,
  `modules/vulpea/tests.el` — source-text assertions rewritten.
- Other `modules/*/tests.el` using `search-forward` — audited;
  rewritten only where the target is module source.
- `openspec/specs/contributor-internals/spec.md` — one ADDED
  requirement.
- No production code changes; test-only.
