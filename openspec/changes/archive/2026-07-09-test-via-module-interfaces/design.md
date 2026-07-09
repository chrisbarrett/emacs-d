# Design: test-via-module-interfaces

## Context

The interface of a config module is its post-load state; tests that
`insert-file-contents` a module's init.el and `search-forward` for
exact source strings pin the implementation instead. Confirmed
offenders: `modules/evil/tests.el` (mode enablement, undo system,
cursor shapes, hook registration all asserted as source substrings),
`modules/org-capture/tests.el`, `modules/vulpea/tests.el`. Eleven test
files use `search-forward` overall; most hits are legitimate searches
over test buffers/fixtures and must not be churned.

## Goals / Non-Goals

**Goals**

- Source-text assertions replaced by state assertions with equivalent
  or better coverage.
- The convention recorded as a requirement so new suites don't regress.

**Non-Goals**

- Rewriting healthy tests that search fixture buffers.
- Restructuring the gfm library test suites' internal-state pokes —
  that is a library-shape question (missing public engine queries),
  out of scope here and better addressed alongside gfm API changes.
- Adding new test coverage beyond what the rewritten assertions
  replace.

## Decisions

### Decision: the convention lands in `contributor-internals`

The axis covers contributor-facing tooling and the "what passes/fails
before merge" contract; test-authoring conventions gate commits the
same way. Its Purpose is widened accordingly at sync/archive time.
Alternative — `spec-conventions` — rejected: that meta-spec governs
spec documents, not test code.

### Decision: assert state, not load-time side effects replay

Rewrites load the module once (as the suites already do) and assert:
`evil-mode` non-nil; `(eq evil-undo-system 'undo-redo)`; cursor
variables equal configured shapes; hook membership via `memq` on the
hook value. Where init.el registers lazy config
(`with-eval-after-load`), tests force the load and then assert state,
rather than asserting the registration text exists.

### Decision: mechanical enforcement stays a spec scenario, not a prek hook

A dedicated pre-commit grep for `insert-file-contents` on own-module
sources is disproportionate for three historical offenders; the spec
scenario plus review suffices. Revisit if it recurs.

## Risks / Trade-offs

- [A source-string test guarded something state assertions can't see
  (e.g. a form must run at a particular phase)] → for each rewritten
  assertion, identify the observable consequence and assert that; if
  none exists, the original test asserted nothing real and is dropped
  with a note in the commit message.
- [State assertions flake because module load order differs under the
  test harness] → suites already load their module explicitly
  (`load` of init.el at top of file); keep that mechanism.

## Migration Plan

Test-only change; rewrite one suite per commit, `make test` green
after each. Rollback is a revert.

## Open Questions

- None.
