# Tasks: extract-blockquote-decorator-core

## 1. Baseline

- [ ] 1.1 Run the full gfm test set plus the narrowing-regression suite
      (`:tags '(narrowing-regression)`) and record green baseline
- [ ] 1.2 Diff the ten paired private functions between
      `gfm-pretty-callouts.el` and `gfm-pretty-blockquotes.el`; note
      every intentional divergence as a future spec-struct parameter

## 2. Core scaffold

- [ ] 2.1 Write failing unit tests for `gfm-pretty-quote-spec` struct
      construction and validation
- [ ] 2.2 Create `lisp/gfm/gfm-pretty-quote-base.el` with the
      `cl-defstruct` and package boilerplate; tests green

## 3. Extract discovery

- [ ] 3.1 Write failing tests for shared marker-line scan +
      block collection parameterised by marker regex (callout and
      blockquote fixtures)
- [ ] 3.2 Implement core discovery/collection; port callouts to it;
      gfm tests + narrowing suite green
- [ ] 3.3 Port blockquotes discovery/collection to the core; delete
      both private scan loops; suites green

## 4. Extract rebuild policy and adjacency gating

- [ ] 4.1 Write failing tests for shared `full-rebuild-required-p` and
      edit-adjacency gating over both marker styles
- [ ] 4.2 Implement in core; port both decorators; resolve the
      `region-adjacent-to-*-p` divergence (parameter or unify); suites
      green

## 5. Extract anchors and lifecycle

- [ ] 5.1 Write failing tests for shared anchor-overlay application
      (width-independent overlays keyed per decorator)
- [ ] 5.2 Implement in core; port both decorators' `apply-block-anchors`
      and `apply-block` dispatch; render fns stay decorator-local
- [ ] 5.3 Move `on-enable`/`on-disable` lifecycle into core helpers;
      callouts keeps its font-lock/theme-refresh additions locally

## 6. Consolidate tests and close out

- [ ] 6.1 Collapse duplicated per-decorator machinery tests in
      `lisp/gfm/gfm-pretty-tests.el`; keep adapter-specific assertions
- [ ] 6.2 Full run: `make test` including narrowing-regression suite
- [ ] 6.3 Byte-compile clean (`make build-affected`) and checkdoc clean
