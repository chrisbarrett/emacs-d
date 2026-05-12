## 1. Sandbox repro

- [x] 1.1 Add a `make test-sandbox-narrowing` recipe (or document the manual invocation in `AGENTS.md`) that spins up `emacs -Q --bg-daemon=claude-sandbox`, loads the three gfm libs against the user's `markdown-mode`, and runs the three repro forms below.  Expected failures are recorded in the change proposal (`fix-gfm-narrowing-safety/proposal.md`) — keep this as a living document of pre-fix state.
- [x] 1.2 Capture each pre-fix repro in a sibling ERT test (skipped via `ert-deftest :tags '(narrowing-regression)` so they're not part of `make test-quick`) — these will go green as part of the fix and replace 1.1.

## 2. Shared teardown fix

- [x] 2.1 Write a failing ERT test in `modules/lang-markdown/tests.el`: enable any gfm mode in a buffer, narrow, call `gfm-block-borders--remove-overlays REGISTRY` with no `BEG`/`END`, widen, assert that no overlay tagged with the registry's tag remains.
- [x] 2.2 Wrap the full-clear branch of `gfm-block-borders--remove-overlays` in `(save-restriction (widen) …)`.  Scoped branch unchanged.
- [x] 2.3 Run the test from 2.1; confirm green.

## 3. `gfm-tables` narrowing-safety

- [x] 3.1 Write a failing ERT: two-slide buffer with tables in each slide; enable `gfm-tables-mode` (widened rebuild); narrow to slide 1; call `gfm-tables--rebuild`; assert no signal.
- [x] 3.2 Wrap the body of `gfm-tables--find-blocks-1` in `(save-restriction (widen) …)` around the existing `save-excursion` / `save-match-data`.  Confirm 3.1 passes.
- [x] 3.3 Replace `gfm-tables--remove-overlays`'s body with a call into `gfm-block-borders--remove-overlays gfm-tables--registry BEG END`.  If a `gfm-tables--registry` does not yet exist, define one analogous to `gfm-callouts--registry` / `gfm-code-fences--registry`.  Update existing call sites (`gfm-tables--remove-overlays-in-block`, the `gfm-tables-mode` teardown branch, etc.) to pass through unchanged.
- [x] 3.4 Write a failing ERT: two-slide buffer; enable mode; narrow; rebuild; widen; assert `(length gfm-tables--overlays) == (count-tagged-on-buffer)`.  Make it pass with 3.3.
- [x] 3.5 Run `make test`.  Resolve any regression in the existing tables suite.

## 4. `gfm-code-fences` narrowing-safety

- [x] 4.1 Write failing ERTs mirroring 3.1 and 3.4 for fences.
- [x] 4.2 Wrap the bodies of `gfm-code-fences--find-blocks-1`, `gfm-code-fences--find-yaml-helmet-1`, and `gfm-code-fences--find-indent-blocks-1` in `(save-restriction (widen) …)`.
- [x] 4.3 Confirm 4.1 passes; run `make test`.

## 5. `gfm-callouts` narrowing-safety

- [x] 5.1 Write failing ERTs mirroring 3.1 and 3.4 for callouts.
- [x] 5.2 Wrap the body of `gfm-callouts--find-blocks-1` in `(save-restriction (widen) …)`.
- [x] 5.3 Confirm 5.1 passes; run `make test`.

## 6. Cross-narrow partial-block coverage

- [x] 6.1 Write three ERTs (one per mode): a block whose opener sits outside the slide's narrowing.  Assert that post-`widen`, a fresh widened rebuild produces an overlay set identical (by source positions and tag counts) to "narrow → narrowed-rebuild → widen → rebuild".  This is the regression net for any future narrowing-scoped optimisation that creeps back in.
- [x] 6.2 Confirm green on the post-fix tree.

## 7. Spec deltas

- [x] 7.1 Cross-check the three `specs/<cap>/spec.md` deltas in this change against the requirements they reference in `openspec/specs/<cap>/spec.md`; ensure exact heading text matches.
- [x] 7.2 Run `openspec validate fix-gfm-narrowing-safety` (if the validator is configured).

## 8. Smoke

- [x] 8.1 Open a real presentation document.  Enter `+presentation-mode`, navigate forward and back through slides containing each block kind, save the file (triggers `after-change`), split-window the buffer, resize.  Confirm: no errors, no doubled borders, no missing rendering on widen.
- [x] 8.2 Outside `+presentation-mode`, open a markdown file, `markdown-narrow-to-subtree`, navigate, edit, widen.  Same checks.
