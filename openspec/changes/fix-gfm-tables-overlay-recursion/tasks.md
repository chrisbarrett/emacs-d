## 1. Failing regression tests

- [ ] 1.1 Add a `gfm-pretty-tests` ert test that enables `gfm-pretty-mode`
      on a small fixture with one 4-column table, runs the tables
      decorator's rebuild path twice without an intervening edit, and
      asserts the second pass's per-row `display` strings equal the
      first pass's character-for-character. Verify it fails on `main`.
- [ ] 1.2 Add a second ert test, tagged `narrowing-regression`, that
      drives `(enable → narrow-to-region-containing-table → run
      `gfm-pretty-tables--reconcile-windows` → widen → rebuild)` and
      asserts the final overlay set matches a fresh widened rebuild.
      Verify it fails on `main`.

## 2. Fix

- [ ] 2.1 In `lisp/gfm/gfm-pretty-tables.el`, inside
      `gfm-pretty-tables--transcribe-source-overlays` (around line 111),
      skip overlays carrying `gfm-pretty-tables-display` so the
      parser ignores its own prior render.
- [ ] 2.2 Confirm both new ert tests now pass.

## 3. Manual verification

- [ ] 3.1 Open
      `/private/tmp/presentations/2026-05-21T22-07-drift-detection-onboarding.md`,
      enable `gfm-present-mode`, page to the "Scheduled scopes" slide,
      and confirm the table renders cleanly (4 columns, no nested
      pipes, no `│ │ │ │ │ ` prefix).
- [ ] 3.2 Page back and forth across the slide several times to confirm
      repeated `--reconcile-windows` invocations do not corrupt the
      overlay set.

## 4. Test suite

- [ ] 4.1 Run `make test-quick` and confirm pass.
- [ ] 4.2 Run `make test` and confirm pass (covers the
      `narrowing-regression` suite end-to-end).
