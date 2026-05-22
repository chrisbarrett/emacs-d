## 1. Fix the leak

- [x] 1.1 Update `early-init.el` `+sync-frame-parameters`: read `bg`
      from `+theme-default-background` (with `boundp` guard) and skip
      the `set-face-foreground` block entirely when `bg` is missing
      or starts with `"unspecified"`.
- [x] 1.2 Reset the four currently-poisoned faces (`fringe`,
      `window-divider`, `window-divider-first-pixel`,
      `window-divider-last-pixel`) so existing live sessions converge
      on the new value at next theme change.

## 2. Regression test

- [x] 2.1 Add an ERT test under `modules/tty/tests.el` that runs the
      `+sync-frame-parameters` after-init/theme-change block and
      asserts no face in `(face-list)` has `:foreground
      "unspecified-bg"` or `:background "unspecified-fg"` (cross-
      stamp hazard).
- [x] 2.2 (Optional, if feasible) add a dual-frame integration test
      that opens a sandbox daemon, attaches both a tty client and an
      NS frame, pulses on the tty side, and asserts `*Messages*`
      contains no `Unable to load color` line. **Skipped:** dual-
      frame setup (live tty client + live NS frame in one daemon) is
      not reproducible from `make test` (batch emacs). The
      producer-side assertion in 2.1 is strictly stronger — it
      forbids the source of the leak rather than the symptom.

## 3. Verify

- [x] 3.1 Run `make test` (full suite, includes the new tty test).
      821/821 passed; byte-compile and checkdoc clean.
- [x] 3.2 Manually: in the live daemon, switch between `*Messages*`
      and a markdown buffer in a tty frame; confirm no new
      `Unable to load color` line lands in `*Messages*`.
      Verified during diagnosis: post-fix `(pulsar-pulse-line)` on
      the tty frame produced an empty `*Messages*` delta.

## 4. Archive

- [ ] 4.1 Run `/opsx:archive` to fold the delta spec into
      `openspec/specs/tty/spec.md` and move this change directory to
      `openspec/changes/archive/`.
