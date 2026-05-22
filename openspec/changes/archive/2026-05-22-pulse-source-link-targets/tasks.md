# Tasks

## 1. Implementation

- [x] 1.1 Add `(require 'pulsar)` (lazy-OK) to `gfm-present.el`.
- [x] 1.2 Rewrite `gfm-present--follow-source-link (path start end)` to:
  - `find-file-noselect` PATH (resolved against `default-directory`).
  - `pop-to-buffer` the result.
  - Widen, then `goto-char` line START's beginning.
  - Compute `(beg . end-pos)` covering line START..END inclusive (use
    `(line-beginning-position 1)` and `(line-end-position 1)` after
    `forward-line`).
  - Call `(pulsar-highlight-pulse (cons beg end-pos))`.
- [x] 1.3 Delete `gfm-present--render-narrowed-source`,
  `gfm-present--cleanup-source-render`,
  `gfm-present--source-overlays`,
  `gfm-present--source-restorer`,
  `defface gfm-present-focus-face`.
- [x] 1.4 Search for any remaining callers of the deleted symbols and
  the focus face; remove or update.

## 2. Tests

- [x] 2.1 Delete the `§13 Detached narrowed-source renderer` test
  block in `gfm-present-tests.el`.
- [x] 2.2 Delete `gfm-present/focus-face-defined`,
  `gfm-present/focus-face-no-extend`, and any
  `--source-overlays` / `--source-restorer` assertions.
- [x] 2.3 Rewrite `§9 Click escape: source-range link` tests:
  - `source-link-click-pushes-mark` keeps assertion (mark pushed).
  - Replace `opens-narrowed-readonly` with
    `opens-widened-at-start-line`: file opened, point at line START
    beginning, NOT narrowed, NOT read-only.
  - Replace `applies-focus-overlay` with
    `pulses-requested-range`: stub `pulsar-highlight-pulse` to
    capture LOCUS; assert (cons line-START-bol line-END-eol).
  - Drop `installs-kill-hook` (no kill-hook any more).
  - Drop `leaves-presentation-mode-off` is fine to keep.
- [x] 2.4 Remove `lib-el-exports-public-symbols` entry for
  `gfm-present-focus-face`.

## 3. Specs

- [x] 3.1 Apply the delta from
  `openspec/changes/pulse-source-link-targets/specs/gfm-present/spec.md`
  to the stable spec via `openspec sync pulse-source-link-targets`
  once tests are green.

## 4. Verification

- [x] 4.1 Run `make test-quick` and confirm `gfm-present-tests` is
  fully green.
- [x] 4.2 Smoke test: open a presentation, click a `path#L` link,
  observe pulse + no narrowing + no read-only.
