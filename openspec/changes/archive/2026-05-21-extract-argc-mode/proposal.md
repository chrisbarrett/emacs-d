## Why

`argc-mode` is a self-contained minor mode (~344 LOC) that fontifies
[argc](https://github.com/sigoden/argc) CLI directives in shell-script
comments and draws box overlays around directive blocks. It lives at
`modules/lang-shscript/lib/+argc.el` but has no real ties to the
`lang-shscript` module beyond also targeting shell buffers — its
faces, regexes, overlay logic, and the minor mode itself form a
cohesive library. The mode mirrors `bats-mode`'s shape almost
exactly (self-contained derived major mode for one file family), and
`bats-mode` already lives at `lisp/bats-mode/bats-mode.el`.

Activation glue (`+argc-maybe-enable` + the `sh-mode-hook` /
`bash-ts-mode-hook` registrations at
`modules/lang-shscript/init.el:23-40`) is genuine composition and
belongs in the module. Extracting the library separates those
concerns cleanly and unblocks giving `argc-mode` its own spec axis.

The argc tests are currently interleaved in
`modules/lang-shscript/tests.el` (~50 of the ~80 tests in that file).
Moving them to `lisp/argc-mode/argc-mode-tests.el` alongside the
library makes the lang-shscript test file an order of magnitude
clearer.

## What Changes

- Relocate the library to `lisp/argc-mode/argc-mode.el`. Provide
  `argc-mode` (matching the file base name; the minor mode entry
  point `argc-mode` is unchanged). Move all `defface`, `defconst`,
  `defvar`, `defun`, and the `define-minor-mode` form into the new
  file.
- Delete `modules/lang-shscript/lib/+argc.el` and its
  `(provide '+argc)` line.
- Leave `+argc-maybe-enable` and the two `add-hook` calls in
  `modules/lang-shscript/init.el` — they are composition concerns.
  Drop the `;;; argc-mode — fontify argc directives` divider comment
  if the moved-out code makes the section heading misleading.
- Refresh `lisp/+autoloads.el` so the autoload entry at line 275 for
  `argc-mode` points at the new path.
- Split tests:
  - Move every `argc-test-*` test that exercises the library
    (face/box/overlay/mode toggling) from
    `modules/lang-shscript/tests.el` into
    `lisp/argc-mode/argc-mode-tests.el`. Update the load form to
    `(require 'argc-mode)` instead of the current
    `(load (expand-file-name "lib/+argc.el" …))`.
  - Keep the `+argc-maybe-enable` tests
    (`argc-test-maybe-enable-no-double`,
    `argc-test-maybe-enable-no-directives`,
    `argc-test-maybe-enable-beyond-50-lines`,
    `argc-test-maybe-enable-within-50-lines`,
    `argc-test-maybe-enable-skip-indirect`) in
    `modules/lang-shscript/tests.el` since the function under test
    lives in that module.
- New spec axis `argc-mode` documenting library behaviour:
  activation as a minor mode, the seven faces, the face-rule
  cascade for directive fontification, box overlay rendering,
  spell-fu advice, indirect-buffer skip, and debounced rebuild.
- `spec-conventions` recognised-axes list gains an
  `argc-mode (lib)` entry.

## Capabilities

### New Capabilities
- `argc-mode`: behaviour-facing spec for the `argc-mode` library —
  minor-mode lifecycle (enable/disable idempotence, overlay add/
  remove, hook attachment), the seven argc faces and what tokens
  they cover, directive-block detection and box rendering (min
  width 80, expand for long lines, function-name label on top
  border), spell-fu advice for skipping directive lines, and
  indirect-buffer skip in the rebuild scheduler.

### Modified Capabilities
- `spec-conventions`: recognised-axes list gains `argc-mode (lib)`.

## Impact

- New files: `lisp/argc-mode/argc-mode.el`,
  `lisp/argc-mode/argc-mode-tests.el`.
- Deleted file: `modules/lang-shscript/lib/+argc.el`.
- Edited files:
  - `modules/lang-shscript/tests.el` — argc-library tests removed;
    `+argc-maybe-enable` tests remain.
  - `lisp/+autoloads.el` — regenerated entries point at the new
    path.
- No changes to activation behaviour for users: the same
  `+argc-maybe-enable` hook still gates entry, and the autoload
  cookie continues to keep `argc-mode` available without an
  explicit `require`.
- No external package additions.
