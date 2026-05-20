## Context

`modules/lang-shscript/lib/+argc.el` defines `argc-mode`, a minor
mode that fontifies argc directive comments (`# @cmd …`, `# @arg …`,
`# @option …`, etc.) in shell scripts and draws box overlays around
contiguous directive blocks. Surface area:

- 7 faces (`argc-directive-face`, `argc-param-name-face`,
  `argc-flag-face`, `argc-modifier-face`, `argc-notation-face`,
  `argc-choice-face`, `argc-default-value-face`, `argc-box-face`).
- A face-rule cascade (`argc--face-rules`) where each rule creates
  overlays at an increasing priority; later rules override earlier
  ones on overlapping ranges.
- Directive-block detection (`argc--find-blocks`,
  `argc--function-after`, `argc--block-max-col`) plus the box
  renderer (`argc--apply-box-overlays`,
  `argc--make-border`) which draws a Unicode box with `box-width =
  max(80, max-line + 4)` and labels the top border with the
  following function's name when one exists.
- `spell-fu` `:around` advice that skips directive lines so reason
  strings aren't underlined as misspelled.
- A debounced rebuild scheduler (`argc--schedule-rebuild`) on a
  buffer-local idle timer that skips indirect buffers.
- The `;;;###autoload`-cookied `define-minor-mode argc-mode` that
  installs/uninstalls the hook, advice, and overlays.

Activation happens via `+argc-maybe-enable` in
`modules/lang-shscript/init.el:23-40`, attached to both
`sh-mode-hook` and `bash-ts-mode-hook`. That function scans the
first 50 lines of the buffer for an argc directive comment and
turns the mode on only if it finds one.

Tests at `modules/lang-shscript/tests.el:77-507` mix two concerns:
library tests (face/overlay assertions, mode toggling) and
activation-gate tests (`+argc-maybe-enable` behaviour). The library
tests `(load …)` the file directly today; once the library moves,
they should `(require 'argc-mode)` instead.

## Goals / Non-Goals

**Goals:**

- Move `argc-mode` to `lisp/argc-mode/argc-mode.el`, mirroring
  `lisp/bats-mode/bats-mode.el`. Preserve the minor-mode API
  byte-for-byte: same `argc-mode` symbol, same faces, same
  observable behaviour.
- Keep `+argc-maybe-enable` and the mode hooks in
  `modules/lang-shscript/init.el` — those are composition glue
  (which buffer types get the mode auto-enabled is a `lang-shscript`
  concern).
- Split the test file: library tests move with the library;
  `+argc-maybe-enable` tests stay with the module that owns the
  function.
- File a behaviour-facing spec at `openspec/specs/argc-mode/spec.md`
  and add `argc-mode (lib)` to the recognised-axes list in
  `openspec/specs/spec-conventions/spec.md`.

**Non-Goals:**

- No face-set or fontification-rule changes.
- No box-rendering changes (corner glyphs, min width, label
  alignment all preserved).
- No autoload-cookie scheme changes. The mode keeps its existing
  `;;;###autoload` cookie; the harvester regenerates
  `lisp/+autoloads.el`.
- No tree-sitter rewrite. The existing regex-based face cascade
  stays as-is.
- `+argc-maybe-enable`'s heuristic (50-line scan, indirect-buffer
  skip) is unchanged.

## Decisions

### Decision: new axis `argc-mode`

The library is a standalone lib; no existing axis covers it. Per
`spec-conventions` §"One spec per axis", a new lib axis fits. Spec
name `argc-mode` matches the library directory `lisp/argc-mode/`
and the file `lisp/argc-mode/argc-mode.el`, paralleling
`bats-mode`.

Alternative considered: fold the spec into a new `lang-shscript`
module spec. Rejected: the library is not lang-shscript-specific
(it could ship to any buffer that contains argc directive
comments, e.g. a `.bats` test that documents a CLI). The activation
glue is a `lang-shscript` choice; the library itself is portable.

### Decision: file path `lisp/argc-mode/argc-mode.el`, provide name `argc-mode`

Match the `bats-mode` precedent (`lisp/bats-mode/bats-mode.el`,
`provide 'bats-mode`). The provide symbol equals the file base
name. The current `(provide '+argc)` is replaced by
`(provide 'argc-mode)`; the `+argc` symbol has no external
references outside the file itself.

Alternative considered: `lisp/argc/argc.el`. Rejected: leaves no
room for an unrelated `argc-*` library to sit beside this one if
one is ever added; `argc-mode` is the only user-facing surface so
it should be the file name.

### Decision: `+argc-maybe-enable` stays in `lang-shscript/init.el`

The activation heuristic decides *when* a shell-script buffer
should get `argc-mode` turned on. That's a composition policy
specific to which derived modes the user is willing to scan:
adding `bash-ts-mode-hook` is a `lang-shscript` decision. Moving
the function would couple the library to `sh-mode` /
`bash-ts-mode`, which it currently has no awareness of.

The companion tests (`argc-test-maybe-enable-*`) follow the
function and stay in `modules/lang-shscript/tests.el`.

Alternative considered: move `+argc-maybe-enable` into
`argc-mode.el` as `argc-mode-maybe-enable`. Rejected: bakes a
specific buffer-shape policy (50-line scan, comment syntax) into
the library and forces every consumer to opt out if they want a
different gate.

### Decision: tests live at `lisp/argc-mode/argc-mode-tests.el`

Mirror `lisp/bats-mode/bats-mode-tests.el` and the
`find-sibling-rules` convention from `modules/lang-lisp/init.el:12-18`
(`<base>-tests.el` ↔ `<base>.el`).

Test moves are pure relocations. Each library test currently calls
into `argc-` symbols only — replacing the file's `(load …)` line
with `(require 'argc-mode)` is the entire wiring change.

### Decision: load helper `argc-test-has-face-p`

`modules/lang-shscript/tests.el:81-87` defines
`argc-test-has-face-p` as a buffer-applying helper. That helper
ships with the library tests (it's a test-only construct, not
production code), so it moves to `argc-mode-tests.el` alongside
the tests that use it.

## Risks / Trade-offs

- [Risk] Autoload regeneration may miss the new path if the
  harvester walks `modules/` before `lisp/` and caches. →
  Mitigation: tasks.md requires regenerating
  `lisp/+autoloads.el` and adds a smoke test
  (`argc-mode/autoloaded-without-require`) that visits a fresh
  Emacs without explicit `require` and asserts
  `(fboundp 'argc-mode)`.

- [Risk] Tests that currently rely on `(load …)` semantics
  (re-evaluating the file in a clean env) might depend on
  redefinition-on-load. → Mitigation: the file is pure
  definitions (faces, defconsts, defvars, defuns, define-minor-mode)
  with no `eval-when-compile` side effects; `(require)` semantics
  are equivalent.

- [Risk] The `+argc-maybe-enable` tests still want a reachable
  `argc-mode` symbol; if autoloads aren't regenerated those tests
  fail. → Mitigation: same as the first risk; the smoke test
  catches it before the rest of the suite runs.

- [Trade-off] Splitting tests across two files makes one library
  test file larger and one module test file smaller, but matches
  the existing structure for `bats-mode` and the convention for
  `find-sibling-rules`. Locality wins.

- [Trade-off] The library file ends up at the same path depth as
  the original (`lisp/argc-mode/argc-mode.el` vs
  `modules/lang-shscript/lib/+argc.el`). The relocation pays off
  via spec-axis ownership and test locality, not via shorter
  paths.
