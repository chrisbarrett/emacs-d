## Why

Bats (Bash Automated Testing System) files are bash with extra non-bash syntax
(`@test "name" { … }`, fixture functions, helper-library assertions). Opening
a `.bats` file currently lands in `bash-ts-mode`, which neither highlights bats
directives nor exposes test names to imenu, so navigation and visual
scanning lag the rest of the editor. A dedicated major mode that builds on
`bash-ts-mode` recovers TS-grade bash editing while layering bats-aware
font-lock and navigation on top.

## What Changes

- New self-contained library `lisp/bats-mode/bats-mode.el` defining a
  `bats-mode` major mode derived from `bash-ts-mode`.
- Activation:
  - `auto-mode-alist` entry for `\.bats\'`.
  - `interpreter-mode-alist` entry for `bats` (handles `#!/usr/bin/env bats`).
- Faces:
  - `bats-directive-face` — `@test`, `setup`, `teardown`, `setup_file`,
    `teardown_file`, `setup_suite`, `teardown_suite`, `load`,
    `bats_load_library`, `bats_require_minimum_version`, `run`,
    `bats_pipe`, `skip`, `bats::on_failure`.
  - `bats-assertion-face` — `assert*` / `refute*` / `fail` family, gated
    by a profile alist so only assertions from libraries the buffer
    actually loads are highlighted.
  - `bats-variable-face` — `$BATS_*` special variables.
- Font-lock is layered on top of bash-ts-mode via
  `font-lock-add-keywords`; existing TS faces remain.
- Profile detection: scan buffer for `load` / `bats_load_library` lines,
  follow one level of `load <relpath>` into the referenced bash file
  (cached per buffer, refreshed on `after-save-hook`), and union the
  resulting profile set. Recognised profiles: `:core`, `:bats-assert`,
  `:bats-support`, `:bats-file`.
- Imenu: custom `imenu-create-index-function` composing three sections
  on top of the inherited bash-ts-mode index — **Tests** (`@test "name"`
  entries), **Fixtures** (setup/teardown variants present in the buffer),
  and the bash-ts-mode function list.
- Tests live at `lisp/bats-mode/bats-mode-tests.el`.

## Capabilities

### New Capabilities

- `bats-mode`: behaviour-facing spec for the `bats-mode` library —
  activation, faces and font-lock layering, profile-gated assertion
  highlighting, `$BATS_*` variable highlighting, and imenu composition.

### Modified Capabilities

None.

## Impact

- New files: `lisp/bats-mode/bats-mode.el`, `lisp/bats-mode/bats-mode-tests.el`.
- `lisp/spec-conventions/spec.md` recognised-axes list gains a
  `bats-mode (lib)` entry (handled in tasks; not a requirement-body change
  to `spec-conventions` itself, so no delta spec needed there beyond the
  list edit).
- No changes to `modules/lang-shscript/init.el` are required for
  activation — autoloads + `auto-mode-alist` / `interpreter-mode-alist`
  cover it. A `(require 'bats-mode)` may be added from `lang-shscript`
  only if autoload harvesting under `lisp/` proves insufficient at
  startup; design.md decides.
- `lisp/+modules.el` autoload-extractor (`+modules--autoload-form`)
  generalised: unrecognised `;;;###autoload`-annotated top-level forms
  now pass through verbatim so activation triggers like
  `(add-to-list 'auto-mode-alist …)` work in `lisp/<family>/<lib>.el`
  libraries (the extractor previously only handled defun/defmacro/
  define-(derived|minor)-mode/defvar/defconst). Companion tests in
  `lisp/+modules-tests.el` updated to assert pass-through semantics.
- No new external packages.
