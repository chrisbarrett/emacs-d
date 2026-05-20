## Context

`.bats` files are bash with extra preprocessor syntax: `@test "name" { … }`
test declarations, fixture functions (`setup` / `teardown` /
`setup_file` / `teardown_file` / `setup_suite` / `teardown_suite`),
load directives (`load`, `bats_load_library`), runtime helpers (`run`,
`bats_pipe`, `skip`, `bats::on_failure`, `bats_require_minimum_version`),
`$BATS_*` special variables, and ecosystem assertion helpers from
[bats-assert](https://github.com/bats-core/bats-assert),
[bats-support](https://github.com/bats-core/bats-support), and
[bats-file](https://github.com/bats-core/bats-file).

Tree-sitter's bash grammar does not parse `@test "name" { … }` cleanly —
`@test` is treated as a command word and the trailing `{ … }` does not
bind to it as a function body would. `bash-ts-mode`'s TS imenu therefore
cannot discover test names. Existing precedent for layering bats-style
extras on top of `bash-ts-mode` lives in
`modules/lang-shscript/lib/+argc.el`, which adds regex font-lock for
argc comment directives via a minor mode.

This repo's `lisp/` directory holds self-contained packages
(`lisp/<family>/<lib>.el`, e.g. `lisp/gfm/gfm-pretty.el`); modules
under `modules/` are composition units. The bats major mode is a
self-contained library and belongs under `lisp/`.

The recognised-axes list in
`openspec/specs/spec-conventions/spec.md` does not yet include
`bats-mode`.

## Goals / Non-Goals

**Goals:**

- A `bats-mode` major mode derived from `bash-ts-mode`, with the
  ergonomics of `bash-ts-mode` intact (TS-based indent, structural
  navigation, bash font-lock) plus bats-aware overlays:
  - directive face for bats-specific functions and syntax;
  - assertion face for helper-library assertions, gated by which
    helpers the buffer actually loads;
  - variable face for `$BATS_*` special variables;
  - imenu that surfaces `@test` names and fixture functions alongside
    inherited bash functions.
- Automatic activation on `.bats` files and on `bats` interpreter shebangs.
- TDD-driven implementation backed by ERT in
  `lisp/bats-mode/bats-mode-tests.el`.

**Non-Goals:**

- Running bats tests from within Emacs (no compile-mode wiring,
  no `bats` runner integration).
- Fixing tree-sitter's parsing of `@test`. The grammar is upstream and
  we layer around it, not on top of a fork.
- Custom indentation rules. Inherit bash-ts-mode indent as-is.
- A keymap with new bindings. Mode is presentational; bash-ts-mode
  keymap is enough.
- Snippet / yasnippet integration. Out of scope.

## Decisions

### Decision: new axis `bats-mode`

A new lib axis `bats-mode` is added to the recognised-axes list in
`openspec/specs/spec-conventions/spec.md`. Justification: bats is a
distinct testing dialect with its own surface vocabulary (`@test`,
fixture functions, assertion families) and its own activation contract
(file extension and interpreter). It is not a sub-feature of
`lang-shscript` because (a) `lang-shscript` is a module-shape axis that
composes existing libraries while `bats-mode` is itself a
self-contained library, and (b) bats-mode lives under
`lisp/bats-mode/` per the lib-axis rule. Folding the spec into a
hypothetical `lang-shscript` spec would force a category mismatch
between lib-shape and module-shape axes.

**Alternatives considered:**

- *Fold into `lang-shscript`*: rejected — `lang-shscript` is a module
  axis covering shell-script glue (sh-mode/bash-ts-mode remapping,
  argc, separedit). Bats is its own library with its own activation
  surface and its own faces. Mixing the two would violate
  "one spec per axis".
- *Make `bats-mode` a module under `modules/lang-bats/`*: rejected —
  the code is small and self-contained, has no init-time wiring beyond
  autoloads + alist entries, and matches the lib-axis criterion
  (`spec-conventions/spec.md:51-55`). Modules pay for themselves only
  when they compose multiple libs or install global hooks.

### Decision: Layer regex font-lock on top of `bash-ts-mode`

Use `font-lock-add-keywords` (with `'bats-mode` as the mode argument)
to add bats-specific keyword regexes on top of the inherited TS
font-lock. TS faces remain; bats faces compose.

**Why:** `bash-ts-mode` font-lock is driven by `treesit-font-lock-rules`
and runs through `treesit-font-lock-fontify-region` which then defers
to `font-lock-default-fontify-region` for non-TS keywords. Layering is
the documented composition path (`treesit.el` —
`treesit-font-lock-fontify-region`).

**Alternatives considered:**

- *Custom `treesit-font-lock-rules`*: rejected — the bats directives
  do not correspond to clean TS nodes (`@test` is mis-parsed). Regex
  is the right tool here.
- *Override `bash-ts-mode` font-lock*: rejected — loses bash
  highlighting.

### Decision: Profile-gated assertion highlighting

Highlight assertion helpers only when the buffer (or one level of
`load`-resolved companion bash file) shows evidence of loading the
corresponding helper library. Profile alist
`bats-mode-profile-keywords`:

| Profile         | Keywords (excerpt)                                        |
| :-------------- | :-------------------------------------------------------- |
| `:core`         | `@test`, fixtures, `run`, `skip`, `load`, …               |
| `:bats-assert`  | `assert`, `refute`, `assert_equal`, `assert_output`, …    |
| `:bats-support` | `fail`, `batslib_decorate`, `batslib_is_caller`, …        |
| `:bats-file`    | `assert_file_exists`, `assert_dir_exists`, …              |

`:core` is always active. Other profiles activate when a load line
mentions the corresponding library name (`bats-assert`, `bats-support`,
`bats-file`).

Detection runs:

1. On `bats-mode` entry, scan the buffer line-by-line for
   `^\s*(load|bats_load_library)\s+["']?(\S+)["']?`.
2. For each matched argument, check whether it names a known helper
   library (`bats-assert`, `bats-support`, `bats-file`) — add the
   matching profile.
3. If the argument resolves to a local file
   (`<dirname-of-buffer>/<arg>.bash` or `<arg>.bash`, also without the
   `.bash` suffix), read that file once and run step 1 against its
   contents. No deeper recursion.
4. Cache the union per buffer in a buffer-local var.
   `after-save-hook` invalidates and recomputes.

**Why one level of follow-through:** the dominant real-world pattern is
`load 'test_helper'` where `test_helper.bash` does the
`bats_load_library` calls. One level catches that pattern without
opening the can of arbitrary file traversal.

**Alternatives considered:**

- *No detection, always-on*: rejected — false positives in files that
  don't load bats-assert make the highlighting noise rather than signal.
- *Buffer-only*: rejected — misses the common `test_helper` pattern.
- *Recursive follow-through*: rejected — opens caching/invalidation
  concerns disproportionate to value. One level is the 80/20 cut.

### Decision: `$BATS_*` variables get their own face

`bats-variable-face` highlights an exact-match list of documented
`$BATS_*` special variables (`$BATS_TEST_NAME`, `$BATS_TMPDIR`,
`$BATS_RUN_COMMAND`, `$BATS_TEST_DIRNAME`, `$BATS_TEST_FILENAME`,
`$BATS_TEST_NUMBER`, `$BATS_SUITE_TEST_NUMBER`, `$BATS_TEST_TAGS`,
`$BATS_VERSION`, `$BATS_FILE_TMPDIR`, `$BATS_SUITE_TMPDIR`,
`$BATS_RUN_TMPDIR`, `$BATS_TEST_TMPDIR`, `$BATS_TEST_DESCRIPTION`,
`$BATS_TEST_NAMES`, plus configurable `BATS_TEST_NAME_PREFIX`,
`BATS_TEST_RETRIES`, `BATS_TEST_TIMEOUT`, `BATS_FILE_EXTENSION`).

**Why exact-match, not `\$BATS_[A-Z_]+`:** a wide regex would fontify
any variable a user names with a `BATS_` prefix, which is noise. The
documented list is small and stable; emphasising it makes the
language-defined surface visually distinct from user state.

### Decision: Imenu composes Tests + Fixtures + bash functions

`imenu-create-index-function` returns a nested alist:

```
Tests
  "name of @test"  →  marker at `@test` line
  …
Fixtures
  setup            →  marker
  teardown         →  marker
  setup_file       →  marker
  teardown_file    →  marker
  setup_suite      →  marker
  teardown_suite   →  marker
Functions
  <inherited bash-ts-mode entries>
```

Tests and Fixtures are computed by regex over the buffer (TS won't
help). The Functions section reuses whatever bash-ts-mode would have
returned — call its TS-driven index function and graft it under
`Functions`.

**Why merge, not replace:** `test_helper.bash`-style files that get
opened in `bats-mode` (because they `load` from bats files) still
benefit from bash function navigation. The cost of inheriting is
trivial.

### Decision: Activation via `auto-mode-alist` + `interpreter-mode-alist`

`auto-mode-alist` matches `\.bats\'`. `interpreter-mode-alist` matches
`bats`, which handles `#!/usr/bin/env bats` and `#!/usr/local/bin/bats`
uniformly (Emacs strips path and `env` for the basename match — see
`set-auto-mode` in `files.el`).

**Why both:** `auto-mode-alist` runs first based on filename; if the
file lacks a `.bats` extension but has a `#!` line with `bats`,
`interpreter-mode-alist` picks it up. Together they cover every common
case.

**Alternative considered:** `magic-mode-alist` with a shebang regex
(precedent at `modules/lang-shscript/init.el:10`). Rejected because
`interpreter-mode-alist` is the canonical mechanism for shebang-based
mode selection and handles the same cases more declaratively.

### Decision: TDD entry test

First failing tests in `lisp/bats-mode/bats-mode-tests.el`:

```elisp
(ert-deftest bats-mode/activates-on-bats-extension ()
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/foo.bats"))
      (set-auto-mode)
      (should (derived-mode-p 'bats-mode))
      (should (derived-mode-p 'bash-ts-mode)))))

(ert-deftest bats-mode/fontifies-test-directive ()
  (with-temp-buffer
    (insert "@test \"adds\" {\n  run true\n}\n")
    (bats-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "@test")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'bats-directive-face))))
```

Profile, variable, and imenu tests follow once the first two pass.

## Risks / Trade-offs

- **TS-parse breakage on `@test`** → font-lock layering tolerates it; structural
  navigation across `@test` blocks may be imperfect. Acceptable — the
  bash function inside the `{ … }` still parses, only the wrapper is
  off.
- **Profile false negative** when `test_helper.bash` itself loads
  another file that loads the helper → assertions in the buffer won't
  highlight. Mitigation: one-level recursion catches the common case;
  users can also `(add-to-list 'bats-mode-extra-profiles :bats-assert)`
  via dir-locals (decided by impl-time defcustom shape).
- **`after-save-hook` profile recompute cost** for buffers with very
  large `test_helper.bash` files → cache result + only recompute when
  the buffer or the helper's modtime changed.
- **Autoload harvesting under `lisp/<family>/`** → if `lisp/bats-mode/`
  is not on the autoloads scan path, activation won't happen
  until the file is loaded. Mitigation: design verifies the existing
  `+autoloads` mechanism covers `lisp/<family>/<lib>.el` (precedent:
  `lisp/gfm/gfm-pretty.el` is autoloaded). If insufficient, add a
  `(require 'bats-mode)` from `lang-shscript/init.el`.
