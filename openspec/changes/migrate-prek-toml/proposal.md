## Why

The repo runs `prek` but its hooks live in legacy `.pre-commit-config.yaml`.
The three local hooks ignore prek's file-filtering machinery
(`pass_filenames: false, always_run: true`), so every gate re-derives its
own staged-file set instead of taking it from prek. No built-in checks are
enabled — trailing whitespace, EOF newline, syntax checks on yaml/toml/json,
merge-conflict markers, large-file accidents, shebang/exec drift all sail
through. And `prek install` runs in two places (flake shellHook and
`make setup-hooks`) without either being authoritative.

Migrating to a native `prek.toml` with the prek schema is the foundation for
later cleanups (collapsing the `--affected` plumbing, slimming the
Makefile, dep-graph rewrite). It also lets us turn on a baseline of cheap
built-in checks for free.

## What Changes

- Replace `.pre-commit-config.yaml` with `prek.toml` carrying the
  `#:schema` directive and prek-native idioms (glob `files`, expanded
  array-of-tables for local hooks).
- Add built-in hooks from `pre-commit/pre-commit-hooks`:
  trailing-whitespace, end-of-file-fixer, mixed-line-ending,
  check-yaml, check-toml, check-json, check-merge-conflict,
  check-added-large-files, check-executables-have-shebangs,
  check-shebang-scripts-are-executable, check-case-conflict.
- Scope built-in fixers to exclude `templates/`, `capture-templates/`,
  `file-templates/`, `site/`, `var/`, `elpaca/`, `eln-cache/` — anywhere
  literal-content templates or generated artefacts live.
- Switch the three local hooks (ert-tests, byte-compile, checkdoc) to
  `pass_filenames = true` with glob filters. Scripts now receive the
  filename set from prek instead of re-running `git diff` internally.
- Wrap local hook entries in `nix develop --command` so prek runs them
  with `TREESIT_EXTRA_LOAD_PATH` populated even when invoked outside the
  devShell.
- Remove `make setup-hooks` and its dependency from `make test`; flake
  shellHook stays the canonical install path.

## Capabilities

### New Capabilities

- `contributor-internals`: developer-facing tooling axis covering the
  pre-commit pipeline (prek configuration, built-in checks, local hook
  contracts) and its relationship to flake / Makefile / scripts.

### Modified Capabilities

_None._ No existing spec covers contributor tooling.

## Impact

- `.pre-commit-config.yaml` — deleted.
- `prek.toml` — new, replaces the yaml.
- `flake.nix` — unchanged (already provides `prek` + `TREESIT_EXTRA_LOAD_PATH`
  + `prek install` in shellHook).
- `Makefile` — drop `setup-hooks` target and the `test: setup-hooks`
  dependency. No other changes in this slice.
- `scripts/run-tests.sh`, `scripts/byte-compile.sh`, `scripts/checkdoc.sh`
  — accept positional filenames from prek; existing `--affected` flag and
  no-args full-run branch retained for Makefile use.
- Repo content may be touched by autofixers on first run (trailing
  whitespace, EOF newline). Pre-flight sweep + scoped excludes contain the
  blast radius.
