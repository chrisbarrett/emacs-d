## Context

Repository currently uses `prek` to drive a thin `.pre-commit-config.yaml`
with three local hooks (`ert-tests`, `byte-compile`, `checkdoc`). Each hook
sets `pass_filenames: false, always_run: true` and invokes a script that
re-derives staged files via `git diff --cached`. No built-in checks from
`pre-commit/pre-commit-hooks` are enabled, so the pipeline lets through
trailing whitespace, missing EOF newlines, malformed yaml/toml/json,
merge-conflict markers, accidental large binaries, and shebang/exec drift.

`prek install` is invoked from two places: `flake.nix:shellHook` (on every
`nix develop`) and `Makefile:setup-hooks` (called from `make test`). The
Makefile guard is effectively dead — devShell entry beat it to the install.

The flake exports `TREESIT_EXTRA_LOAD_PATH` (flake.nix:23) used by
`yaml-ts-mode` in tests. Hooks run outside the devShell currently break
silently on yaml-touching tests; the existing AGENTS.md note tells humans to
prefix git commands with `nix develop --command`, but prek hooks themselves
do not enforce this.

Tooling axis (`contributor-internals`) has no existing spec. Per
spec-conventions, a new axis requires a `Decision: new axis` entry in this
design.

## Goals / Non-Goals

**Goals:**

- Replace `.pre-commit-config.yaml` with `prek.toml` carrying `#:schema`
  directive and prek-native idioms.
- Enable a baseline of built-in hygiene checks scoped away from
  literal-content directories.
- Ensure local hooks run inside `nix develop` regardless of caller
  environment.
- Reduce hook-install duplication to a single canonical entry point.

**Non-Goals:**

- Flipping local hooks to `pass_filenames = true`. That requires
  transitive-dependent expansion to live somewhere prek can call;
  currently it lives in `scripts/affected.sh`. Belongs to the dep-graph
  change.
- Rewriting `affected.sh` / `affected-tests.sh` in Elisp. Dep-graph rework
  is a separate change.
- Collapsing the three gate scripts into one dispatcher. Separate change.
- Trimming the Makefile beyond removing the `setup-hooks` target.
- Migrating CI definitions (none exist in this repo).

## Decisions

### Decision: new axis `contributor-internals`

Pre-commit pipeline, build tooling, and dev-environment glue are
internals-facing and span no single module. Per spec-conventions, a new
axis is declared here. The axis spec is internals-facing — it documents
which checks the pipeline guarantees and the contract between prek and the
local gate scripts.

### Decision: native `prek.toml`, not legacy yaml

Alternatives considered:
- **Keep yaml**: works, but loses prek-native features (glob, `env`,
  `priority`, `minimum_prek_version`) and discoverable schema. Rejected —
  no portability benefit since this repo only ever runs `prek`.

Toml file carries `#:schema https://json.schemastore.org/prek.json` on
line 1 for editor completion.

Native TOML support landed in prek 0.3.0; the repo's pinned nixpkgs
(2026-01-25) shipped prek 0.2.30 which is YAML-only. Bumping the
`nixpkgs` flake input to 2026-05-21 picks up prek 0.3.11. The flake.lock
update is part of this change.

### Decision: built-in hook set

Enable from `pre-commit/pre-commit-hooks v6.0.0`:

| Hook | Rationale | Fixer? |
|---|---|---|
| `trailing-whitespace` (`--chars= \t`) | catches editor drift; arg pins fixer to space+tab so Emacs form-feed (`^L`) page-break markers are preserved | yes |
| `end-of-file-fixer` | POSIX text-file convention | yes |
| `mixed-line-ending` w/ `--fix=lf` | repo is unix-only | yes |
| `check-yaml` | flake/prek configs would fail loudly | no |
| `check-toml` | same | no |
| `check-json` | claude/openspec configs | no |
| `check-merge-conflict` | catches half-resolved rebases | no |
| `check-added-large-files` | accidental binary commits | no |
| `check-executables-have-shebangs` | scripts/ hygiene | no |
| `check-shebang-scripts-are-executable` | scripts/ hygiene | no |
| `check-case-conflict` | cross-platform safety | no |

Excluded from consideration: `check-symlinks` (we deliberately symlink
CLAUDE.md → AGENTS.md), `no-commit-to-branch` (we commit to main).

Fixer scoping — exclude via top-level `exclude` glob:

```
{templates,capture-templates,file-templates,site,var,elpaca,eln-cache,.worktrees}/**
```

These dirs either contain literal template text (where trailing whitespace
may be semantic), generated artefacts, or third-party builds. Trailing
whitespace inside `.el` files in `lisp/`/`modules/`/`lib/` is non-semantic
and safe to strip.

### Decision: local hooks keep `pass_filenames = false`, `always_run = true`

Each local hook continues to be invoked as `--affected`:

```toml
[[repos.hooks]]
id = "byte-compile"
language = "system"
entry = "nix develop --command ./scripts/byte-compile.sh --affected"
pass_filenames = false
always_run = true
```

Rationale: `scripts/affected.sh` and `scripts/affected-tests.sh` compute
the **transitive dependent set** of staged elisp files — change
`lisp/+core.el` and byte-compile / tests run on every file that
(transitively) requires `+core`. Flipping to `pass_filenames = true`
with a glob filter would shrink scope to the directly-staged files
only, missing downstream breakage.

The follow-up dep-graph change relocates that transitive expansion into
Emacs and exposes it as a callable, at which point the local hooks can
flip to `pass_filenames = true` cleanly.

Alternatives considered:
- **`pass_filenames = true` + glob now**: smaller diff, but lossy as
  above. Rejected.
- **`pass_filenames = true` + glob + script expands argv → transitive
  set internally**: doable, but pushes the dep-graph reform into this
  change. Rejected — staying scoped.

### Decision: `nix develop --command` wrapping

Each local hook's `entry`:

```toml
entry = "nix develop --command ./scripts/<gate>.sh"
```

When called from inside the devShell, `nix develop` re-resolves the shell
in-place (sub-second). When called from outside, it provides the
`TREESIT_EXTRA_LOAD_PATH` env var. Cost is acceptable; correctness wins.

Alternative considered: a `scripts/in-nix-shell` re-exec guard at the top
of each gate (`[ -n "$IN_NIX_SHELL" ] || exec nix develop --command "$0" "$@"`).
Cleaner per-script but spreads the policy across files. Rejected in
favour of policy-in-config.

### Decision: drop `make setup-hooks`

`flake.nix:shellHook` already runs `prek install`. The Makefile target
ran the same command, gated on hook-file absence. Removing the target
and its `test: setup-hooks` dependency removes a redundant install path
and the only Makefile-side coupling to nix. Users who do not enter the
devShell already cannot reasonably run `make test` (no `prek` in PATH).

## Risks / Trade-offs

- **Autofixer first run will rewrite files.** → Pre-flight sweep before
  enabling; commit the cleanup separately so the gate-enable commit has
  zero file changes.
- **`trailing-whitespace` strips Emacs form-feed (`^L`) markers by
  default.** → `args = ["--chars= \t"]` limits the fixer to space and
  tab. Form-feed page breaks (used by `forward-page` /
  `backward-page` navigation across top-level Lisp sections) are
  preserved. Surfaced during verification — pre-flight regex
  `' +$'` did not detect this because the files had no plain trailing
  spaces, only form-feeds.
- **Templates with intentional trailing whitespace.** → Scoped exclude
  glob covers `templates/`, `capture-templates/`, `file-templates/`. Audit
  output of `git ls-files templates capture-templates file-templates | xargs grep -lE ' +$'`
  before enabling; widen exclude if needed.
- **`nix develop --command` overhead on every hook run.** → Single-digit
  hundred-ms when already in devShell. Acceptable; if it becomes
  measurable later, switch to a re-exec guard.
- **Migration changes no script behaviour.** → Local hooks invoke
  `<script> --affected`, exactly as the legacy yaml did. The only
  change is the wrapping in `nix develop --command`.
- **YAML/TOML/JSON check may fail on first run.** → Run `prek run --all-files --hook-stage manual`
  before enabling at default stage; fix any existing offenders in the
  same cleanup commit as the autofix sweep.
- **Built-in hook repo pins `v6.0.0`.** → Acceptable; `prek autoupdate`
  available later.

## Migration Plan

1. Pre-flight sweep: enumerate files the fixers would touch.
2. Cleanup commit: strip trailing whitespace / fix EOF newlines on
   in-scope files only.
3. Bump `flake.lock` to nixpkgs carrying prek ≥ 0.3.0 for native TOML.
4. Add `prek.toml` with built-in hooks plus local hooks (in their
   existing `--affected` shape, wrapped with `nix develop --command`);
   delete `.pre-commit-config.yaml` in the same commit.
5. Remove `make setup-hooks` target and `test: setup-hooks` dependency.
6. Verify: `prek validate-config` clean; `prek run --all-files` green
   inside devShell; spot-check `make test` still runs.

Rollback: `git revert` the migration commit; `.pre-commit-config.yaml`
returns from history. No state outside the repo to roll back.

## Follow-up

The pre-flight regex `grep -lE ' +$'` used in tasks 1.1 silently missed
files containing `^L` (form-feed) with no plain trailing space. The
`trailing-whitespace` hook treats form-feed as whitespace and stripped
them on first run. Surfaced during verification; fixed by passing
`--chars= \t` to the hook.

When the dep-graph follow-up change runs its own pre-flight sweep,
broaden the probe so similar surprises surface earlier — e.g.
`LC_ALL=C grep -PE '[\t\v\f ]+$'`, or replace the regex with a
language-aware Emacs batch scan that respects buffer-text semantics.
