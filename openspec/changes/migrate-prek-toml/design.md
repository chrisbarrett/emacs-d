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
- Make prek the source of truth for staged-file dispatch — scripts receive
  filenames from prek, no longer git-diff internally for the prek path.
- Ensure local hooks run inside `nix develop` regardless of caller
  environment.
- Reduce hook-install duplication to a single canonical entry point.

**Non-Goals:**

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
- **Use `prek util yaml-to-toml`**: lossy (comments dropped) and produces
  inline-table style. Used as a starting skeleton, then hand-edited.

Toml file carries `#:schema https://json.schemastore.org/prek.json` on
line 1 for editor completion.

### Decision: built-in hook set

Enable from `pre-commit/pre-commit-hooks v6.0.0`:

| Hook | Rationale | Fixer? |
|---|---|---|
| `trailing-whitespace` | catches editor drift | yes |
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

### Decision: `pass_filenames = true` with glob filters

Each local hook gains:

```toml
files = { glob = "**/*.el" }
exclude = { glob = "**/*-tests.el" }
pass_filenames = true
```

Prek matches staged files against the glob and passes them as argv. The
scripts retain three branches:

1. No args → run all (Makefile / manual path).
2. Single arg `--affected` → preserve existing behaviour (Makefile
   `*-affected` targets).
3. Positional filenames → run on the given set (new prek path).

The script's git-diff path is unused under prek but kept for explicit
`--affected` callers. Removing it is the next change's concern.

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
- **Templates with intentional trailing whitespace.** → Scoped exclude
  glob covers `templates/`, `capture-templates/`, `file-templates/`. Audit
  output of `git ls-files templates capture-templates file-templates | xargs grep -lE ' +$'`
  before enabling; widen exclude if needed.
- **`nix develop --command` overhead on every hook run.** → Single-digit
  hundred-ms when already in devShell. Acceptable; if it becomes
  measurable later, switch to a re-exec guard.
- **`pass_filenames=true` changes script invocation contract.** → Scripts
  retain `--affected` and no-args branches; new positional branch is
  additive. Behaviour change is local to the prek code-path.
- **YAML/TOML/JSON check may fail on first run.** → Run `prek run --all-files --hook-stage manual`
  before enabling at default stage; fix any existing offenders in the
  same cleanup commit as the autofix sweep.
- **Built-in hook repo pins `v6.0.0`.** → Acceptable; `prek autoupdate`
  available later.

## Migration Plan

1. Pre-flight sweep: enumerate files the fixers would touch.
2. Cleanup commit: strip trailing whitespace / fix EOF newlines on
   in-scope files only.
3. Add `prek.toml` with built-in hooks AND local hooks in their new
   `pass_filenames=true` shape; delete `.pre-commit-config.yaml` in the
   same commit.
4. Update scripts: add positional-filenames branch alongside existing
   `--affected` and no-args branches.
5. Remove `make setup-hooks` target and `test: setup-hooks` dependency.
6. Verify: `prek validate-config` clean; `prek run --all-files` green
   inside devShell; spot-check `make test` still runs.

Rollback: `git revert` the migration commit; `.pre-commit-config.yaml`
returns from history. No state outside the repo to roll back.
