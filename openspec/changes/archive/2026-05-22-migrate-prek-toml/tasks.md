## 1. Pre-flight sweep

- [x] 1.1 Enumerate files with trailing whitespace outside the exclude scope:
      `git ls-files | grep -vE '^(templates|capture-templates|file-templates|site|var|elpaca|eln-cache|\.worktrees)/' | xargs grep -lE ' +$' 2>/dev/null` and record the list. → **none**
- [x] 1.2 Enumerate files missing final newline outside the exclude scope:
      `git ls-files | grep -vE '^(templates|capture-templates|file-templates|site|var|elpaca|eln-cache|\.worktrees)/' | while read f; do [ -n "$(tail -c1 "$f" 2>/dev/null)" ] && echo "$f"; done`. → **none**
- [x] 1.3 Enumerate files with CRLF line endings outside the exclude scope:
      `git ls-files | grep -vE '^(templates|capture-templates|file-templates|site|var|elpaca|eln-cache|\.worktrees)/' | xargs file 2>/dev/null | grep CRLF`. → **none**
- [x] 1.4 Confirm no existing yaml/toml/json file is invalid: spot-check
      `flake.nix` (no — that's nix), `.workmux.yaml`, any `*.toml`, any
      `.claude/*.json` parse cleanly. → `.workmux.yaml` OK; no committed
      json/toml.
- [x] 1.5 Check shebang/exec drift: `git ls-files scripts/ | xargs ls -l`
      — every `.sh` is executable and has `#!/usr/bin/env bash`. → **OK**

## 2. Hygiene cleanup commit

- [x] 2.1 Strip trailing whitespace from files identified in 1.1. → no-op.
- [x] 2.2 Add final newline to files identified in 1.2. → no-op.
- [x] 2.3 Convert CRLF to LF for files identified in 1.3 (if any). → no-op.
- [x] 2.4 Fix any yaml/toml/json invalidity or shebang/exec drift surfaced. → no-op.
- [x] 2.5 Commit as a single hygiene-cleanup commit, separate from the
      config migration. → **skipped: pre-flight surfaced nothing to fix.**

## 3. Write prek.toml

- [x] 3.1 Create `prek.toml` with `#:schema https://json.schemastore.org/prek.json`
      on line 1.
- [x] 3.2 Add top-level `exclude` glob covering `templates/`,
      `capture-templates/`, `file-templates/`, `site/`, `var/`,
      `elpaca/`, `eln-cache/`, `.worktrees/`.
- [x] 3.3 Add `[[repos]]` block for `pre-commit/pre-commit-hooks` at
      `rev = "v6.0.0"` listing the eleven hooks from the design doc.
- [x] 3.4 Add `[[repos]]` block with `repo = "local"` containing three
      `[[repos.hooks]]` entries (`ert-tests`, `byte-compile`, `checkdoc`).
- [x] 3.5 Each local hook sets `language = "system"`,
      `entry = "nix develop --command ./scripts/<gate>.sh --affected"`,
      `pass_filenames = false`, `always_run = true`. (Design revised
      mid-implementation — `pass_filenames=true` flip deferred to the
      dep-graph follow-up change so we don't lose transitive expansion
      from `scripts/affected.sh`.)
- [x] 3.6 Run `prek validate-config` inside devShell; resolve any errors.
      → required bumping nixpkgs flake input (prek 0.2.30 → 0.3.11) to
      get native TOML support. Flake update committed as part of this
      change.

## 4. Update gate scripts

- [x] 4.1 No script changes. Prek hooks invoke `--affected`, which scripts
      already support. Positional-filenames branch deferred to the
      dep-graph change.
- [x] 4.2 (deferred)
- [x] 4.3 (deferred)
- [x] 4.4 (deferred)

## 5. Delete legacy yaml + Makefile setup-hooks

- [x] 5.1 Delete `.pre-commit-config.yaml`.
- [x] 5.2 Remove `setup-hooks` target from `Makefile`.
- [x] 5.3 Remove `setup-hooks` from `test`'s prerequisite list.
- [x] 5.4 Remove `setup-hooks` from the `help` target's printed list.

## 6. Verification

- [x] 6.1 Run `prek validate-config` — exit zero.
- [x] 6.2 Run `prek run --all-files` inside devShell — every hook green.
- [x] 6.3 Stage a deliberately broken file (e.g. add trailing whitespace
      to a `lisp/*.el`); confirm `trailing-whitespace` autofixes it. → OK.
- [x] 6.4 Stage a non-elisp file change (e.g. README.org); confirm
      pipeline runs. → Built-in hooks file-filter correctly; local hooks
      `always_run=true` still execute (intended — matches legacy yaml).
- [x] 6.5 Run `make test` — 848/848 tests pass.
- [x] 6.6 Run `make pre-commit` — passes.
- [x] 6.7 `.git/hooks/pre-commit` still installed via shellHook (verified
      after re-entering devShell during validation).

## 7. Archive

- [x] 7.1 Sync delta spec to `openspec/specs/contributor-internals/spec.md`.
- [ ] 7.2 Archive the change via `/opsx:archive`.
