## 1. Pre-flight sweep

- [ ] 1.1 Enumerate files with trailing whitespace outside the exclude scope:
      `git ls-files | grep -vE '^(templates|capture-templates|file-templates|site|var|elpaca|eln-cache|\.worktrees)/' | xargs grep -lE ' +$' 2>/dev/null` and record the list.
- [ ] 1.2 Enumerate files missing final newline outside the exclude scope:
      `git ls-files | grep -vE '^(templates|capture-templates|file-templates|site|var|elpaca|eln-cache|\.worktrees)/' | while read f; do [ -n "$(tail -c1 "$f" 2>/dev/null)" ] && echo "$f"; done`.
- [ ] 1.3 Enumerate files with CRLF line endings outside the exclude scope:
      `git ls-files | grep -vE '^(templates|capture-templates|file-templates|site|var|elpaca|eln-cache|\.worktrees)/' | xargs file 2>/dev/null | grep CRLF`.
- [ ] 1.4 Confirm no existing yaml/toml/json file is invalid: spot-check
      `flake.nix` (no — that's nix), `.workmux.yaml`, any `*.toml`, any
      `.claude/*.json` parse cleanly.
- [ ] 1.5 Check shebang/exec drift: `git ls-files scripts/ | xargs ls -l`
      — every `.sh` is executable and has `#!/usr/bin/env bash`.

## 2. Hygiene cleanup commit

- [ ] 2.1 Strip trailing whitespace from files identified in 1.1.
- [ ] 2.2 Add final newline to files identified in 1.2.
- [ ] 2.3 Convert CRLF to LF for files identified in 1.3 (if any).
- [ ] 2.4 Fix any yaml/toml/json invalidity or shebang/exec drift surfaced.
- [ ] 2.5 Commit as a single hygiene-cleanup commit, separate from the
      config migration. Message: "Strip trailing whitespace and fix
      EOF newlines ahead of prek built-ins".

## 3. Write prek.toml

- [ ] 3.1 Create `prek.toml` with `#:schema https://json.schemastore.org/prek.json`
      on line 1.
- [ ] 3.2 Add top-level `exclude` glob covering `templates/`,
      `capture-templates/`, `file-templates/`, `site/`, `var/`,
      `elpaca/`, `eln-cache/`, `.worktrees/`.
- [ ] 3.3 Add `[[repos]]` block for `pre-commit/pre-commit-hooks` at
      `rev = "v6.0.0"` listing the eleven hooks from the design doc.
- [ ] 3.4 Add `[[repos]]` block with `repo = "local"` containing three
      `[[repos.hooks]]` entries (`ert-tests`, `byte-compile`, `checkdoc`).
- [ ] 3.5 Each local hook sets `language = "system"`,
      `entry = "nix develop --command ./scripts/<gate>.sh"`,
      `pass_filenames = true`, and a `files = { glob = "..." }` filter:
      - `byte-compile`: `files = { glob = "**/*.el" }`,
        `exclude = { glob = "**/*-tests.el" }`
      - `checkdoc`: `files = { glob = "{lisp,lib}/**/*.el" }`,
        `exclude = { glob = "**/*-tests.el" }`
      - `ert-tests`: `files = { glob = "**/*.el" }`
- [ ] 3.6 Run `prek validate-config` inside devShell; resolve any errors.

## 4. Update gate scripts

- [ ] 4.1 In `scripts/byte-compile.sh`: keep no-args branch and
      `--affected` branch; positional-args branch already exists. Ensure
      positional args bypass the git-diff path entirely.
- [ ] 4.2 In `scripts/checkdoc.sh`: same shape — verify the positional
      branch is the path prek will hit.
- [ ] 4.3 In `scripts/run-tests.sh`: same — confirm positional `.el` test
      files are loaded via `-l` and ERT runs.
- [ ] 4.4 Add a regression note at the top of each script: "Called by
      prek with positional filenames; called by Makefile with no args or
      `--affected`; do not git-diff when positional args are present."

## 5. Delete legacy yaml + Makefile setup-hooks

- [ ] 5.1 Delete `.pre-commit-config.yaml`.
- [ ] 5.2 Remove `setup-hooks` target from `Makefile`.
- [ ] 5.3 Remove `setup-hooks` from `test`'s prerequisite list.
- [ ] 5.4 Remove `setup-hooks` from the `help` target's printed list.

## 6. Verification

- [ ] 6.1 Run `prek validate-config` — exit zero.
- [ ] 6.2 Run `prek run --all-files` inside devShell — every hook green.
- [ ] 6.3 Stage a deliberately broken file (e.g. add trailing whitespace
      to a `lisp/*.el`); confirm `trailing-whitespace` autofixes it.
- [ ] 6.4 Stage a non-elisp file change (e.g. README.org); confirm local
      elisp hooks skip it (glob filter works).
- [ ] 6.5 Run `make test` — passes.
- [ ] 6.6 Run `make pre-commit` — passes (kept for now; future change
      collapses it).
- [ ] 6.7 Exit and re-enter devShell; confirm `.git/hooks/pre-commit`
      still installed via shellHook.

## 7. Archive

- [ ] 7.1 Sync delta spec to `openspec/specs/contributor-internals/spec.md`.
- [ ] 7.2 Archive the change via `/opsx:archive`.
