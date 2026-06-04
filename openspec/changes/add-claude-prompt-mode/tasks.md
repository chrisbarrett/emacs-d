## 1. Scaffolding & detection

- [ ] 1.1 Add `lib/claude-prompt.el` with the lexical-binding header and a `claude-prompt-filename-regexp` built with `rx` matching `claude-<uid>/claude-prompt-<guid>.md` under the temp dir.
- [ ] 1.2 Write a failing test asserting the regexp matches a sample prompt path and rejects an ordinary `notes.md`.
- [ ] 1.3 Implement `+claude-prompt-setup-check-buffer` for `find-file-hook`; wire activation in `modules/claude/init.el`; declare `with-editor`/`consult` in `packages.eld` if not already resolvable.

## 2. Editing surface (with-editor)

- [ ] 2.1 Write a failing test: enabling the mode on a server-less buffer turns on `with-editor-mode` and `+claude-prompt-mode`, binds `C-c C-c`/`C-c C-k`, and protects against `kill-buffer`.
- [ ] 2.2 Define `+claude-prompt-mode` (minor mode, `defvar-keymap`), enabling `with-editor-mode` in setup; set the mode-line lighter.
- [ ] 2.3 Set a buffer-local `with-editor-usage-message` (or equivalent) listing `C-c C-c`, `C-c C-k`, `M-p`/`M-n`, `C-r`; confirm it shows on open.

## 3. Repo context & classification

- [ ] 3.1 Write failing tests for path normalisation: `/repo`, `/repo__worktrees/x`, `/repo/.worktrees/x` all normalise to `/repo`; classification against a given `repo_root` includes/excludes correctly.
- [ ] 3.2 Implement the `rx`-based worktree-suffix stripper and the repo-membership predicate.
- [ ] 3.3 Implement `+claude-prompt-register-context` (stash `(truename . repo_root)` alist) and setup-time lookup/pop by `buffer-file-name`; fallback to newest history-entry project when absent. Add a test for the fallback path.

## 4. History ring

- [ ] 4.1 Write a failing test: parsing a fixture `history.jsonl` yields `(display . project)` entries, most-recent-first, de-duplicated; mtime caching avoids re-parse.
- [ ] 4.2 Implement the mtime-cached `history.jsonl` reader (`json-parse-buffer`, no shell-out).
- [ ] 4.3 Implement `M-p`/`M-n` over a repo-scoped ring that replaces the buffer body; add a test driving previous/next over a fixture.

## 5. Recall picker (consult)

- [ ] 5.1 Define two `consult--multi` sources — `[r]` repo (default) and `[g]` global — over the parsed history, annotated by project; bind `C-r`.
- [ ] 5.2 Selection inserts the chosen prompt text into the buffer; add a test exercising source construction and the insert action (stub `consult--read` if needed).

## 6. Cross-repo coordination (nix-configuration — tracked, not implemented here)

- [ ] 6.1 Note for the nix-configuration change: `claude-prompt-edit` wrapper computes `repo_root` via git, calls `emacsclient -e (+claude-prompt-register-context …)`, then `exec emacsclient -nw -c "$file"`; on zero exit + opt-in env + `$TMUX`, runs `tmux send-keys -t "$TMUX_PANE" Enter`. Set as `programs.claude-code.settings.env.EDITOR`.
- [ ] 6.2 Verify empirically that Claude Code's `settings.env` reaches the spawned editor's environment before relying on the wrapper rebind.

## 7. Verification

- [ ] 7.1 Run `make test-quick` and confirm the new ERT tests pass.
- [ ] 7.2 In a sandbox daemon, simulate the server flow: open a fixture prompt file, exercise `C-c C-c` (finish → server-done) and `C-c C-k` (cancel → non-zero), and confirm ring/consult recall against a fixture log.
- [ ] 7.3 Run `make test` for the full suite + lint/byte-compile.
