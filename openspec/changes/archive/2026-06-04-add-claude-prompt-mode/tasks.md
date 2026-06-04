## 1. Scaffolding & detection

- [x] 1.1 Add `lisp/claude-prompt/claude-prompt.el` (proper package: lexical-binding header, `provide`, autoload cookies) with a `claude-prompt-filename-regexp` built with `rx` matching `claude-<uid>/claude-prompt-<guid>.md` under the temp dir.
- [x] 1.2 Write a failing test asserting the regexp matches a sample prompt path and rejects an ordinary `notes.md`.
- [x] 1.3 Implement `claude-prompt-setup-check-buffer` for `server-visit-hook` (server-visited buffers only — no global `find-file-hook`), gated on the wrapper-registered context with the filename pattern as fallback; wire activation in `modules/claude/init.el`. (`with-editor`/`consult` resolve via Magit/completion modules; no new `packages.eld` entry needed.)

## 2. Editing surface (with-editor)

- [x] 2.1 Write a failing test: enabling the mode on a server-less buffer turns on `with-editor-mode` and `claude-prompt-mode`, binds `C-c C-c`/`C-c C-k`, and protects against `kill-buffer`.
- [x] 2.2 Define `claude-prompt-mode` (minor mode, `defvar-keymap`), enabling `with-editor-mode` in setup; set the mode-line lighter.
- [x] 2.3 Set a buffer-local `with-editor-usage-message` (`claude-prompt-usage-message`) listing `C-c C-c`, `C-c C-k`, `M-p`/`M-n`, `C-r`; confirm it shows on open.

## 3. Repo context & classification

- [x] 3.1 Write failing tests for path normalisation: `/repo`, `/repo__worktrees/x`, `/repo/.worktrees/x` all normalise to `/repo`; classification against a given `repo_root` includes/excludes correctly.
- [x] 3.2 Implement the `rx`-based worktree-suffix stripper and the repo-membership predicate.
- [x] 3.3 Implement `claude-prompt-register-context` (stash `(truename . repo_root)` table) and setup-time lookup/pop by `buffer-file-name`; fallback to newest history-entry project when absent. Add a test for the fallback path.

## 4. History ring

- [x] 4.1 Write a failing test: parsing a fixture `history.jsonl` yields `(display . project)` entries, most-recent-first, de-duplicated; mtime caching avoids re-parse.
- [x] 4.2 Implement the mtime-cached `history.jsonl` reader (`json-parse-string`, no shell-out).
- [x] 4.3 Implement `M-p`/`M-n` over a repo-scoped ring that replaces the buffer body; add a test driving previous/next over a fixture.

## 5. Recall picker (consult)

- [x] 5.1 Define two `consult--multi` sources — `[r]` repo (default) and `[g]` global — over the parsed history, annotated by project; bind `C-r`.
- [x] 5.2 Selection inserts the chosen prompt text into the buffer; add a test exercising source construction and the insert action.

## 6. Cross-repo coordination (nix-configuration — tracked, not implemented here)

- [x] 6.1 Note for the nix-configuration change: `claude-prompt-edit` wrapper computes `repo_root` via git, calls `emacsclient -e (claude-prompt-register-context …)`, then `exec emacsclient -nw -c "$file"`; on zero exit + opt-in env + `$TMUX`, runs `tmux send-keys -t "$TMUX_PANE" Enter`. Set as `programs.claude-code.settings.env.EDITOR`. (Captured in design.md "tmux/auto-send stays entirely in the wrapper".)
- [x] 6.2 Verified empirically that Claude Code's `settings.env` reaches the spawned editor's environment: a tool subprocess inherits `EDITOR=emacsclient -nw -c`, `CLAUDE_CODE_TMPDIR=/tmp/claude-503`, `TMUX_PANE`, and `TMUX`, so the wrapper rebind and tmux gating are viable.

## 7. Verification

- [x] 7.1 Run `make test-quick` and confirm the new ERT tests pass.
- [x] 7.2 In a sandbox daemon, simulate the server flow: open a fixture prompt file, exercise `C-c C-c` (finish → server-done) and `C-c C-k` (cancel → non-zero), and confirm ring/consult recall against a fixture log.
- [x] 7.3 Run `make test` for the full suite + lint/byte-compile.
