## Context

Claude Code's CLI runs in an external terminal (under tmux), not in an `eat`
buffer. `$EDITOR` is `emacsclient`, so when Claude opens a prompt for external
editing it spawns `emacsclient -nw -c <file>` against the user's running Emacs
server and blocks until `server-done`. The file is
`/tmp/claude-$UID/claude-prompt-$GUID.md` (`/private/tmp/...` on darwin) and
opens in `gfm-mode` (already mapped in `lang-markdown`). Claude keeps a log of
every submitted prompt at `~/.claude/history.jsonl` — one JSON object per line
with `display`, `project`, `timestamp`, `sessionId` (verified: ~8.5k lines,
~300 projects, appended live).

The integration module already exists at `modules/claude/` (init/config/lib/
tests), so this is a new behaviour axis on an existing module.

## Goals / Non-Goals

**Goals:**
- A `git-commit`-style editing surface for Claude prompt files: explicit
  finish (`C-c C-c`) and cancel (`C-c C-k`), buffer protection, mode-line
  lighter, on-open help.
- Recall of prior prompts: a repo-scoped ring (`M-p`/`M-n`) and a `consult`
  picker (`C-r`) with `[r]` repo / `[g]` global scopes, sourced from
  `history.jsonl`.
- Clean layering: the Emacs layer is tmux-agnostic; all tmux/auto-send logic
  lives in the editor wrapper (nix-configuration repo).

**Non-Goals:**
- The wrapper script and `settings.env.EDITOR` wiring (lives in
  nix-configuration; tracked there). This change assumes the side-channel
  contract but degrades gracefully without it.
- tmux auto-submit (wrapper concern, gated on `emacsclient` exit code).
- Any new external package. `with-editor` (Magit dep) and `consult` are
  already built.
- Editing `history.jsonl`; it is read-only input.

## Decisions

### Decision: new axis `claude`

No `claude` spec exists under `openspec/specs/`, but `modules/claude/` does, so
per the axis rule this introduces a new behaviour-facing axis named `claude`.
The prompt-editing mode, history ring, and recall picker all fold into this one
axis spec.

### Decision: build on `with-editor`, do not reimplement server handshake

`with-editor-return` (`with-editor.el:371`) branches on `server-buffer-clients`
and calls `server-done` on finish / sends `-error Canceled by user` then
`delete-process` on cancel — it operates on any server buffer, including ones
Emacs did not spawn. So enabling `with-editor-mode` (`with-editor.el:423`) in
the prompt buffer gives finish (`with-editor-finish`, :333), cancel
(`with-editor-cancel`, :351), `kill-buffer` protection (:435), and the usage
message (:438) for free. `git-commit` uses exactly this layering (minor mode
keyed by `git-commit-filename-regexp`, then flips on `with-editor-mode`), which
we mirror — except for the activation hook (see below).

### Decision: activate on `server-visit-hook`, not `find-file-hook`

`git-commit` enables its mode from a global `find-file-hook`, which runs for
every file Emacs opens. The prompt files only ever arrive through the
`emacsclient` editor wrapper, so we instead hook `server-visit-hook`, which
fires only for server-visited buffers — ordinary editing pays nothing. The
hook enables the mode when the wrapper has registered a context for the file
(the primary, wrapper-driven trigger) or, as a fallback for editor invocations
outside the wrapper, when the filename matches `claude-prompt-filename-regexp`.
_Alternative rejected:_ a global `find-file-hook` — needless per-file overhead
for a buffer that only ever opens via the server.

Cancel keeps `with-editor-cancel` as-is: it makes `emacsclient` exit non-zero,
which the wrapper uses to suppress auto-send and which Claude treats as "keep
original prompt". A bespoke revert-then-finish cancel is held in reserve only if
Claude misbehaves on non-zero editor exit. _Alternative rejected:_ rolling our
own server handshake — redundant given `with-editor` already handles the
non-spawned-client case.

### Decision: `history.jsonl` is the history store; cache by mtime

Claude already persists every submitted prompt to `~/.claude/history.jsonl`, so
the mode owns no persistence. The file is parsed (jq-free; `json-parse-buffer`
line by line) into a list of `(display . project)` cached against the file's
modification time, re-read only when mtime changes. New submissions reappear
automatically on the next open. _Alternative rejected:_ maintaining a separate
ring file — duplicates Claude's own log and risks drift.

### Decision: git anchor for current repo, string heuristic for history

The current repo root is authoritative and comes from git, but only the *live*
cwd can be queried — `git worktree list` omits deleted worktrees, of which the
history has many (50+ `__worktrees` projects, 1 live at inspection). So:
- The wrapper runs `git rev-parse --path-format=absolute --git-common-dir` in
  Claude's cwd and takes `dirname` → the main worktree root (resolves correctly
  even when invoked from a linked worktree). It passes this `repo_root` to Emacs
  via `emacsclient --eval` keyed by the prompt file path. Fallback to
  `--show-toplevel` for non-standard `.git` locations.
- Emacs classifies each history entry by normalising its `project` with an `rx`
  that strips a trailing `__worktrees/<name>` or `/.worktrees/<name>` segment,
  then comparing to `repo_root`. This is pure string math, so prompts from
  *deleted* worktrees still classify into the repo scope.

_Alternative rejected:_ classifying history entries with git — impossible for
deleted dirs, and a per-entry `git` call across ~300 projects is slow.
_Alternative rejected:_ a wrapper-stamped persistent cache of
`worktree_root → common_dir` — more state, only covers worktrees where the
editor was actually opened.

### Decision: side-channel is two `emacsclient` calls, keyed by path

`emacsclient --eval` returns immediately (non-blocking) while file-visiting
blocks until `server-done`. The wrapper therefore makes two calls: first
`emacsclient -e '(claude-prompt-register-context "<file>" "<repo_root>")'` to
stash an entry keyed by `(file-truename file)`, then
`emacsclient -nw -c <file>` to open and block. The registered entry both arms
activation (`server-visit-hook`, see above) and supplies the repo root: setup
looks the context up by `buffer-file-name`, applies it buffer-local, and pops
the entry. Keyed by path → no race between the two calls. When no entry exists
(editor invoked outside the wrapper), repo root falls back to the newest
history entry.

### Decision: tmux/auto-send stays entirely in the wrapper

The Emacs layer carries only `repo_root` over the side-channel — no tmux pane,
no `send-keys`. Auto-submit is the wrapper's job: it blocks on
`emacsclient -nw -c`, and on a zero exit (finish) with an opt-in env flag set
and `$TMUX` present, issues `tmux send-keys -t "$TMUX_PANE" Enter`. A non-zero
exit (cancel) naturally suppresses it. This keeps the mode portable and free of
terminal concerns.

## Risks / Trade-offs

- **Side-channel contract is cross-repo.** The `repo_root` plumbing depends on
  the nix-configuration wrapper. Mitigation: graceful fallback to the
  newest-history-entry project so the mode is useful even before the wrapper
  lands.
- **`settings.env.EDITOR` reaching the spawned editor is unverified.** Whether
  Claude Code exports its `settings.env` into the external editor's environment
  must be confirmed empirically (wrapper-side concern, flagged in tasks).
- **Deleted-worktree classification is heuristic.** Relies on the workmux
  `__worktrees/` / `.worktrees/` naming; a non-conforming layout would land such
  prompts in global scope only. Acceptable — global still finds them.
- **`gfm-mode` overlays during editing.** Kept per user preference; the
  pretty-overlay engine runs on the prompt buffer. No action unless it proves
  distracting.
- **History parse cost.** ~8.5k lines parsed on first recall; mitigated by
  mtime-keyed caching so it happens at most once per log change.
