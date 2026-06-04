## Why

When Claude Code's external editor fires (`$EDITOR` on `/tmp/claude-$UID/claude-prompt-$GUID.md`), the prompt opens in a bare `gfm-mode` buffer with no submit/cancel affordance, no recall of past prompts, and no return path other than a manual `server-edit`. A `git-commit`-style editing surface — explicit finish/cancel, a searchable history of prior prompts, and optional auto-submit — makes prompt authoring in Emacs first-class.

## What Changes

- New `+claude-prompt-mode`: a minor mode layered on the prompt file's `gfm-mode`, activated via `find-file-hook` when the buffer's filename matches the Claude prompt-file pattern.
- Built on `with-editor-mode` (already present as a Magit dependency): **C-c C-c** saves + `server-done` (returns text to Claude); **C-c C-k** cancels (`with-editor-cancel`, exits non-zero); kill-buffer protection; a usage message on open.
- History recall from `~/.claude/history.jsonl` (Claude's own prompt log): **M-p / M-n** cycle a repo-scoped, reverse-chronological, deduplicated ring; **C-r** opens a `consult--multi` picker with narrow keys `[r]` repo / `[g]` global.
- Repo scoping: the current repo root is supplied by the editor wrapper (git-derived) via an `emacsclient --eval` side-channel; historical entries are classified by normalising their `project` path against that root (string heuristic, so deleted worktrees still classify).
- Customised mode-line lighter and an on-open help message listing the bindings.
- **Coordinated (nix-configuration repo, out of scope here):** a `claude-prompt-edit` wrapper set as Claude Code's `settings.env.EDITOR`. The wrapper computes `repo_root` via git, passes it to Emacs through the eval side-channel, runs the blocking edit, and — gated on a non-zero exit and an opt-in env flag — performs tmux auto-submit (`tmux send-keys … Enter`). All tmux/auto-send logic lives in the wrapper; the Emacs layer is tmux-agnostic.

## Capabilities

### New Capabilities
- `claude`: behaviour of the Claude Code integration module — the prompt-editing minor mode, its finish/cancel keys and mode-line, the history ring, and the `consult` recall picker with repo/global scoping. (New axis; module `modules/claude` already exists. See "Decision: new axis claude" in design.md.)

### Modified Capabilities
<!-- none -->

## Impact

- `modules/claude/`: new `lib/claude-prompt.el` (regexp, context register, setup, ring, consult sources, repo classification), activation wiring in `init.el`, `packages.eld` (declare `with-editor`/`consult` availability — both already built), tests in `tests.el`.
- New `openspec/specs/claude/spec.md` (new axis).
- No new external packages: `with-editor` (Magit) and `consult` are already installed.
- Reads `~/.claude/history.jsonl` (cached by mtime); reads nothing else outside the buffer.
- Cross-repo dependency on the nix-configuration `EDITOR` wrapper for repo scoping and auto-send; the mode degrades gracefully (newest-history-entry fallback for repo root) when the side-channel is absent.
