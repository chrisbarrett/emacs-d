# claude Specification

## Purpose

Behaviour-facing. Covers the `claude-prompt-mode` editing surface for Claude
Code prompt files opened through the Emacs server (`modules/claude/`): a
`git-commit`-style finish/cancel session layered on `with-editor`, a mode-line
lighter and on-open help, a repo-scoped prompt-history ring sourced from
`~/.claude/history.jsonl`, a `consult`-based recall picker with repo and global
scopes, and the git-anchored repo classification that scopes both. No `claude`
axis existed under `openspec/specs/` but `modules/claude/` does, so this
introduces the `claude` behaviour axis (see the change's design.md, "Decision:
new axis `claude`"). The Emacs layer is tmux-agnostic; auto-send/tmux logic
lives in the external editor wrapper and is out of scope here.

## Requirements

### Requirement: Activation on Claude prompt files

`claude-prompt-mode` SHALL be a buffer-local minor mode layered on the file's
existing major mode (normally `gfm-mode`). Activation SHALL be driven by the
editor wrapper opening the prompt file through the Emacs server: a
`server-visit-hook` entry — which runs only for server-visited buffers, so
ordinary `find-file` visits incur no cost — SHALL enable the mode when either
the wrapper has registered a context for the file (the primary path, see "Repo
classification") or, as a fallback, the visited file's name matches the Claude
prompt-file pattern. That pattern — a `claude-prompt-<guid>.md` file inside a
`claude-<uid>` directory under the system temporary directory (e.g.
`/private/tmp/claude-503/claude-prompt-<guid>.md`) — SHALL be defined with
`rx`. The mode SHALL NOT be enabled from a global `find-file-hook`.

#### Scenario: Wrapper-opened prompt file enables the mode

- **WHEN** the editor wrapper registers a context for the prompt file and the server visits it
- **THEN** `claude-prompt-mode` is enabled in that buffer
- **AND** `with-editor-mode` is enabled in that buffer

#### Scenario: Prompt file enables the mode by pattern fallback

- **WHEN** the server visits `/private/tmp/claude-503/claude-prompt-abc.md` with no registered context
- **THEN** `claude-prompt-mode` is enabled in that buffer

#### Scenario: Unrelated markdown file does not enable the mode

- **WHEN** the server visits an ordinary `notes.md` with no registered context
- **THEN** `claude-prompt-mode` is not enabled

### Requirement: Finish and cancel via with-editor

The mode SHALL complete the edit session via `with-editor`'s `server-done`
with a zero exit status in both the finish and cancel cases, because Claude
Code crashes when its editor exits non-zero. `C-c C-c` SHALL save the buffer
and finish, returning the edited contents. `C-c C-k` and `C-x C-c` SHALL
cancel by restoring the buffer to the content Claude opened it with and then
finishing, so Claude receives the original prompt unchanged. Cancel SHALL NOT
use `with-editor-cancel` (which exits non-zero). The buffer SHALL be protected
from ordinary `kill-buffer` while the session is live.

#### Scenario: Finish returns contents

- **WHEN** the user edits the prompt and presses `C-c C-c`
- **THEN** the buffer is saved
- **AND** the server edit session completes with a zero exit so the Claude process resumes

#### Scenario: Cancel restores original and exits zero

- **WHEN** the user presses `C-c C-k` or `C-x C-c`
- **THEN** the buffer is restored to the content Claude opened it with
- **AND** the edit session finishes with a zero exit status

### Requirement: Mode-line lighter and on-open help

The mode SHALL display a distinct mode-line lighter identifying the Claude
prompt session. On activation it SHALL show a one-time, minimal help message in
the style of `git-commit-mode` — naming only the finish and cancel bindings,
not the full binding set.

#### Scenario: Help shown on open

- **WHEN** `claude-prompt-mode` activates in a buffer
- **THEN** a one-line message names the finish (`C-c C-c`) and cancel bindings

### Requirement: Repo-scoped prompt history ring

The mode SHALL provide a history ring of previously submitted prompts sourced
from Claude's prompt log at `~/.claude/history.jsonl`, where each line is a JSON
object with a `display` (prompt text) and `project` (working directory) field.
Entries SHALL be ordered most-recent-first and de-duplicated. `M-p` SHALL
replace the buffer body with the previous prompt in the ring and `M-n` with the
next. The ring SHALL be scoped to the current git repository (see "Repo
classification"). The parsed log SHALL be cached and re-read only when the
file's modification time changes.

#### Scenario: Cycle to previous prompt

- **WHEN** the user presses `M-p` in a fresh prompt buffer
- **THEN** the buffer body is replaced with the most recent prior prompt from the current repository

#### Scenario: Duplicate prompts collapse

- **WHEN** the history log contains the same prompt text consecutively
- **THEN** the ring presents it once

### Requirement: Recall search with repo and global scope

The mode SHALL bind `C-r` to a `consult`-based picker over the prompt history,
presented as a multi-source reader with narrow keys: `[r]` restricts to prompts
from the current git repository (the default source) and `[g]` spans all
projects. Selecting an entry SHALL insert its prompt text into the buffer. Each
candidate SHALL be annotated with its originating project.

#### Scenario: Repo scope is the default source

- **WHEN** the user presses `C-r`
- **THEN** a `consult` picker opens showing current-repository prompts first
- **AND** typing the `g` narrow key widens the candidates to all projects

#### Scenario: Selection inserts the prompt

- **WHEN** the user selects a candidate
- **THEN** the selected prompt text is inserted into the prompt buffer

### Requirement: Repo classification with git anchor and path fallback

The current repository root SHALL be supplied by the editor wrapper through an
`emacsclient --eval` side-channel keyed by the prompt file path, computed by git
(`git rev-parse --git-common-dir`, resolved to the main worktree root). When the
side-channel context is absent, the mode SHALL fall back to the `project` field
of the newest entry in the history log. A history entry SHALL be classified as
belonging to the current repository when its `project` path, after stripping any
trailing worktree segment (`__worktrees/<name>` or `/.worktrees/<name>` matched
with `rx`), equals the current repository root. The Emacs layer SHALL NOT
contain any tmux logic.

#### Scenario: Worktree entries classify to their repository

- **WHEN** the current repository root is `/repo` and the log contains a prompt from `/repo__worktrees/feature`
- **THEN** that prompt is included in the repo-scoped ring and the `[r]` source

#### Scenario: Fallback when side-channel absent

- **WHEN** no registered context exists for the prompt file
- **THEN** the current repository is taken from the newest history entry's `project`
