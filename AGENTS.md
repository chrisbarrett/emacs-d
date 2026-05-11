# AGENTS.md

`emacsclient` is available for interacting with the running `emacs` instance and
evaluating Elisp code.

When evaluating Lisp via `emacsclient`, use `save-excursion`,
`save-window-excursion`, `with-current-buffer` etc as appropriate to avoid
disrupting the user's buffer & window selection, or accidentally capturing their
inputs while they work.

### Sandboxed Emacs for risky experiments

For unattended work that may hang, crash, infinite-loop, or otherwise wedge
Emacs (e.g. driving overlay rebuilds under narrowing, soak-testing a minor
mode, calling fragile internals), spin up a separate daemon with its own
server name and `-Q` so the user's session is never disrupted.

Use a server name with prefix `emacs-claude-sandbox-` followed by a
unique suffix (PID, timestamp, or random tag) so concurrent agents don't
collide and `pkill` patterns can't hit unrelated processes. Bind the name
to a shell var for the session:

```bash
SANDBOX="emacs-claude-sandbox-$$"
emacs -Q --bg-daemon="$SANDBOX"
```

Call it via `-s`:

```bash
emacsclient -s "$SANDBOX" -e '<elisp>'
```

Load only what the experiment needs — add the module's `lib/` to `load-path`
and `(load …)` the specific files. Stub missing faces/vars rather than
booting the full init; this avoids elpaca/eln-cache write-contention with
the user's primary daemon.

Kill the sandbox between experiments to discard polluted state:

```bash
emacsclient -s "$SANDBOX" -e '(kill-emacs 0)'   # graceful
pkill -f "$SANDBOX"                             # hard, when wedged
```

Default socket: `${TMPDIR:-/tmp}/emacs$UID/<server-name>`. The sandbox
daemon writes to that path only; it never touches the user's socket.

Hang-safety: when an `emacsclient -s "$SANDBOX" -e …` call does not
return, do **not** keep retrying or sending further commands — `pkill`
the sandbox by its exact name and restart fresh. Never `pkill -f emacs`
or `pkill -f emacs-claude-sandbox-` unqualified — both can hit other
agents' sandboxes; always include your unique suffix.

## Commands

| Task             | Command                 |
| :--------------- | :---------------------- |
| All checks       | `make test`             |
| Quick tests      | `make test-quick`       |
| Integration test | `make test-integration` |
| Setup hooks      | `make setup-hooks`      |

### Nix devShell for git commands

Run `git commit`, `git rebase`, and other git ops via
`nix develop --command bash -c '<cmd>'`. The devShell exports
`TREESIT_EXTRA_LOAD_PATH` pointing at the bundled tree-sitter grammars;
the prek pre-commit hook needs that path so tests like
`lang-markdown/gfm-code-fences-yaml-*` can load `yaml-ts-mode`'s
grammar. Outside the devShell those tests fail with `(eq yaml-ts-mode
nil)` and the hook blocks the commit.

## Structure

| Path                 | Purpose                                                   |
| :------------------- | :-------------------------------------------------------- |
| `early-init.el`      | Pre-display settings                                      |
| `init.el`            | Main Emacs config entrypoint; orchestrates module loading |
| `lisp/`              | Elisp libs used in config                                 |
| `modules/`           | Self-contained configuration units; most config in these  |
| `templates/`         | File templates                                            |
| `capture-templates/` | Org capture templates                                     |
| `site/`              | Site-specific (not committed)                             |
