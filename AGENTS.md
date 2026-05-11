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

Start:

```bash
emacs -Q --bg-daemon=claude-sandbox
```

Call it via `-s`:

```bash
emacsclient -s claude-sandbox -e '<elisp>'
```

Load only what the experiment needs — add the module's `lib/` to `load-path`
and `(load …)` the specific files. Stub missing faces/vars rather than
booting the full init; this avoids elpaca/eln-cache write-contention with
the user's primary daemon.

Kill the sandbox between experiments to discard polluted state:

```bash
emacsclient -s claude-sandbox -e '(kill-emacs 0)'   # graceful
pkill -f 'claude-sandbox'                           # hard, when wedged
```

Default socket: `${TMPDIR:-/tmp}/emacs$UID/<server-name>`. The sandbox
daemon writes to that path only; it never touches the user's socket.

Hang-safety: when an `emacsclient -s claude-sandbox -e …` call does not
return, do **not** keep retrying or sending further commands — `pkill` the
sandbox and restart fresh. Never `pkill -f emacs` unqualified; always
include the server name.

## Commands

| Task             | Command                 |
| :--------------- | :---------------------- |
| All checks       | `make test`             |
| Quick tests      | `make test-quick`       |
| Integration test | `make test-integration` |
| Setup hooks      | `make setup-hooks`      |

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
