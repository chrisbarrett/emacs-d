# AGENTS.md

`emacsclient` is available for interacting with the running `emacs` instance and
evaluating Elisp code.

When evaluating Lisp via `emacsclient`, use `save-excursion`,
`save-window-excursion`, `with-current-buffer` etc as appropriate to avoid
disrupting the user's buffer & window selection, or accidentally capturing their
inputs while they work.

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
