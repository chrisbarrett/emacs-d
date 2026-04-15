# AGENTS.md

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
