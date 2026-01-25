# AGENTS.md

Emacs 30+ configuration with Elpaca, Evil mode, and modular architecture.

## Commands

| Task        | Command            |
| :---------- | :----------------- |
| All checks  | `make test`        |
| Quick tests | `make test-quick`  |
| Setup hooks | `make setup-hooks` |

## Structure

| Path                 | Purpose                       |
| :------------------- | :---------------------------- |
| `init.el`            | Entry point                   |
| `early-init.el`      | UI, themes, performance       |
| `lisp/`              | Core libraries (`+*.el`)      |
| `config/`            | Feature modules (`mod-*.el`)  |
| `templates/`         | File templates                |
| `capture-templates/` | Org capture templates         |
| `site/`              | Site-specific (not committed) |

## Code Style

- Avoid excessive comments in generated code
- Use full hook symbols in `use-package` `:hook` forms
- Use `general-def` or `:general` for keybindings
- Tests use `-tests.el` suffix; use `find-sibling-rules` to navigate

## Key Patterns

- Packages deferred by default (`use-package-always-defer t`)
- Custom hooks: `+first-input-hook`, `+first-file-hook`, `+switch-buffer-hook`
- Themes: `catppuccin` (dark), `modus-operandi-tinted` (light)

## Session Completion

> [!CRITICAL]
> Work is NOT complete until `git push` succeeds.

1. File issues for remaining work
2. Run `make test` if code changed
3. Push to remote:
   ```bash
   git pull --rebase && bd sync && git push
   ```
4. Verify: `git status` shows "up to date with origin"
