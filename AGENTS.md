# AGENTS.md

## Commands

| Task             | Command                 |
| :--------------- | :---------------------- |
| All checks       | `make test`             |
| Quick tests      | `make test-quick`       |
| Integration test | `make test-integration` |
| Setup hooks      | `make setup-hooks`      |

## Structure

| Path                 | Purpose                       |
| :------------------- | :---------------------------- |
| `init.el`            | Entry point                   |
| `early-init.el`      | UI, themes, performance       |
| `init/`              | Bootstrap files (pre-module)  |
| `lisp/`              | Core libraries (`+*.el`)      |
| `modules/`           | Self-contained module units   |
| `specs/`             | Active spec symlinks          |
| `templates/`         | File templates                |
| `capture-templates/` | Org capture templates         |
| `site/`              | Site-specific (not committed) |

### Bootstrap Architecture

The init sequence loads files in this order:

1. `early-init.el` → `+corelib.el`, `+use-package-keywords.el`, `theme-lib.el`
2. `init.el` → Elpaca bootstrap, `+modules.el`
3. Module packages queued via `+modules-install-packages`
4. Module autoloads registered via `+modules-register-autoloads`
5. `lisp/+hooks.el` → lifecycle hooks
6. `init/*.el` bootstrap files
7. Module `init.el` files via `+modules-load-inits`
8. `site/*.el` local customizations

## Module System

Modules are self-contained units under `modules/{slug}/`:

| File           | Purpose                     |
| :------------- | :-------------------------- |
| `spec.md`      | Module specification        |
| `packages.eld` | Elpaca specs (data only)    |
| `init.el`      | Evaluated during init       |
| `lib.el`       | Autoloaded functions        |
| `lib/*.el`     | Alternative: multiple libs  |
| `tests.el`     | ERT tests                   |

## Code Style

- Avoid excessive comments in generated code
- Use full hook symbols in `use-package` `:hook` forms
- Use `general-def` or `:general` for keybindings
- Tests use `-tests.el` suffix; use `find-sibling-rules` to navigate
