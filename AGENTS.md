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
| `modules/`           | Self-contained module units   |
| `specs/`             | Active spec symlinks          |
| `templates/`         | File templates                |
| `capture-templates/` | Org capture templates         |
| `site/`              | Site-specific (not committed) |

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

### Active Specs Workflow

Active specs are symlinks in `specs/` pointing to `modules/{slug}/spec.md`.

**Promote** a spec (mark as active):

```bash
ln -s ../modules/{slug}/spec.md specs/NNN-{slug}.md
```

**Demote** a spec (mark as stable):

```bash
rm specs/NNN-{slug}.md
```

**Verify** active specs:

```bash
readlink specs/*.md  # Should show ../modules/{slug}/spec.md paths
```

Spec numbers (NNN) indicate priority order during development.

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
