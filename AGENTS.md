# AGENTS.md

Emacs 30+ configuration with Elpaca, Evil mode, and modular architecture.

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
5. `init/*.el` bootstrap files (hooks)
6. Module `init.el` files via `+modules-load-inits`
7. `site/*.el` local customizations

**Bootstrap files cannot be migrated to modules** because they define
infrastructure that modules depend on:

| File            | Why It's Pre-Module                          |
| :-------------- | :------------------------------------------- |
| `init-hooks.el` | Defines `+first-*-hook` used by `:after-call`|

**Module load-path** is set up in early-init.el before requiring module files.
Only directories containing canonical module files (init.el, lib.el, packages.eld)
are added, so `theme-lib.el` from `modules/theme/` is available for early-init.

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
