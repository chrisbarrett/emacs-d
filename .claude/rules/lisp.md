---
paths:
  - "**/*.el"
---

## Module System

Modules are self-contained units under `modules/{slug}/`:

| File           | Purpose                    |
| :------------- | :------------------------- |
| `spec.md`      | Module specification       |
| `packages.eld` | Elpaca specs (data only)   |
| `init.el`      | Evaluated during init      |
| `lib.el`       | Autoloaded functions       |
| `lib/*.el`     | Alternative: multiple libs |
| `tests.el`     | ERT tests                  |

## Code Style

This is a personal Emacs configuration. All references are internal to the
codebase.

- Avoid excessive comments in generated code
- Use full hook symbols in `use-package` `:hook` forms
- Use `general-def` or `:general` for keybindings
- Tests use `-tests.el` suffix; use `find-sibling-rules` to navigate
