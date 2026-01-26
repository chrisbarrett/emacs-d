# Eglot Feature Specification

LSP client configuration with flymake integration and performance booster.

## Files

| File               | Purpose                             |
| :----------------- | :---------------------------------- |
| init/init-eglot.el | Flymake, eglot, eglot-booster setup |

## Packages

| Package       | Source                            | Purpose                   |
| :------------ | :-------------------------------- | :------------------------ |
| flymake       | built-in                          | In-buffer error checking  |
| eglot         | built-in                          | LSP client                |
| eglot-booster | github:jdtsmith/eglot-booster     | Performance optimization  |

## Behaviors

### Flymake

| Behavior                        | Trigger           | Result                            |
| :------------------------------ | :---------------- | :-------------------------------- |
| Enable flymake                  | `prog-mode-hook`  | Flymake active in programming     |
| Navigate to next error          | `M-n`             | Jump to next flymake diagnostic   |
| Navigate to previous error      | `M-p`             | Jump to previous flymake error    |
| Full diagnostic display         | Error at point    | Shows origin, code, and message   |

### Eglot

| Behavior                        | Trigger           | Result                            |
| :------------------------------ | :---------------- | :-------------------------------- |
| Code actions                    | `M-RET`           | Show available code actions       |
| Rename symbol                   | `C-c C-r`         | Rename symbol across project      |
| Code action indicator           | LSP available     | Empty string (no indicator)       |
| Inlay hints disabled            | Eglot connects    | Inlay hints mode turned off       |
| Open markdown links             | `RET` in eldoc    | Browse URL from hover docs        |

### Eglot-booster

| Behavior                        | Trigger           | Result                            |
| :------------------------------ | :---------------- | :-------------------------------- |
| Performance boost               | After eglot loads | `eglot-booster-mode` enabled      |

## API

### Commands

| Command                    | Description                        | Keybinding         |
| :------------------------- | :--------------------------------- | :----------------- |
| `flymake-goto-next-error`  | Navigate to next error             | `M-n`              |
| `flymake-goto-prev-error`  | Navigate to previous error         | `M-p`              |
| `eglot-code-actions`       | Show code actions at point         | `M-RET`            |
| `eglot-rename`             | Rename symbol at point             | `C-c C-r`          |
| `+eglot-open-link`         | Open URL in eldoc buffer           | `RET` (in eldoc)   |

### Variables

| Variable                   | Default | Description                       |
| :------------------------- | :------ | :-------------------------------- |
| `eglot-code-action-indicator` | `""`  | Indicator when actions available  |

### Hooks

| Hook                           | When triggered              |
| :----------------------------- | :-------------------------- |
| `eglot-managed-mode-hook`      | Eglot connects to buffer    |

## Design Decisions

### Flymake over Flycheck

Uses built-in flymake rather than flycheck:

- Built-in to Emacs, no external dependency
- Integrates natively with eglot
- Lighter weight

### Inlay Hints Disabled

Inlay hints are explicitly disabled:

- Can cause visual clutter
- May slow down rendering
- Type information available via hover

### Eglot-booster Enabled Globally

`eglot-booster-mode` is enabled globally after eglot loads:

- Requires external `emacs-lsp-booster` binary
- Significantly improves LSP communication performance
- Uses bytecode compilation for JSON parsing

### Eldoc Link Navigation

Custom advice adds `RET` binding in eldoc buffer:

- Allows clicking through documentation links
- Uses `help-echo` text property for URL detection
- Consistent with other link-opening patterns in config

## Testable Properties

1. `flymake-mode` is active in `prog-mode` buffers
2. `M-n` is bound to `flymake-goto-next-error` in flymake-mode-map
3. `M-p` is bound to `flymake-goto-prev-error` in flymake-mode-map
4. `eglot-code-action-indicator` is empty string
5. `M-RET` is bound to `eglot-code-actions` in eglot-mode-map (insert/normal)
6. `C-c C-r` is bound to `eglot-rename` in eglot-mode-map (normal)
7. `eglot-booster-mode` is enabled after eglot loads
8. `+eglot-inlay-hints-off` is on `eglot-managed-mode-hook`
9. `+eglot-open-link` is defined and interactive

## Dependencies

### Required

- `+corelib` for `alist-set!`
- `general` for keybinding definitions

### External Tools

- `emacs-lsp-booster` binary for eglot-booster functionality
