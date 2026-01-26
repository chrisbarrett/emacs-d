# Feature: lang-shscript

Shell script editing with Tree-sitter, auto-executable, and file templates.

## Files

| File                  | Purpose                            |
| :-------------------- | :--------------------------------- |
| init/init-shscript.el | Mode setup, templates, executable  |

## Packages

| Package    | Source   | Purpose                   |
| :--------- | :------- | :------------------------ |
| sh-script  | Built-in | Shell script major mode   |

## Behaviors

### Mode Detection

| Pattern                          | Mode          |
| :------------------------------- | :------------ |
| Shebang containing `nix-shell`   | bash-ts-mode  |

### Auto-Executable

| Hook             | Function                                          |
| :--------------- | :------------------------------------------------ |
| after-save-hook  | `executable-make-buffer-file-executable-if-script-p` |

When a shell script is saved, it's automatically made executable if it has a shebang line.

### Shell Detection Silencing

The `sh-set-shell` function is advised to suppress messages, avoiding noise when auto-detecting shell type.

### File Template

| Pattern              | Template                       |
| :------------------- | :----------------------------- |
| `*.sh`, `*.bash`, `*.zsh` | `shell-script.eld`        |

Template content varies by extension:
- `.zsh` files get `#!/usr/bin/env zsh`
- All others get `#!/usr/bin/env bash`

All include:
```bash
set -eu -o pipefail
```

### Tempel Snippets

| File          | Mode          | Snippets                              |
| :------------ | :------------ | :------------------------------------ |
| sh-base.eld   | sh-base-mode  | `dir` (script directory), `err` (stderr) |

## Testable Properties

1. Files with `nix-shell` shebang open in bash-ts-mode
2. `sh-set-shell` does not emit messages
3. New `.sh` files get bash shebang template
4. New `.zsh` files get zsh shebang template
5. Script files made executable on save
6. Tempel snippet `dir` available in shell modes
7. Tempel snippet `err` available in shell modes
