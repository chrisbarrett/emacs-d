# Feature: lang-conf

Configuration file format support for JSON, YAML, KDL, and Unix config files.

## Files

| File             | Purpose                          |
| :--------------- | :------------------------------- |
| init/init-conf.el | Mode associations and LSP setup |

## Packages

| Package       | Source   | Purpose                        |
| :------------ | :------- | :----------------------------- |
| conf-mode     | Built-in | Unix config file mode          |
| json-ts-mode  | Built-in | JSON with Tree-sitter          |
| yaml-ts-mode  | Built-in | YAML with Tree-sitter          |
| kdl-ts-mode   | External | KDL (Zellij config) with Tree-sitter |

## Behaviors

### Mode Associations

| Pattern          | Mode          |
| :--------------- | :------------ |
| `*rc`            | conf-mode     |
| `.dockerignore`  | conf-mode     |
| `.gitignore`     | conf-mode     |
| `*.kdl`          | kdl-ts-mode   |

### LSP Integration

| Mode          | Hook                           | Action        |
| :------------ | :----------------------------- | :------------ |
| json-ts-mode  | json-ts-mode-local-vars-hook   | eglot-ensure  |
| yaml-ts-mode  | yaml-ts-mode-local-vars-hook   | eglot-ensure  |

### YAML Settings

| Setting     | Value |
| :---------- | :---- |
| `tab-width` | 2     |

## External Tools

| Tool             | Purpose              |
| :--------------- | :------------------- |
| yaml-language-server | LSP for YAML     |
| vscode-json-languageserver | LSP for JSON |

## Testable Properties

1. Files ending in `rc` open in conf-mode
2. `.dockerignore` opens in conf-mode
3. `.gitignore` opens in conf-mode
4. `*.kdl` files open in kdl-ts-mode
5. `*.json` files have LSP via eglot
6. `*.yaml` files have LSP via eglot
7. YAML mode has tab-width 2
