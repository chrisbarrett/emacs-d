# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Overview

This is a sophisticated Emacs configuration using Elpaca package manager with a
modular architecture. It's designed for Emacs 30+ and uses Evil mode for
vim-like keybindings.

## Core Architecture

### File Structure

- `init.el` - Main configuration entry point
- `early-init.el` - Early initialization (UI, themes, performance)
- `lisp/` - Core utility libraries and helpers
- `modules/` - Feature-specific configuration modules
- `templates/` - File templates and snippets
- `capture-templates/` - Org-mode capture templates
- `site/` - Site-specific customizations

### Key Libraries

- `+corelib.el` - Core utilities, logging, and macros
- `+load-incrementally.el` - Deferred package loading system
- `+compile.el` - Enhanced compilation support
- `+theme.el` - Theme management utilities

### Modules

Each module in `modules/` configures a specific feature:

- `mod-evil.el` - Evil mode configuration
- `mod-org*.el` - Org-mode related modules
- `mod-emacs-lisp.el` - Emacs Lisp development
- `mod-compilation.el` - Compilation output handling

## Development Commands

### Emacs Lisp Development

- **Run tests**: `SPC m t t` (runs ERT tests)
- **Macro expansion**: `C-c RET` for `pp-macroexpand-last-sexp`
- When asked to evaluate a buffer, use
  `emacsclient --eval '(with-current-buffer (find-file-noselect "$<file>") (eval-buffer))'`

### Testing

- ERT is used for Emacs Lisp testing
- Tests follow `-tests.el` naming convention
- Use `find-sibling-rules` to navigate between implementation and tests

### Compilation

- Custom compilation error parsers in `mod-compilation.el`
- ANSI color support in compilation buffers
- URL highlighting and navigation in compilation output

### File Templates

- Located in `templates/` directory
- Support for multiple languages: Elixir, Rust, TypeScript, etc.
- Template system integrated with file creation

## Key Configuration Patterns

### Package Management

- Uses Elpaca with `use-package` integration
- Packages are deferred by default (`use-package-always-defer t`)
- Expensive packages are loaded incrementally to optimize startup

### Hook System

- Custom transient hooks: `+first-input-hook`, `+first-file-hook`,
  `+first-buffer-hook`
- Switch hooks: `+switch-buffer-hook`, `+switch-window-hook`,
  `+switch-frame-hook`

### Theming

- Automatic light/dark switching managed by desktop environment
- `catppuccin` (mocha) is the dark theme; `modus-operandi-tinted` is the light
  theme
- Custom face configurations for better visual hierarchy
- Fira Code font with ligature support

## Directory Variables

- `org-directory` - Default: `~/org/`
- `org-roam-directory` - Default: `~/org/roam/`
- `+site-files-directory` - Site-specific files
- `+templates-dir` - File templates location

## Utility Scripts

- `scripts/update-agenda-files.sh` - Updates org-agenda files automatically

## Notes

- Configuration requires Emacs 30+
- Evil mode is deeply integrated
- No littering approach with organized directory structure

## Development Best Practices

- Use the full hook symbol when settings hooks via the `:hooks` form in
  `use-package`
- Use general-def or :general in use-package as appropriate when setting
  keybindings
