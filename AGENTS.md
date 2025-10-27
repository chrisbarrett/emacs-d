# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a sophisticated Emacs configuration using Elpaca package manager with a modular architecture. It's designed for Emacs 30+ and uses Evil mode for vim-like keybindings.

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
- When asked to evaluate a buffer, use `emacsclient --eval '(with-current-buffer (find-file-noselect "$<file>") (eval-buffer))'`

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
- Custom transient hooks: `+first-input-hook`, `+first-file-hook`, `+first-buffer-hook`
- Switch hooks: `+switch-buffer-hook`, `+switch-window-hook`, `+switch-frame-hook`

### Theming
- Modus themes with automatic light/dark switching
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
- Heavy use of lexical binding throughout
- Evil mode is deeply integrated
- No littering approach with organized directory structure

## Development Best Practices
- Use the full hook symbol when settings hooks via the `:hooks` form in `use-package`
- Use general-def or :general in use-package as appropriate when setting keybindings

## Issue Tracking with bd (beads)

> [!IMPORTANT] This project uses **bd (beads)** for ALL issue tracking. Do NOT
> use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`

> [!IMPORTANT] When closing an issue, ALWAYS include the commit SHA(s) that
> resolved it in the close reason. Example:
>
> ```bash
> bd close terrace-69 --reason "Completed in 61eda2a: Removed true/false as keywords"
> ```
>
> This creates traceability between issues and code changes.

### Auto-Sync

bd automatically syncs with git:

- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### MCP Server (Recommended)

If using Claude or MCP-compatible clients, install the beads MCP server:

```bash
pip install beads-mcp
```

Add to MCP config (e.g., `~/.config/claude/config.json`):

```json
{
  "beads": {
    "command": "beads-mcp",
    "args": []
  }
}
```

Then use `mcp__beads__*` functions instead of CLI commands.

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and QUICKSTART.md.
