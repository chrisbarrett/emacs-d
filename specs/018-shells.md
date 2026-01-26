# Feature: Shells

Eshell and terminal emulator configuration for Emacs.

## Files

| File                   | Purpose                                |
| :--------------------- | :------------------------------------- |
| `init/init-shells.el`  | Eshell and eat terminal configuration  |
| `config/mod-eshell.el` | Eshell custom commands (zoxide, git)   |

## Dependencies

### External Packages

- `eat` - Terminal emulator with xterm-256color support

### Built-in Features

- `eshell` - Emacs Lisp shell
- `em-dirs` - Eshell directory commands

### External Tools

- `zoxide` - Smart directory jumper (must be in PATH)

## Behavior

### Eshell Configuration

| Setting          | Value                           |
| :--------------- | :------------------------------ |
| Extra commands   | Loaded via mod-eshell on demand |

### Eat Terminal

| Setting                  | Value              |
| :----------------------- | :----------------- |
| `eat-term-name`          | `xterm-256color`   |
| `eat-kill-buffer-on-exit`| `t`                |

### Eat Keybindings

In `eat-semi-char-mode-map`:

| Key         | Behavior                           |
| :---------- | :--------------------------------- |
| `s-v`       | `eat-yank` (paste)                 |
| `C-u`       | Pass through to terminal           |
| `C-o`       | Pass through to terminal           |
| `[escape]`  | Pass through to terminal           |
| `M-B`       | Intercepted (nil)                  |
| `M-P`       | Intercepted (nil)                  |
| `M-m`       | Intercepted (nil)                  |
| `M-o`       | Intercepted (nil)                  |
| `M-<`       | Intercepted (nil)                  |
| `M->`       | Intercepted (nil)                  |
| `M-_`       | Intercepted (nil)                  |

### Evil Integration

- `eat` added to `+evil-collection-disabled-list` (no evil-collection bindings)
- Buffer regex `"^*eat"` added to `evil-buffer-regexps` (evil disabled in eat)

### Eat Scroll Behavior

Hook: `eat-mode-hook`

| Setting                   | Value |
| :------------------------ | :---- |
| `scroll-conservatively`   | 101   |
| `maximum-scroll-margin`   | 0.5   |

### Zoxide Integration (Eshell)

Custom eshell command `j` for zoxide navigation:

**Given** zoxide is installed
**When** user runs `j <query>` in eshell
**Then** cd to the zoxide-matched directory

**Given** eshell cd is invoked with arguments
**When** cd completes
**Then** zoxide database is updated via `zoxide add`

The zoxide update is suppressed when `+eshell-suppress-zoxide-updates-p` is let-bound to t (e.g., during programmatic navigation).

### Git Root Navigation (Eshell)

Custom eshell command `g` for git root navigation:

**Given** current directory is inside a git repository
**When** user runs `g` in eshell
**Then** cd to the git repository root

**Given** current directory is not in a git repo but is in a project
**When** user runs `g` in eshell
**Then** cd to the project root

**Given** current directory is not in a git repo or project
**When** user runs `g` in eshell
**Then** signal user-error "Not in a project or git repo"

## API

### Commands (Eshell)

| Command      | Description                    |
| :----------- | :----------------------------- |
| `eshell/j`   | Zoxide jump to directory       |
| `eshell/g`   | Jump to git/project root       |

### Functions

| Function         | Description                              |
| :--------------- | :--------------------------------------- |
| `+zoxide-query`  | Query zoxide for directory match         |
| `+zoxide-add`    | Add directory to zoxide database         |

### Variables

| Variable                             | Description                       |
| :----------------------------------- | :-------------------------------- |
| `+eshell-suppress-zoxide-updates-p`  | Suppress zoxide updates when t    |

### Provided Features

| Feature        |
| :------------- |
| `init-shells`  |
| `mod-eshell`   |

## Properties to Verify

1. `eat-term-name` is `"xterm-256color"`
2. `eat-kill-buffer-on-exit` is t
3. `s-v` in eat-semi-char-mode-map binds to `eat-yank`
4. `+zoxide-query` calls zoxide binary with query
5. `eshell/j` changes directory to zoxide result
6. `eshell/cd` advice updates zoxide database
7. `eshell/g` finds git root via `locate-dominating-file`
8. Evil is disabled in eat buffers (evil-buffer-regexps match)
9. `+evil-collection-disabled-list` includes `eat`
