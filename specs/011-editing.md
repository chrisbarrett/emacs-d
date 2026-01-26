# Editing Spec

General editing behavior: autorevert, recentf, uniquify, file IO settings.

## Files

| File              | Purpose                              |
| :---------------- | :----------------------------------- |
| init-editing.el   | Core editing settings and behaviors  |

## Dependencies

**Built-in:** simple, files, uniquify, autorevert, recentf
**Internal:** +corelib (+visible-buffers, +switch-buffer-hook, +switch-window-hook)

## Behaviors

### Disabled Commands Re-enabled

| Command           | Effect                           |
| :---------------- | :------------------------------- |
| erase-buffer      | No longer prompts for confirmation |
| narrow-to-region  | No longer prompts for confirmation |
| downcase-region   | No longer prompts for confirmation |

### macOS Keybindings Removed

On Darwin systems, these super-key bindings are unset:

| Binding | Default Action (removed) |
| :------ | :----------------------- |
| s-o     | Open file                |
| s-f     | Search                   |
| s-q     | Quit                     |
| s-t     | Font dialog              |
| s-n     | New frame                |

### Fill and Wrap Settings

| Setting                     | Value   | Effect                               |
| :-------------------------- | :------ | :----------------------------------- |
| fill-column                 | 80      | Default wrap column                  |
| word-wrap                   | t       | Break at word boundaries             |
| truncate-lines              | t       | No visual line wrapping (default)    |
| truncate-partial-width-windows | nil | Partial windows respect truncate-lines |

### Sound and Lockfiles

| Setting             | Value    | Effect                    |
| :------------------ | :------- | :------------------------ |
| ring-bell-function  | #'ignore | No audible/visual bell    |
| create-lockfiles    | nil      | No .#file lock files      |

### simple.el Settings

| Setting                      | Value                                    |
| :--------------------------- | :--------------------------------------- |
| kill-do-not-save-duplicates  | t                                        |
| read-extended-command-predicate | command-completion-default-include-p |
| indent-tabs-mode             | nil (default off)                        |

### Minibuffer Behavior

| Behavior                    | Description                              |
| :-------------------------- | :--------------------------------------- |
| cursor-intangible-mode      | Cursor cannot enter prompt               |
| C-p / C-n                   | Navigate history with line movement      |

### files.el Settings

| Setting                         | Value | Effect                               |
| :------------------------------ | :---- | :----------------------------------- |
| backup-inhibited                | t     | No backup files                      |
| make-backup-files               | nil   | No ~file backups                     |
| require-final-newline           | t     | Ensure files end with newline        |
| find-file-visit-truename        | t     | Follow symlinks                      |
| confirm-nonexistent-file-or-buffer | nil | No prompt for new files           |
| auto-mode-case-fold             | nil   | Case-sensitive auto-mode matching    |

### Autosave Exists Prompt Suppressed

The `after-find-file` advice prevents blocking prompt when autosave exists.

### Uniquify Settings

| Setting                   | Value    | Effect                           |
| :------------------------ | :------- | :------------------------------- |
| uniquify-buffer-name-style | forward | Show path prefix for disambiguation |

### Autorevert Configuration

Uses custom hooks instead of file watcher for efficiency.

| Hook                    | Action                          |
| :---------------------- | :------------------------------ |
| after-save-hook         | Revert visible buffers          |
| +switch-buffer-hook     | Revert current buffer           |
| +switch-window-hook     | Revert current buffer           |
| after-focus-change-function | Revert visible buffers      |

| Setting                   | Value | Effect                              |
| :------------------------ | :---- | :---------------------------------- |
| auto-revert-use-notify    | nil   | Disable file watcher                |
| auto-revert-stop-on-user-input | nil | Don't interrupt revert for input |
| revert-without-query      | "."   | Revert any file without prompting   |

### Recentf Configuration

| Trigger                | Mode     |
| :--------------------- | :------- |
| :after-call recentf    | Deferred |
| :after-call consult-buffer | Deferred |

| Setting               | Value | Effect                    |
| :-------------------- | :---- | :------------------------ |
| recentf-max-saved-items | 100 | Max files in history      |

### Keybinding

| Binding    | Command                |
| :--------- | :--------------------- |
| C-c e e    | toggle-debug-on-error  |

## API

### Functions

| Function                        | Description                                  |
| :------------------------------ | :------------------------------------------- |
| +auto-revert-current-buffer-h   | Revert current buffer if file-backed and stale |
| +auto-revert-visible-buffers-h  | Revert all visible file buffers              |

## Testable Properties

1. `erase-buffer` is not disabled (get 'erase-buffer 'disabled) returns nil
2. `narrow-to-region` is not disabled
3. `downcase-region` is not disabled
4. `fill-column` default is 80
5. `indent-tabs-mode` default is nil
6. `create-lockfiles` is nil
7. `uniquify-buffer-name-style` is 'forward
8. `recentf-mode` is enabled after trigger
9. `auto-revert-use-notify` is nil
