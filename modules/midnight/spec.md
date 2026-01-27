# Feature: Midnight

Automatic buffer cleanup at midnight.

## Summary

Runs a cleanup hook at midnight to prevent long-running Emacs sessions from accumulating stale buffers. Useful for Emacs server instances that stay open for days or weeks.

## Dependencies

### Built-in Features

| Feature  | Purpose                     |
| :------- | :-------------------------- |
| midnight | Periodic cleanup scheduler  |

## Behavior

### B1: Activation

**Given** a file or buffer is opened
**When** `+first-file-hook` or `+first-buffer-hook` runs
**Then** `midnight-mode` is enabled

### B2: Buffer Cleanup

**Given** `midnight-mode` is active
**When** the clock reaches midnight
**Then** `clean-buffer-list` removes old, unmodified buffers

### B3: Customization

**Given** `midnight-mode` is active
**When** the user sets `clean-buffer-list-delay-general`
**Then** buffers older than that many days are cleaned

## Configuration

Buffer cleanup behavior is customized via:

| Variable                         | Default | Purpose                          |
| :------------------------------- | :------ | :------------------------------- |
| `clean-buffer-list-delay-general` | 3       | Days before cleanup (general)    |
| `clean-buffer-list-delay-special` | 1       | Days before cleanup (special)    |
| `clean-buffer-list-kill-regexps`  | nil     | Patterns to always kill          |
| `clean-buffer-list-kill-never-regexps` | ...| Patterns to never kill           |

## Properties to Verify

### P1: Midnight Mode Active

```elisp
;; Given first-file-hook has run
;; Then midnight-mode is enabled
(run-hooks '+first-file-hook)
(should (bound-and-true-p midnight-mode))
```

## Files

| File    | Purpose               |
| :------ | :-------------------- |
| init.el | Package configuration |
