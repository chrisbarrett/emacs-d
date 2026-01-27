# Feature: Tramp

Remote file editing configuration.

## Summary

Configures TRAMP for editing files on remote systems via SSH and other protocols.

## Dependencies

### Built-in Features

| Feature | Purpose                    |
| :------ | :------------------------- |
| tramp   | Transparent Remote Access  |

## Behavior

### B1: Remote Path Configuration

**Given** TRAMP connects to a remote host
**When** executing commands remotely
**Then** the remote user's PATH is included via `tramp-own-remote-path`

## Configuration

| Variable            | Value                     | Purpose                        |
| :------------------ | :------------------------ | :----------------------------- |
| `tramp-remote-path` | includes `tramp-own-remote-path` | Use remote user's PATH   |

## Properties to Verify

### P1: Own Remote Path Configured

```elisp
;; Given tramp module loaded
;; Then tramp-own-remote-path is in tramp-remote-path
(require 'tramp)
(should (memq 'tramp-own-remote-path tramp-remote-path))
```

## Files

| File    | Purpose               |
| :------ | :-------------------- |
| init.el | Package configuration |
