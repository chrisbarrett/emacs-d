# Feature: Terminal Compatibility

Clipboard and mouse support for terminal Emacs sessions.

## Summary

Enables GUI-like clipboard and mouse behavior when running Emacs in a terminal emulator.

## Dependencies

### External Packages

| Package   | Purpose                              |
| :-------- | :----------------------------------- |
| clipetty  | Copy to system clipboard in terminal |

### Built-in Features

| Feature  | Purpose                    |
| :------- | :------------------------- |
| xt-mouse | Mouse support in xterm     |

## Behavior

### B1: Clipboard Integration

**Given** Emacs is running in a terminal
**When** the user copies text
**Then** text is copied to the system clipboard via OSC 52

### B2: Mouse Support

**Given** Emacs starts
**When** running in a terminal with xterm compatibility
**Then** mouse clicks and scrolling work as expected

## Properties to Verify

### P1: Clipetty Mode Active

```elisp
;; Given terminal-compat module loaded
;; Then global-clipetty-mode is enabled
(should (bound-and-true-p global-clipetty-mode))
```

### P2: Xterm Mouse Mode Active

```elisp
;; Given terminal-compat module loaded
;; Then xterm-mouse-mode is enabled
(should (bound-and-true-p xterm-mouse-mode))
```

## Files

| File    | Purpose                    |
| :------ | :------------------------- |
| init.el | Package configuration      |
