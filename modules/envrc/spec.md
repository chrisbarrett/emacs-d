# Feature: Envrc

Direnv integration for per-directory environment variables.

## Summary

Automatically loads `.envrc` files via direnv, ensuring shell environment variables are available in Emacs buffers. Integrates with org-babel so source blocks execute in the host buffer's environment.

## Dependencies

### External Packages

| Package | Purpose                        |
| :------ | :----------------------------- |
| envrc   | Direnv integration for Emacs   |

### External Tools

| Tool   | Purpose                              |
| :----- | :----------------------------------- |
| direnv | Per-directory environment management |

## Behavior

### B1: Global Activation

**Given** a file is opened
**When** `+first-file-hook` runs
**Then** `envrc-global-mode` is enabled

### B2: Environment Loading

**Given** `envrc-global-mode` is active
**When** visiting a file in a directory with `.envrc`
**Then** the environment is loaded before major-mode hooks run

### B3: Org Babel Integration

**Given** an org file with source blocks
**When** executing a source block with `org-babel-execute-src-block`
**Then** the block runs with the buffer's direnv environment

### B4: Silent Operation

**Given** envrc loads an environment
**When** the environment changes
**Then** no minibuffer summary is shown (quiet operation)

## Configuration

| Variable                        | Value | Purpose                |
| :------------------------------ | :---- | :--------------------- |
| `envrc-show-summary-in-minibuffer` | nil   | Suppress noisy messages |

## Properties to Verify

### P1: Global Mode Activates

```elisp
;; Given first-file-hook has run
;; Then envrc-global-mode is enabled
(run-hooks '+first-file-hook)
(should (bound-and-true-p envrc-global-mode))
```

### P2: Org Babel Advised

```elisp
;; Given envrc module loaded
;; Then org-babel-execute-src-block is advised
(should (advice-member-p #'envrc-propagate-environment #'org-babel-execute-src-block))
```

### P3: Mode Setup Hook Installed

```elisp
;; Given envrc-global-mode is active
;; Then environment loads before major-mode body
(should (memq #'+direnv-mode-setup-h envrc-global-mode-hook))
```

## Files

| File    | Purpose               |
| :------ | :-------------------- |
| init.el | Package configuration |
