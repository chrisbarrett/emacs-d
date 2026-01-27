# Feature: Misery

Mise integration for tool version management.

## Summary

Integrates with [mise](https://mise.jdx.dev) to automatically configure tool versions (node, python, etc.) based on `.mise.toml` or `.tool-versions` files in the project.

## Dependencies

### External Packages

| Package | Source                              | Purpose              |
| :------ | :---------------------------------- | :------------------- |
| misery  | chrisbarrett/emacs-misery (GitHub)  | Mise integration     |

### External Tools

| Tool | Purpose                         |
| :--- | :------------------------------ |
| mise | Polyglot tool version manager   |

## Behavior

### B1: Global Activation

**Given** a file is opened
**When** `+first-file-hook` runs
**Then** `misery-global-mode` is enabled

### B2: Environment Loading

**Given** `misery-global-mode` is active
**When** visiting a file in a directory with `.mise.toml`
**Then** tool paths are added to `exec-path` and `process-environment`

### B3: Silent Operation

**Given** misery loads an environment
**When** the environment changes
**Then** no minibuffer summary is shown (quiet operation)

## Configuration

| Variable                          | Value | Purpose                |
| :-------------------------------- | :---- | :--------------------- |
| `misery-show-summary-in-minibuffer` | nil   | Suppress noisy messages |

## Properties to Verify

### P1: Global Mode Activates

```elisp
;; Given first-file-hook has run
;; Then misery-global-mode is enabled
(run-hooks '+first-file-hook)
(should (bound-and-true-p misery-global-mode))
```

## Files

| File    | Purpose               |
| :------ | :-------------------- |
| init.el | Package configuration |
