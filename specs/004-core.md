# Feature: Core

Bootstrap, lifecycle hooks, custom use-package keywords, and foundational utilities.

## Summary

The core feature provides the foundational infrastructure for the Emacs configuration:

- Entry point and early initialization (UI, paths, native compilation)
- Package manager bootstrap (Elpaca)
- Custom use-package keywords for deferred loading
- Lifecycle hooks for lazy loading
- Foundation utilities (logging, hooks, collection operations)
- Module system for self-contained config units

## Dependencies

### External Packages

| Package               | Purpose                                |
| :-------------------- | :------------------------------------- |
| elpaca                | Package manager                        |
| general               | Keybinding system                      |
| no-littering          | Organized cache/data file paths        |
| auto-compile          | Prevent loading stale .elc files       |
| exec-path-from-shell  | Import shell PATH on macOS             |

### Built-in Features

| Feature       | Purpose                           |
| :------------ | :-------------------------------- |
| cl-lib        | Common Lisp compatibility         |
| subr-x        | Extra subroutines                 |
| server        | Emacsclient support               |
| use-package   | Package configuration DSL         |

## Behavior

### B1: Early Initialization

**Given** Emacs starts
**When** early-init.el loads
**Then**:
- Load paths are configured (`+lisp-dir`, `+init-dir`, `+config-dir`)
- Package archives are disabled (`package-enable-at-startup nil`)
- UI elements are disabled (menu bar, tool bar, scroll bars)
- Frame parameters are set (borders, dividers, undecorated on macOS)
- Native compilation warnings are silenced unless debug mode
- use-package defaults are configured (defer, hook-name-suffix nil)
- Theme light/dark variables are set
- Custom file redirected to temp file

### B2: Main Initialization

**Given** early-init.el has completed
**When** init.el loads
**Then**:
- Elpaca package manager is bootstrapped
- Critical packages load synchronously: general, no-littering, auto-compile
- All files in `init/` are loaded via `use-package :demand t`
- All files in `site/` are loaded if the directory exists

### B3: Lifecycle Hooks

**Given** the hooks feature is loaded
**Then** transient hooks are available:

| Hook               | Triggers On                        | Runs |
| :----------------- | :--------------------------------- | :--- |
| +first-input-hook  | First `pre-command-hook`           | Once |
| +first-file-hook   | First `find-file-hook`             | Once |
| +first-buffer-hook | First buffer or file switch        | Once |

**And** recurring hooks are available:

| Hook               | Triggers On                 |
| :----------------- | :-------------------------- |
| +switch-buffer-hook | Buffer changes in window   |
| +switch-frame-hook  | Frame focus changes        |
| +switch-window-hook | Window focus changes       |

### B4: Local Vars Hooks

**Given** a buffer with a major mode
**When** `hack-local-variables-hook` runs
**Then** `${major-mode}-local-vars-hook` is invoked for mode and all parents

This allows hooks to depend on buffer-local variables set via dir-locals.

### B5: Incremental Loading

**Given** `+load-packages-incrementally` is called with a list of packages
**When** Emacs becomes idle for `+load-packages--first-idle-timer` seconds
**Then** packages are loaded one-by-one during idle time

### B6: Server Mode

**Given** display-graphic-p returns non-nil
**When** init.el loads
**Then** Emacs server is started if not already running

### B7: use-package Keywords

The following custom keywords are available:

| Keyword              | Behavior                                           |
| :------------------- | :------------------------------------------------- |
| `:ensure-unless-local` | Use local path if exists, else elpaca recipe     |
| `:defer-incrementally` | Queue packages for idle-time loading             |
| `:after-call`          | Load package when function/hook first invoked    |

## Provided API

### Variables

| Variable                    | Purpose                                      |
| :-------------------------- | :------------------------------------------- |
| `+lisp-dir`                 | Path to lisp/ directory                      |
| `+init-dir`                 | Path to init/ directory                      |
| `+config-dir`               | Path to config/ directory                    |
| `+site-files-directory`     | Path to site/ directory                      |
| `org-directory`             | Default org files location                   |
| `org-roam-directory`        | Default org-roam location                    |
| `+first-input-hook`         | Transient hook for first input               |
| `+first-file-hook`          | Transient hook for first file                |
| `+first-buffer-hook`        | Transient hook for first buffer              |
| `+switch-buffer-hook`       | Recurring hook for buffer switch             |
| `+switch-frame-hook`        | Recurring hook for frame switch              |
| `+switch-window-hook`       | Recurring hook for window switch             |
| `+inhibit-local-var-hooks`  | Dynamically bind to suppress local-var hooks |
| `+modules-directory`        | Path to modules/ directory                   |

### Functions

| Function                       | Purpose                                   |
| :----------------------------- | :---------------------------------------- |
| `+run-hook-once`               | Run hook once when triggers fire          |
| `+run-hooks`                   | Run hooks with error handling             |
| `+run-local-var-hooks-h`       | Run mode-local-vars-hooks                 |
| `+load-packages-incrementally` | Queue packages for idle loading           |
| `+modules-discover`            | Find module directories                   |
| `+modules-read-packages`       | Read packages.eld from module             |
| `+modules-collect-packages`    | Aggregate package specs from all modules  |
| `+modules-install-packages`    | Install package specs via elpaca          |
| `+modules-collect-autoloads`   | Gather autoloads from module libs         |
| `+modules-register-autoloads`  | Evaluate autoload forms                   |
| `+modules-collect-init-files`  | Find init.el files in modules             |
| `+modules-load-inits`          | Load module init files                    |

### Macros

| Macro                       | Purpose                                     |
| :-------------------------- | :------------------------------------------ |
| `add-hook!`                 | Add N functions to M hooks                  |
| `add-transient-hook!`       | Add self-removing hook/advice               |
| `setq-hook!`                | Set buffer-local variables on hooks         |
| `pushnew!`                  | Variadic cl-pushnew                         |
| `delq!`                     | In-place delq                               |
| `+log`                      | Debug logging macro                         |
| `+with-inhibit-local-var-hooks` | Suppress local-var hooks in body        |

### Utility Functions (+corelib)

| Function                 | Purpose                                  |
| :----------------------- | :--------------------------------------- |
| `+separate`              | Partition sequence by predicate          |
| `+split-with`            | Split sequence at first non-match        |
| `+chunk-by`              | Split sequence into chunks               |
| `+alist-from-hash-table` | Convert hash-table to alist              |
| `+plist-from-hash-table` | Convert hash-table to plist              |
| `+tree-map`              | Pre-order tree traversal                 |
| `+read-eld`              | Read lisp-data file                      |
| `+plist-delete`          | Remove key from plist                    |
| `+point-in-comment-p`    | Check if point is in comment             |
| `+syntax-ppss`           | Memoized syntax-ppss                     |
| `+dirlocals-set`         | Set dir-locals by path prefix            |
| `+dirlocals-set-regexp`  | Set dir-locals by regexp                 |
| `+visible-buffers`       | List non-buried buffers                  |

## Properties to Verify

### P1: Emacs Version Check

```elisp
;; Given init.el loads on Emacs < 30
;; Then an error is signaled
(should-error (when (< emacs-major-version 30)
                (user-error "Emacs 30 required")))
```

### P2: Load Paths Configured

```elisp
;; Given early-init has loaded
;; Then init/, lisp/, config/ are in load-path
(should (member +init-dir load-path))
(should (member +lisp-dir load-path))
(should (member +config-dir load-path))
```

### P3: UI Disabled

```elisp
;; Given early-init has loaded
;; Then UI clutter is disabled
(should-not menu-bar-mode)
(should-not tool-bar-mode)
(should-not scroll-bar-mode)
```

### P4: Transient Hook Fires Once

```elisp
;; Given +first-input-hook has a function
;; When pre-command-hook fires twice
;; Then the function is called exactly once
(let ((call-count 0))
  (add-hook '+first-input-hook (lambda () (cl-incf call-count)))
  (run-hooks 'pre-command-hook)
  (run-hooks 'pre-command-hook)
  (should (= 1 call-count)))
```

### P5: Local Vars Hook Runs

```elisp
;; Given emacs-lisp-mode-local-vars-hook has a function
;; When a buffer enters emacs-lisp-mode and hack-local-variables runs
;; Then the function is called
(let ((ran nil))
  (add-hook 'emacs-lisp-mode-local-vars-hook (lambda () (setq ran t)))
  (with-temp-buffer
    (emacs-lisp-mode)
    (hack-local-variables))
  (should ran))
```

### P6: Module Discovery

```elisp
;; Given modules/ contains valid module directories
;; When +modules-discover is called
;; Then it returns paths to valid modules only
(let ((modules (+modules-discover)))
  (should (cl-every #'file-directory-p modules))
  (should (cl-every #'+modules--valid-module-p modules)))
```

### P7: Package Spec Reading

```elisp
;; Given a module with packages.eld containing ((foo) (bar :host github))
;; When +modules-read-packages is called
;; Then it returns the list of specs
(should (equal '((foo) (bar :host github))
               (+modules-read-packages "/path/to/module")))
```

### P8: Autoload Registration

```elisp
;; Given a module lib.el with ;;;###autoload before a defun
;; When +modules-register-autoloads is called
;; Then the symbol becomes fboundp
(should (fboundp 'module-function-name))
```

## Files

| File                        | Purpose                          |
| :-------------------------- | :------------------------------- |
| init.el                     | Main entry point                 |
| early-init.el               | Early UI and path configuration  |
| init/init-hooks.el          | Lifecycle hooks                  |
| lisp/+corelib.el            | Foundation utilities             |
| lisp/+use-package-keywords.el | Deferred loading keywords      |
| lisp/+modules.el            | Module system                    |
