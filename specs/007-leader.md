# Leader Keys Specification

## Purpose

Provide a discoverable, hierarchical keymap under `SPC` (and `M-m` fallback) for accessing commands across the configuration.

## Files

| Path                  | Purpose                                     |
| :-------------------- | :------------------------------------------ |
| `init/init-leader.el` | Leader key definitions via general.el       |
| `lisp/+edit-cmds.el`  | Editing commands (kill sexp, format)        |
| `lisp/+files.el`      | File navigation (+find-sibling-file)        |
| `lisp/+window.el`     | Window management (split, dedicate, swap)   |

## External Packages

| Package | Purpose                            |
| :------ | :--------------------------------- |
| general | Keybinding macros, definer, which-key |

## Behavior

### Root Bindings

| Key       | Command                   | Behavior                               |
| :-------- | :------------------------ | :------------------------------------- |
| `SPC SPC` | `consult-buffer`          | Switch buffer/file                     |
| `SPC RET` | `claude-code-ide-menu`    | LLM menu                               |
| `SPC {`   | `other-frame-prefix`      | Execute next command in other frame    |
| `SPC .`   | `other-window-prefix`     | Execute next command in other window   |
| `SPC !`   | `async-shell-command`     | Run shell command asynchronously       |
| `SPC \|`  | `rotate-layout`           | Rotate window layout                   |
| `SPC -`   | `window-toggle-side-windows` | Toggle side windows                 |
| `SPC :`   | `pp-eval-expression`      | Evaluate elisp expression              |
| `SPC ;`   | `ielm`                    | Interactive elisp REPL                 |
| `SPC d`   | `dired-jump`/`dired`      | Directory editor (context-aware)       |
| `SPC i`   | `consult-imenu`           | Jump to symbol in buffer               |
| `SPC r`   | `vertico-repeat`          | Repeat last vertico session            |
| `SPC s`   | `save-buffer`             | Save current buffer                    |
| `SPC S`   | `save-some-buffers`       | Save modified buffers                  |
| `SPC u`   | `universal-argument`      | C-u prefix                             |
| `SPC x`   | `execute-extended-command` | M-x                                   |
| `SPC K`   | `man`                     | Manual pages                           |
| `SPC T`   | `+goto-mode-template-file` | Edit tempel templates for mode        |
| `SPC '`   | context-aware edit        | Separedit/org-edit-special/exit        |
| `SPC /`   | `consult-ripgrep`         | Project-wide search                    |
| `SPC *`   | `+consult-ripgrep-symbol` | Search for symbol at point             |
| `SPC TAB` | `+swap-buffers`           | Toggle between last two buffers        |
| `SPC k`   | `consult-yank-pop`        | Kill ring browser                      |
| `SPC z`   | `global-text-scale-adjust` | Adjust text size                      |

### Prefix Groups

| Prefix    | Description             |
| :-------- | :---------------------- |
| `SPC ,`   | Structure (s-exp manipulation via puni) |
| `SPC a`   | Apps (eshell, calc, eat, profiler, elpaca) |
| `SPC b`   | Buffers (bury, kill, clone indirect) |
| `SPC c`   | Code/Comments (eglot, comment-dwim) |
| `SPC e`   | Errors (flymake navigation) |
| `SPC f`   | Files (find, save, rename, sibling) |
| `SPC g`   | Git/Goto (magit, browse-at-remote, goto files) |
| `SPC h`   | Help (delegates to help-map) |
| `SPC n`   | Narrowing (defun, region, widen) |
| `SPC o`   | Org (agenda, capture, roam, clocking) |
| `SPC p`   | Project (delegates to project-prefix-map) |
| `SPC t`   | Toggles (modes, visual features) |
| `SPC w`   | Windows (split, delete, balance, rotate) |
| `SPC W`   | Worktrees (worktree management menu) |

### Structure (SPC ,)

| Key     | Command                        | Behavior                    |
| :------ | :----------------------------- | :-------------------------- |
| `n`     | `puni-forward-sexp`            | Move forward over sexp      |
| `p`     | `puni-backward-sexp`           | Move backward over sexp     |
| `<`     | `puni-backward-sexp-or-up-list` | Back or up out of list     |
| `c`     | `puni-convolute`               | Exchange nesting            |
| `d`     | `+forward-kill-sexp`           | Kill sexp forward, format   |
| `D`     | `+backward-kill-sexp`          | Kill sexp backward, format  |
| `k`     | `puni-splice-killing-forward`  | Splice killing forward      |
| `K`     | `puni-splice-killing-backward` | Splice killing backward     |
| `r`     | `puni-raise`                   | Raise sexp                  |
| `b`     | `puni-barf-forward`            | Barf forward                |
| `B`     | `puni-barf-backward`           | Barf backward               |
| `m`     | `puni-slurp-forward`           | Slurp forward               |
| `M`     | `puni-slurp-backward`          | Slurp backward              |
| `t`     | `puni-transpose`               | Transpose sexps             |
| `u`     | `puni-splice`                  | Splice (unwrap)             |
| `x`     | `puni-split`                   | Split list                  |

### Apps (SPC a)

| Key     | Command                | Behavior                    |
| :------ | :--------------------- | :-------------------------- |
| `w`     | `eww`                  | Web browser                 |
| `c`     | `quick-calc`           | Quick calculator            |
| `C`     | `full-calc`            | Full calculator             |
| `e`     | `eshell`               | Eshell                      |
| `s`     | `eat`/`eat-beframed`   | Terminal (beframe-aware)    |
| `r`     | profiler start/report  | Toggle profiler             |
| `p`     | → `SPC a p` prefix     | Elpaca package manager      |

#### Elpaca (SPC a p)

| Key | Command           |
| :-- | :---------------- |
| `p` | `elpaca-manager`  |
| `l` | `elpaca-log`      |
| `i` | `elpaca-info`     |
| `b` | `elpaca-browse`   |
| `v` | `elpaca-visit`    |

### Buffers (SPC b)

| Key | Command                          | Behavior                    |
| :-- | :------------------------------- | :-------------------------- |
| `b` | `bury-buffer`                    | Bury current buffer         |
| `d` | `bury-buffer`                    | Bury current buffer         |
| `D` | `kill-current-buffer`            | Kill current buffer         |
| `l` | `bufler`                         | Buffer list                 |
| `n` | `next-buffer`                    | Next buffer                 |
| `s` | `bufler-switch-buffer`           | Switch buffer               |
| `p` | `previous-buffer`                | Previous buffer             |
| `c` | `clone-indirect-buffer`/region   | Clone buffer (region-aware) |

### Code/Comments (SPC c)

| Key | Command                     | Behavior                    |
| :-- | :-------------------------- | :-------------------------- |
| `m` | `xref-find-references`      | Find references             |
| `r` | `comment-dwim`              | Comment (do what I mean)    |
| `d` | `eglot-find-typeDefinition` | Find type definition        |
| `c` | `eglot-find-declaration`    | Find declaration            |
| `i` | `eglot-find-implementation` | Find implementation         |
| `l` | `comment-line`              | Comment/uncomment line      |

### Errors (SPC e)

| Key | Command            | Behavior        |
| :-- | :----------------- | :-------------- |
| `l` | `consult-flymake`  | List errors     |
| `e` | `first-error`      | Jump to first   |
| `n` | `next-error`       | Next error      |
| `p` | `previous-error`   | Previous error  |

### Files (SPC f)

| Key | Command                    | Behavior                      |
| :-- | :------------------------- | :---------------------------- |
| `f` | `find-file`                | Find file                     |
| `g` | `magit-find-file`          | Find file in git revision     |
| `F` | `find-file-other-window`   | Find file in other window     |
| `s` | `save-buffer`              | Save buffer                   |
| `R` | `rename-visited-file`      | Rename file                   |
| `r` | `recentf`                  | Recent files                  |
| `w` | `write-file`               | Write copy                    |
| `o` | `+find-sibling-file`       | Toggle to sibling file        |
| `D` | `+delete-file-and-buffer`  | Delete file and kill buffer   |
| `y` | `+copy-file-path`          | Copy file path to clipboard   |
| `d` | `+copy-file-directory`     | Copy directory to clipboard   |
| `v` | `+revisit-file`            | Reload file from disk         |

### Git/Goto (SPC g)

| Key | Command                    | Behavior                      |
| :-- | :------------------------- | :---------------------------- |
| `b` | `magit-blame`              | Git blame                     |
| `d` | `magit-diff-buffer-file`   | Diff current file             |
| `f` | `magit-file-dispatch`      | File actions menu             |
| `g` | `magit-status`             | Git status                    |
| `l` | `magit-log-buffer-file`    | Log for current file          |
| `p` | `forge-browse-pullreq`     | Browse pull request           |
| `r` | `browse-at-remote`         | Open on GitHub                |
| `t` | `git-timemachine-toggle`   | File history viewer           |
| `y` | `browse-at-remote-kill`    | Copy GitHub link              |
| `?` | `+goto-messages`           | Go to *Messages* buffer       |
| `c` | `+goto-claude-settings`    | Edit Claude settings          |
| `e` | `+goto-emacs-init-file`    | Edit init.el                  |
| `s` | `+goto-emacs-site-file`    | Edit site file                |
| `n` | `+goto-nix-file`           | Edit nix config               |

### Narrowing (SPC n)

| Key | Command            | Behavior                |
| :-- | :----------------- | :---------------------- |
| `f` | `narrow-to-defun`  | Narrow to function      |
| `r` | `narrow-to-region` | Narrow to region        |
| `w` | `widen`            | Widen (remove narrowing) |

### Org (SPC o)

| Key | Command                          | Behavior                   |
| :-- | :------------------------------- | :------------------------- |
| `n` | `+org-goto-notes`                | Go to notes file           |
| `i` | `+goto-org-roam-index`           | Go to roam index           |
| `t` | `+goto-org-todos`                | Go to todos file           |
| `a` | `+org-agenda-dwim`               | Open agenda (daily view)   |
| `j` | `consult-org-agenda`             | Jump to agenda heading     |
| `g` | `org-capture-goto-last-stored`   | Go to last capture         |
| `v` | `org-tags-view`                  | Search by tag              |
| `k` | `org-capture`                    | Capture                    |
| `l` | `org-store-link`                 | Store link                 |
| `f` | `+org-roam-node-find`            | Find roam node             |
| `s` | `org-roam-search`                | Search roam                |
| `w` | `timekeep-visit-node`            | Go to work file            |
| `c` | → `SPC o c` prefix               | Clock commands             |
| `r` | → `SPC o r` prefix               | Roam/review commands       |

#### Clock (SPC o c)

| Key | Command                  | Behavior             |
| :-- | :----------------------- | :------------------- |
| `c` | `org-clock-in-last`      | Clock in (last task) |
| `d` | `org-clock-display`      | Display clocking     |
| `i` | `org-clock-in`           | Clock in             |
| `o` | `org-clock-out`          | Clock out            |
| `r` | `org-resolve-clocks`     | Resolve clocks       |
| `g` | `org-clock-goto`         | Go to clocked task   |
| `q` | `org-clock-cancel`       | Cancel clock         |

#### Roam/Review (SPC o r)

| Key | Command                            | Behavior               |
| :-- | :--------------------------------- | :--------------------- |
| `d` | `org-roam-review-list-recently-added` | List recent nodes   |
| `l` | `org-roam-links`                   | Show linked nodes      |
| `r` | `org-roam-review`                  | Start review session   |
| `t` | `org-roam-search-tags`             | Search by tag          |

### Toggles (SPC t)

| Key | Command                                   | Behavior                    |
| :-- | :---------------------------------------- | :-------------------------- |
| `b` | `breadcrumb-mode`                         | Header breadcrumbs          |
| `d` | `dimmer-mode`                             | Dim unfocused windows       |
| `h` | `global-hl-line-mode`                     | Highlight current line      |
| `f` | `global-display-fill-column-indicator-mode` | Fill column indicator    |
| `i` | `indent-bars-mode`                        | Indent guides               |
| `l` | `global-display-line-numbers-mode`        | Line numbers                |
| `m` | `toggle-input-method`                     | Input method                |
| `s` | `spell-fu-mode`                           | Spell checking              |
| `r` | `read-only-mode`                          | Read-only                   |
| `w` | `whitespace-mode`                         | Show whitespace             |
| `v` | `visual-line-mode`                        | Line wrapping (buffer)      |
| `V` | `global-visual-line-mode`                 | Line wrapping (global)      |

### Windows (SPC w)

| Key | Command                         | Behavior                      |
| :-- | :------------------------------ | :---------------------------- |
| `-` | `+split-window-vertically-dwim` | Vertical split (smart buffer) |
| `/` | `+split-window-horizontally-dwim` | Horizontal split (smart buffer) |
| `=` | `balance-windows`               | Balance window sizes          |
| `d` | `delete-window`                 | Delete current window         |
| `o` | `+delete-nondedicated-windows`  | Delete non-dedicated windows  |
| `O` | `delete-other-windows`          | Delete all other windows      |
| `q` | `delete-window`                 | Delete current window         |
| `r` | `evil-window-rotate-downwards`  | Rotate windows                |
| `s` | `consult-register-load`         | Load window register          |
| `S` | `window-configuration-to-register` | Save to register           |
| `t` | `+toggle-window-dedication`     | Toggle window dedication      |
| `w` | `other-window`                  | Cycle to other window         |

## API

### Commands from +edit-cmds.el

| Function             | Description                              |
| :------------------- | :--------------------------------------- |
| `+forward-kill-sexp` | Kill sexp forward with formatting cleanup |
| `+backward-kill-sexp`| Kill sexp backward with formatting cleanup |
| `+kill-line`         | Kill line with puni, then format         |
| `+insert-uuid`       | Insert a UUID at point                   |

### Commands from +files.el

| Function                                      | Description                          |
| :-------------------------------------------- | :----------------------------------- |
| `+find-sibling-file`                          | Find sibling file (creates if needed) |
| `+find-sibling-file-search-including-nonexisting` | Search including nonexistent files |

### Commands from +window.el

| Function                          | Description                              |
| :-------------------------------- | :--------------------------------------- |
| `+split-window-horizontally-dwim` | Split horizontal, show sibling/other buffer |
| `+split-window-vertically-dwim`   | Split vertical, show sibling/other buffer |
| `+toggle-window-dedication`       | Toggle window dedication                 |
| `+delete-nondedicated-windows`    | Delete all non-dedicated windows         |
| `+clone-indirect-buffer-of-region`| Clone buffer narrowed to region          |
| `+toggle-side-window-raised`      | Raise/return side window to main area    |
| `+toggle-window-fullframe`        | Toggle fullframe for current window      |
| `+win-swap-up`                    | Swap or move side window up              |
| `+win-swap-down`                  | Swap or move side window down            |
| `+win-swap-left`                  | Swap or move side window left            |
| `+win-swap-right`                 | Swap or move side window right           |
| `+move-side-window-to-side`       | Move side window to specified side       |

### Variables from +window.el

| Variable                      | Description                         |
| :---------------------------- | :---------------------------------- |
| `+side-window-raised-hook`    | Hook run when side window is raised |
| `+side-window-returned-hook`  | Hook run when side window returns   |
| `+side-window-default-width`  | Default width for side windows (80) |
| `+side-window-default-height` | Default height for side windows (0.3) |

### Macros

| Symbol                | Description                             |
| :-------------------- | :-------------------------------------- |
| `+define-leader-keys` | general.el definer for SPC prefix       |

### Keymaps

| Symbol        | Description                     |
| :------------ | :------------------------------ |
| `+leader-key` | Root leader keymap (SPC prefix) |

## Design Decisions

1. **Alternative binding**: `M-m` provides leader access in non-evil modes (insert, emacs state)

2. **Chained universal-argument**: `SPC u SPC u` continues adding universal-argument prefixes

3. **Context-aware commands**: Several commands use `general-predicate-dispatch`:
   - `SPC d`: `dired-jump` normally, `dired` with prefix or in dired
   - `SPC '`: separedit in strings/comments, org-edit-special in org, etc.
   - `SPC a s`: `eat-beframed` when beframe is active
   - `SPC a r`: toggle profiler start/report

4. **Smart splits**: `+split-window-*-dwim` functions show sibling file or other buffer in new window, auto-balance

5. **Side window management**: Side windows can be raised to main area, returned, or moved between sides while preserving their identity

## Testable Properties

1. `SPC SPC` opens consult-buffer
2. `SPC s` saves the current buffer
3. `SPC w -` splits vertically and shows sibling/other buffer
4. `SPC w t` toggles window dedication (verified by `window-dedicated-p`)
5. `SPC f o` finds sibling file based on `find-sibling-rules`
6. `M-m` activates `+leader-key` in emacs state
7. `SPC u SPC u` produces (16) universal argument
8. `+forward-kill-sexp` kills sexp and cleans up whitespace
9. Side window swap functions move windows to correct sides
