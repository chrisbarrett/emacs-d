# Feature: Evil

Modal editing with Vim emulation and ecosystem packages.

## Summary

The evil feature provides comprehensive Vim-style modal editing:

- Evil-mode with custom settings (undo-redo, C-u scroll, Y yank-to-eol)
- Evil-collection for consistent bindings across many modes
- Evil-surround for surrounding objects with delimiters
- Evil-multiedit for multiple cursors
- Evil-anzu for search hit counts in modeline
- Vundo for visual undo tree navigation
- Custom escape handling via +escape-hook
- Smart join that handles comment delimiters
- TTY cursor shape changes per state

## Dependencies

### External Packages

| Package            | Purpose                                    |
| :----------------- | :----------------------------------------- |
| evil               | Core Vim emulation                         |
| evil-collection    | Community-managed mode keybindings         |
| evil-surround      | Surround text objects with delimiters      |
| evil-multiedit     | Multiple cursors compatible with evil      |
| evil-anzu          | Search hit count in modeline               |
| evil-tty-cursor    | TTY cursor shape per evil state            |
| vundo              | Visual undo tree                           |
| puni               | Structured editing (slurp, barf, wrap)     |
| string-inflection  | Cycle identifier casing styles             |
| general            | Keybinding DSL                             |

### Built-in Features

| Feature      | Purpose                         |
| :----------- | :------------------------------ |
| elec-pair    | Automatic matching pairs        |
| newcomment   | Comment commands                |
| paragraphs   | Sentence/paragraph parsing      |

### Internal Dependencies

| Feature | Reason                              |
| :------ | :---------------------------------- |
| core    | +corelib, +escape-hook, add-hook!   |
| leader  | +leader-key for SPC binding         |

## Behavior

### B1: Evil Mode Activation

**Given** Emacs starts
**When** init-evil.el loads
**Then** evil-mode is enabled globally

### B2: Evil Settings

**Given** evil is loaded
**Then** the following settings apply:

| Setting                          | Value     | Effect                               |
| :------------------------------- | :-------- | :----------------------------------- |
| evil-symbol-word-search          | t         | * and # search symbols not words     |
| evil-undo-system                 | undo-redo | Use native undo-redo                 |
| evil-v$-excludes-newline         | t         | v$ stops before newline              |
| evil-want-C-u-scroll             | t         | C-u scrolls up                       |
| evil-want-C-w-delete             | t         | C-w deletes word in insert mode      |
| evil-want-Y-yank-to-eol          | t         | Y yanks to end of line               |
| evil-want-keybinding             | nil       | Let evil-collection handle bindings  |

### B3: Cursor Shapes

**Given** evil is active
**Then** cursor shape changes per state:

| State   | Cursor  |
| :------ | :------ |
| normal  | box     |
| insert  | bar     |
| visual  | hollow  |
| emacs   | hollow  |

**And** in TTY, cursor escape codes are sent via evil-tty-cursor.

### B4: Escape Handling

**Given** the +escape-hook is defined
**When** ESC is pressed in normal state
**Then** functions in +escape-hook run until one returns non-nil

**Given** evil-ex-search is active
**When** +escape is invoked
**Then** search highlights are cleared

### B5: Evil-Collection Deferred Loading

**Given** evil-collection is configured
**When** a supported mode is first activated
**Then** evil-collection bindings for that mode are initialized
**And** loading is logged via +log

The following modes are initialized immediately:
- comint (for shell interaction)

The following modes are disabled from evil-collection:

| Mode         | Reason                           |
| :----------- | :------------------------------- |
| eldoc        | Conflicts with custom behavior   |
| help         | Deferred to help-mode hook       |
| elisp-mode   | Deferred to emacs-lisp-mode hook |
| simple       | Too invasive                     |
| tab-bar      | Custom tab-bar behavior          |

### B6: Evil-Surround

**Given** evil-surround-mode is active
**When** visual selection is made and 's' is pressed
**Then** user is prompted for delimiter and selection is wrapped

**Given** emacs-lisp-mode is active
**Then** backtick pairs use `...' style (Emacs convention)

Custom surround pairs:

| Key | Result                    |
| :-- | :------------------------ |
| (   | (text)                    |
| [   | [text]                    |
| {   | {text}                    |
| #   | #{text}                   |
| f   | Prompt for function name  |
| t   | Prompt for HTML tag       |

### B7: Evil-Multiedit

**Given** visual mode is active
**When** 'v' is pressed twice in succession
**Then** all matches of the selection become editable cursors

**Given** evil-multiedit-mode is active
**Then** the following bindings apply:

| Key   | Action                          |
| :---- | :------------------------------ |
| Y     | Copy current occurrence         |
| TAB   | Toggle selection of occurrence  |
| n/N   | Next/previous occurrence        |
| S     | Change entire line              |

### B8: Search Hit Count

**Given** evil search is performed (/, ?, *, #)
**When** search completes
**Then** modeline shows "X/Y" where X is current match and Y is total

### B9: Visual Undo Tree

**Given** C-x u is pressed
**Then** vundo buffer opens showing undo history as a tree
**And** navigation uses Unicode box-drawing characters

### B10: Smart Join

**Given** point is on a comment line
**When** J (evil-join) is executed
**Then** comment delimiters are removed from joined line
**And** excess whitespace is normalized

### B11: RET Opens URLs

**Given** point is on a URL
**When** RET is pressed in normal state
**Then** URL is opened in browser

### B12: Minibuffer Keybindings

**Given** minibuffer is active
**Then** the following bindings apply:

| Key   | Action                              |
| :---- | :---------------------------------- |
| C-a   | Beginning of line                   |
| C-r   | Paste from evil register            |
| C-u   | Delete back to indentation          |
| C-v   | Yank (paste)                        |
| C-w   | Delete word backward (no kill-ring) |
| ESC   | +escape (abort minibuffer)          |

### B13: Input Handling

**Given** GUI Emacs is running
**Then** C-i and C-m are distinguished from TAB and RET

**Given** C-c SPC is pressed
**Then** non-breaking space is inserted

### B14: Structured Editing (Puni)

**Given** puni-mode is active in prog/text/conf modes
**Then** the following bindings apply:

| Key   | Action            |
| :---- | :---------------- |
| C-w   | Backward kill word (insert) |
| C-k   | Kill line or region |
| M-(   | Wrap in parens    |
| M-]   | Wrap in brackets  |
| M-{   | Wrap in braces (or tempel-previous if template active) |
| M-}   | Wrap in braces (or tempel-next if template active) |

### B15: String Inflection

**Given** M-- is pressed
**Then** identifier at point cycles through case styles:
- UpperCamelCase
- lowerCamelCase
- snake_case
- SCREAMING_SNAKE_CASE
- kebab-case

### B16: Comment Behavior

**Given** insert mode is active in prog/text modes
**When** RET is pressed inside a comment
**Then** new comment line is created with proper delimiter

### B17: Evil State Shift-Width Sync

**Given** major mode changes
**When** after-change-major-mode hook runs
**Then** evil-shift-width is synchronized to tab-width

## Provided API

### Variables

| Variable               | Purpose                                    |
| :--------------------- | :----------------------------------------- |
| +escape-hook           | Hook run on ESC, returns non-nil to stop   |
| +default-minibuffer-maps | Keymaps for minibuffer bindings          |
| evil-collection-mode-list | Modes supported by evil-collection      |
| +evil-collection-disabled-list | Modes excluded from evil-collection |

### Commands

| Command                | Binding        | Purpose                            |
| :--------------------- | :------------- | :--------------------------------- |
| +escape                | ESC (normal)   | Quit/abort handler                 |
| +insert-char           | C-x SPC        | Insert character (GUI picker on macOS) |
| +insert-nbsp           | C-c SPC        | Insert non-breaking space          |
| +multiedit             | v v (visual)   | Start multiedit on selection       |
| vundo                  | C-x u          | Visual undo tree                   |
| string-inflection-all-cycle | M--       | Cycle identifier case              |

### Functions

| Function                                   | Purpose                            |
| :----------------------------------------- | :--------------------------------- |
| +evil-collection-init                      | Initialize evil-collection module  |
| +evil-collection-defer-install-to-mode-activation | Set up deferred loading    |
| +delete-backward-word-no-kill              | Delete word without kill-ring      |
| +evil-multiedit-copy                       | Copy current multiedit occurrence  |

## Properties to Verify

### P1: Evil Mode Active

```elisp
;; Given init-evil has loaded
;; Then evil-mode is enabled
(should evil-mode)
```

### P2: Undo System

```elisp
;; Given evil is configured
;; Then undo-redo is the undo system
(should (eq evil-undo-system 'undo-redo))
```

### P3: Cursor Shapes

```elisp
;; Given evil is loaded
;; Then cursor shapes are configured per state
(should (eq evil-normal-state-cursor 'box))
(should (eq evil-insert-state-cursor 'bar))
```

### P4: Escape Clears Highlights

```elisp
;; Given evil search highlight is active
;; When +escape is called
;; Then highlight is deactivated
(let ((hook-ran nil))
  (add-hook '+escape-hook (lambda () (setq hook-ran t) t))
  (+escape)
  (should hook-ran))
```

### P5: Evil-Collection Deferred

```elisp
;; Given evil-collection is configured
;; Then modes are loaded on demand not at startup
(should (not (featurep 'evil-collection-help)))
;; After help-mode activates, bindings are set
(with-temp-buffer
  (help-mode)
  (should (featurep 'evil-collection-help)))
```

### P6: Surround in Visual

```elisp
;; Given evil-surround-mode is active
;; Then 's' is bound in visual state
(should (keymapp (lookup-key evil-surround-mode-map [remap evil-substitute])))
```

### P7: Shift-Width Sync

```elisp
;; Given tab-width is set in a mode
;; When mode is activated
;; Then evil-shift-width matches
(with-temp-buffer
  (setq-local tab-width 4)
  (run-hooks 'after-change-major-mode-hook)
  (should (= evil-shift-width 4)))
```

### P8: Minibuffer Escape

```elisp
;; Given minibuffer keymaps
;; Then ESC is bound to +escape
(should (eq (lookup-key minibuffer-local-map [escape]) '+escape))
```

### P9: Smart Join Removes Delimiters

```elisp
;; Given two comment lines
;; When evil-join is called
;; Then result has single comment prefix
(with-temp-buffer
  (emacs-lisp-mode)
  (insert ";; first line\n;; second line")
  (goto-char (point-min))
  (evil-join (point-min) (point-max))
  (should (string-match-p "^;; first line second line$" (buffer-string))))
```

## Files

| File                     | Purpose                                    |
| :----------------------- | :----------------------------------------- |
| init/init-evil.el        | Evil core setup and ecosystem packages     |
| init/init-esc.el         | +escape function and hook                  |
| init/init-input.el       | Input handling, puni, electric pairs       |
| config/mod-evil.el       | Cursor shapes, smart join, RET opens URLs  |
| lisp/+evil-collection.el | Deferred evil-collection loading           |
