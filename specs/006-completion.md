# Feature: Completion

Minibuffer and in-buffer completion framework using Vertico, Corfu, and Consult.

## Summary

The completion feature provides a modern completion experience:

- Vertico for vertical minibuffer completion UI with cycling
- Marginalia for rich annotations alongside candidates
- Orderless for fuzzy, space-separated matching
- Corfu for automatic in-buffer completion popups
- Cape for completion-at-point extensions (file, elisp, etc.)
- Consult for enhanced commands (buffers, search, imenu, etc.)
- Embark for contextual actions on selected items
- Which-key for keybinding discovery
- Savehist for completion history persistence

## Dependencies

### External Packages

| Package           | Purpose                                    |
| :---------------- | :----------------------------------------- |
| vertico           | Vertical minibuffer completion UI          |
| marginalia        | Rich annotations in minibuffer             |
| orderless         | Space-separated fuzzy matching             |
| corfu             | In-buffer completion popup                 |
| nerd-icons-corfu  | Icons in corfu popup                       |
| cape              | completion-at-point extensions             |
| consult           | Enhanced completing-read commands          |
| embark            | Contextual actions on candidates           |
| embark-consult    | Integration between embark and consult     |
| which-key         | Display keybinding hints                   |

### Built-in Features

| Feature          | Purpose                            |
| :--------------- | :--------------------------------- |
| minibuffer       | Core completion settings           |
| savehist         | History persistence                |
| crm              | Completing-read-multiple           |
| dabbrev          | Dynamic abbreviation               |
| find-func        | Find library functions             |
| minibuf-eldef    | Electric default display           |

### Internal Dependencies

| Feature | Reason                                   |
| :------ | :--------------------------------------- |
| core    | +corelib, +first-input-hook, pushnew!    |

## Behavior

### B1: Vertico Activation

**Given** first user input occurs
**When** +first-input-hook runs
**Then** vertico-mode is enabled

### B2: Vertico Keybindings

**Given** vertico is active in the minibuffer
**Then** the following bindings apply:

| Key       | Action                         |
| :-------- | :----------------------------- |
| C-RET     | Exit with exact input          |
| RET       | Enter directory or confirm     |
| DEL       | Delete char or up directory    |
| C-l / M-l | Insert candidate               |
| C-h / M-h | Delete word (directory)        |
| M-P / M-N | Previous/next vertico history  |

### B3: Vertico Settings

**Given** vertico is loaded
**Then** the following settings apply:

| Setting           | Value     | Effect                              |
| :---------------- | :-------- | :---------------------------------- |
| vertico-preselect | no-prompt | Don't preselect first candidate     |
| vertico-cycle     | t         | Wrap around at list boundaries      |

### B4: Directory Navigation

**Given** vertico-directory is active
**When** typing a path in the minibuffer
**Then** DEL deletes path components intelligently
**And** rfn-eshadow-update-overlay removes shadowed paths

### B5: Vertico History

**Given** vertico-repeat is configured
**When** minibuffer-setup-hook runs
**Then** current session is saved for recall
**And** history persists across sessions via savehist

### B6: Marginalia Annotations

**Given** first user input occurs
**When** +first-input-hook runs
**Then** marginalia-mode is enabled
**And** candidates show rich annotations (file size, docstrings, etc.)

**Given** minibuffer is active
**When** M-A is pressed
**Then** marginalia annotation style cycles

### B7: Completion Styles

**Given** minibuffer completion is active
**Then** completion styles are tried in order:

| Priority | Style            | Behavior                            |
| :------- | :--------------- | :---------------------------------- |
| 1        | basic            | Prefix matching                     |
| 2        | substring        | Substring anywhere                  |
| 3        | initials         | Match initials (e.g. ffap → find-file-at-point) |
| 4        | flex             | Flex matching                       |
| 5        | orderless        | Space-separated terms               |

### B8: Category-Specific Styles

**Given** completion category is known
**Then** override styles apply:

| Category   | Styles                              |
| :--------- | :---------------------------------- |
| file       | basic, partial-completion, orderless |
| bookmark   | basic, substring                    |
| library    | basic, substring                    |
| imenu      | basic, substring, orderless         |
| kill-ring  | emacs22, orderless                  |
| eglot      | emacs22, substring, orderless       |

### B9: Case Insensitivity

**Given** completion is active
**Then** matching is case-insensitive for:
- File names
- Buffer names
- General completion

### B10: Recursive Minibuffers

**Given** enable-recursive-minibuffers is t
**When** command is invoked from within minibuffer
**Then** nested minibuffer opens

### B11: CRM Indicator

**Given** completing-read-multiple is called
**Then** prompt shows "[CRM <sep>]" with styled separator

### B12: Corfu Activation

**Given** first user input occurs
**When** +first-input-hook runs
**Then** global-corfu-mode is enabled
**And** corfu is disabled in org-mode and help-mode

### B13: Corfu Settings

**Given** corfu is active in a buffer
**Then** the following settings apply:

| Setting                | Value      | Effect                                |
| :--------------------- | :--------- | :------------------------------------ |
| corfu-auto             | t          | Automatic popup                       |
| corfu-auto-delay       | 0.24       | Popup delay in seconds                |
| corfu-quit-no-match    | separator  | Quit on no match after separator      |
| corfu-cycle            | t          | Wrap around candidates                |
| corfu-preselect        | prompt     | First candidate not selected          |
| corfu-count            | 16         | Max visible candidates                |
| corfu-max-width        | 120        | Max popup width                       |
| corfu-on-exact-match   | nil        | Don't auto-complete exact matches     |
| tab-always-indent      | complete   | TAB completes after indentation       |

### B14: Corfu Keybindings

**Given** corfu popup is visible
**Then** the following bindings apply:

| Key       | Action              |
| :-------- | :------------------ |
| RET       | Send to command     |
| ESC       | Reset/dismiss       |
| C-n / C-p | Navigate candidates |

### B15: Corfu Popup Info

**Given** corfu is active
**When** candidate is selected for 1 second
**Then** documentation popup appears

### B16: Corfu Eshell Behavior

**Given** eshell-mode is active
**Then** corfu-auto is disabled (manual trigger only)

### B17: Corfu Icons

**Given** nerd-icons-corfu is configured
**When** corfu popup displays
**Then** candidates show appropriate icons by type

### B18: Cape File Completion

**Given** prog-mode is active
**Then** cape-file is added to completion-at-point-functions
**And** file paths complete with negative priority (-10)

### B19: Cape Elisp Block

**Given** org-mode is active
**Then** cape-elisp-block completes Elisp in source blocks

### B20: Cape Wrapping

**Given** comint, eglot, or pcomplete provides completions
**Then** completion is wrapped as non-exclusive
**And** other backends can provide additional candidates

### B21: Which-Key Activation

**Given** Emacs starts
**Then** which-key-mode is enabled immediately (demand)

### B22: Which-Key Settings

**Given** which-key is active
**Then** the following settings apply:

| Setting                       | Value                    | Effect                        |
| :---------------------------- | :----------------------- | :---------------------------- |
| which-key-prefix-prefix       | "…"                      | Ellipsis for prefix           |
| which-key-idle-delay          | 0.4                      | Seconds before popup          |
| which-key-sort-order          | which-key-key-order-alpha | Alphabetical sorting         |
| which-key-sort-uppercase-first| nil                      | Lowercase before uppercase    |
| which-key-min-display-lines   | 6                        | Minimum popup height          |
| which-key-side-window-slot    | -10                      | Window slot priority          |

### B23: Consult Command Remappings

**Given** consult is loaded
**Then** standard commands are remapped:

| Original Command                  | Consult Command                |
| :-------------------------------- | :----------------------------- |
| bookmark-jump                     | consult-bookmark               |
| evil-show-marks                   | consult-mark                   |
| evil-show-registers               | consult-register               |
| goto-line                         | consult-goto-line              |
| imenu                             | consult-imenu                  |
| Info-search                       | consult-info                   |
| locate                            | consult-locate                 |
| load-theme                        | consult-theme                  |
| recentf-open-files                | consult-recent-file            |
| switch-to-buffer                  | consult-buffer                 |
| switch-to-buffer-other-window     | consult-buffer-other-window    |
| switch-to-buffer-other-frame      | consult-buffer-other-frame     |
| yank-pop                          | consult-yank-pop               |

### B24: Consult Xref Integration

**Given** xref operations are performed
**Then** consult-xref is used for display
**And** preview is available with C-SPC

### B25: Consult Async Settings

**Given** consult async command runs (grep, find, etc.)
**Then** the following settings apply:

| Setting                    | Value | Effect                           |
| :------------------------- | :---- | :------------------------------- |
| consult-async-min-input    | 2     | Min chars before search          |
| consult-async-refresh-delay| 0.15  | Refresh delay in seconds         |
| consult-async-input-throttle| 0.2  | Input throttle in seconds        |
| consult-async-input-debounce| 0.1  | Input debounce in seconds        |

### B26: Consult Preview

**Given** consult command is active
**Then** C-SPC triggers preview
**And** theme preview has 0.5s debounce

### B27: Consult Register Preview

**Given** register preview is requested
**Then** consult-register-window is used
**And** preview delay is 0.5 seconds

### B28: Embark Actions

**Given** normal, emacs, or motion state is active
**Then** the following bindings apply:

| Key | Action       |
| :-- | :----------- |
| C-@ | embark-act   |
| C-t | embark-dwim  |

**Given** minibuffer is active
**Then** the following bindings apply:

| Key     | Action          |
| :------ | :-------------- |
| C-@     | embark-act      |
| C-c C-e | embark-export   |
| C-c C-c | embark-collect  |

### B29: Embark Consult Integration

**Given** embark-collect-mode is active
**Then** consult-preview-at-point-mode is enabled

### B30: Savehist Persistence

**Given** Emacs exits
**Then** the following are persisted:
- kill-ring (with text properties stripped)
- register-alist (printable items only)
- mark-ring and global-mark-ring
- search-ring and regexp-search-ring
- vertico-repeat-history
- corfu-history

### B31: Ignored Extensions

**Given** completion is active
**Then** the following are hidden from file completion:
- .DS_Store
- .eln
- .drv
- .direnv/
- .git/

### B32: Minibuffer Default Display

**Given** minibuffer-electric-default-mode is active
**When** prompt has a default value
**Then** default is shown as " [%s]" suffix

### B33: Dabbrev Configuration

**Given** dabbrev expansion is triggered
**Then** leading regexp [$*/=~'] is skipped
**And** uppercase means case-sensitive search
**And** docview-mode and pdf-view-mode are ignored

## Provided API

### Commands

| Command            | Binding   | Purpose                              |
| :----------------- | :-------- | :----------------------------------- |
| marginalia-cycle   | M-A       | Cycle annotation styles              |
| embark-act         | C-@       | Show contextual actions              |
| embark-dwim        | C-t       | Default action on candidate          |
| embark-export      | C-c C-e   | Export candidates to buffer          |
| embark-collect     | C-c C-c   | Collect candidates                   |
| consult-buffer     | C-x b     | Switch buffer with preview           |
| consult-imenu      | M-g i     | Jump to symbol in buffer             |
| consult-goto-line  | M-g g     | Go to line with preview              |
| consult-mark       | (remap)   | Browse marks with preview            |
| consult-register   | (remap)   | Browse registers with preview        |
| consult-yank-pop   | M-y       | Yank from kill-ring with preview     |
| consult-theme      | (remap)   | Load theme with preview              |
| consult-recent-file| (remap)   | Open recent file with preview        |

### Variables

| Variable                    | Purpose                                   |
| :-------------------------- | :---------------------------------------- |
| completion-styles           | Ordered list of completion strategies     |
| completion-category-overrides | Per-category style overrides           |
| consult-narrow-key          | Key to narrow candidates ("<")            |
| consult-preview-key         | Key to trigger preview ("C-SPC")          |
| global-corfu-modes          | Modes where corfu is active/inactive      |

## Properties to Verify

### P1: Vertico Active

```elisp
;; Given init-completion has loaded and input triggered
;; Then vertico-mode is enabled
(run-hooks '+first-input-hook)
(should vertico-mode)
```

### P2: Marginalia Active

```elisp
;; Given init-completion has loaded and input triggered
;; Then marginalia-mode is enabled
(run-hooks '+first-input-hook)
(should marginalia-mode)
```

### P3: Corfu Active

```elisp
;; Given init-completion has loaded and input triggered
;; Then global-corfu-mode is enabled
(run-hooks '+first-input-hook)
(should global-corfu-mode)
```

### P4: Which-Key Active

```elisp
;; Given init-completion has loaded
;; Then which-key-mode is enabled
(should which-key-mode)
```

### P5: Completion Styles

```elisp
;; Given init-completion has loaded
;; Then completion-styles includes orderless
(should (memq 'orderless completion-styles))
```

### P6: Case Insensitivity

```elisp
;; Given init-completion has loaded
;; Then completion is case-insensitive
(should completion-ignore-case)
(should read-file-name-completion-ignore-case)
(should read-buffer-completion-ignore-case)
```

### P7: Consult Buffer Remapping

```elisp
;; Given consult is loaded
;; Then switch-to-buffer is remapped
(should (eq (command-remapping 'switch-to-buffer) 'consult-buffer))
```

### P8: Savehist Variables

```elisp
;; Given savehist is configured
;; Then vertico-repeat-history is saved
(should (memq 'vertico-repeat-history savehist-additional-variables))
```

### P9: Corfu Disabled in Eshell

```elisp
;; Given eshell-mode-hook has run
;; Then corfu-auto is nil
(with-temp-buffer
  (eshell-mode)
  (should (not corfu-auto)))
```

## Files

| File                    | Purpose                                    |
| :---------------------- | :----------------------------------------- |
| init/init-completion.el | All completion configuration               |
