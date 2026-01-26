# Help Feature Spec

Help system enhancements including helpful.el, eldoc, info, man, and rfc-mode.

## Files

| File              | Purpose                                     |
| :---------------- | :------------------------------------------ |
| `init/init-help.el` | Help system configuration                 |

## External Packages

| Package   | Purpose                               |
| :-------- | :------------------------------------ |
| helpful   | Enhanced help buffers with definitions |
| rfc-mode  | RFC document browser                  |

## Behaviors

### Help Buffers

| When                        | Then                                      |
| :-------------------------- | :---------------------------------------- |
| Help buffer opens           | Visual line mode enabled                  |
| Help buffer opens           | Window is selected (focus moves to help)  |
| In help buffer (normal)     | `^` goes back in help history             |
| In help buffer              | `M-n`/`C-n` moves to next button          |
| In help buffer              | `M-p`/`C-p` moves to previous button      |

### Help Map Rebindings

| Key     | Command                   | Notes                             |
| :------ | :------------------------ | :-------------------------------- |
| `C-h h` | (unbound)                 | Removes view-hello-file           |
| `C-h l` | `find-library`            | Find elisp library source         |
| `C-h c` | `describe-face`           | Replaces describe-key-briefly     |
| `C-h P` | `describe-text-properties`| Text property inspector           |
| `C-h f` | `helpful-callable`        | Enhanced function/macro help      |
| `C-h v` | `helpful-variable`        | Enhanced variable help            |
| `C-h k` | `helpful-key`             | Enhanced key binding help         |
| `C-h s` | `info-apropos`            | Search info manuals               |
| `C-h w` | `rfc-mode-browse`         | Browse RFC documents              |

### Eldoc

| When                        | Then                                      |
| :-------------------------- | :---------------------------------------- |
| After evil state changes    | Eldoc refreshes hint in echo area         |
| After evil-insert           | Eldoc re-runs to show context             |
| After evil-change           | Eldoc re-runs to show context             |
| After evil-delete           | Eldoc re-runs to show context             |
| After evil-replace          | Eldoc re-runs to show context             |

### Info Mode

| When                        | Then                                      |
| :-------------------------- | :---------------------------------------- |
| In Info buffer (normal)     | `^` goes up in info hierarchy             |
| In Info buffer              | `C-n` moves to next node                  |
| In Info buffer              | `C-p` moves to previous node              |
| In Info buffer              | `consult-imenu` remapped to `Info-menu`   |

### Man Pages

| When                        | Then                                      |
| :-------------------------- | :---------------------------------------- |
| `M-x man` completes         | Man page buffer opens and is selected     |

### RFC Mode

| When                        | Then                                      |
| :-------------------------- | :---------------------------------------- |
| RFC mode buffer opens       | Evil uses motion state (read-only nav)    |
| RFC browser title           | Uses bold face (allows vertico highlights)|
| RFCs downloaded             | Stored in `~/.cache/emacs/rfc-mode/rfcs/` |

## API

### Commands (via help-map)

| Binding | Command                    | Purpose                         |
| :------ | :------------------------- | :------------------------------ |
| `C-h f` | `helpful-callable`         | Describe function or macro      |
| `C-h v` | `helpful-variable`         | Describe variable               |
| `C-h k` | `helpful-key`              | Describe key binding            |
| `C-h l` | `find-library`             | Find elisp library              |
| `C-h c` | `describe-face`            | Describe face                   |
| `C-h P` | `describe-text-properties` | Describe text properties        |
| `C-h s` | `info-apropos`             | Search info manuals             |
| `C-h w` | `rfc-mode-browse`          | Browse RFC documents            |

### Variables

| Variable            | Default                              | Purpose                     |
| :------------------ | :----------------------------------- | :-------------------------- |
| `help-window-select`| t                                    | Select help window on open  |
| `Man-notify-method` | `aggressive`                         | Select man page on open     |
| `rfc-mode-directory`| `~/.cache/emacs/rfc-mode/rfcs/`      | RFC cache directory         |

## Testable Properties

1. `C-h f` calls `helpful-callable` (not `describe-function`)
2. `C-h v` calls `helpful-variable` (not `describe-variable`)
3. `C-h h` is unbound
4. `C-h l` calls `find-library`
5. `help-window-select` is t
6. `Man-notify-method` is `aggressive`
7. Eldoc re-runs after evil state transitions
8. In help-mode, `^` is bound to `help-go-back`
9. In Info-mode, `^` is bound to `Info-up`
