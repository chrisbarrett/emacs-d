# Feature: format

Code formatting, whitespace management, and indentation behavior.

## Files

| File               | Purpose                               |
| :----------------- | :------------------------------------ |
| init/init-format.el | Apheleia, align, whitespace trimming |

## External Packages

| Package   | Purpose                    |
| :-------- | :------------------------- |
| apheleia  | Format-on-save framework   |
| ws-butler | Conservative whitespace trimming (disabled) |

## Behaviors

### Align

| When                | Then                         |
| :------------------ | :--------------------------- |
| `C-x a a`           | Invoke `align-regexp`        |

### Indentation

| Setting                       | Value                   |
| :---------------------------- | :---------------------- |
| `tab-first-completion`        | `word-or-paren-or-punct` |

### Apheleia (Format-on-Save)

| When                          | Then                                |
| :---------------------------- | :---------------------------------- |
| First file opened             | Enable `apheleia-global-mode`       |
| File saved                    | Format buffer with language formatter |

| Setting                               | Value   | Effect                        |
| :------------------------------------ | :------ | :---------------------------- |
| `apheleia-remote-algorithm`           | `local` | Format TRAMP files locally    |
| `apheleia-formatters-respect-fill-column` | `t` | Pass fill-column to formatters |

### Whitespace Trimming

| Variable                                  | Default | Purpose                         |
| :---------------------------------------- | :------ | :------------------------------ |
| `+trim-trailing-whitespace-aggressively` | `t`     | Buffer-local trim control       |

| When                                           | Then                          |
| :--------------------------------------------- | :---------------------------- |
| `before-save-hook` with aggressive trim on    | Delete all trailing whitespace |
| `before-save-hook` with aggressive trim nil   | No whitespace trimming        |

### Tabify

| Setting         | Value             | Effect                        |
| :-------------- | :---------------- | :---------------------------- |
| `tabify-regexp` | `"^\t* [ \t]+"`  | Only tabify at line beginning |

## API

### Variables

| Symbol                                   | Type    | Purpose                       |
| :--------------------------------------- | :------ | :---------------------------- |
| `+trim-trailing-whitespace-aggressively` | boolean | Per-buffer whitespace control |

## Testable Properties

| Property                   | Verification                                   |
| :------------------------- | :--------------------------------------------- |
| Apheleia loads lazily      | Not loaded until first file opened             |
| Apheleia global mode on    | `apheleia-global-mode` is non-nil after file   |
| Remote format local        | `apheleia-remote-algorithm` eq `local`         |
| Trim whitespace on save    | Trailing whitespace removed after save         |
| Trim respects buffer-local | Setting nil prevents trimming                  |
| Align keybinding           | `C-x a a` invokes `align-regexp`               |
| Tab completion behavior    | `tab-first-completion` eq `word-or-paren-or-punct` |
