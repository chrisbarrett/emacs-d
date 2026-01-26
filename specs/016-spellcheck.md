# Feature: Spellcheck

Lightweight spell checking with aspell backend and vim-style keybindings.

## Dependencies

### External Packages

| Package           | Purpose                              |
| :---------------- | :----------------------------------- |
| spell-fu          | Lightweight on-the-fly spell checker |
| flyspell-correct  | Correction popup interface           |

### Built-in

| Package | Purpose                      |
| :------ | :--------------------------- |
| ispell  | Aspell configuration backend |

### System

| Program | Purpose              |
| :------ | :------------------- |
| aspell  | Spell checking engine |

## Behavior

### Ispell Configuration

| Setting              | Value                                   |
| :------------------- | :-------------------------------------- |
| `ispell-dictionary`  | `"en_AU"`                               |
| `ispell-personal-dictionary` | `<org-directory>/aspell.en.pws` |

- Personal dictionary encoded as UTF-8 (via `file-coding-system-alist`)
- Warns if aspell executable not found in PATH

### Spell-fu

**Given** `text-mode`, `prog-mode`, or `conf-mode` is active
**When** the mode initializes
**Then** spell-fu-mode is enabled

**Given** spell-fu-mode is active
**When** the mode hook runs
**Then** both en_AU and fr dictionaries are loaded

#### Org-mode Face Exclusions

**Given** org-mode is active
**When** spell-fu checks for errors
**Then** these faces are excluded from checking:
- `org-meta-line`
- `org-link`
- `org-code`
- `org-block`
- `org-block-begin-line`
- `org-block-end-line`
- `org-footnote`
- `org-tag`
- `org-modern-tag`
- `org-verbatim`

### Flyspell-correct

Provides correction interface via `flyspell-correct-at-point`.

## API

### Keybindings

| Key     | Command                        | State         |
| :------ | :----------------------------- | :------------ |
| `zn`    | `spell-fu-goto-next-error`     | normal/motion |
| `zp`    | `spell-fu-goto-previous-error` | normal/motion |
| `zg`    | `spell-fu-word-add`            | normal/motion |
| `zx`    | `spell-fu-word-remove`         | normal/motion |
| `z SPC` | `flyspell-correct-at-point`    | normal        |

## Properties to Verify

1. **P1**: `ispell-dictionary` equals `"en_AU"`
2. **P2**: `ispell-personal-dictionary` ends with `"aspell.en.pws"`
3. **P3**: `spell-fu-mode-hook` is added to `text-mode-hook`
4. **P4**: `spell-fu-mode-hook` is added to `prog-mode-hook`
5. **P5**: `spell-fu-mode-hook` is added to `conf-mode-hook`
6. **P6**: `zn` is bound to `spell-fu-goto-next-error` in normal state
7. **P7**: `zg` is bound to `spell-fu-word-add` in normal state
8. **P8**: `z SPC` is bound to `flyspell-correct-at-point` in normal state
9. **P9**: In org-mode, `spell-fu-faces-exclude` includes `org-link`
