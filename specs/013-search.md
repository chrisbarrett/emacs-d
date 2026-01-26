# Feature: search

Search tools: grep, wgrep, xref with ripgrep backend.

## Files

| File               | Purpose                              |
| :----------------- | :----------------------------------- |
| init/init-search.el | Search configuration                |

## Packages

| Package | Source   | Purpose                           |
| :------ | :------- | :-------------------------------- |
| replace | built-in | Search+replace, occur             |
| grep    | built-in | Grep results buffer               |
| wgrep   | ELPA     | Editable grep buffers             |
| xref    | built-in | Symbol navigation                 |

## Behaviors

### Occur Mode

| Given | When | Then |
| :---- | :--- | :--- |
| Occur buffer displayed | Buffer opens | hl-line-mode enabled |

### Grep Configuration

| Setting         | Value                                                    |
| :-------------- | :------------------------------------------------------- |
| grep-use-headings | t (group results by file)                              |
| grep-template   | Uses ripgrep with `--line-number --with-filename --null` |

### Wgrep

| Setting                | Value |
| :--------------------- | :---- |
| wgrep-auto-save-buffer | t     |

The wgrep package enables editing files directly from grep result buffers.
Changes are auto-saved when applying.

> Note: Replace with built-in `grep-edit-mode` when upgrading to Emacs 31.

### Xref

| Setting            | Value   |
| :----------------- | :------ |
| xref-search-program | ripgrep |

Xref uses ripgrep for project-wide symbol searches.

## Keybindings

Keybindings are defined in leader (007-leader.md):

| Key     | Command                 | Description          |
| :------ | :---------------------- | :------------------- |
| SPC /   | consult-ripgrep         | Search project (rg)  |
| SPC *   | +consult-ripgrep-symbol | Search symbol at point |
| SPC c m | xref-find-references    | Find references      |

## API

### Commands

| Command                  | Description                        |
| :----------------------- | :--------------------------------- |
| wgrep-change-to-wgrep-mode | Make grep buffer editable        |
| xref-find-definitions    | Jump to symbol definition          |
| xref-find-references     | Find all references to symbol      |
| consult-ripgrep          | Interactive ripgrep search (consult) |

## Testable Properties

1. `grep-use-headings` is t
2. `grep-template` contains "rg" command
3. `xref-search-program` is `ripgrep`
4. `wgrep-auto-save-buffer` is t
5. Opening occur buffer enables `hl-line-mode`
6. `consult-ripgrep` is bound to `SPC /`
7. `xref-find-references` is bound to `SPC c m`
