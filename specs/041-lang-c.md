# Feature: lang-c

C language development support with Tree-sitter and smart editing.

## Files

| File           | Purpose                                      |
| :------------- | :------------------------------------------- |
| init/init-c.el | c-ts-mode configuration, electric insertion |

## Packages

| Package    | Source   | Purpose                          |
| :--------- | :------- | :------------------------------- |
| c-ts-mode  | built-in | Tree-sitter major mode for C     |

## Behaviors

### Mode Remapping

| Original Mode | Remapped To |
| :------------ | :---------- |
| c-mode        | c-ts-mode   |

Tree-sitter mode is used automatically when opening C files.

### Electric Angle Bracket

In insert state, `<` is electric on `#include` lines:

| Context                    | Result                          |
| :------------------------- | :------------------------------ |
| On `#include` line         | Insert `<>`, cursor inside      |
| Other lines                | Normal `<` insertion            |

The function normalizes spacing before the angle bracket.

### Auto Semicolon Newline

`S-RET` provides smart line completion:

| Current Line Ends With | Action                                    |
| :--------------------- | :---------------------------------------- |
| `{`, `:`, or `;`       | Newline only                              |
| Other                  | Insert `;` at EOL, then newline and indent |

After newline, enters evil insert state.

## Keybindings

### c-ts-mode-map

| Key     | State  | Command                         | Description           |
| :------ | :----- | :------------------------------ | :-------------------- |
| `<`     | insert | +c-electric-left-angle-bracket  | Smart angle brackets  |
| `S-RET` | normal | +c-auto-insert-semi-newline     | Auto-semicolon newline|
| `S-RET` | insert | +c-auto-insert-semi-newline     | Auto-semicolon newline|

## API

### Commands

| Command                        | Description                               |
| :----------------------------- | :---------------------------------------- |
| +c-electric-left-angle-bracket | Insert `<>` pair on include lines         |
| +c-auto-insert-semi-newline    | Insert semicolon if needed, then newline  |

## Testable Properties

1. Opening `.c` file activates c-ts-mode (not c-mode)
2. On `#include` line, `<` inserts `<>` with cursor between
3. On non-include line, `<` inserts single `<`
4. S-RET on line ending with text inserts `;` and newline
5. S-RET on line ending with `{` inserts only newline
6. S-RET on line ending with `:` inserts only newline
7. S-RET on line ending with `;` inserts only newline
