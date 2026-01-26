# Feature: lang-text

Plain text file editing configuration.

## Files

| File             | Purpose                        |
| :--------------- | :----------------------------- |
| init/init-text.el | Text mode settings            |

## Packages

| Package    | Source   | Purpose                  |
| :--------- | :------- | :----------------------- |
| text-mode  | Built-in | Parent mode for text files |

## Behaviors

### Mode Associations

| Pattern     | Mode       |
| :---------- | :--------- |
| `/LICENSE`  | text-mode  |

### Settings

| Setting                         | Value | Rationale                    |
| :------------------------------ | :---- | :--------------------------- |
| `text-mode-ispell-word-completion` | nil | Disable ispell TAB completion |

### Disabled Features

| Feature          | Status   | Reason                        |
| :--------------- | :------- | :---------------------------- |
| visual-line-mode | Disabled | Performance impact uncertain  |

## Testable Properties

1. `/LICENSE` files open in text-mode
2. `text-mode-ispell-word-completion` is nil
3. TAB in text-mode does not trigger ispell completion
