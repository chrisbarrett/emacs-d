# Feature: lang-latex

LaTeX editing support with format-on-save via latexindent.

## Files

| File              | Purpose                     |
| :---------------- | :-------------------------- |
| init/init-latex.el | Latexindent formatter setup |

## Packages

| Package   | Source   | Purpose                |
| :-------- | :------- | :--------------------- |
| tex-mode  | Built-in | LaTeX major mode       |
| apheleia  | External | Format-on-save backend |

## Behaviors

### Formatter Configuration

| Setting                     | Value                              |
| :-------------------------- | :--------------------------------- |
| Formatter                   | `latexindent`                      |
| Log file                    | `/dev/null` (disabled)             |
| Indent style (tabs mode)    | Tab character (`\t`)               |
| Indent style (spaces mode)  | Two spaces (`"  "`)                |
| Respects `indent-tabs-mode` | Yes, when `apheleia-formatters-respect-indent-level` is t |

### Formatter Arguments

The latexindent formatter is configured with:

```elisp
'("latexindent" "--logfile=/dev/null"
  (when apheleia-formatters-respect-indent-level
    (if indent-tabs-mode
        "-y=defaultIndent:\"\\t\""
      "-y=defaultIndent:\"  \"")))
```

## Dependencies

- `latexindent` executable (from texlive or similar)
- `apheleia` package (configured in format feature)

## Testable Properties

1. `apheleia-formatters` contains `latexindent` entry after tex-mode loads
2. Formatter command includes `--logfile=/dev/null`
3. Formatter command respects `indent-tabs-mode` when `apheleia-formatters-respect-indent-level` is t
4. Format-on-save triggers latexindent for .tex files
