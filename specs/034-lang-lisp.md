# Emacs Lisp Development

Emacs Lisp development configuration with testing, linting, and enhanced indentation.

## Files

| File                        | Purpose                                    |
| :-------------------------- | :----------------------------------------- |
| `init/init-lisp.el`         | elisp-mode, ert, checkdoc, flymake config  |
| `lisp/+consult-imenu-elisp.el` | Imenu visibility grouping for consult   |

## External Packages

| Package        | Purpose                              |
| :------------- | :----------------------------------- |
| flymake-eldev  | Flymake backend for eldev projects   |
| buttercup      | BDD-style testing framework          |

## Dependencies

- +corelib
- +file-templates
- helpful (optional, for K lookup)
- evil (for visual state handling)

## Behaviors

### Profiler Mode

| Keybinding | Command                          | State  |
| :--------- | :------------------------------- | :----- |
| `A`        | profiler-report-ascending-sort   | normal |
| `D`        | profiler-report-descending-sort  | normal |
| `K`        | profiler-report-describe-entry   | normal |

### Parenthesis Checking

`check-parens` runs before save in:
- `lisp-data-mode`
- `emacs-lisp-mode`

### Sibling File Rules

Navigate between implementation and test files:

| From Pattern          | To Pattern           |
| :-------------------- | :------------------- |
| `*-tests.el`          | `*.el`               |
| `*.el`                | `*-tests.el`         |

### Emacs Config Mode

Minor mode `emacs-config-mode` enabled via dir-locals for:
- `~/.config/emacs/lisp/`
- `~/.config/emacs/init/`
- `~/.config/emacs/config/`

Effect: Adds `lisp/` to `elisp-flymake-byte-compile-load-path`.

### Elpaca Load Path Integration

For emacs-config-mode buffers, elpaca build directories are added to flymake's byte-compile load path.

### Elisp Mode Keybindings

| Keybinding  | Command                    | Purpose               |
| :---------- | :------------------------- | :-------------------- |
| `C-c RET`   | pp-macroexpand-last-sexp   | Expand macro at point |
| `C-c C-c`   | +elisp-eval-dwim           | Context-sensitive eval|

### Local Leader (emacs-lisp-mode)

| Keybinding  | Command              | Purpose             |
| :---------- | :------------------- | :------------------ |
| `, e`       | (prefix)             | Eval commands       |
| `, eb`      | +elisp-eval-buffer   | Eval entire buffer  |
| `, t`       | (prefix)             | Test commands       |
| `, tt`      | +ert                 | Run tests           |
| `, td`      | ert-delete-test      | Delete test         |
| `, tD`      | ert-delete-all-tests | Delete all tests    |

### Eval Commands

`+elisp-eval-dwim`:
- With region: eval-region and message result
- Without region: eval-defun and message result
- After visual eval: returns to normal state

`+elisp-eval-buffer`:
- Calls eval-buffer with inhibit-redisplay
- Messages "Buffer evaluated" on completion

### Evil K Lookup

In emacs-lisp-mode, `K` (evil-lookup) calls:
1. `helpful-at-point` if helpful is available
2. Falls back to `describe-symbol`

### Visual State After Eval

Advice on `eval-region` exits visual state after evaluation completes.

### Prettify Symbols

`prettify-symbols-mode` enabled in emacs-lisp-mode.
Default renders `lambda` as `λ`.

### File Template

Files matching `*.el` use the `emacs-lisp.eld` template.

### Checkdoc

| Setting                       | Value |
| :---------------------------- | :---- |
| checkdoc-force-docstrings-flag | nil   |

### ERT Configuration

| Keybinding | Command                                      | Keymap               |
| :--------- | :------------------------------------------- | :------------------- |
| `C-c C-t`  | +ert                                         | ert-results, elisp   |
| `L`        | ert-results-toggle-printer-limits-for-test-at-point | ert-results (motion) |
| `T`        | ert-results-pop-to-timings                   | ert-results (motion) |
| `B`        | ert-results-pop-to-backtrace-for-test-at-point | ert-results (motion) |
| `H`        | ert-results-describe-test-at-point           | ert-results (motion) |
| `M-n`      | ert-results-next-test                        | ert-results (motion) |
| `M-p`      | ert-results-previous-test                    | ert-results (motion) |

### Flymake Eldev

`flymake-eldev` provides eldev-aware byte-compile checking:
- Autoloaded at init time
- Respects project's `Eldev` file for dependencies

### Buttercup

BDD testing framework loaded after elisp-mode:
- Ensures macro indentation works correctly
- Ordinarily installed by eldev, loaded here for editor support

### Improved Lisp Indentation

Custom `calculate-lisp-indent` with:
- Better quoted/backquoted list indentation
- Plist keyword alignment when newlines begin with `:` keywords
- Works with unquoted plist macros

## consult-imenu Visibility Grouping

Splits imenu categories by symbol visibility.

### Classification Rules

| Pattern     | Classification |
| :---------- | :------------- |
| `^_`        | Internal       |
| Contains `--` | Internal     |
| Otherwise   | Public         |

### Excluded Categories

Not split by visibility:
- "Sections"
- "Headings"
- "Package"
- Any category containing "Section" or "Heading"

### Transformation

Categories become:
- `Functions (Public)` - Public functions
- `Functions (Internal)` - Internal functions (prefixed with `_` or containing `--`)

### API

| Function                        | Purpose                              |
| :------------------------------ | :----------------------------------- |
| +consult-imenu-elisp-enable     | Enable visibility grouping           |
| +consult-imenu-elisp-disable    | Disable visibility grouping          |

| Variable                                    | Purpose                        |
| :------------------------------------------ | :----------------------------- |
| +consult-imenu-elisp-excluded-categories    | Categories to not split        |

## API Summary

### Commands

| Function             | Purpose                          |
| :------------------- | :------------------------------- |
| +elisp-eval-dwim     | Context-sensitive eval           |
| +elisp-eval-buffer   | Eval buffer with feedback        |
| +ert                 | Run ERT tests                    |

### Minor Modes

| Mode              | Purpose                             |
| :---------------- | :---------------------------------- |
| emacs-config-mode | Enables config-specific load paths  |

## Testable Properties

1. check-parens is in emacs-lisp-mode before-save-hook
2. find-sibling-rules contains -tests.el pattern
3. C-c C-c bound to +elisp-eval-dwim in emacs-lisp-mode
4. checkdoc-force-docstrings-flag is nil
5. C-c C-t bound to +ert in emacs-lisp-mode
6. flymake-eldev autoloads are required
7. buttercup loads after elisp-mode
8. +consult-imenu-elisp--internal-p returns t for "foo--bar"
9. +consult-imenu-elisp--internal-p returns t for "_private"
