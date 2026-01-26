# Compile Feature

Compilation mode with custom error regex parsers for diverse toolchains.

## Files

| File                     | Purpose                                |
| :----------------------- | :------------------------------------- |
| `init/init-inf.el`       | Comint and compile basics              |
| `config/mod-compilation.el` | Error regex parsers for 30+ tools   |
| `lisp/+compile.el`       | DSL for defining error parsers         |

## External Dependencies

None (uses built-in compile.el, comint.el)

## Behavior

### Comint Settings

| Setting                       | Value   | Purpose                          |
| :---------------------------- | :------ | :------------------------------- |
| `comint-prompt-read-only`     | `t`     | Prevent editing prompts          |
| `comint-buffer-maximum-size`  | `2048`  | Double default buffer size       |

### Compilation Settings

| Setting                       | Value         | Purpose                              |
| :---------------------------- | :------------ | :----------------------------------- |
| `compilation-always-kill`     | `t`           | Kill previous compilation on rerun   |
| `compilation-ask-about-save`  | `nil`         | Auto-save before compiling           |
| `compilation-scroll-output`   | `first-error` | Scroll to first error                |
| `compilation-message-face`    | `default`     | Plain face for compilation messages  |

### Visual Enhancements

1. **ANSI Colors**: `ansi-color-compilation-filter` on `compilation-filter-hook`
2. **URL Highlighting**: URLs fontified and clickable via `goto-address-fontify`
3. **RET Behavior**: Opens URL at point if present, otherwise `compile-goto-error`
4. **Error Line Face**: `next-error-message` inherits from `hl-line`

### Error Parser Architecture

All built-in parsers are disabled (`compilation-error-regexp-alist` set to nil).
Custom parsers defined via `define-compilation-error-rx` macro.

### Parsers by Tool Category

#### Generic

| Parser               | Pattern                               |
| :------------------- | :------------------------------------ |
| `generic`            | `file:line:col: message`              |
| `generic-no-message` | `file:line:col` (no message)          |

#### JavaScript/TypeScript

| Parser                 | Handles                                      |
| :--------------------- | :------------------------------------------- |
| `node-warnings`        | Node.js deprecation warnings                 |
| `js-error-stacktrace`  | `at func (file:line:col)` stack traces       |
| `typescript-tsc`       | TypeScript compiler errors (TS2305, etc.)    |
| `vitest-trace-line`    | Vitest test stack traces                     |
| `vitest-error`         | Vitest serialized errors                     |

#### Deno

| Parser                 | Handles                                      |
| :--------------------- | :------------------------------------------- |
| `deno-typecheck`       | Deno type errors (TS codes)                  |
| `deno-fmt`             | Deno lint/format errors                      |
| `deno-test-failure`    | Deno test failures                           |
| `deno-stacktrace`      | Deno stack traces (`at file://...`)          |

#### Zig

| Parser               | Handles                                |
| :------------------- | :------------------------------------- |
| `zig`                | Zig compiler errors/warnings/notes     |
| `zig-stack-line`     | Zig stack trace lines                  |

#### Terraform/Terragrunt

| Parser                       | Handles                                  |
| :--------------------------- | :--------------------------------------- |
| `terraform`                  | Terraform errors (box-drawing format)    |
| `terragrunt`                 | Terragrunt validation errors             |
| `terragrunt-err`             | Terragrunt ERROR log entries             |
| `terragrunt-info`            | Terragrunt info-level file references    |
| `terragrunt-unit-operation`  | Terragrunt unit processing logs          |
| `terragrunt-stack-modules`   | Terragrunt stack module list             |
| `terragrunt-unit-reference`  | Terragrunt unit processing messages      |

Terragrunt paths transform `.terragrunt-stack/` to `/` via `compilation-transform-file-match-alist`.

#### Elixir/BEAM

| Parser                         | Handles                              |
| :----------------------------- | :----------------------------------- |
| `elixirc`                      | Elixir compiler errors               |
| `elixir-mix`                   | Mix warnings/errors with hints       |
| `elixir-test-failure`          | ExUnit test failures                 |
| `elixir-test-stacktrace-line`  | Elixir test stack traces             |
| `beam-stacktrace`              | BEAM VM stack traces                 |

#### Rust

| Parser              | Handles                               |
| :------------------ | :------------------------------------ |
| `rustc`             | Rust compiler errors/warnings/notes   |
| `rust-panic`        | Thread panic messages                 |
| `rust-stacktrace`   | Rust panic stack traces               |

#### Other Tools

| Parser       | Tool      | Handles                              |
| :----------- | :-------- | :----------------------------------- |
| `actionlint` | actionlint| GitHub Actions workflow lint errors  |
| `tflint`     | tflint    | Terraform linter warnings/errors     |
| `trivy-file` | Trivy     | Trivy security scan file references  |

## API

### Macro: `define-compilation-error-rx`

```elisp
(define-compilation-error-rx NAME RX-FORMS...
  [:where SYMBOL = RX-FORM]*
  [:type error|warn|info|(N . M)]
  [:file GROUP-REF]
  [:line GROUP-REF]
  [:col GROUP-REF]
  [:hyperlink GROUP-REF]
  [:highlights ((GROUP FACE)...)])
```

Declarative macro for defining compilation error parsers with:
- **Metavariables**: `file`, `line`, `col`, `message` auto-expand to patterns
- **Named Groups**: `foo:` syntax creates named capture groups
- **`:where` Bindings**: Local pattern aliases for readability
- **`:type`**: Error level (`error`, `warn`, `info`, or conditional)
- **`:hyperlink`**: Which group becomes clickable
- **`:highlights`**: Additional face specs for groups

### Function: `+compile-pp-parser`

Interactive command to pretty-print a compilation parser definition with group number annotations.

### Constant: `+compile-metavars-alist`

Default metavariables available in `define-compilation-error-rx`:

| Metavar   | Pattern                              |
| :-------- | :----------------------------------- |
| `file`    | Alphanumeric path (no colons/newlines) |
| `line`    | Non-zero integer                     |
| `col`     | Non-zero integer                     |
| `message` | One or more printable characters     |

### Integration

- **imenu**: "Parsers" category added to `lisp-imenu-generic-expression`
- **consult-imenu**: "P" type for parser entries in `consult-imenu-config`

## Testable Properties

1. `comint-prompt-read-only` is `t`
2. `compilation-always-kill` is `t`
3. `compilation-ask-about-save` is `nil`
4. `compilation-scroll-output` is `first-error`
5. `compilation-error-regexp-alist` contains all custom parser names
6. Built-in parsers are disabled (no default entries in `compilation-error-regexp-alist`)
7. `ansi-color-compilation-filter` is in `compilation-filter-hook`
8. `define-compilation-error-rx` produces valid `compilation-error-regexp-alist-alist` entries
9. RET in compilation buffer opens URL at point when present
