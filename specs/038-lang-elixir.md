# lang-elixir

Elixir development with Tree-sitter mode and LSP integration.

## Overview

Provides Elixir language support using the built-in `elixir-ts-mode` with:
- LSP integration via eglot (elixir-ls)
- Interactive REPL with inf-elixir
- Project detection via mix.exs
- Test file navigation via sibling rules
- File templates for modules and tests
- Tempel snippets for common patterns

## Files

| File                              | Purpose                           |
| :-------------------------------- | :-------------------------------- |
| `init/init-elixir.el`             | Mode configuration, LSP setup     |
| `templates/elixir-ts.eld`         | Tempel snippet definitions        |
| `file-templates/elixir/lib.eld`   | Module file template              |
| `file-templates/elixir/test.eld`  | Test file template                |

## Packages

| Package          | Source   | Purpose                      |
| :--------------- | :------- | :--------------------------- |
| `elixir-ts-mode` | built-in | Tree-sitter major mode       |
| `inf-elixir`     | elpaca   | Interactive Elixir REPL      |

## Behaviors

### Mode Association

| Pattern   | Mode             |
| :-------- | :--------------- |
| `*.ex`    | `elixir-ts-mode` |
| `*.exs`   | `elixir-ts-mode` |

### LSP Integration

| Trigger              | Behavior                        |
| :------------------- | :------------------------------ |
| Enter `elixir-ts-mode` | `eglot-ensure` starts elixir-ls |

Server program configured as `"elixir-ls"`.

### Project Detection

| Root Marker | Purpose                    |
| :---------- | :------------------------- |
| `mix.exs`   | Elixir/Phoenix project root |

Added to `project-vc-extra-root-markers`.

### Sibling File Navigation

Rules for navigating between implementation and tests:

| From                           | To                            |
| :----------------------------- | :---------------------------- |
| `*/lib/**/*.ex`                | `*/test/**/*_test.exs`        |
| `*/test/**/*_test.exs`         | `*/lib/**/*.ex`               |

Use `+find-sibling-file` (SPC f s) to switch.

### File Templates

#### Module Template (`lib.eld`)

For files in `/lib/` directory:

```elixir
defmodule MyApp.Path.To.Module do
  _cursor_
end
```

Module name derived from file path using PascalCase conversion.

#### Test Template (`test.eld`)

For files in `/test/` directory matching `*_test.exs`:

```elixir
defmodule MyApp.Path.To.ModuleTest do
  use ExUnit.Case

  _cursor_
end
```

### Tempel Snippets

14 snippets for Elixir development:

| Key    | Expands To                              |
| :----- | :-------------------------------------- |
| `im`   | `import ...`                            |
| `d`    | `def name do ... end`                   |
| `dd`   | `defp name do ... end`                  |
| `mo`   | `defmodule Name do ... end`             |
| `ds`   | `defstruct ...`                         |
| `dp`   | `defprotocol Name do ... end`           |
| `di`   | `defimpl Name, for: Type do ... end`    |
| `u`    | `use ...`                               |
| `ca`   | `case val do pat -> ... end`            |
| `re`   | `receive do pat -> ... end`             |
| `cast` | GenServer `handle_cast` with `@impl`    |
| `call` | GenServer `handle_call` with `@impl`    |

### inf-elixir Integration

Interactive REPL support via `inf-elixir` package:

| Command                      | Purpose                      |
| :--------------------------- | :--------------------------- |
| `inf-elixir`                 | Start Elixir REPL            |
| `inf-elixir-send-line`       | Send current line to REPL    |
| `inf-elixir-send-region`     | Send region to REPL          |
| `inf-elixir-send-buffer`     | Send buffer to REPL          |

## API

### Functions

None (uses standard eglot and inf-elixir functionality).

### Keybindings

Inherits standard eglot keybindings:

| Key       | Command              | Context          |
| :-------- | :------------------- | :--------------- |
| `M-RET`   | `eglot-code-actions` | elixir-ts-mode   |
| `C-c C-r` | `eglot-rename`       | elixir-ts-mode   |

Plus standard completion and navigation via eglot.

## Testable Properties

1. Opening `.ex` file activates `elixir-ts-mode`
2. Opening `.exs` file activates `elixir-ts-mode`
3. `eglot-server-programs` contains entry for `elixir-ts-mode`
4. `project-vc-extra-root-markers` contains `"mix.exs"`
5. `find-sibling-rules` contains lib↔test patterns
6. New file in `/lib/` uses module template with PascalCase name
7. New file in `/test/` uses test template with `use ExUnit.Case`
8. Tempel snippet `d` expands to `def` template
9. `inf-elixir` package available for REPL interaction
