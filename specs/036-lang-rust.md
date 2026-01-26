# lang-rust

Rust development with Tree-sitter mode and LSP integration.

## Overview

Provides Rust language support using the built-in `rust-ts-mode` with:
- LSP integration via eglot (rust-analyzer)
- Markdown-based code editing with separedit
- Colored Cargo output
- Tempel snippets for common patterns
- Compilation error parsing for rustc output

## Files

| File                         | Purpose                           |
| :--------------------------- | :-------------------------------- |
| `init/init-rust.el`          | Mode configuration, LSP setup     |
| `templates/rust-ts.eld`      | Tempel snippet definitions        |
| `config/mod-compilation.el`  | Error parsers (rustc, panic, stacktrace) |

## Packages

| Package       | Source   | Purpose                      |
| :------------ | :------- | :--------------------------- |
| `rust-ts-mode`| built-in | Tree-sitter major mode       |

No external packages required.

## Behaviors

### LSP Integration

| Trigger                        | Behavior                              |
| :----------------------------- | :------------------------------------ |
| Enter `rust-ts-mode`           | `eglot-ensure` starts rust-analyzer   |

Hooks into `rust-ts-mode-local-vars-hook` for proper timing.

### Editor Configuration

| Setting                        | Value                | Purpose                       |
| :----------------------------- | :------------------- | :---------------------------- |
| `separedit-default-mode`       | `markdown-mode`      | Rust doc comments are Markdown |
| `CARGO_TERM_COLOR` env var     | `"always"`           | Colored Cargo output in Emacs |

### Tempel Snippets

31 snippets for Rust development:

| Key       | Expands To                           |
| :-------- | :----------------------------------- |
| `ec`      | `extern crate ...;`                  |
| `u`       | `use ...;`                           |
| `l`       | `let ... = ...;`                     |
| `lm`      | `let mut ... = ...;`                 |
| `ep`      | `eprintln!("...");`                  |
| `c`       | `const ... = ...;`                   |
| `t`       | `type ... = ...;`                    |
| `i`       | `if PRED { ... }`                    |
| `il`      | `if let ... = ... { ... }`           |
| `ei`      | `else if ... { ... }`                |
| `e`       | `else { ... }`                       |
| `ie`      | `if PRED { ... } else { ... }`       |
| `r`       | `return ...;`                        |
| `for`     | `for VAR in ITEMS { ... }`           |
| `m`       | `match TERM { ... }`                 |
| `st`      | `struct NAME { ... }`                |
| `ps`      | `pub struct NAME { ... }`            |
| `tr`      | `trait NAME { ... }`                 |
| `pt`      | `pub trait NAME { ... }`             |
| `en`      | `enum NAME { ... }`                  |
| `pe`      | `pub enum NAME { ... }`              |
| `im`      | `impl NAME { ... }`                  |
| `f`       | `fn NAME(...) ... { ... }`           |
| `pf`      | `pub fn NAME(...) ... { ... }`       |
| `der`     | `#[derive(...)]`                     |
| `tests`   | Test module with `#[cfg(test)]`      |
| `test`    | Single `#[test]` function            |

### Compilation Error Parsers

Three custom parsers for Rust output:

| Parser             | Pattern                                           | Type   |
| :----------------- | :------------------------------------------------ | :----- |
| `rustc`            | `error[E0000]: message` → `file:line:col`         | error/warning/info |
| `rust-panic`       | `thread '...' panicked at file:line:col`          | error  |
| `rust-stacktrace`  | Stack frame `at file:line:col`                    | info   |

### Org Babel Integration

Source block language mappings (in `mod-org.el`):

| Org block     | Mode           |
| :------------ | :------------- |
| `rs`          | `rust-ts`      |
| `rust`        | `rust-ts`      |

## API

### Functions

None (uses standard eglot and built-in functionality).

### Keybindings

Inherits standard eglot keybindings:

| Key         | Command                | Context        |
| :---------- | :--------------------- | :------------- |
| `M-RET`     | `eglot-code-actions`   | rust-ts-mode   |
| `C-c C-r`   | `eglot-rename`         | rust-ts-mode   |

Plus standard completion and navigation via eglot.

## Testable Properties

1. `rust-ts-mode-local-vars-hook` contains `eglot-ensure`
2. Opening `.rs` file activates `rust-ts-mode`
3. `CARGO_TERM_COLOR` env var is `"always"` after loading init-rust
4. `separedit-default-mode` is `markdown-mode` in rust buffers
5. Tempel snippet `pf` expands to `pub fn` template
6. `rustc` compilation errors navigate to correct location
7. Panic output parsed as compilation error
8. Stacktrace lines parsed as info-level compilation messages
9. Org source blocks tagged `rust` use `rust-ts-mode`
