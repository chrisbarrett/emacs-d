# Feature: reader

General-purpose document reader using native modules for performance.

## Files

| File                | Purpose                       |
| :------------------ | :---------------------------- |
| init/init-reader.el | Reader package initialization |

## Packages

| Package | Source | Purpose                              |
| :------ | :----- | :----------------------------------- |
| reader  | Native | Document reading with buffered rendering |

## Behaviors

### Package Injection

The reader package is a native module (compiled C/Rust) that is injected into Emacs via Nix. It is not installed through Elpaca.

### Autoloads

| When         | Then                           |
| :----------- | :----------------------------- |
| During init  | Load `reader-autoloads`        |

Autoloads are required at init time to register file associations and commands.

### Rendering

The reader package uses buffered rendering with native modules to improve performance when displaying documents.

## Dependencies

- Native module support in Emacs
- Nix package injection (package not available via Elpaca)

## Testable Properties

1. `reader-autoloads` feature is available
2. Reader commands are defined (autoloaded)
3. File type associations registered for supported formats
