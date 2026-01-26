# Feature: lang-erlang

Erlang support (currently disabled) with BEAM file hiding.

## Files

| File                | Purpose                       |
| :------------------ | :---------------------------- |
| init/init-erlang.el | Erlang mode and dired config  |

## Packages

| Package | Source   | Status   | Purpose            |
| :------ | :------- | :------- | :----------------- |
| erlang  | External | Disabled | Erlang major mode  |

## Behaviors

### Mode Associations (Disabled)

| Pattern   | Mode         | Status   |
| :-------- | :----------- | :------- |
| `*.erl`   | erlang-mode  | Disabled |
| `*.hrl`   | erlang-mode  | Disabled |

The erlang package is disabled because it's large and takes a long time to clone.

### BEAM File Hiding

Erlang/BEAM compilation artifacts are hidden from completion and dired.

| Setting                        | Extensions Added          |
| :----------------------------- | :------------------------ |
| `completion-ignored-extensions` | `.jam`, `.vee`, `.beam`  |
| `dired-omit-extensions`        | `.jam`, `.vee`, `.beam`  |

## Testable Properties

1. `.beam` files hidden in completion
2. `.jam` files hidden in completion
3. `.vee` files hidden in completion
4. `.beam` files omitted in dired-omit-mode
5. erlang package not loaded (disabled)
