# Debug Feature Spec

Enhancements to the built-in Emacs Lisp debugger for better usability.

## Files

| File             | Purpose                                 |
| :--------------- | :-------------------------------------- |
| init/init-debug.el | Debugger configuration and enhancements |

## Dependencies

| Type     | Name      | Purpose                     |
| :------- | :-------- | :-------------------------- |
| built-in | debug     | Emacs Lisp debugger         |
| internal | +corelib  | `setq-hook!` macro          |

## Behaviors

### Toggle Debug on Exit Frame

| When                        | Then                                              |
| :-------------------------- | :------------------------------------------------ |
| Press `t` in debugger       | Toggle debug-on-exit for current frame            |
| Frame marked with `*`       | Clear the mark, disable debug on exit             |
| Frame not marked            | Set the mark, enable debug on exit                |
| Toggle occurs               | Message displayed indicating new state            |

### Debugger Record Buffer Display

| When                           | Then                                     |
| :----------------------------- | :--------------------------------------- |
| Evaluate with `R` (record)     | Result displayed via `display-buffer`    |
| `debugger-record-buffer` used  | Buffer appears in appropriate window     |

### Custom Mode Line

| When                  | Then                                               |
| :-------------------- | :------------------------------------------------- |
| Enter debugger buffer | Mode line shows key reference                      |
| Mode line displays    | `d`:step, `c`:continue, `r`:return in first group  |
| Mode line displays    | `t`:toggle, `J`:jump, `L`:locals in second group   |
| Mode line displays    | `E`:eval, `R`:eval & record in third group         |

## API

### Commands

| Command                        | Description                               |
| :----------------------------- | :---------------------------------------- |
| `+debugger-toggle-on-exit-frame` | Toggle debug-on-exit for current frame  |

### Keybindings

| Key | Mode           | State  | Binding                          |
| :-- | :------------- | :----- | :------------------------------- |
| `t` | debugger-mode  | normal | `+debugger-toggle-on-exit-frame` |

### Variables

| Variable                   | Value                        | Purpose                    |
| :------------------------- | :--------------------------- | :------------------------- |
| `+debugger-mode-line-format` | Formatted string           | Custom mode line for debugger |

## Testable Properties

1. `+debugger-toggle-on-exit-frame` is defined as interactive command
2. `+debugger-mode-line-format` is a non-empty string
3. `t` bound in debugger-mode-map normal state
4. Mode line format contains all documented key references (d, c, r, t, J, L, E, R)
5. `debugger-record-expression` has advice for display-buffer
