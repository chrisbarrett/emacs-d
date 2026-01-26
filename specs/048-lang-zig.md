# Feature: lang-zig

Zig language support with zig-mode and Tempel snippets.

## Files

| File              | Purpose                |
| :---------------- | :--------------------- |
| init/init-zig.el  | Zig mode configuration |

## Packages

| Package   | Source   | Purpose          |
| :-------- | :------- | :--------------- |
| zig-mode  | External | Zig major mode   |

## Behaviors

### Mode Associations

| Pattern    | Mode      |
| :--------- | :-------- |
| `*.zig`    | zig-mode  |
| `*.zon`    | zig-mode  |

### Formatter

| Setting              | Value | Rationale                |
| :------------------- | :---- | :----------------------- |
| `zig-format-on-save` | nil   | Use apheleia instead     |

Format-on-save is handled by apheleia (the format feature) rather than zig-mode's built-in formatter.

### Tempel Snippets

| Abbrev | Expansion                           |
| :----- | :---------------------------------- |
| `a`    | Array declaration                   |
| `v`    | Variable declaration (var)          |
| `c`    | Constant declaration (const)        |
| `im`   | Import statement                    |
| `f`    | Function definition                 |
| `pf`   | Public function definition          |
| `r`    | Return statement                    |
| `i`    | If statement                        |
| `ei`   | Else if clause                      |
| `e`    | Else clause                         |
| `ie`   | If-else statement                   |
| `wh`   | While loop                          |
| `p`    | Debug print (std.debug.print)       |
| `for`  | For loop                            |
| `err`  | Error type definition               |
| `sw`   | Switch statement                    |

## Testable Properties

1. `*.zig` files open in zig-mode
2. `*.zon` files open in zig-mode
3. `zig-format-on-save` is nil
4. Tempel snippet `f` available in zig-mode
5. Tempel snippet `pf` available in zig-mode
6. Tempel snippet `im` available in zig-mode
