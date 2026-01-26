# Tree-Sitter Feature Spec

Tree-sitter integration for modern syntax parsing.

## Files

| File                  | Purpose                                    |
| :-------------------- | :----------------------------------------- |
| `init/init-treesit.el`| tree-sitter config, expreg for selections  |

## Packages

| Package  | Source  | Purpose                             |
| :------- | :------ | :---------------------------------- |
| treesit  | built-in| Tree-sitter syntax parsing          |
| expreg   | MELPA   | Syntactic region expansion/contract |

## Behaviors

### Tree-Sitter Configuration

| Setting                      | Value    | Effect                              |
| :--------------------------- | :------- | :---------------------------------- |
| `treesit-enabled-modes`      | `t`      | Enable for all supported modes      |
| `treesit-auto-install-grammar`| `'always`| Auto-install missing grammars       |

### Expreg Region Expansion

| Keybinding | State          | Condition       | Action                        |
| :--------- | :------------- | :-------------- | :---------------------------- |
| `+`        | normal, motion | always          | `+expreg-expand-dwim`         |
| `-`        | normal, motion | region active   | `expreg-contract`             |
| `-`        | normal, motion | no region       | `avy-goto-char-timer`         |

### +expreg-expand-dwim Behavior

| Condition                    | Result                                     |
| :--------------------------- | :----------------------------------------- |
| iedit-mode active            | Exit iedit-mode first                      |
| Point on word = symbol       | Single expand (mark symbol/word)           |
| Point on symbol (not word)   | Two expands (mark full symbol)             |
| Neither word nor symbol      | Regular expreg-expand                      |

## API

### Commands

| Command              | Description                                   |
| :------------------- | :-------------------------------------------- |
| `+expreg-expand-n`   | Expand region N times (default 1)             |
| `+expreg-expand-dwim`| Smart expand: word → symbol → larger          |

## Design Decisions

1. **Automatic Grammar Installation**: Grammars are installed automatically
   rather than requiring manual setup, reducing friction for new languages.

2. **Universal Tree-Sitter**: All modes that support tree-sitter use it,
   providing consistent syntax handling across languages.

3. **DWIM Expansion**: `+expreg-expand-dwim` intelligently handles the common
   case of wanting to select the symbol at point, accounting for the difference
   between "words" (alphabetic) and "symbols" (includes dashes/underscores).

4. **Dual-Purpose `-`**: Rather than waste a prime keybinding, `-` contracts
   when there's a region and invokes avy-goto-char-timer otherwise, providing
   fast character-based navigation.

5. **iedit Integration**: Exiting iedit-mode before expanding prevents
   confusing interactions between multiple-cursor and region expansion states.

## Testable Properties

1. `treesit-enabled-modes` is t
2. `treesit-auto-install-grammar` is 'always
3. `+` in normal state calls `+expreg-expand-dwim`
4. `-` in normal state with no region calls `avy-goto-char-timer`
5. `-` in normal state with active region calls `expreg-contract`
6. `+expreg-expand-dwim` exits iedit-mode if active
7. `+expreg-expand-dwim` on word expands once to symbol
8. `+expreg-expand-dwim` on symbol (non-word) expands twice
