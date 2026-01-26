# Input Methods Feature Spec

## Overview

French postfix input method with smart punctuation handling for French spacing
conventions.

## Files

| File                       | Purpose                               |
| :------------------------- | :------------------------------------ |
| `config/mod-input-methods.el` | Quail package customization        |

## External Packages

None (uses built-in quail)

## Behaviors

### Default Input Method

| Setting                         | Value            |
| :------------------------------ | :--------------- |
| `default-input-method`          | `french-postfix` |
| `default-transient-input-method`| `french-postfix` |

### Smart Semicolon (;)

When french-postfix is active, typing `;` inserts French-style semicolon with
surrounding spaces.

**Given** french-postfix input method is active
**When** user types `;`
**Then** horizontal space before point is deleted and ` ; ` is inserted

Example:
```
word|  ->  word ;|
```

### Smart Colon (:)

When french-postfix is active, typing `:` inserts French-style colon with
context-aware spacing.

**Given** french-postfix input method is active
**When** user types `:`
**Then** horizontal space before point is deleted and appropriate spacing is
applied

| Context                          | Behavior          |
| :------------------------------- | :---------------- |
| Normal text                      | ` : ` (spaced)    |
| After existing colon             | `: ` (no left pad)|
| Org-mode list item (non-desc)    | ` : ` (spaced)    |

### +quail-defun Macro

The `+quail-defun` macro enables defining quail keys that execute arbitrary
Lisp forms instead of simple character insertion.

```elisp
(+quail-defun PACKAGE-NAME KEY &rest BODY)
```

- `PACKAGE-NAME`: quail package name string (e.g., "french-postfix")
- `KEY`: key string to intercept
- `BODY`: forms to execute when key is typed

The macro:
1. Creates a function that deletes pending quail conversion
2. Clears quail state (`quail-current-str`, `quail-converting`, `quail-conversion-str`)
3. Executes BODY in an `atomic-change-group`
4. Throws to `quail-tag` to complete quail processing

## API

### Macros

| Macro           | Purpose                                    |
| :-------------- | :----------------------------------------- |
| `+quail-defun`  | Define quail key to execute Lisp forms     |

### Variables (Built-in)

| Variable                         | Value            | Purpose                    |
| :------------------------------- | :--------------- | :------------------------- |
| `default-input-method`           | `french-postfix` | Default input method       |
| `default-transient-input-method` | `french-postfix` | Transient input method     |

## Testable Properties

1. `default-input-method` equals "french-postfix"
2. `default-transient-input-method` equals "french-postfix"
3. In french-postfix mode, typing ";" deletes horizontal space and inserts " ; "
4. In french-postfix mode, typing ":" deletes horizontal space and inserts " : "
5. +quail-defun is a macro with correct argument list
6. +quail-defun creates function that clears quail state before executing body
