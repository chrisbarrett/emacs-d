# Feature: Consult Imenu Elisp Visibility Grouping

Split Emacs Lisp imenu categories by symbol visibility (public vs internal).

## Dependencies

- `consult` package
- `imenu` (built-in)

## Consumed API

### consult-imenu

`consult-imenu-config` — alist mapping major modes to imenu configuration.

### imenu

`imenu--index-alist` — buffer-local alist of `(NAME . POSITION)` or
`(CATEGORY . ((NAME . POSITION) ...))` entries.

## Behavior

### Classification Rules

| Pattern              | Visibility | Example            |
| -------------------- | ---------- | ------------------ |
| `package--symbol`    | Internal   | `my-pkg--helper`   |
| `_symbol`            | Internal   | `_temp-var`        |
| `package-symbol`     | Public     | `my-pkg-init`      |
| `symbol` (no prefix) | Public     | `main`             |

Detection: internal if matches `^_` or contains `--`.

### Category Transformation

Input categories from `imenu`:

| Original Category | Output Categories                          |
| ----------------- | ------------------------------------------ |
| Functions         | Functions (Public), Functions (Internal)   |
| Variables         | Variables (Public), Variables (Internal)   |
| Faces             | Faces (Public), Faces (Internal)           |
| Types             | Types (Public), Types (Internal)           |
| Macros            | Macros (Public), Macros (Internal)         |

### Excluded Categories

Structural categories pass through unchanged:
- Sections
- Headings
- Package
- Any category containing "Section" or "Heading"

### Ordering

Within each original category's output:
1. Public group first
2. Internal group second

Empty groups are omitted.

## Provided API

| Symbol                                    | Type     | Description                          |
| ----------------------------------------- | -------- | ------------------------------------ |
| `+consult-imenu-elisp-enable`             | command  | Enable visibility grouping           |
| `+consult-imenu-elisp--internal-p`        | function | Test if symbol name is internal      |
| `+consult-imenu-elisp--transform-alist`   | function | Transform imenu alist with grouping  |
| `+consult-imenu-elisp-excluded-categories`| defcustom| Categories to exclude from splitting |

## Properties to Verify

1. `my-pkg--helper` classified as internal
2. `_temp` classified as internal
3. `my-pkg-init` classified as public
4. `main` classified as public
5. Functions category splits into Functions (Public) and Functions (Internal)
6. Public groups appear before Internal groups in output
7. Sections category passes through unchanged
8. Empty groups omitted from output
9. Works with consult-imenu in emacs-lisp-mode

## Testing Strategy

- Unit test `+consult-imenu-elisp--internal-p` with various symbol patterns
- Test `+consult-imenu-elisp--transform-alist` with mock imenu alists
- Test excluded categories pass through unchanged
- Test empty group omission
- Integration test with actual Emacs Lisp buffer
