# 039 lang-ocaml

OCaml language support with neocaml tree-sitter mode, LSP integration, and Tempel snippets.

## Files

| File | Purpose |
|:-----|:--------|
| `init/init-ocaml.el` | Mode setup, LSP, formatter, dune support |
| `config/mod-ocaml.el` | Tempel context helpers for let bindings |
| `templates/ocaml.eld` | 8 Tempel snippets |

## Dependencies

| Package | Source | Purpose |
|:--------|:-------|:--------|
| neocaml | GitHub (bbatsov/neocaml) | Tree-sitter major mode |
| ocaml-eglot | MELPA | Extended LSP functionality |
| (ocamllsp) | External | Language server |
| (ocamlformat) | External | Code formatter |

## Behaviors

### Mode Associations

| Pattern | Mode |
|:--------|:-----|
| `*.ml` | `neocaml-mode` |
| `*.mli` | `neocamli-mode` |
| `.ocamlinit` | `neocaml-mode` |
| `*.opam` | `conf-colon-mode` |
| `dune`, `dune-workspace`, `dune-project` | `dune-config-mode` |

### Read-Only Protection

| Path Pattern | Condition |
|:-------------|:----------|
| `/_build/` | Always read-only via dirlocals |
| `*.opam` | Read-only if contains "# This file is generated" |

### neocaml-mode

- Tree-sitter based OCaml major mode (experimental, replaces tuareg)
- Enables `neocaml-repl-minor-mode` for REPL integration
- Formatter: ocamlformat via apheleia
- LSP: ocamllsp via ocaml-eglot hook on `neocaml-mode-local-vars-hook`

### neocamli-mode

- Tree-sitter based OCaml interface file mode
- Formatter: ocamlformat via apheleia
- LSP: ocamllsp with `language-id "ocaml"` for both modes

### dune-config-mode

Custom derived mode for Dune build files:
- Inherits from `lisp-data-mode` (S-expression syntax)
- `comment-add` set to 0 (single semicolon comments)

### Project Detection

- `dune-project` added to `project-vc-extra-root-markers`
- Projects with dune-project are recognized as project roots

### Org Babel Integration

| Block Type | Mode |
|:-----------|:-----|
| `ocaml` | `neocaml-mode` |
| `ocamli` | `neocamli-mode` |

## Tempel Snippets

### Shared (neocaml-mode and neocamli-mode)

| Key | Expansion |
|:----|:----------|
| `t` | `type NAME = BODY` |

### Interface Only (neocamli-mode)

| Key | Expansion |
|:----|:----------|
| `v` | `val NAME : TYPE` |

### Implementation Only (neocaml-mode)

| Key | Expansion |
|:----|:----------|
| `l` | `let NAME = BODY [in]` |
| `lr` | `let rec NAME = BODY [in]` |
| `m` | `match EXPR with \| PATTERN` |
| `ms` | `module NAME = struct ... end` |
| `mt` | `module type NAME = sig ... end` |
| `inc` | `include MODULE` |

### Context-Aware Let Bindings

The `l` and `lr` snippets use tree-sitter context detection:
- **Module level**: `let x = ...` (no `in` suffix)
- **Expression context**: `let x = ... in` (adds `in` suffix)

Detection logic:
1. Find ancestor node from point
2. If ancestor is `let_binding` and point is in body field, expression context
3. If ancestor is `structure` or `signature`, module level

## API

### Public Functions

| Function | Purpose |
|:---------|:--------|
| `+ocaml-capture-let-context` | Tempel helper: captures context, returns "" |
| `+ocaml-maybe-in` | Tempel helper: returns " in" or "" based on context |

### Internal Functions

| Function | Purpose |
|:---------|:--------|
| `+ocaml--treesit-in-expr-context-p` | Detect expression vs module context |
| `+ocaml--point-in-node-field-p` | Check if point is within node's field |

### Buffer-Local Variables

| Variable | Purpose |
|:---------|:--------|
| `+ocaml--tempel-in-expr-p` | Stores context during template expansion |

## Design Decisions

### neocaml vs tuareg

Uses neocaml (tree-sitter) instead of tuareg:
- Better syntax highlighting via tree-sitter
- Consistent with other lang-* features using tree-sitter modes
- Currently experimental (marked in comments)

### LSP Not Auto-Activated

Comment in code notes LSP is not automatically activated due to flake+direnv issues:
- `exec-path` may not be set correctly
- LSP server may not be found
- Hook is present but may need manual intervention

### ocaml-eglot Package

Uses ocaml-eglot for extended LSP features beyond base eglot:
- Additional OCaml-specific LSP capabilities
- Better integration with ocamllsp

## Testable Properties

1. `*.ml` files open in `neocaml-mode`
2. `*.mli` files open in `neocamli-mode`
3. `dune` files open in `dune-config-mode`
4. `/_build/` directories are read-only
5. Generated `.opam` files are read-only
6. `dune-project` is recognized as project root marker
7. `l` snippet adds `in` when inside let binding body
8. `l` snippet omits `in` at module level
9. Org `ocaml` blocks use `neocaml-mode`
