# Feature: templates

File templates and text snippets with Tempel and skeleton-based autoinsert.

## Files

| File                     | Purpose                           |
| :----------------------- | :-------------------------------- |
| init/init-templates.el   | Tempel and autoinsert integration |
| lisp/+file-templates.el  | Macros for skeleton-based file templates |

## Directories

| Path            | Purpose                              |
| :-------------- | :----------------------------------- |
| templates/      | Tempel snippet definitions (*.eld)   |
| file-templates/ | Skeleton file templates              |

## External Packages

| Package | Purpose                           |
| :------ | :-------------------------------- |
| tempel  | Modern template/snippet expansion |

## Behaviors

### Tempel Snippets

Text snippets defined in ELD format, scoped per major mode.

| Setting                        | Value                               |
| :----------------------------- | :---------------------------------- |
| `tempel-path`                  | `templates/*.eld`                   |

| When                            | Then                                  |
| :------------------------------ | :------------------------------------ |
| In insert state, press `M-e`    | Invoke `tempel-expand`                |
| `M-{` / `M-}` in active snippet | Navigate between template fields      |
| Press ESC in active snippet     | Exit snippet via `tempel-done`        |
| In prog/text/config mode        | `tempel-expand` added to `completion-at-point-functions` |

### Tempel Template Format

Each ELD file starts with one or more mode symbols, followed by template definitions:

```elisp
emacs-lisp-mode

(s "(setq " p ")")          ; 's' expands to setq template
(d "(defun " p " (" p ")")  ; 'd' expands to defun template
```

Template elements:
- `p` - Prompt point (editable field)
- `q` - Final cursor position
- `n` - Newline
- `n>` - Newline with indent
- `>` - Reindent current line
- `(s name)` - Mirror field `name`

### Available Snippet Collections

| File                    | Mode(s)                              |
| :---------------------- | :----------------------------------- |
| emacs-lisp.eld          | emacs-lisp-mode                      |
| org.eld                 | org-mode                             |
| rust-ts.eld             | rust-ts-mode                         |
| typescript-ts-base.eld  | typescript-ts-base-mode              |
| javascript-base.eld     | typescript-ts-base-mode, js-base-mode |
| sh-base.eld             | sh-base-mode                         |
| elixir-ts.eld           | elixir-ts-mode                       |
| terraform.eld           | terraform-mode                       |
| hcl.eld                 | hcl-mode                             |
| c.eld                   | c-ts-mode, c-mode                    |
| zig.eld                 | zig-mode                             |
| ocaml.eld               | tuareg-mode, utop-mode               |
| markdown.eld            | markdown-mode                        |
| cdk.eld                 | typescript-ts-mode (CDK-specific)    |

### File Templates (Autoinsert)

Skeleton-based templates inserted automatically into new files.

| Setting                | Value                 |
| :--------------------- | :-------------------- |
| `auto-insert-directory` | `file-templates/`    |
| `auto-insert-query`    | `nil` (no prompt)     |

| When                       | Then                                   |
| :------------------------- | :------------------------------------- |
| Create new empty file      | Insert matching template automatically |
| Template has `_` marker    | Cursor positioned at marker            |
| Template evaluates elisp   | Dynamic values computed (filename, etc.) |

### File Template Format

Skeleton format with elisp evaluation:

```elisp
(nil
 '(progn
    (setq file-name (file-name-nondirectory (buffer-file-name))))

 ";;; " file-name " --- DESC -*- lexical-binding: t; -*-"
 _

 '(progn
    (makunbound 'file-name)))
```

Elements:
- `nil` - No initial condition
- `'(progn ...)` - Elisp evaluated but not inserted
- String literals - Inserted as-is
- `_` - Final cursor position
- Variables - Interpolated from preceding elisp

### Available File Templates

| Path                           | Target                            |
| :----------------------------- | :-------------------------------- |
| emacs-lisp.eld                 | Emacs Lisp files                  |
| shell-script.eld               | Shell scripts (bash/zsh)          |
| flake.eld                      | Nix flakes                        |
| elixir/lib.eld                 | Elixir library modules            |
| elixir/test.eld                | Elixir test modules               |
| cdk/stack.eld                  | CDK stack classes                 |
| cdk/construct.eld              | CDK construct classes             |
| terragrunt/terragrunt.eld      | Terragrunt module configs         |
| terragrunt/root.eld            | Terragrunt root configs           |
| terragrunt/region.eld          | Terragrunt region configs         |

## API

### Macros

| Symbol                             | Signature                              | Purpose                              |
| :--------------------------------- | :------------------------------------- | :----------------------------------- |
| `+define-file-template`            | `(mode-or-regexp template-file)`       | Register simple file template        |
| `+define-file-template-dispatcher` | `(mode-or-regexp clause &rest clauses)` | Register conditional file template   |

### Variables

| Symbol          | Type   | Purpose                          |
| :-------------- | :----- | :------------------------------- |
| `+templates-dir` | string | Path to templates/ directory     |

### Integration Points

| Hook/Function                    | Purpose                           |
| :------------------------------- | :-------------------------------- |
| `completion-at-point-functions`  | Tempel added at priority -90      |
| `+escape-hook`                   | Exit active snippet on ESC        |

## Testable Properties

| Property                        | Verification                                      |
| :------------------------------ | :------------------------------------------------ |
| Tempel path configured          | `tempel-path` ends with `templates/*.eld`         |
| M-e bound in insert state       | Keybinding exists in `text-mode-map`              |
| Tempel in capf                  | `tempel-expand` in `completion-at-point-functions` after mode hook |
| Escape exits snippet            | `+escape-hook` contains snippet exit function     |
| Autoinsert enabled              | `auto-insert-mode` is non-nil                     |
| Autoinsert no prompt            | `auto-insert-query` is nil                        |
| File template macros exist      | `+define-file-template` is bound                  |
| Dispatcher macro exists         | `+define-file-template-dispatcher` is bound       |
| string-inflection available     | Required for dynamic template names               |
