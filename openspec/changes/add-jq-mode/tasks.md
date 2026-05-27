## 1. Module scaffold

- [ ] 1.1 Create `modules/lang-jq/packages.eld` declaring the `jq-mode`
      package for elpaca.
- [ ] 1.2 Create `modules/lang-jq/init.el` with the standard header,
      `;;; Commentary:`, and `;;; Code:` blocks following the
      `lang-nix/init.el` shape.

## 2. Wire jq-mode

- [ ] 2.1 In `modules/lang-jq/init.el`, add a `use-package jq-mode`
      form with `:mode "\\.jq\\'"` so `.jq` files select `jq-mode`.
- [ ] 2.2 In the same form, bind `jq-interactively` to `C-c C-j` in
      both `json-mode-map` and `json-ts-mode-map` using
      `:general-config` / `general-def` (per the project's keybinding
      convention in `.claude/rules/lisp.md`).
- [ ] 2.3 Ensure the binding is wrapped so the command symbol resolves
      after `jq-mode` is loaded (e.g. `with-eval-after-load 'jq-mode`).

## 3. Verification

- [ ] 3.1 Run `make test-quick` and confirm no failures introduced.
- [ ] 3.2 Restart Emacs, open a scratch `.jq` file, confirm the major
      mode is `jq-mode`.
- [ ] 3.3 Open a JSON buffer, press `C-c C-j`, confirm the
      `jq-interactively` minibuffer prompt appears.
- [ ] 3.4 Run `openspec validate add-jq-mode` and confirm the change
      passes.
