# Feature Inventory

Analysis of existing configuration to identify logical feature groupings.

## File Counts

| Path     | Pattern      | Count |
| :------- | :----------- | ----: |
| config/  | mod-*.el     |    17 |
| init/    | init-*.el    |    51 |
| lisp/    | +*.el        |    14 |
| root     | init.el      |     1 |
| root     | early-init.el|     1 |

Total: 84 source files (excluding tests)

## Proposed Feature Groupings

### 1. core

**Summary:** Bootstrap, hooks, custom use-package keywords, site loading

**Files:**
- `init.el` - Entry point, loads init/, site/
- `early-init.el` - Early UI, paths, use-package config
- `init/init-elpaca.el` - Elpaca manager, `:ensure-unless-local` keyword
- `init/init-hooks.el` - Custom hooks (+first-input-hook, +switch-buffer-hook, etc.)
- `init/init-system.el` - Environment (exec-path-from-shell, envrc, misery, server)
- `init/init-readonly.el` - Auto-read-only for vendor/elpaca/node_modules
- `lisp/+corelib.el` - Foundation utilities (logging, hooks, collection ops)
- `lisp/+load-incrementally.el` - Deferred loading (:defer-incrementally, :after-call)
- `lisp/+modules.el` - Module system (packages.eld, autoloads, init.el)

**External Packages:** elpaca, general, no-littering, auto-compile, exec-path-from-shell, envrc, misery

---

### 2. evil

**Summary:** Modal editing with Evil and its ecosystem

**Files:**
- `init/init-evil.el` - Evil setup, evil-collection, surround, multiedit, anzu, vundo
- `init/init-esc.el` - Custom +escape function and hook
- `init/init-input.el` - C-i/C-m distinction, electric pairs, puni, string-inflection
- `config/mod-evil.el` - Cursor styles, smart join, RET opens URLs, smart newline
- `lisp/+evil-collection.el` - Deferred evil-collection loading

**External Packages:** evil, evil-collection, evil-surround, evil-multiedit, evil-anzu, evil-tty-cursor, vundo, puni, string-inflection

---

### 3. completion

**Summary:** Minibuffer and in-buffer completion framework

**Files:**
- `init/init-completion.el` - Vertico, marginalia, orderless, corfu, cape, consult, embark

**External Packages:** vertico, marginalia, orderless, corfu, nerd-icons-corfu, cape, which-key, consult, embark, embark-consult

---

### 4. leader

**Summary:** SPC leader key hierarchy and global keybindings

**Files:**
- `init/init-leader.el` - Comprehensive leader key hierarchy
- `lisp/+edit-cmds.el` - Kill commands referenced by leader
- `lisp/+files.el` - +find-sibling-file referenced by leader
- `lisp/+window.el` - Window commands referenced by leader

**External Packages:** general

---

### 5. theme

**Summary:** Theme management, light/dark switching, system detection

**Files:**
- `init/init-theme.el` - catppuccin-theme setup
- `lisp/+theme.el` - System theme detection, light/dark switching, +theme-changed-hook

**External Packages:** catppuccin-theme, modus-themes (built-in Emacs 30)

---

### 6. ui

**Summary:** Visual enhancements, modeline, display-buffer rules

**Files:**
- `init/init-ui.el` - Ligatures, hideshow, page-break-lines, line numbers, hl-todo, indent-bars, dimmer, bufler, breadcrumb
- `init/init-modeline.el` - doom-modeline configuration
- `init/init-display-buffer.el` - Extensive display-buffer-alist rules
- `config/mod-tabs.el` - Tab-bar transient, alert system, worktree icons
- `config/mod-pulsar.el` - Visual feedback on eval, jump, search
- `config/mod-tty-frames.el` - TTY-specific display setup
- `config/mod-bufler.el` - Buffer grouping with magit-section

**External Packages:** doom-modeline, ligature, page-break-lines, hide-mode-line, highlight-numbers, hl-todo, indent-bars, pulsar, dimmer, paren-face, breadcrumb, bufler, nerd-icons

---

### 7. nav

**Summary:** Window navigation, jumping, avy, winner, save-place

**Files:**
- `init/init-nav.el` - Windmove, winner, better-jumper, avy, save-place, rotate

**External Packages:** rotate, better-jumper, avy

---

### 8. editing

**Summary:** General editing behavior, autosave, recentf, uniquify

**Files:**
- `init/init-editing.el` - Autorevert, recentf, uniquify, lockfiles disabled

**External Packages:** None (built-in features)

---

### 9. format

**Summary:** Code formatting, whitespace trimming

**Files:**
- `init/init-format.el` - Apheleia, whitespace trimming, indent settings

**External Packages:** apheleia

---

### 10. search

**Summary:** Search tools (grep, wgrep, xref with ripgrep)

**Files:**
- `init/init-search.el` - ripgrep as grep program, wgrep, xref

**External Packages:** wgrep

---

### 11. help

**Summary:** Help system enhancements

**Files:**
- `init/init-help.el` - helpful, eldoc, rfc-mode

**External Packages:** helpful, rfc-mode

---

### 12. templates

**Summary:** File templates and snippets

**Files:**
- `init/init-templates.el` - Tempel, autoinsert integration
- `lisp/+file-templates.el` - Template macros

**External Packages:** tempel

---

### 13. spellcheck

**Summary:** Spell checking with aspell and spell-fu

**Files:**
- `init/init-spellcheck.el` - ispell, spell-fu, flyspell-correct

**External Packages:** spell-fu, flyspell-correct

---

### 14. dired

**Summary:** Directory browser configuration

**Files:**
- `init/init-dired.el` - dired, wdired, diredfl, nerd-icons-dired

**External Packages:** diredfl, nerd-icons-dired

---

### 15. shells

**Summary:** Eshell and terminal emulators

**Files:**
- `init/init-shells.el` - Eshell, eat terminal
- `config/mod-eshell.el` - Zoxide integration, git root navigation

**External Packages:** eat

---

### 16. eglot

**Summary:** LSP client configuration

**Files:**
- `init/init-eglot.el` - Flymake, eglot, eglot-booster

**External Packages:** eglot-booster

---

### 17. treesit

**Summary:** Tree-sitter configuration

**Files:**
- `init/init-treesit.el` - treesit, expreg

**External Packages:** expreg

---

### 18. project

**Summary:** Project management with beframe isolation

**Files:**
- `init/init-project.el` - project.el, beframe integration
- `config/mod-beframe.el` - Frame isolation, project-switch-beframed

**External Packages:** beframe

---

### 19. vcs

**Summary:** Git porcelain, worktrees, forge integration

**Files:**
- `init/init-vcs.el` - Magit, beads, git-timemachine, browse-at-remote, forge
- `config/mod-magit.el` - Display buffer, emoji, worktree prune
- `config/mod-worktrees.el` - Zellij-style worktree workflow, tabs, claude-code
- `config/mod-browse-at-remote.el` - Emacs 31 fixes, git-timemachine support
- `lisp/+git.el` - Repository display name utilities

**External Packages:** magit, beads, git-timemachine, browse-at-remote, forge, git-auto-commit-mode

---

### 20. org

**Summary:** Org-mode core configuration

**Files:**
- `init/init-org.el` - org, evil-org, org-super-agenda, org-modern, ox-gfm
- `config/mod-org.el` - TODO keywords, babel, refile, archive, smart heading
- `config/mod-org-link.el` - Custom link types (crate, github, RFC)
- `lisp/+clockreport.el` - Clock table formatting

**External Packages:** org, evil-org, org-super-agenda, separedit, org-modern, org-cliplink, ox-gfm

---

### 21. org-agenda

**Summary:** Org agenda views and habits

**Files:**
- `config/mod-org-agenda.el` - Custom views, super-agenda groups, habit config
- `lisp/+agenda.el` - Skip functions, predicates

**External Packages:** org-habit, org-super-agenda

---

### 22. org-capture

**Summary:** Org capture templates and workflow

**Files:**
- `config/mod-org-capture.el` - Capture templates
- `lisp/+capture.el` - Litnote workflow, URL metadata extraction

**External Packages:** org-capture

---

### 23. org-roam

**Summary:** Zettelkasten with org-roam

**Files:**
- `init/init-org-roam.el` - org-roam setup, custom node display
- `init/init-nursery.el` - org-roam extensions (review, search, links, timekeep)

**External Packages:** org-roam, nursery (org-roam-review, org-roam-search, org-roam-links, timekeep, etc.)

---

### 24. compile

**Summary:** Compilation mode with custom error parsers

**Files:**
- `init/init-inf.el` - Comint, compile basics
- `config/mod-compilation.el` - Error regex parsers for 30+ tools
- `lisp/+compile.el` - DSL for defining error parsers

**External Packages:** None (built-in compile.el)

---

### 25. debug

**Summary:** Debugger enhancements

**Files:**
- `init/init-debug.el` - Toggle debug-on-exit-frame, custom mode-line

**External Packages:** None (built-in)

---

### 26. diff

**Summary:** Diff and ediff configuration

**Files:**
- `init/init-diff.el` - Read-only diffs, horizontal ediff splits

**External Packages:** None (built-in)

---

### 27. input-methods

**Summary:** French postfix and quail customization

**Files:**
- `config/mod-input-methods.el` - Smart semicolon/colon insertion

**External Packages:** None (built-in quail)

---

### 28. auth

**Summary:** 1Password auth-source integration

**Files:**
- `init/init-auth.el` - auth-source-op backend

**External Packages:** auth-source-op

---

### 29. claude

**Summary:** Claude Code IDE integration

**Files:**
- `init/init-claude.el` - claude-code-ide with MCP bridge

**External Packages:** claude-code-ide

---

### 30. anthropic

**Summary:** Anthropic API client library

**Files:**
- `lisp/+anthropic.el` - Sync/async API calls, auth-source

**External Packages:** None (uses built-in url.el)

---

### 31. lang-lisp

**Summary:** Emacs Lisp development

**Files:**
- `init/init-lisp.el` - elisp-mode, checkdoc, ert, buttercup, flymake-eldev
- `lisp/+consult-imenu-elisp.el` - Imenu visibility grouping

**External Packages:** flymake-eldev, buttercup

---

### 32. lang-js

**Summary:** JavaScript/TypeScript development

**Files:**
- `init/init-js.el` - js, typescript-ts-mode, deno/node detection

**External Packages:** None (uses built-in typescript-ts-mode)

---

### 33. lang-rust

**Summary:** Rust development

**Files:**
- `init/init-rust.el` - rust-ts-mode, eglot integration

**External Packages:** None (uses built-in rust-ts-mode)

---

### 34. lang-nix

**Summary:** Nix development

**Files:**
- `init/init-nix.el` - nix-ts-mode, eglot, nixpkgs-fmt

**External Packages:** nix-ts-mode

---

### 35. lang-elixir

**Summary:** Elixir development

**Files:**
- `init/init-elixir.el` - elixir-ts-mode, inf-elixir

**External Packages:** inf-elixir

---

### 36. lang-ocaml

**Summary:** OCaml development

**Files:**
- `init/init-ocaml.el` - neocaml, ocaml-eglot, dune support
- `config/mod-ocaml.el` - Tempel helpers, tree-sitter context

**External Packages:** neocaml, ocaml-eglot

---

### 37. lang-terraform

**Summary:** Terraform/HCL development

**Files:**
- `init/init-terraform.el` - hcl-mode, terraform-mode, tofu formatter

**External Packages:** hcl-mode, terraform-mode

---

### 38. lang-c

**Summary:** C development

**Files:**
- `init/init-c.el` - c-ts-mode, electric angle bracket

**External Packages:** None (uses built-in c-ts-mode)

---

### 39. lang-markdown

**Summary:** Markdown editing

**Files:**
- `init/init-markdown.el` - markdown-mode, GFM callouts

**External Packages:** markdown-mode

---

### 40. lang-latex

**Summary:** LaTeX editing

**Files:**
- `init/init-latex.el` - Latexindent configuration

**External Packages:** None (uses apheleia)

---

### 41. lang-conf

**Summary:** Configuration file formats (JSON, YAML, KDL)

**Files:**
- `init/init-conf.el` - conf-mode, kdl-ts-mode, json-ts-mode, yaml-ts-mode

**External Packages:** kdl-ts-mode

---

### 42. lang-text

**Summary:** Plain text and LICENSE files

**Files:**
- `init/init-text.el` - text-mode associations

**External Packages:** None (built-in)

---

### 43. lang-shscript

**Summary:** Shell script editing

**Files:**
- `init/init-shscript.el` - sh-script, auto-executable

**External Packages:** None (built-in)

---

### 44. lang-erlang

**Summary:** Erlang development (currently disabled)

**Files:**
- `init/init-erlang.el` - erlang-mode (disabled)

**External Packages:** erlang (disabled)

---

### 45. lang-zig

**Summary:** Zig development

**Files:**
- `init/init-zig.el` - zig-mode

**External Packages:** zig-mode

---

### 46. reader

**Summary:** Document reader (native module)

**Files:**
- `init/init-reader.el` - reader module

**External Packages:** reader (native, injected via Nix)

---

### 47. hex

**Summary:** Hexl mode keybindings

**Files:**
- `init/init-hex.el` - Vim-style hexl keybindings

**External Packages:** None (built-in)

---

## Coupling Analysis

### Tightly Coupled Clusters

1. **Worktree Workflow** (should stay together):
   - mod-worktrees ↔ mod-tabs (alert system)
   - mod-worktrees ↔ mod-beframe (frame management)
   - mod-worktrees ↔ mod-magit (git operations)

2. **Org Mode Stack** (logically grouped):
   - mod-org (core)
   - mod-org-agenda (views)
   - mod-org-capture (templates)
   - mod-org-link (link types)
   - +agenda, +capture, +clockreport (utilities)

3. **Evil Ecosystem** (should stay together):
   - init-evil, init-esc, init-input
   - mod-evil
   - +evil-collection

4. **UI/Visual Feedback** (loosely coupled):
   - mod-pulsar (visual feedback)
   - mod-tabs (uses pulsar colors)
   - init-ui (general UI)

### Cross-Cutting Dependencies

- **+corelib** - Used by 34+ files (foundation)
- **general** - Used for keybindings across many features
- **+theme** - Referenced by mod-tabs for color derivation
- **+window** - Referenced by init-leader, init-display-buffer

## Recommended Consolidations

Some features could be merged:

1. **vcs** could absorb project (tightly coupled via worktrees)
2. **leader** could absorb parts of nav (both about keybindings/navigation)
3. **org** could absorb org-agenda, org-capture (same domain)
4. **lang-\*** could potentially share a common "lang-common" base

## Next Steps

Please review this inventory and confirm:
1. Are these groupings appropriate?
2. Should any features be merged or split?
3. Are there any features missing?
4. Should any features be prioritized for spec writing?
