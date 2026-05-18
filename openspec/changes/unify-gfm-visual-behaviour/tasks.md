## 1. Scaffolding

- [x] 1.1 Create `lisp/gfm/` directory. Confirm `lisp/` already on `load-path` via `+core-paths.el` / `+modules.el`; verify the autoload generator scans `lisp/gfm/` (or add a one-line entry to `+autoloads.el` so it does).
- [x] 1.2 In the makefile / Eldev config, confirm the test runner picks up `lisp/gfm/*-tests.el`. If the glob is `modules/**/tests.el` only, extend it to include `lisp/**/tests.el` and `lisp/**/*-tests.el`.
- [x] 1.3 Smoke-test the autoload + test pipeline by adding a one-line `lisp/gfm/gfm-smoke.el` defining `(defun gfm--smoke () t)` and a `lisp/gfm/gfm-smoke-tests.el` asserting it. Run `make test`. Confirm the test runs. Delete both files.

## 2. gfm-present relocation (no engine work)

- [x] 2.1 `git mv modules/presentation/lib.el lisp/gfm/gfm-present.el`. Rename top-level `;;; lib.el ---` header to `;;; gfm-present.el ---`. Add `(require 'gfm-pretty)` at the top.
- [x] 2.2 `git mv modules/presentation/tests.el lisp/gfm/gfm-present-tests.el`. Update `require` lines.
- [x] 2.3 Replace `+presentation-` prefix with `gfm-present-` across both files (`fastmod` or sed). Variable: `+presentation-mode` → `gfm-present-mode`, `+presentation-mode-map` → `gfm-present-mode-map`, `+presentation-focus-face` → `gfm-present-focus-face`, commands `+present-markdown` → `gfm-present-markdown`, etc.
- [x] 2.4 Delete `modules/presentation/` directory (its `init.el` is the 11-line autoload shim, redundant once `gfm-present` autoloads from `lisp/gfm/`).
- [x] 2.5 Update any caller of `+presentation-*` symbols (search the whole tree). Likely targets: `leader/init.el`, capture-templates, keybindings.
- [x] 2.6 `git mv openspec/specs/presentation openspec/specs/gfm-present`. Update spec heading `# presentation Specification` → `# gfm-present Specification`. Update internal `+presentation-*` references to `gfm-present-*`. Update Purpose to note placement at `lisp/gfm/gfm-present.el`.
- [x] 2.7 Run `make test`. Confirm gfm-present tests pass.

## 3. Pass 1 — Relocate + rename gfm-pretty files

- [x] 3.1 `git mv modules/lang-markdown/lib/+gfm-block-borders.el lisp/gfm/gfm-pretty-borders.el`. Rename `gfm-block-borders-*` and `gfm-block-borders--*` symbols → `gfm-pretty--*` (private). Public primitive aliases `gfm-pretty-top-border`, `gfm-pretty-bottom-border`, `gfm-pretty-right-after`, `gfm-pretty-right-after-overflow`, `gfm-pretty-simulate-wrap`, `gfm-pretty-last-visual-col`, `gfm-pretty-available-width`, `gfm-pretty-make-anchor`, `gfm-pretty-make-display`, `gfm-pretty-remove-overlays`, `gfm-pretty-wrap-prefix`. Provide form `(provide 'gfm-pretty-borders)`.
- [x] 3.2 `git mv modules/lang-markdown/lib/+gfm-callouts.el lisp/gfm/gfm-pretty-callouts.el`. Rename `gfm-callouts-*` / `gfm-callouts--*` → `gfm-pretty-callouts-*` / `gfm-pretty-callouts--*`. Update `(require '+gfm-block-borders)` → `(require 'gfm-pretty-borders)`. Provide `gfm-pretty-callouts`. Keep `(define-minor-mode gfm-pretty-callouts-mode …)` in place — pass 1 does not collapse the lifecycle.
- [x] 3.3 `git mv modules/lang-markdown/lib/+gfm-code-fences.el lisp/gfm/gfm-pretty-fences.el`. Same renaming: `gfm-code-fences-*` → `gfm-pretty-fences-*`. Custom group `gfm-code-fences` → `gfm-pretty-fences`. Slow-rebuild threshold customvar renames to `gfm-pretty-fences-slow-rebuild-threshold`.
- [x] 3.4 `git mv modules/lang-markdown/lib/+gfm-tables.el lisp/gfm/gfm-pretty-tables.el`. Same renaming: `gfm-tables-*` → `gfm-pretty-tables-*`. Faces (`gfm-tables-active-cell-face`, `gfm-tables-row-alt-face`, `gfm-tables-row-alt-cap-face`) rename with the prefix. Mode keymap rename.
- [x] 3.5 `git mv modules/lang-markdown/lib/+gfm-hrule.el lisp/gfm/gfm-pretty-hrule.el`. Same renaming: `gfm-hrule-*` → `gfm-pretty-hrule-*`. Face `+markdown-gfm-hrule-face` renames to `gfm-pretty-hrule-face`.
- [x] 3.6 `git mv modules/lang-markdown/lib/+gfm-links.el lisp/gfm/gfm-pretty-links.el`. Same renaming: `gfm-links-*` → `gfm-pretty-links-*`. The `gfm-links--maybe-enable` helper becomes `gfm-pretty-links--maybe-enable`.
- [x] 3.7 Move callout faces, body-face refresh function (`+markdown-gfm-callout-refresh-body-faces`), callout font-lock fontifier (`+markdown-fontify-gfm-callouts`), and `+markdown-style-header-faces` from `modules/lang-markdown/lib.el` into `lisp/gfm/gfm-pretty-callouts.el`. Rename `+markdown-gfm-callout-*-face` → `gfm-pretty-callouts-*-face`; rename `+markdown-fontify-gfm-callouts` → `gfm-pretty-callouts--install-font-lock` (private helper invoked by the decorator's `:on-enable` in pass 2; for pass 1, called from the existing `gfm-pretty-callouts-mode` enable body).
- [x] 3.8 Move `markdown-blockquote-face` neutralisation block from `modules/lang-markdown/init.el` into a new `lisp/gfm/gfm-pretty-callouts.el` top-level form (or a private `gfm-pretty-callouts--neutralise-blockquote-face` called at lib load). Behaviour: same `set-face-attribute` cascade applied once at file load.
- [x] 3.9 Create `lisp/gfm/gfm-pretty.el` as the family entry point. It SHALL: `(require 'gfm-pretty-borders)` and the five decorator files; expose autoloads for `gfm-pretty-mode` (defined later in pass 2), `gfm-pretty-toggle-decorator`, `gfm-pretty-block-at-point`, `gfm-pretty-edit-block-at-point`, `gfm-pretty-define-decorator`. In pass 1, the body of `gfm-pretty.el` is minimal — `(provide 'gfm-pretty)` and `(require)`s only.
- [x] 3.10 Update `modules/lang-markdown/init.el`: remove the five `gfm-mode-hook . gfm-<name>-mode` lines; add a single `gfm-mode-hook . (lambda () (require 'gfm-pretty) (gfm-pretty-callouts-mode 1) (gfm-pretty-fences-mode 1) (gfm-pretty-tables-mode 1) (gfm-pretty-hrule-mode 1) (gfm-pretty-links--maybe-enable))` — same behaviour as today, via the five per-decorator modes still defined in pass 1. (Replaced by the umbrella in pass 2.)
- [x] 3.11 Update `modules/leader/init.el:85-87` references from `gfm-tables--block-at-point` / `gfm-tables-edit-table-at-point` → `gfm-pretty-tables--block-at-point` / `gfm-pretty-tables-edit-table-at-point` (still private — public API lands in pass 2).
- [x] 3.12 `git mv modules/lang-markdown/tests.el lisp/gfm/gfm-pretty-tests.el`. Update `require` lines to point at new file names (`gfm-pretty-callouts`, `gfm-pretty-fences`, etc.). Add a thin `modules/lang-markdown/tests.el` covering only the non-visual concerns retained there (`+markdown-tab-dwim`, lang-mode memoise advice, clamp advice).
- [x] 3.13 Delete `modules/lang-markdown/lib/` (now empty after step 3.6).
- [x] 3.14 Run `make test`. Narrowing-regression suite (`:tags '(narrowing-regression)`) and all decorator tests pass. If any test fails, the failure is unambiguously a pass-1 rename error — fix before proceeding.

## 4. Pass 2 — Engine + umbrella mode

- [ ] 4.1 In `lisp/gfm/gfm-pretty.el`, define `cl-defstruct gfm-pretty--decorator` with slots: name, collect, range, apply-anchors, apply-display, font-lock, revealable-p, block-at-point, edit-at-point, on-enable, on-disable, enable-bit-symbol, overlays-symbol.
- [ ] 4.2 Define the `gfm-pretty-define-decorator` macro that takes the keyword args, builds the struct, and stores it in `gfm-pretty--decorators` (alist by name). Also auto-defines the buffer-local enable bit symbol (`gfm-pretty-<name>-enabled`) and overlay-list symbol (`gfm-pretty-<name>--overlays`).
- [ ] 4.3 Define `gfm-pretty-toggle-decorator NAME` (interactive autoload): flips the enable bit, runs `:on-enable` or `:on-disable`, removes that decorator's overlays if disabling, schedules a rebuild if enabling.
- [ ] 4.4 Define `gfm-pretty-block-at-point` and `gfm-pretty-edit-block-at-point` (interactive autoloads): iterate active decorators, return first non-nil `:block-at-point` result; dispatch to matching decorator's `:edit-at-point`.
- [ ] 4.5 Define `gfm-pretty-mode` (`define-minor-mode`, `:lighter " gfmp"`, autoloaded): on enable, runs every active decorator's `:on-enable`, installs one of each engine hook (`after-change-functions`, `window-configuration-change-hook`, `post-command-hook`), schedules an initial rebuild; on disable, reverses everything.
- [ ] 4.6 Define `gfm-pretty--rebuild` (full), `gfm-pretty--rebuild-scoped DIRTY-REGION`, `gfm-pretty--reconcile-windows`, `gfm-pretty--reveal` (engine-level). Each iterates active decorators calling their contributed fns. Reuse the existing borders reconciler machinery via thin shims.
- [ ] 4.7 Convert `lisp/gfm/gfm-pretty-callouts.el` from a `define-minor-mode` body to a `(gfm-pretty-define-decorator 'callouts …)` registration. Delete the per-decorator mode definition (`gfm-pretty-callouts-mode`). Move the `:on-enable` / `:on-disable` body to handle font-lock install/uninstall + `+theme-changed-hook` wire/unwire + `font-lock-extend-region-functions` install/uninstall. Remove per-decorator after-change / window-config / post-command hook setups (engine drives now).
- [ ] 4.8 Convert `lisp/gfm/gfm-pretty-fences.el` to a `(gfm-pretty-define-decorator 'fences …)` registration. Move `cursor-intangible-mode` toggle to `:on-enable` / `:on-disable`.
- [ ] 4.9 Convert `lisp/gfm/gfm-pretty-tables.el` to a `(gfm-pretty-define-decorator 'tables …)` registration. Move keymap installation into `:on-enable` / `:on-disable`. Expose `:block-at-point` → `gfm-pretty-tables--block-at-point` and `:edit-at-point` → `gfm-pretty-tables-edit-table-at-point`.
- [ ] 4.10 Convert `lisp/gfm/gfm-pretty-hrule.el` to a `(gfm-pretty-define-decorator 'hrule …)` registration.
- [ ] 4.11 Convert `lisp/gfm/gfm-pretty-links.el` to a `(gfm-pretty-define-decorator 'links …)` registration. Move xref-backend, eldoc-function, suppress-compose-region advice installation into `:on-enable` / `:on-disable`. Implement the `markdown-hide-urls` watch via `add-variable-watcher` driving the decorator's enable bit (replacing `gfm-pretty-links--maybe-enable`).
- [ ] 4.12 Update `modules/lang-markdown/init.el` `gfm-mode-hook` from the five-mode lambda (pass 1) to a single `(require 'gfm-pretty)` + `(gfm-pretty-mode 1)`.
- [ ] 4.13 Update `modules/leader/init.el:85-87` from private `gfm-pretty-tables--*` symbols to public `gfm-pretty-block-at-point` / `gfm-pretty-edit-block-at-point`. Update the local-leader `t` binding (currently `gfm-tables-mode`) to call `(gfm-pretty-toggle-decorator 'tables)`.
- [ ] 4.14 Run `make test`. Same suite passes. Any failure is unambiguously a pass-2 engine refactor error.

## 5. Lang-markdown composition cleanup

- [x] 5.1 Shrink `modules/lang-markdown/lib.el` to: `+markdown-tab-dwim`, `+markdown--lang-mode-cache` + `+markdown--memoise-lang-mode` advice, `+markdown--clamp-extend-region` advice. Remove all callout faces, body-face refresh, callout font-lock fontifier, `+markdown-style-header-faces`. Final size ~80 lines.
- [x] 5.2 Confirm `modules/lang-markdown/init.el` retains file associations, `major-mode-remap-alist`, `markdown-code-lang-modes`, `apheleia` wiring, local-leader bindings (`markdown-toggle-url-hiding`, `markdown-insert-footnote`, `markdown-narrow-to-subtree`), the `+markdown-tab-dwim` insert-state binding, and the single-line `gfm-mode-hook` enabling `gfm-pretty-mode`. Remove anything visual.
- [x] 5.3 Update `modules/lang-markdown/packages.eld` to remove any entries no longer needed (none should reference visual concerns; `markdown-mode` and any markdown-specific deps stay).

## 6. Spec deltas applied

- [ ] 6.1 Apply the `lang-markdown` delta: remove the 80 visual-behaviour requirements from `openspec/specs/lang-markdown/spec.md`; add the new "Visual behaviour delegated to gfm-pretty" requirement. Update Purpose to reflect the narrower scope.
- [ ] 6.2 Create `openspec/specs/gfm-pretty/spec.md` from the change's ADDED requirements. Heading: `# gfm-pretty Specification`. Purpose: notes placement at `lisp/gfm/gfm-pretty.el` (library axis), behaviour-facing (engine extension protocol + decorator behaviours). Then `## Requirements`.
- [ ] 6.3 Already-relocated `openspec/specs/gfm-present/spec.md` (from task 2.6) gets a Purpose line noting placement at `lisp/gfm/gfm-present.el`. Cross-reference the `gfm-pretty` axis.
- [ ] 6.4 Update `openspec/specs/spec-conventions/spec.md` with the reworded `Requirement: One spec per axis; spec name matches lib or module name` from the change delta. New recognised-axes list (each entry tagged `(lib)` or `(module)`); `gfm-pretty` and `gfm-present` added as `(lib)`; `presentation` removed; `lang-markdown` description narrowed.

## 7. Verification

- [ ] 7.1 Run `make test`. Full suite passes including narrowing-regression tags. Tests under `lisp/gfm/*-tests.el` are discovered.
- [ ] 7.2 `emacs -Q --batch -L lisp -L lisp/gfm --eval "(require 'gfm-pretty)" --eval "(message \"OK\")"`. Confirms gfm-pretty loads cleanly without the module-loading machinery.
- [ ] 7.3 Open a `.md` file in a fresh `emacs` session loaded with this config. Confirm: callouts decorated, fenced code blocks decorated, tables decorated, HRs decorated, links decorated, `gfm-pretty-mode` active in `M-x describe-mode`. Toggle each decorator via `M-x gfm-pretty-toggle-decorator NAME`; confirm clean enable/disable.
- [ ] 7.4 Open a multi-section markdown file in two side-by-side windows of different widths (`C-x 3`). Confirm per-window rendering for fences, callouts, tables, HRs (each window's box width matches its own width).
- [ ] 7.5 Run `M-x gfm-present-markdown` on a multi-heading markdown buffer. Confirm slide navigation works and decorations render inside narrowed slides without zombie overlays after navigation. Run the narrowing-regression suite once more with `gfm-present-mode` toggled mid-test (manual).
- [ ] 7.6 With point inside a GFM table, invoke the local-leader `t` binding. Confirm the tables decorator toggles via the public path `(gfm-pretty-toggle-decorator 'tables)` and that `gfm-pretty-edit-block-at-point` opens the indirect editor when invoked on a cell.
- [ ] 7.7 Confirm autoloads work: start `emacs -Q` with the config, open a `.md` file without explicitly requiring `gfm-pretty`. The mode SHALL autoload via the `gfm-mode-hook` in `lang-markdown/init.el`. Also confirm `M-x gfm-present-markdown` autoloads `gfm-present`.
- [ ] 7.8 Run `openspec validate unify-gfm-visual-behaviour --strict`. No errors.
- [ ] 7.9 Run `make test-integration` if separate from `make test`.
