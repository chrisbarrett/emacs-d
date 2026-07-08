# Design: add-lang-setup-helper

## Context

Language modules repeat two wiring idioms: eglot activation via
`:hook (<mode>-local-vars-hook . eglot-ensure)` (7 modules) and
apheleia formatter registration via `add-to-list`/`alist-set!` inside
`use-package apheleia :config` blocks (8 modules). Both idioms carry
non-obvious constraints — the `local-vars-hook` variant exists so
dir-locals apply before eglot starts (`+hooks.el` machinery), and
apheleia registration must be deferred to load time and paired
(definition + mode association). Nothing enforces these; each new
language module copies a neighbour and hopes it copied a correct one.

## Goals / Non-Goals

**Goals**

- One declarative call per language expressing LSP + formatter wiring.
- The hook-choice and lazy-registration subtleties encoded once.
- Zero behaviour change in migrated modules.

**Non-Goals**

- Abstracting tree-sitter mode remapping, `org-src-lang-modes`
  entries, or per-language `use-package` config — those vary per
  language and are not duplicated boilerplate.
- Wrapping eglot server configuration (`eglot-server-programs`) —
  modules that customise servers keep doing so directly.
- A general plugin/framework layer; this is a helper, not a DSL.

## Decisions

### Decision: new axis `lang-support`

The recognised-axes list does not include it and no existing axis
fits: `eglot` (recognised, spec-less) covers only half the concern —
formatter wiring is apheleia, not eglot. The helper lives in a new
`modules/lang-support/` module, so the axis-name-matches-module rule
holds. Internals-facing spec.

### Decision: helper is an autoloaded function, not a use-package keyword

Shape: `(+lang-declare MODE &key lsp formatter)` in
`modules/lang-support/lib.el`, marked `;;;###autoload`, where
`:formatter` is either a symbol (existing apheleia formatter) or
`(NAME . COMMAND-LIST)` (register + associate).

Alternative considered: extend `+use-package-keywords.el` with
`:eglot`/`:formatter` keywords. Rejected — the wiring is not about the
package being configured (the declaration for `rust-ts-mode` sits in a
`use-package rust-ts-mode` block only incidentally), keyword
implementations are harder to test than a plain function, and
`+use-package-keywords.el` is core infrastructure with no axis of its
own.

Alternative considered: a data file (à la `packages.eld`) consumed
centrally. Rejected — over-structured for 7–8 call sites and hides
control flow from the module reader.

### Decision: lazy application via `with-eval-after-load`

The helper mutates `apheleia-formatters`/`apheleia-mode-alist` inside
`with-eval-after-load 'apheleia`, and adds the `eglot-ensure` hook
entry immediately (hook symbols need no package loaded). Declaring
must be safe before either package loads, and must not force loads.

### Decision: migrate all hand-rolled sites in this change

A helper that half the modules use is a second idiom, not a
consolidation. tasks.md migrates every current site; the spec's "no
hand-rolled wiring remains" scenario keeps future modules honest.

## Risks / Trade-offs

- [Indirection hides wiring from someone reading a lang module] → the
  declaration is a single self-describing call in the same init.el;
  helper docstring documents exactly what it expands to.
- [A migrated module needed a subtle variation the helper doesn't
  express] → migrate module-by-module with `make test` green after
  each; a genuine variation stays hand-rolled and the spec scenario's
  search pattern gets an explicit documented exception — only if one
  actually exists.
- [Helper loads before `+autoloads` registration in some early module]
  → module inits load after `+autoloads-rebuild` per init.el ordering;
  no early callers exist.

## Migration Plan

Add helper + tests first; migrate modules one commit at a time;
verify each language still gets its server and formatter (existing
module tests plus manual spot-check of one migrated language).
Rollback: revert migration commits; helper is additive.

## Open Questions

- Whether `lang-latex`'s formatter setup (custom `latexindent` entry)
  fits the `(NAME . COMMAND-LIST)` shape or needs the escape hatch —
  resolve during migration.
