# nix-treesitter-polymode

Replace all regex-based polymode innermodes in `lang-nix` with a single
tree-sitter-powered `pm-inner-auto-chunkmode`. Detect embedded language from
comment annotations (`/* bash */`, `# python`) AND attribute names
(`shellHook`, `buildPhase`, etc.) via tree-sitter AST queries.

## Problem

Current Nix polymode defines 8 separate regex innermodes, each matching a
specific `/* lang */` or `# lang` annotation before `''`. Strings assigned to
well-known nixpkgs attributes (`shellHook`, `buildPhase`, `installPhase`, ...)
get no fontification unless manually annotated — which breaks convention for
simple flakes and derivations.

## Approach

- **One auto-chunkmode** with function-based `:head-matcher`, `:tail-matcher`,
  and `:mode-matcher` that all read from an eagerly-cached span list.
- **Tree-sitter queries** find all `(binding ... (indented_string_expression))`
  nodes, extracting comment annotations and attribute names.
- **Language resolution**: annotation comment wins; fallback to attr-name lookup
  in a known-attrs table; no match → no innermode.
- **Eager cache** rebuilt via `treesit-parser-notifiers`; matchers do position
  lookup only.
- **Head boundary** includes attr name or annotation through opening `''` —
  gives code-fences box drawing meaningful context (e.g. `╭── shellHook ──`).

## Scope

- Replace all 8 `define-innermode` + `+nix-poly--head-matcher` with one
  `define-innermode` using `pm-inner-auto-chunkmode`
- Add `+nix-bash-attrs` known-attrs table (shellHook, buildPhase, phases, etc.)
- Add `+nix-lang-mode-alist` for annotation → mode mapping
- Add tree-sitter span scanning and caching
- Update `+code-fences-register` callbacks for new head structure
- Existing annotation-based detection preserved (just via tree-sitter now)
