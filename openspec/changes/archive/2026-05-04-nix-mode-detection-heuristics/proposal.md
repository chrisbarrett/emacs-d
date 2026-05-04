## Why

Nix multiline strings often contain code whose mode can't be determined from
`+nix-lang-mode-alist` or `+nix-bash-attrs`. A `# may-i-config` comment
annotation refers to `may-i-config-mode` directly, but the current scanner
ignores it because "may-i-config" isn't in the alist. Similarly, strings bound
to domain-specific attrpaths like `programs.may-i.extraConfig` could be matched
by regexp without any annotation at all. Two new heuristics — comment-as-mode
probe and full-attrpath regexp matching — close these gaps.

## What Changes

- Comment annotation resolution gains an `fboundp` probe fallback: after alist
  lookup fails, `(intern (concat comment "-mode"))` is tested. If the symbol is
  a defined function, use it. Alist always wins over probe.
- `+nix-bash-attrs` (flat list → hardcoded `bash-ts-mode`) replaced by
  `+nix-attrpath-mode-alist` (regexp alist → mode symbol). Subsumes the old
  list and supports arbitrary attrpath patterns.
- `+nix-ts--scan-spans` walks up parent binding nodes in the tree-sitter AST to
  reconstruct the full attrpath (e.g. `programs.may-i.extraConfig`), working
  identically for flat and nested attrset styles.
- Span cache tuple gains an attrpath field for debugging/introspection (not
  displayed in fence headers — mode-only display preserved).

## Capabilities

### New Capabilities
- `nix-mode-heuristics`: Comment fboundp probe and full-attrpath regexp matching for Nix polymode inner-mode detection

### Modified Capabilities
- `treesitter-span-detection`: Annotation resolution adds fboundp fallback; attr-name matching replaced by full-attrpath regexp alist; span tuple gains attrpath field

## Impact

- `modules/lang-nix/lib/+nix-ts-spans.el` — new resolution logic, attrpath walk, `+nix-attrpath-mode-alist` replaces `+nix-bash-attrs`
- `modules/lang-nix/init.el` — no structural changes expected (registration stays the same)
- `modules/treesit/config/+code-fences.el` — no changes (mode-only headers already)
- `modules/treesit/config/+code-fences-tests.el` — new tests for heuristic resolution
- `modules/lang-nix/tests.el` — new tests for attrpath walk and fboundp probe
