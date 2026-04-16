## Context

`+nix-ts--scan-spans` in `+nix-ts-spans.el` resolves inner-mode for Nix
multiline strings via two mechanisms: comment annotation lookup in
`+nix-lang-mode-alist`, and leaf attr-name membership in `+nix-bash-attrs`.
Both are closed — unknown annotations and domain-specific attrpaths produce no
innermode. The tree-sitter AST provides full attrpath information (via parent
walking) that the current scanner ignores.

## Goals / Non-Goals

**Goals:**
- Comment annotations that name an Emacs mode directly (e.g. `# may-i-config`)
  resolve without alist entries
- Full attrpath regexp matching replaces flat `+nix-bash-attrs` list
- Existing `+nix-lang-mode-alist` entries and bash-attr behaviour preserved
- All resolution happens at scan time (no polymode hot-path changes)

**Non-Goals:**
- Changing fence header display (stays mode-only)
- Adding `major-mode-remap-alist` entries for modes like `python-mode` →
  `python-ts-mode` (orthogonal concern)
- Supporting heuristics in non-Nix hosts (bash heredocs already have explicit
  delimiters)

## Decisions

### 1. Comment resolution chain: alist → fboundp probe

**Decision**: After `+nix-lang-mode-alist` lookup fails, try
`(intern (concat comment-text "-mode"))` and accept if `fboundp`.

**Rationale**: Alist is explicit and handles ts-mode remapping (e.g. "bash" →
`bash-ts-mode`). The probe is a best-effort fallback for modes that don't need
remapping. Alist-first means `# bash` still resolves to `bash-ts-mode` (not
`bash-mode`).

**Alternative**: Only use the alist, require entries for every mode. Rejected —
too much maintenance for one-off modes like `may-i-config-mode`.

### 2. Full attrpath via tree-sitter parent walk

**Decision**: For each binding with an `indented_string_expression`, walk up
`binding → parent → parent → ...`, collecting attrpath text from each ancestor
`binding` node. Join with `.` to produce e.g. `programs.may-i.extraConfig`.

**Rationale**: Works identically for flat (`programs.may-i.extraConfig = ''...''`)
and nested (`programs.may-i = { extraConfig = ''...''; }`) styles. The
tree-sitter AST structure `binding → binding_set → attrset_expression → binding`
is stable. Verified experimentally.

**Alternative**: Only use leaf attr name. Rejected — can't distinguish
`programs.may-i.extraConfig` from `programs.firefox.extraConfig`.

### 3. `+nix-attrpath-mode-alist` replaces `+nix-bash-attrs`

**Decision**: New variable `+nix-attrpath-mode-alist` is an alist of
`(REGEXP . MODE-SYMBOL)`. Scanned in order against the full attrpath string.
First match wins.

```elisp
(defvar +nix-attrpath-mode-alist
  '(("\\(?:shellHook\\|buildPhase\\|installPhase\\|configurePhase\\|checkPhase\\|fixupPhase\\|unpackPhase\\|patchPhase\\|preBuild\\|postBuild\\|preInstall\\|postInstall\\|preConfigure\\|postConfigure\\|preCheck\\|postCheck\\|preFixup\\|postFixup\\|script\\|preStart\\|postStart\\|preStop\\|postStop\\|ExecStart\\|ExecStartPre\\|ExecStartPost\\|ExecStop\\|ExecStopPost\\|ExecReload\\)\\'" . bash-ts-mode))
  "Alist of (REGEXP . MODE-SYMBOL) for attrpath-based mode detection.")
```

Regexp runs against the full dotted attrpath. The `\\'` anchor matches end of
string, so `shellHook` matches as a leaf regardless of prefix path. Users add
entries for their own domain-specific patterns.

**Rationale**: Subsumes `+nix-bash-attrs` with no loss. Gains full-path
matching. Standard Emacs alist-of-regexp pattern (cf. `auto-mode-alist`).

**Alternative**: Keep `+nix-bash-attrs` alongside the new alist. Rejected —
redundant; the regexp alist is strictly more expressive.

### 4. Span tuple gains attrpath field

**Decision**: Extend cached span from 5-tuple to 6-tuple:
`(HEAD-BEG HEAD-END TAIL-BEG TAIL-END MODE-SYMBOL ATTRPATH-STRING)`.

**Rationale**: Useful for debugging and introspection. Cheap to store (already
computed during scan). Not used in display.

### 5. Resolution order

```
1. Comment annotation → +nix-lang-mode-alist       (explicit, handles ts remap)
2. Comment annotation → fboundp probe              (fallback for direct modes)
3. Full attrpath      → +nix-attrpath-mode-alist   (regexp first-match)
4. No match           → no innermode
```

Comment always wins over attrpath (existing behaviour preserved).

## Risks / Trade-offs

- **fboundp on arbitrary comment text**: `# foo` would probe `foo-mode`. If
  `foo-mode` exists but is unrelated, false positive. → Mitigated by alist
  taking priority; probe is last resort for comments. In practice, Nix comment
  annotations are intentional.
- **Attrpath walk performance**: One parent-walk per binding during scan. Nix
  files rarely exceed ~50 bindings with multiline strings. → Negligible cost.
- **Regexp ordering**: First match wins. User-appended entries go to end by
  default. → Document that prepending overrides built-in patterns.
