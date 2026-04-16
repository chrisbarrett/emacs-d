## Context

`lang-nix/init.el` defines 8 regex-based polymode innermodes for embedded
languages in Nix multiline strings. Detection requires explicit `/* lang */` or
`# lang` annotations. Well-known nixpkgs attrs like `shellHook` and
`buildPhase` get no fontification without manual annotation.

Nix files already use `nix-ts-mode` (tree-sitter). The AST exposes binding
structure, comment children, and `indented_string_expression` nodes — enough
to detect both annotation-based and attribute-name-based language embedding.

## Goals / Non-Goals

**Goals:**
- One `pm-inner-auto-chunkmode` replaces 8 fixed innermodes
- Annotation-based detection preserved via tree-sitter comment child queries
- Attr-name-based detection added for known nixpkgs bash attrs
- Eager span cache rebuilt on tree-sitter parse changes
- Code-fences integration unchanged (head-valid-p, unquoted-p, interpolation-fn)

**Non-Goals:**
- Changing code-fences rendering or API
- Supporting non-indented-string Nix expressions
- Auto-detecting language from string content (heuristic)

## Decisions

### 1. Single auto-chunkmode with function matchers

Polymode's `pm-inner-auto-chunkmode` accepts function matchers and a
`:mode-matcher` for dynamic mode dispatch. One innermode handles all languages.

```
(define-innermode poly-nix-ts-auto-innermode
  :mode nil
  :head-matcher #'+nix-ts--head-matcher
  :tail-matcher #'+nix-ts--tail-matcher
  :mode-matcher #'+nix-ts--mode-matcher
  :head-mode 'host
  :tail-mode 'host)
```

Function matcher contract: `(fn AHEAD)` where AHEAD > 0 means search forward,
< 0 means backward. Returns `(BEG . END)` cons or nil.

### 2. Two tree-sitter queries for span detection

**Query A** — bindings with annotation comment:
```
(binding
  attrpath: (attrpath attr: (identifier) @attr)
  (comment) @lang-comment
  expression: (indented_string_expression) @str)
```

**Query B** — bindings without annotation (attr-name lookup):
```
((binding
   attrpath: (attrpath attr: (identifier) @attr)
   expression: (indented_string_expression) @str)
 (:match "shellHook\\|buildPhase\\|..." @attr))
```

Query A results take priority (annotation is explicit). Query B only applies
for bindings not already matched by Query A.

AST fact: comment nodes are **children** of the binding node, positioned
between attrpath and expression. Both `/* bash */` and `# bash` styles appear
as `(comment)` children.

AST fact: multi-part attrpaths like `packages.x86.default.installPhase`
capture all identifiers. Match against last identifier, or accept any match in
the attrpath.

### 3. Eager cache via buffer-local span list

```elisp
(defvar-local +nix-ts--cached-spans nil
  "Sorted list of (HEAD-BEG HEAD-END STR-BEG STR-END MODE-SYMBOL).")
```

Rebuilt by `+nix-ts--scan-spans` registered in `treesit-parser-notifiers`.
Matchers do position-based lookup on this list.

**Head boundary**: from annotation comment start (or `''` start if no
annotation and no attr-name head context) through first 2 chars of
`indented_string_expression` (the opening `''`).

For attr-name-based spans, head starts at the attr identifier — gives
code-fences box drawing context like `╭── shellHook ──`.

**Tail boundary**: last 2 chars of `indented_string_expression` (closing `''`).

### 4. Known-attrs table and lang-mode alist

```elisp
(defvar +nix-bash-attrs
  '("shellHook" "buildPhase" "installPhase" "configurePhase"
    "checkPhase" "fixupPhase" "unpackPhase" "patchPhase"
    "preBuild" "postBuild" "preInstall" "postInstall"
    "preConfigure" "postConfigure" "preCheck" "postCheck"
    "preFixup" "postFixup" "script"
    "preStart" "postStart" "preStop" "postStop"
    "ExecStart" "ExecStartPre" "ExecStartPost"
    "ExecStop" "ExecStopPost" "ExecReload"))

(defvar +nix-lang-mode-alist
  '(("bash"   . bash-ts-mode)
    ("python" . python-ts-mode)
    ("elisp"  . emacs-lisp-mode)
    ("lua"    . lua-ts-mode)
    ("json"   . json-ts-mode)
    ("sql"    . sql-mode)
    ("rust"   . rust-ts-mode)
    ("c"      . c-ts-mode)))
```

Both user-extensible via `add-to-list`.

### 5. Mode-matcher resolution

Called at head-start position. Looks up cached span for that position:
1. If span has annotation → look up `+nix-lang-mode-alist`
2. If span has attr-name match → `bash-ts-mode`
3. Return mode symbol string (polymode resolves via `pm-get-mode-symbol-from-name`)

### 6. Code-fences callbacks unchanged

`:head-valid-p` — check if tree-sitter node at overlay start is still a valid
binding with indented string expression.

`:unquoted-p` — always t (Nix strings always interpolate).

`:interpolation-fn` — existing `+nix--add-interpolation-overlays`, unchanged.

`:count-openers` — count cached spans, compare with overlay count.

## Risks / Trade-offs

- **[Known-attrs heuristic]** Custom attrs named `shellHook` containing non-bash
  get false-positive fontification. Acceptable — matches nixpkgs convention and
  annotation override is always available.
- **[Cache staleness]** Brief window between edit and parse notification where
  cache is stale. Polymode already tolerates transient inconsistency.
- **[Single innermode]** `pm-inner-auto-chunkmode` is less commonly used than
  fixed innermodes. Tested in markdown-mode polymode (fenced code blocks).
  Function matchers confirmed working via `pm-fun-matcher` source.
- **[Multi-part attrpath]** `a.b.shellHook = ''...''` captures `a`, `b`, and
  `shellHook` as @attr. Matching any identifier against known-attrs is correct
  for last-segment convention but could false-match pathological cases like an
  attr literally named `buildPhase` in an unrelated context. Very unlikely.
