## Context

`+code-fences.el` renders box-drawing overlays, active-state dimming, and body refontification for polymode inner spans. It currently contains shell heredoc-specific logic: unquoted detection (`<<` vs `<<'`), stale overlay validation via `<<-?` regex, and `$VAR`/`${}`/`$()` interpolation highlighting. This prevents Nix (and future hosts) from using interpolation highlighting without duplicating the rendering core.

Two polymode hosts exist today: `bash-ts-mode` (shell heredocs) and `nix-ts-mode` (multiline `''` strings with `/* lang */` or `# lang` annotations).

## Goals / Non-Goals

**Goals:**
- `+code-fences.el` contains zero host-specific syntax knowledge
- Each lang module owns its parsing functions and registers them via a public API
- Nix gets `${...}` interpolation highlighting with correct `''$` escape handling
- Existing shell functionality preserved exactly

**Non-Goals:**
- Adding new polymode host languages (future work uses the new API)
- Changing overlay visual design (box drawing, dimming)
- Modifying polymode itself or its span detection

## Decisions

### 1. Registration via alist keyed by host major-mode

`+code-fences-host-config` is a module-level alist. Each host registers with `+code-fences-register`.

```
(defvar +code-fences-host-config nil
  "Alist of (HOST-MODE . PLIST).")

(defun +code-fences-register (host-mode &rest plist)
  (setf (alist-get host-mode +code-fences-host-config) plist))
```

Host mode resolved at runtime: `(oref (oref pm/polymode -hostmode) :mode)`.

**Alternatives considered:**
- Buffer-local variables: simpler but scattered — no single place to see all hosts
- EIEIO mixin on hostmode objects: couples to polymode internals, harder to debug

### 2. Three callback keys

| Key                 | Signature                        | Purpose                          |
|:--------------------|:---------------------------------|:---------------------------------|
| `:head-valid-p`     | `(fn BEG) → bool`               | Stale overlay detection          |
| `:unquoted-p`       | `(fn HEAD-BEG HEAD-END) → bool` | Body has interpolation?          |
| `:interpolation-fn` | `(fn BEG END BASE-BUF)`         | Create interpolation overlays    |

All optional. Missing `:head-valid-p` → no stale detection (overlays persist until full redecorate). Missing `:unquoted-p` → no interpolation. Missing `:interpolation-fn` → no interpolation overlays.

### 3. Nix escape detection via single-quote parity

Count consecutive `'` chars immediately before `$`. Even count → escaped (`''$` consumed as escape). Odd count → real interpolation (leftover `'` from `'''` escaped-quote sequence).

### 4. Face rename

`+polymode-shell-interpolation-face` → `+polymode-interpolation-face`. Aliased for backwards compat via `define-obsolete-face-alias`.

## Risks / Trade-offs

- **[Stale detection divergence]** Each host must implement its own `:head-valid-p`. If buggy, overlays linger or disappear. → Mitigation: test fixtures per host, existing shell tests cover regression.
- **[Nix quote parity edge cases]** Deeply nested `''''` sequences in practice are rare. → Mitigation: parity counting matches Nix parser semantics; test with multi-level cases.
- **[Face rename breaks customisation]** Users who customised the old face name lose their settings. → Mitigation: `define-obsolete-face-alias` preserves existing customisations.
