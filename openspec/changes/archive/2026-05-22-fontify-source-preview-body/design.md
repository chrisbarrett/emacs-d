## Context

`gfm-present--source-preview-fence` (lisp/gfm/gfm-present.el:352-367)
calls `gfm-present--read-line-range` to get N source lines as a
joined string, then `gfm-present--build-preview-fence` to wrap them in
` ```<lang>\n...\n``` ` syntax. That string is put on an overlay's
`display` property by `gfm-present--make-preview-overlay`
(gfm-present.el:378-384).

Emacs renders `display` strings as plain text with their own text
properties — markdown-mode's font-lock keywords do not re-scan
overlay display strings. So the fence delimiters and language tag
appear literally, and the body has no faces.

Extension-to-language lookup currently uses a hand-rolled alist
`gfm-present--ext-to-lang` (gfm-present.el:288-303) that maps
`.rs → rust`, `.el → elisp`, etc. Missing entries (`.yml`, `.tf`,
`.hcl`, `.nix` is present, `.css`, …) fall back to `text`. Even when
matched, the *language tag* in the fence has no rendering effect
because the fence itself is plain text.

`auto-mode-alist` is Emacs's authoritative file→major-mode map. It
covers the static modes (yaml-mode, hcl-mode) and tree-sitter modes
(yaml-ts-mode, …) the user has configured. Using it means the
preview's fontification matches what `find-file` on the same path
would produce.

## Goals / Non-Goals

**Goals:**

- Preview bodies render with real syntax highlighting matching the
  source file's major-mode.
- File→mode mapping respects the user's `auto-mode-alist` (including
  tree-sitter mode preferences).
- The visual surface (header line + body + optional footer)
  preserves the existing oversize-truncation contract and the
  "click to open" call-to-action.
- Buffer text remains unmodified — still display-only overlay.

**Non-Goals:**

- Embedding live links / clickable spans inside the preview body.
- Supporting languages whose major-mode is not in
  `auto-mode-alist` (rare; user can extend their own config).
- Caching fontified previews across slide entries. Premature for
  current usage (≤ 10 previews per slide).
- Changing diff-preview rendering (different change if needed).

## Decisions

### Decision: temp buffer + font-lock-ensure for fontification

```
1. Resolve major-mode = (assoc-default path auto-mode-alist 'string-match)
2. With-temp-buffer:
     (funcall major-mode)              ; sets up font-lock
     (insert "\n".join(lines))
     (font-lock-ensure)                ; force fontification
     (buffer-substring (point-min) (point-max))  ; keeps text props
3. Result string carries `face` properties; embed in overlay display.
```

`buffer-substring` (vs `buffer-substring-no-properties`) preserves
text properties from font-lock. Those properties survive into the
`display` overlay and render with their faces.

Alternatives considered:

- **`with-temp-buffer` + `delay-mode-hooks` to skip hooks.**
  Considered for perf. Reject for now: most mode hooks are cheap,
  and skipping them may disable tree-sitter highlighters that
  install through hooks. Reconsider only if profiling shows them
  as a hotspot.
- **`with-syntax-table` + manual font-lock-fontify-region without a
  major mode.** Rejected: doesn't handle tree-sitter modes (which
  install their own highlighter, not classic font-lock keywords).
- **Render via `markdown-mode` fence font-lock by inserting the
  fence into a temp markdown buffer.** Rejected: indirection;
  markdown-mode would have to spin up the language mode anyway.

### Decision: drop fence delimiters from display

The ` ``` ` markers visible on screen are now confusing visual
noise — they aren't real fences, just plain text. Remove them.
The display becomes:

```
<label> · <path>:<start>-<end>
<fontified body line 1>
<fontified body line 2>
...
+N more lines · click to open    ; only when truncated
```

Header line propertised with `markdown-code-face` (or a new face if
that proves wrong on the user's theme). Body is the propertised
substring. Footer propertised with `shadow`.

Alternative: keep the fence-looking delimiters. Rejected: they're
visual lies — the rendered "fence" can never be re-parsed as a
fence by any consumer.

### Decision: rename the helper

`gfm-present--source-preview-fence` becomes
`gfm-present--source-preview-display` (or similar) to reflect that
it returns a display string, not a fence. Internal change only — no
public callers outside gfm-present.

### Decision: language detection lives in a single helper

Add `gfm-present--major-mode-for-path PATH`:

```elisp
(or (assoc-default path auto-mode-alist #'string-match-p)
    #'fundamental-mode)
```

`fundamental-mode` fallback means unknown extensions still get a
clean (unfontified) preview rather than an error.

## Risks / Trade-offs

- **Risk:** Major-mode setup for some languages is slow
  (`org-mode`, complex tree-sitter grammars).
  → Mitigation: cap of 10 lines per preview limits work. If
  observed slowness becomes a problem, add a per-mode allowlist or
  fontify with `delay-mode-hooks`.

- **Risk:** `font-lock-ensure` does nothing for modes that fontify
  via post-command hooks rather than `font-lock-defaults`. Rare
  modes only; most major-modes implement font-lock correctly.
  → Mitigation: tested cases (elisp, yaml-ts, rust, python) work.
  Document as a known limitation if a user reports a specific mode.

- **Risk:** Tree-sitter modes may require the buffer to be associated
  with a file (`buffer-file-name`) to activate tree-sitter properly.
  → Mitigation: set `buffer-file-name` to the source `PATH` inside
  the temp buffer before calling the major-mode, then unset before
  killing. Avoids find-file side effects (auto-save, hooks) while
  giving tree-sitter the cue it needs.

- **Trade-off:** Dropping fence delimiters changes the visual feel
  slightly. Users who saw the fence as intentional decoration lose
  it. Net positive — the new look matches an inline code snippet
  more honestly.

- **Risk:** Loss of regression coverage when removing the alist —
  tests that asserted "`.rs` resolves to `rust`" become irrelevant.
  → Mitigation: new tests assert the propertised body, which is the
  user-facing concern. Language-tag assertions retired.
