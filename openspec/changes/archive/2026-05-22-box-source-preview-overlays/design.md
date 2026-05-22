## Context

`gfm-present-mode` decorates source-range (`<path>#L<a>-L<b>`) and
diff-range (`diff:<base>...<head>[#<path>]`) markdown links with
preview overlays whose `display` property is a propertised
multi-line string. The current rendering — header line in
`markdown-code-face` plus fontified body plus optional shadow
footer (`lisp/gfm/gfm-present.el:352`, `:456`) — is visually
unrelated to authored fenced code blocks rendered by
`gfm-pretty-fences` (`lisp/gfm/gfm-pretty-fences.el:282`). Slides
that interleave authored fences and preview overlays read as two
unrelated kinds of content.

The fences decorator runs per-window with display overlays sized
to each window's width, anchored to fence marker lines in buffer
text. Preview overlays are different: one display string per
overlay, covering a `[label](url)` span, not scoped per window.
The two cannot share rendering primitives directly — they share a
visual idiom (box-drawing borders, language icon, LHS-margin diff
mode) but not an overlay model.

The preview path also unconditionally decorates *every* matching
link in the slide, including inline links inside prose
(`gfm-present--render-link-previews` at
`lisp/gfm/gfm-present.el:473`), which produces visually broken
prose lines.

## Goals / Non-Goals

**Goals:**

- Source-range previews render inside a box matching the visual
  idiom of authored fenced blocks (corners, border face, top/
  bottom border decoration).
- Diff-range previews share the box treatment, with LHS-margin
  body mode to match authored diff fences.
- Inline (mid-prose) source/diff links are left undecorated;
  only standalone links (whole-line, list-item-only, or
  blockquote-only) get a preview.
- Path identification lives in the top border. The markdown
  `[label]` is dropped from the preview surface.
- Error states (missing file, invalid range, git error) render
  distinctly from healthy previews so the reader recognises
  "broken preview" at a glance.

**Non-Goals:**

- Per-window re-rendering on resize. Previews stay sized to the
  selected window at render time. Resizing the window leaves
  previews stale until next slide navigation. (Same limitation
  the previous rendering had.)
- Reusing `gfm-pretty-fences--apply-bordered-display` directly.
  The per-line overlay model is incompatible with the
  display-string model used by previews.
- Visual wrapping of overflowing body lines. Overflow truncates
  with ellipsis; full content is one RET away.
- Restructuring `gfm-present--render-link-previews` into the
  generic `gfm-pretty` decorator framework. The preview
  overlays remain manual; they are not narrowing- or
  reconciler-driven.

## Decisions

### Decision: render the box as a single propertised display string

Authored fence rendering uses per-line per-window overlays
(`lisp/gfm/gfm-pretty-fences.el:282-428`) — that machinery is
incompatible with the display-string model used by preview
overlays (one overlay covering the whole `[label](url)` span,
`display` property holds the full multi-line rendered
appearance).

Build the box as one string, concatenated from:

1. Top border: `┌─ <abbrev-path>:<a>-<b> ─… ─┐`
2. Body lines, each prefixed with `│ ` and right-padded plus ` │`
   (or `│` / `│` in LHS-margin mode for diff).
3. Bottom border: `└─ +N more lines ─… ─┘` when truncated;
   bare `└─…─┘` otherwise.

**Alternatives considered:**

- *Reuse `gfm-pretty--top-strings` / `--bottom-strings`* — the
  signature expects a `buffer-width` arg (chars on the marker
  line, since the border partially overlays that line). For a
  display-string render there is no marker line; passing 0 works
  but the leading/trailing split adds machinery the preview
  doesn't need. Re-implement the simpler shape inline.
- *Decorate the preview text as a literal ` ```lang ` fence and
  let `gfm-pretty-fences` discover it* — fences mode scans buffer
  chars, not overlay display strings, so it would not see the
  fence markers inside a display property. Switching the model
  (making the preview *replace buffer text* with a real fence) is
  out of scope and conflicts with the overlay-only invariant
  (`### Requirement: Source-range link preview overlay` already
  says "The underlying buffer text SHALL NOT be modified").

### Decision: shared helper `gfm-present--box-display`

Both `gfm-present--source-preview-display` and
`gfm-present--diff-preview-fence` call a shared helper:

```
(gfm-present--box-display
  :label LABEL          ; top-border label string (already abbreviated)
  :body  BODY           ; pre-fontified body string (multi-line)
  :extra EXTRA          ; truncation count for bottom border (or 0)
  :lhs-margin LHS-MARGIN); t for diff body
```

The helper computes box width, builds top/bottom borders with
embedded label/extra, splits body into lines, applies line-
length truncation + right-padding to align the right border, and
returns the assembled string.

**Alternatives considered:**

- *Two separate functions* — duplicates border math and
  truncation logic; diverges silently when one changes.
- *Inline both into a single mega-function with a `kind` arg* —
  the source and diff variants differ in label shape (`path:a-b`
  vs `base...head — path`) and body source (file read vs git
  diff output). Separation by concern: callers build label and
  body, helper renders box.

### Decision: standalone-link detection via line context

Predicate: the link's line, with the link token stripped, matches
`(rx bos (* blank) (? (or "- " "* " "+ " "> " (: (+ digit) ". "))) (* blank) eos)`.
That is — strip the `[label](url)` from the line, what remains
must be only whitespace and optionally a list-item or blockquote
marker.

Run the check inside `gfm-present--render-link-previews` between
match discovery and preview construction; skip non-standalone
matches without consuming any preview-overlay slot.

**Alternatives considered:**

- *Block-level markdown parsing via `markdown-mode`'s parser* —
  overkill; the regex check is cheap and matches the common
  cases (whole-line link, list-item-only, blockquote-only) that
  authors actually write.
- *Require an explicit syntax author opt-in (e.g. wrap link in
  fenced placeholder)* — burdens authoring for what should be
  zero-friction.

### Decision: path abbreviation via project-relative → ~/ → ellipsis

```elisp
(defun gfm-present--abbrev-source-path (abs-path)
  (let ((proj (let ((default-directory (file-name-directory abs-path)))
                (project-current))))
    (cond
     ((and proj (file-in-directory-p abs-path (project-root proj)))
      (file-relative-name abs-path (project-root proj)))
     (t (abbreviate-file-name abs-path)))))
```

Then, at render time, if the abbreviated path plus `:<a>-<b>` plus
border decoration exceeds the box width, truncate with a leading
`…` (preserving the trailing basename and line range — the most
identifying portion).

**Alternatives considered:**

- *Just basename* — `drift.core.yml:13-22` — discards path
  context; two files with the same basename in different
  directories become indistinguishable on a slide that references
  both.
- *`abbreviate-file-name` only* — loses project-relative wins for
  the common case (deck and source co-located in same project).
- *Custom `gfm-present-path-style` defcustom* — premature
  configurability; one sensible default is enough.

### Decision: box width formula

`(min available-width (max 80 (+ longest-body-line decoration-w)))`,
mirroring `gfm-pretty-fences--apply-bordered-display`
(`lisp/gfm/gfm-pretty-fences.el:294-303`).

`available-width` is `gfm-pretty--available-width` of the selected
window, which falls back to `fill-column` or 80 when the buffer
isn't displayed (`lisp/gfm/gfm-pretty-engine.el:55-64`).

`decoration-w` is 4 for normal mode (`│ ` + ` │`) or 2 for LHS-
margin mode (`│` + `│`).

### Decision: long-line truncation with ellipsis

When a body line's `string-width` exceeds `box-width -
decoration-w`, truncate to `box-width - decoration-w - 1` cells
and append `…`.  Computed against `string-width` (cell count),
not `length` (char count), because fontified bodies may contain
multi-cell chars (icons, CJK).

Fontification face properties on the truncated tail of the
original line are dropped. This is acceptable — the tail is no
longer visible.

**Alternatives considered:**

- *Hard cut, no ellipsis* — looks like the line ends there.
- *Wrap inside the box* — would need
  `gfm-pretty--simulate-wrap`'s logic re-implemented for display
  strings, since Emacs visual-line wrapping doesn't apply inside
  a `display` property's string.

### Decision: error states bypass the box

`(file not found: …)`, `(invalid range)`, `(git error: …)`,
`(no changes)` render as a single line:

```
[broken preview] <abbrev-path>:<a>-<b> — <reason>
```

(or the diff equivalent) propertised with `shadow` face. No box.
The visual distinction signals "this preview is broken" at a
glance; a boxed sentinel would be ambiguous (is the error message
*the content*, or is the preview broken?).

### Decision: diff body uses LHS-margin mode

Body lines render as `│+added` and `│-removed` (no left padding
inside the box), matching authored ` ```diff ` fences rendered by
`gfm-pretty-fences` (`lisp/gfm/gfm-pretty-fences--lhs-margin-langs`
at `lisp/gfm/gfm-pretty-fences.el:79-89`).

The helper's `:lhs-margin t` arg drives the box decoration width
(2 cols instead of 4) and the per-line prefix (`│` instead of
`│ `).

### Decision: diff top-border label format

`<base>...<head>` when no path; `<base>...<head> — <path>` when
scoped to a path. SHA-shorten any ref matching
`(rx bos (= 40 hex) eos)` to its first 7 chars. Branch names and
tags pass through untouched.

The em-dash separator distinguishes the path qualifier from the
ref pair (which uses `...`) without competing.

## Risks / Trade-offs

- [Visual mismatch between preview boxes and authored fences if
  the box-width formulas drift] → Both use the same
  `min(window, max(80, content+deco))` formula and the same
  `gfm-pretty-border-face`. Document the shared formula in the
  helper's docstring; do not re-derive in two places.

- [Standalone-link regex may misclassify edge cases (e.g. a link
  followed by trailing punctuation `[x](path#L1).` on its own
  line)] → The "loose" definition accepts list-item / blockquote
  markers but not trailing punctuation. Authors who want a
  preview write a clean line. If this proves too strict in
  practice, the regex can be widened in a follow-up — easier
  than the inverse.

- [Window-resize staleness] → Previews are sized to the selected
  window at render time. Resize → previews look wrong on the new
  width. The previous rendering had the same staleness;
  acceptable for v1. A future change can hook
  `window-size-change-functions` if needed.

- [Project resolution cost on every preview render] →
  `project-current` is cached by `project.el` once a root is
  found. Hit rate is high for repeated previews from the same
  source tree. Cold-cache cost is one `vc-root-dir`-style walk
  per unique source directory; cheap.

- [Truncation drops fontification face properties on the tail] →
  Tail isn't visible, so the loss is invisible to the reader. No
  user-facing impact.

- [Existing tests need rewriting] → Tracked in tasks.md. Tests
  against header text, footer string, and `markdown-code-face`
  presence no longer apply; replace with assertions against the
  border characters, the in-border path/range, and the in-border
  truncation notice.
