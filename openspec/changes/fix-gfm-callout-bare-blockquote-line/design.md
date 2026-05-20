## Context

The callouts decorator in `lisp/gfm/gfm-pretty-callouts.el` substitutes the
two-char `> ` body prefix with a `│ ` box edge via a per-window display
overlay. The relevant branch sits at `gfm-pretty-callouts--apply-block-display`
(lines 399-407 in current HEAD):

```elisp
(when (and (>= (- lend lbeg) 2)
           (eq (char-after lbeg) ?>)
           (eq (char-after (1+ lbeg)) ?\s))
  (gfm-pretty-callouts--make-display
   lbeg (+ lbeg 2) window
   ...
   'display edge))
```

`edge` is `"│ "`. The length-≥2 guard misses bare-`>` body lines (CommonMark
§6.4 permits `>` with no trailing space as a blockquote continuation). Block
discovery already recognises bare `>` via `--blockquote-line-re` (`(rx bol
">")`), and the anchor + right-edge overlays attach correctly. Only the
left-edge substitution is missing, so raw `>` leaks through mid-box.

## Goals / Non-Goals

**Goals:**

- Substitute the left-edge `│` on a bare-`>` body line so the callout box
  renders without source leakage.
- Preserve existing geometry: right-edge `│` lands on the correct column,
  cursor motion still works, reveal walker still exposes source on that
  line.

**Non-Goals:**

- Handling `>foo` (no space, non-blank content after `>`). Out of scope; rare
  in practice and would require widening overflow / line-text logic.
- Reflowing block discovery; the regexes already accept bare `>`.
- Touching anchor, right-edge after-string, or box-width math.

## Decisions

### Decision: extend the existing branch, not add a new code path

Split the substitution into two cases on source length:

```
> + space  (lend - lbeg ≥ 2) → 2-char source range, display "│ "
> alone    (lend - lbeg = 1) → 1-char source range, display "│ "
```

A display string can be any visual width regardless of source range width, so
the same `edge` works for both. Alternative considered: substitute `>` →
`│` (1 char) and let `(space :align-to)` pad to col 2. Rejected — adds a
second `edge` variant for no visible gain, and complicates the cursor /
reveal walker since two body-prefix overlay shapes would need handling.

### Decision: keep `body-prefix` overlay kind and revealable property

The new branch tags the overlay with the same
`gfm-pretty-callouts-kind 'body-prefix` and revealable property as the
existing branch. The reveal walker keys off these properties; reusing them
keeps reveal behaviour uniform across the two source forms.

### Decision: overflow / right-edge math unchanged

`line-content-w` is computed as `(max 0 (- (- lend lbeg) 2))`; for bare-`>`
this clamps to 0 (no content). `line-text` slice is `(min (+ lbeg 2) lend)
.. lend` which yields the empty string. Both downstream computations remain
correct without further changes.

## Risks / Trade-offs

- [Display string wider than source range] → Emacs handles this natively;
  no extra work needed. Right-edge after-string is positioned at `lend`,
  unaffected by display string width.
- [Cursor motion across the 1-char source range] → The existing branch
  already tags the body-prefix overlay with `evaporate t`; the bare-`>`
  branch uses the same flags, so behaviour matches.
- [Reveal scoping] → Reveal walker selects on the revealable property, which
  the new branch sets identically. No divergence.
