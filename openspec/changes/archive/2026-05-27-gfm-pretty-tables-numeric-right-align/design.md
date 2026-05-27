## Context

`gfm-pretty-tables` renders GFM tables with a fixed left-align padding
strategy. Every cell's pad string is appended after the content:

```elisp
;; lisp/gfm/gfm-pretty-tables.el:687
(concat " " cell (make-string pad ?\s) " ")
```

The delimiter row's `:---` / `---:` alignment markers are parsed only
as a row-detection regex; the alignment information is discarded by
`gfm-pretty-tables--parse-table` (lisp/gfm/gfm-pretty-tables.el:977),
which returns `header-cells` and `body-rows` with no alignment vector.

Layout dataflow (`--apply-table-display`, lisp/gfm/gfm-pretty-tables.el:924):

```
parse → header-cells, body-rows
      → col-widths (--column-widths → --fit-widths)
      → row-layout (wraps cells to col-widths, builds bounds-vec)
      → compose-row-from-layout → compose-row (per visual line)
```

`--row-char-bounds` (lisp/gfm/gfm-pretty-tables.el:777) computes cell
segment bounds as `(+ 2 cell-len pad)` — bounds cover the full padded
segment, so reordering the pad inside the segment leaves bounds
identical, and the packed bounds-vec on the display overlay stays
correct.

## Goals / Non-Goals

**Goals:**
- A column of all-numeric body cells (integers, decimals, optional
  sign, optional scientific exponent) renders right-aligned in every
  row of that column, including the header and every visual line of
  a wrapped cell.
- Empty body cells do not disqualify a column; an all-empty column
  stays left-aligned.
- Detection is purely a property of body-cell content; nothing in
  the source row of the user needs to change.
- Bounds-vec on display overlays remains unchanged in shape and
  values — cell-highlight, navigation, and edit commit keep
  working without modification.

**Non-Goals:**
- Honouring `:---` / `---:` / `:---:` markers from the delimiter row.
  The regex still tolerates them, but their information is still
  discarded.
- Centre alignment. The compose path stays binary `left | right`.
- Extended numeric forms: thousands separators (`12,345`), currency
  (`$5.99`), units (`42ms`), percentages (`42%`). These stay
  left-aligned.
- Per-table user override for alignment.

## Decisions

### Decision: detect on trimmed source-cell text

Detection regex runs against the trimmed source string returned by
`gfm-pretty-tables--split-row` (lisp/gfm/gfm-pretty-tables.el:353)
before fontification. Rationale:

- Fontified cells carry text properties and may have width-affecting
  overlay decoration baked in (lisp/gfm/gfm-pretty-tables.el:157).
  Markdown emphasis markers like `**1.2**` survive in the cell
  string and would fail a strict numeric regex; an "is-numeric" check
  on the source string after `string-trim` avoids needing to strip
  markup chars heuristically.
- The parser already produces clean trimmed strings as a by-product;
  no extra walk needed.

**Alternative considered:** regex-test the fontified cell. Rejected —
forces a markup-stripping pre-pass that re-implements work
`markdown-mode` does later when `markdown-hide-markup` is in effect,
and would handle different inline forms inconsistently (` `123` `
already strips fine, `**123**` would not).

### Decision: numeric regex

```elisp
(rx bos
    (? (any "+-"))
    (+ digit)
    (? "." (* digit))
    (? (any "eE") (? (any "+-")) (+ digit))
    eos)
```

Matches: `0`, `42`, `-3`, `+7`, `1.5`, `-0.001`, `1e10`, `2.5E-3`.
Does not match: `1,234`, `$5.99`, `42%`, `0x1f`, `1_000`, `nan`.

Rationale: minimal, predictable, no locale dependency, easy to
extend later. Matches the table the user motivated the change with
(LOC counts).

**Alternative considered:** liberal numeric (commas, percent, currency
sigils). Rejected for first iteration — invites locale/format
disputes and disagrees with what users elsewhere expect from a
right-align heuristic. Extending the regex is a one-line change
later.

### Decision: column qualifies when every body cell is numeric or empty, and at least one is numeric

```
numeric?(c)  := (string-match-p numeric-rx c)
empty?(c)    := (string-empty-p (string-trim c))
column i is right-aligned ⇔
  every body row r: numeric?(r[i]) or empty?(r[i])
  ∧ at least one body row r: numeric?(r[i])
```

Rationale:
- Sparse numeric columns (some empty cells) shouldn't lose alignment.
- An all-empty column would otherwise vacuously qualify; the
  "at least one numeric" clause keeps it left-aligned.
- Header content is excluded from the check (the user explicitly
  asked for the header to follow the column, not gate it).

**Alternative considered:** require every cell including header to be
numeric. Rejected — headers are almost always textual labels.

### Decision: alignment vector built once, threaded as a parameter

`gfm-pretty-tables--col-align` (new helper) returns a vector parallel
to `col-widths`, with each slot `'left` or `'right`. Computed once
inside `--apply-table-display` between the parse and compose phases,
not stored on the overlay (compose-time-only; the bounds-vec is the
only data display callers need post-compose).

`--compose-row` and `--compose-row-from-layout` gain a `col-align`
parameter. `--row-layout` and the bounds-vec layout are unchanged —
alignment affects only the pad placement inside the segment, not
the segment's length or position.

```elisp
;; replacement for the trailing-pad form at lisp/gfm/gfm-pretty-tables.el:687
(let ((pad-str (make-string pad ?\s)))
  (pcase (aref col-align i)
    ('right (concat " " pad-str cell " "))
    (_      (concat " " cell pad-str " "))))
```

**Alternative considered:** store `col-align` on the display overlay
alongside `col-widths`. Rejected — no downstream consumer needs it
after compose; storing it would widen the overlay payload without
purpose.

### Decision: wrap-cell stays unchanged; alignment is a per-visual-line concern

`--wrap-cell` (lisp/gfm/gfm-pretty-tables.el:631) is alignment-blind;
it just slices content into lines of `≤ width`. Because
`--compose-row-from-layout` maps `--compose-row` over every visual
line (`line-cells`), each wrapped line of a right-aligned numeric
cell gets its own leading-pad treatment automatically. No change
to wrap logic is needed.

Practically, multi-line wrapping is rare for numeric cells (they're
short), but the behaviour is well-defined and correct without
special-casing.

## Risks / Trade-offs

- **[Risk] Surprise alignment shift for users who relied on left-align.**
  Numeric-only columns flip to right-align with no opt-out. →
  Mitigation: scope is narrow (strict numeric regex), affects only
  the rendered overlay (source untouched). If feedback arrives, a
  defcustom toggle is a cheap follow-up.
- **[Risk] Detection-pass cost on large tables.** Per-table linear
  scan over body cells. → Mitigation: one regex per body cell, no
  fontification, run once per `--apply-table-display` invocation
  (already the per-table rebuild boundary).
- **[Trade-off] `1,234` and `42%` stay left-aligned.** Acceptable
  for first cut; revisit if it bites in practice.
- **[Trade-off] No honouring of `---:` markers in source.** Authors
  who already write delim-row alignment get no extra help from this
  change. Out of scope per the proposal.

## Migration Plan

No data migration. The feature activates at render time the next
time `gfm-pretty-tables--apply-table-display` runs for a buffer.
Existing buffers pick it up on next rebuild (window resize, edit,
or mode re-enable).

Rollback: revert the change. Overlays rebuild left-aligned on next
trigger.
