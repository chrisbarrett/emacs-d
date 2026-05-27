## Context

`gfm-pretty-links` classifies link URLs into three target classes and
decorates each differently:

| Class    | URL-side overlay        | Visible URL? |
| :------- | :---------------------- | :----------- |
| `web`    | `display` = nerd-icon   | hidden       |
| `anchor` | `display` = `""`        | hidden       |
| `file`   | none created            | shown raw    |

`file` is the odd one out. `decorate-link` only makes the URL overlay
when class ∈ `'(web anchor)`. The asymmetry is visible in real
documents — e.g. README files that link to sibling paths via `../../…`
end up showing `[pretty label](../../long/relative/path)` while
co-located links to host-relative paths look clean.

The decorator's commentary at L21-25 says file is "title-only" (implies
URL hidden), while the docstring on `decorate-link` at L535 says the URL
"renders raw". The implementation matches the docstring; the top-of-file
commentary is wrong.

## Goals / Non-Goals

**Goals:**

- File-class URL span hidden on screen and replaced with a nerd-icons
  file-type glyph (mirroring `web`'s icon treatment).
- Overlay metadata (`gfm-pretty-links-class`, `-url`, `-kind`, etc.)
  remains addressable for RET, eldoc, xref.
- Top-of-file commentary, decorate-link docstring, and spec all agree.
- Graceful fallback to empty-string display when `nerd-icons` is
  unavailable (path stays hidden; just no glyph).

**Non-Goals:**

- No change to URL classification rules. `./`, `../`, `/`, `file:` still
  classify as `file`; everything else still falls through to `web`.
- No change to RET / find-file / eldoc / xref behaviour. They already
  work for file links via the title-side overlay's metadata.

## Decisions

### Decision: route `file` through the existing icon branch

`make-overlay`'s `t` branch already resolves an icon via
`gfm-pretty-links--icon-for-target`, which falls through to
`nerd-icons-icon-for-file` on the URL's basename for relative paths
and `file:` URIs. Letting `file` fall into that branch is the
minimum-friction implementation: keep `(eq class 'anchor) ""`, drop
`file` from the empty-display branch so it inherits the icon path.
`decorate-link`'s class guard still widens to `'(web anchor file)`.

The fallback when `nerd-icons` is unavailable is already encoded in
`make-overlay` as `(or (--icon-for-target …) "")`, so file links keep
hiding the path even without icons.

**Alternative considered:** keep file URL-side as empty `display` and
paint the icon on the title overlay's `before-string`. Rejected —
adds a second visual moving piece (separate face, separate placement
heuristic) and diverges from `web`'s structure where the icon lives
on the URL-side overlay.

### Decision: strip wrapping backticks at the display layer

A label of the form `` `pretty` `` is just visual noise in a
fixed-width font when the entire title is wrapped. Strip the wrap at
display time only — a helper `--strip-wrapping-backticks` runs over
the label in `--make-overlay`'s title branch. The `--link-label` slot
and `gfm-pretty-links-label` overlay property keep the original
string, so eldoc and xref still surface the source representation.

"Wrapping" means: starts and ends with `` ` `` AND no interior
`` ` `` between them. Labels like `` say `hi` world `` are left
intact — partial code spans inside prose are legitimate markdown that
the user expects to read literally.

**Alternative considered:** strip at scan time (mutate the label in
the record). Rejected — `gfm-pretty-links-label` is part of the
overlay's public metadata surface, and consumers (eldoc, xref) read
the source representation. Display-time stripping is a strict
projection, no behaviour change for downstream readers.

### Decision: spec delta on `gfm-pretty`

The behaviour is normatively described in `openspec/specs/gfm-pretty/spec.md`,
requirement *URL-side icon rendering*, scenario *File link omits icon*,
and requirement *Title-side overlay rendering* (for the backtick
strip). All deltas live under `## MODIFIED Requirements`.

### Decision: file URL hiding survives missing `nerd-icons`

`web`'s carve-out (URL renders raw when `nerd-icons` is unavailable)
exists because the URL-side overlay is *omitted* in that branch, so
the text falls back to whatever markdown-mode draws. For `file` the
overlay is always created — only its `display` switches between an
icon (when nerd-icons resolves one) and `""` (fallback). So even
without `nerd-icons`, the path stays hidden. Anchor remains
unaffected; its `display` is always `""`.

## Risks / Trade-offs

- **[Risk] Users may have relied on visible file URLs for debugging
  ("which file does this link to?").** → Mitigation: eldoc already
  surfaces the formatted source on hover, and `M-x describe-char` /
  inspecting the title-side overlay still works. RET goes to the file
  directly. No information loss; only one less always-on affordance.

- **[Risk] Image links (`![alt](path)`) are rejected before
  classification, so this change does not affect them.** → No
  mitigation needed; the regex's leading-`!` group is consulted in
  `scan-inline`.

- **[Trade-off] No file icon means file-class links are visually
  indistinguishable from anchor-class links (both show title-only).**
  → Acceptable: face still differs (`gfm-pretty-links-file-face` vs
  `gfm-pretty-links-anchor-face`), and the title-side keymap dispatches
  correctly regardless. A future change can add a file icon if needed.

## Migration Plan

Single commit; no migration. Existing buffers re-render on next rebuild
tick. Spec delta archives into the stable spec via the standard
`openspec archive` flow.
