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

- File-class URL span hidden on screen, same way anchor's is.
- Overlay metadata (`gfm-pretty-links-class`, `-url`, `-kind`, etc.)
  remains addressable for RET, eldoc, xref.
- Top-of-file commentary, decorate-link docstring, and spec all agree.

**Non-Goals:**

- No icon for file-class links. (Could be added later as a separate
  change — file icons via `nerd-icons-icon-for-file`.)
- No change to URL classification rules. `./`, `../`, `/`, `file:` still
  classify as `file`; everything else still falls through to `web`.
- No change to RET / find-file / eldoc / xref behaviour. They already
  work for file links via the title-side overlay's metadata.

## Decisions

### Decision: extend `anchor`'s empty-display branch to cover `file`

`make-overlay` has the right shape already — the `(eq class 'anchor)`
branch returns `""` for the URL side. Extend it to
`(memq class '(anchor file))`. `decorate-link`'s class guard widens to
`'(web anchor file)`. Two-line change.

**Alternative considered:** give `file` its own URL-side icon (e.g. a
file-type glyph via `nerd-icons-icon-for-file`). Rejected for this
change — it's a separable UX decision, would need icon-resolution work
in `--icon-for-target`, and the immediate goal is parity with the
existing "URL hidden" expectation that authors already write toward.

### Decision: spec delta on `gfm-pretty`

The behaviour is normatively described in `openspec/specs/gfm-pretty/spec.md`,
requirement *URL-side icon rendering*, scenario *File link omits icon*.
Both the requirement body and the scenario change. Delta uses
`## MODIFIED Requirements`.

### Decision: keep `nerd-icons` independence

Anchor's URL hiding doesn't depend on `nerd-icons` (it's just an empty
display string). File hiding inherits the same property. The existing
spec carve-out at L2181-2183 (web URLs render raw when `nerd-icons`
unavailable) does not extend to file — file is unconditional.

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
