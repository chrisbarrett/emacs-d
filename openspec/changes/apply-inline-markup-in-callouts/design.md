## Context

`gfm-callouts-mode` decorates GFM callout blockquotes with bordered
boxes.  Each block has a per-line anchor overlay carrying
width-independent props (background tint, `wrap-prefix`) and
per-window display overlays carrying width-dependent props (borders,
right-edge after-strings).

Body lines inside a callout are markdown blockquotes, so font-lock
applies `markdown-blockquote-face` across them.  That face inherits
from `font-lock-doc-face` (`markdown-mode.el:1986-1989`), which most
themes specify with `:slant italic`.  To kill the italic so callouts
read as alerts rather than quotations, the anchor was originally
declared with `'face 'default` (commit `7cb4d2f`, "Tint GFM callout
box background by border colour"), later evolved to
`(:inherit default :background TINT :extend t)` to add the type-tinted
backdrop (`+gfm-callouts.el:332-333`).

The `:inherit default` is doing two jobs: (1) ensure the anchor
specifies a complete face stack so blockquote-italic doesn't leak
through; (2) provide a neutral foreground for body chars that have no
font-lock face of their own.  Side effect: every other emphasis face
that the buffer text carries — `markdown-italic-face`,
`markdown-bold-face`, `markdown-link-face`,
`markdown-inline-code-face`, `markdown-strike-through-face` — is also
clobbered, because Emacs's face merge applies overlay-specified
attributes over text-property attributes at attribute-level
granularity.  No overlay-only configuration can selectively keep
emphasis while suppressing the blockquote italic: both faces specify
the same attributes (`:slant`, `:weight`, …), and overlay face
attributes either win for all underlying faces or none.

## Goals / Non-Goals

**Goals:**

- `**bold**`, `*italic*`, `[link](url)`, `` `inline code` ``, and
  `~~strike~~` inside a callout body render with their respective
  markdown emphasis faces.
- The callout's tinted background, `:extend t` past-EOL fill,
  `│ ` body-prefix substitution, top/bottom borders, right-edge
  pipe, and `wrap-prefix` continue to render as before.
- Callout boxes still read as alerts: body text is not painted
  italic by default just because the source is a `>` blockquote.

**Non-Goals:**

- Per-span face filtering inside callout bodies (option A from the
  exploration).  Not pursued; cheaper to detach blockquote-face from
  the italic inheritance at the face layer.
- Independent styling for plain blockquotes vs callout blockquotes.
  This change accepts that ordinary blockquotes also lose the italic
  default styling.
- Reworking the anchor / display split or the reconciler.

## Decisions

### Decision: Neutralise `markdown-blockquote-face` (clear every attribute)

Override the face in `modules/lang-markdown/init.el`'s `:config` so it
contributes nothing — every attribute set to `'unspecified`.  Plain
blockquote chars then render with `default`; emphasis faces merge
through cleanly inside callout boxes.

**Rationale:** Themes set more than just italic on this face.  Catppuccin
(and similar) specify `:foreground`, `:background`, `:extend`, and
`:slant italic` directly on `markdown-blockquote-face` — not via
inheritance from `font-lock-doc-face`.  An `:inherit`-only override
leaves the direct attributes (notably foreground) in place, and they
keep clobbering buffer text under the callout anchor.  Clearing every
attribute removes the face's footprint entirely; the face stack
collapses to whatever else is applied (emphasis faces, default).
Aligns with the broader pattern in this configuration: callouts, code
fences, and tables already own their rendering rather than work
alongside markdown-mode's defaults.

**Alternatives considered:**

- *Per-span face-filter overlays under each callout body line.*  Walk
  the body region by `next-single-property-change` on the `face`
  text property, copy each span's face value, remove
  `markdown-blockquote-face` from it, set the filtered list as an
  extra overlay face (mirrors the YAML inner-language fontify path
  at `+gfm-code-fences.el:725-731`).  Rejected: needs additional
  overlays per body line, more rebuild work on every edit, and adds
  another lifecycle to keep in sync with the existing anchor /
  display reconciler.  Cost not justified when a face-attribute
  override settles it.
- *Override blockquote-face only when `gfm-callouts-mode` is on.*
  Would need either a buffer-local face remap or a font-lock keyword
  scoped to callout ranges.  Rejected: scope is too narrow given the
  user's stated direction (eventually own blockquote rendering the
  way callouts and fences are owned).  The global override is the
  honest first step.
- *Accept italic leak (option C from exploration).*  Drop only
  `:inherit default`, leave blockquote-face untouched.  Rejected:
  emphasis-italic would be indistinguishable from surrounding body
  italic; bold/underline/links would render but italic emphasis
  would silently disappear into the field.

### Decision: Narrow anchor face to `(:background TINT :extend t)`

Drop `:inherit default` from the body-line anchor at
`+gfm-callouts.el:332-333`.  With blockquote-face no longer italic,
the anchor no longer needs to mask slant.  Specifying only
`:background` and `:extend` leaves all other face attributes
unspecified, so the buffer text's emphasis faces merge through.

When `tint` is nil (theme can't resolve `+theme-default-background`),
the anchor face collapses to `(:extend t)`.  Background then leaks
to the default — same as today minus `:inherit default`.

## Risks / Trade-offs

- [Plain blockquotes lose italic styling buffer-wide] → Accepted per
  proposal.  Future work may take over blockquote rendering with the
  same anchor/display pattern as callouts; until then, plain
  blockquotes render as ordinary prose.
- [Theme switches that re-customise `markdown-blockquote-face` could
  re-introduce italic] → Apply the override in
  `lang-markdown/init.el` `:config`, which runs once when the package
  loads.  Themes that mutate face attributes after that point would
  override us; if that becomes an issue, repeat the override in a
  theme-switch hook.
- [Emphasis faces with `:background` set could fight the anchor tint]
  → `markdown-italic-face`, `markdown-bold-face`,
  `markdown-link-face`, and `markdown-inline-code-face` ship without
  `:background` (`markdown-mode.el:1951-2028`).  If a user theme
  attaches a background to one of those, that background would
  override the tint on those spans.  Out of scope to mitigate.

## Migration Plan

Single-commit change.  No data migration; no toggle.  Rollback is a
revert.

## Open Questions

None.
