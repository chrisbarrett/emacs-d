## Context

`gfm-pretty-callouts` blends each callout type's header face fg 10%
toward `+theme-default-background` via
`gfm-pretty-callouts--tinted-bg` (`lisp/gfm/gfm-pretty-callouts.el:104`)
and inlines the resulting hex string into every overlay face spec
emitted from `--apply-block-anchors` (line 261) and
`--apply-block-display` (line 317).  Example:

```elisp
(bg-face `(:background ,tint :extend t))
(wrap   (propertize "│ " 'face
                    `(:inherit ,border-face
                      :slant normal :weight light
                      :background ,tint)))
```

Each overlay carries the literal `"#rrggbb"` string in its face plist.
When the user switches theme:

1. Catppuccin (or similar) re-applies face attributes to the standard
   faces (`font-lock-keyword-face`, `font-lock-string-face`, etc.).
2. The callout type faces (`gfm-pretty-callouts-note-face` &c.) re-
   resolve via their `:inherit` chains because Emacs re-resolves
   `:inherit` at every redisplay.
3. `+theme-changed-hook` runs `--refresh-body-faces`
   (line 749) which sets `:background` on the body faces from a
   freshly computed blend.
4. But the OVERLAYS — already laid — carry the OLD blend hex in
   their face spec.  Emacs sees `(:background "#oldhex" ...)` and
   paints that exact colour.  Rectangle stays stale until the
   decorator does a full rebuild (e.g. the user toggles
   `gfm-pretty-mode`).

`gfm-blockquote-rectangle` solved the same class of bug for plain
blockquotes by switching the anchor/display face specs to
`:inherit gfm-pretty-blockquotes-bg-face` (a face the user can
mutate freely; theme change re-evaluates).  Apply the same pattern
to callouts — but callouts have FIVE types, so we need five derived
tint faces refreshed in one `+theme-changed-hook` pass.

## Goals / Non-Goals

**Goals:**

- Callouts overlay specs reference faces by name, never bake
  `:background "#hex"` colour strings.
- Theme change updates the rendered overlays at the next redisplay,
  with no manual `(gfm-pretty-mode -1) / (gfm-pretty-mode 1)`
  cycle.
- Visual output identical to the current implementation immediately
  after a theme change settles (and indistinguishable before).

**Non-Goals:**

- Changing the blend formula (10% toward bg).  Tint colour is
  unchanged.
- Reworking callouts' anchor/display split, reveal mechanism, or
  box-width logic.
- Touching the body-face refresh (`--refresh-body-faces` already
  does the right thing for buffer-char tinting; only OVERLAY faces
  are the bug surface).

## Decisions

### D1. One derived tint face per callout type

Add five new defaces:

```elisp
gfm-pretty-callouts-note-tint-face
gfm-pretty-callouts-tip-tint-face
gfm-pretty-callouts-important-tint-face
gfm-pretty-callouts-warning-tint-face
gfm-pretty-callouts-caution-tint-face
```

Each declared with an empty default spec (`'((t))`) — `:background`
is set dynamically from the blend at theme-change time.  CAUTION and
CRITICAL share the caution tint face (same as the existing
type-face mapping for header colour).

**Alternative considered:** one tint face per (type, decoration)
slot (e.g. a separate face for the box panel vs the rail), or a
single shared tint face with the blend re-keyed per overlay.
Rejected — one face per type is the minimal granularity that
matches how the user reasons about callout types ("NOTE is blue,
TIP is green, …") and mirrors the existing type-face / body-face
shape.

### D2. Refresh tint faces alongside body faces

Extend `gfm-pretty-callouts--refresh-body-faces` (or rename to
`--refresh-faces`) so a single `+theme-changed-hook` callback
computes the blend once per type and sets `:background` on both the
body face AND the tint face for that type.  Sharing the blend call
avoids drift between body-char tint and overlay-cell tint.

**Alternative considered:** keep the body refresh separate and add a
new `--refresh-tint-faces`.  Rejected — both faces depend on the
same per-type blend; running it twice per theme change is wasteful
and risks divergence if the helper changes.

### D3. Overlay face specs use `:inherit (tint-face border-face)`

For overlay cells that previously carried
`(:inherit BORDER :slant normal :background <tint>)`, switch to
`(:inherit (TINT-FACE BORDER) :slant normal)`.  Order matters:

- TINT-FACE first → its `:background` wins.
- BORDER second → its `:foreground` wins (TINT-FACE has no fg).

For the panel-only anchor face that previously carried
`(:background <tint> :extend t)`, switch to
`(:inherit TINT-FACE :extend t)`.

The anchor face spec must still NOT carry `:foreground`, `:weight`,
or `:slant` so emphasis faces on body buffer text merge through.
The tint face's empty default spec satisfies this — only
`:background` is ever set on it dynamically.

**Alternative considered:** use a single anonymous spec list per
overlay site like
`(list :inherit (tint border) :slant 'normal :weight 'light)`.
Rejected — the symbol-only `:inherit` list keeps the spec opaque to
Emacs' face-merging code and matches the blockquote-rectangle
pattern.

### D4. Drop `gfm-pretty-callouts--tinted-bg` callers from the apply path

After D2, the blend is computed only at theme-change time (inside
the refresh helper).  The apply functions stop calling
`--tinted-bg` directly; they consult the tint faces via `:inherit`.
The helper itself stays — it's the blend implementation, called
from the refresh.

**Alternative considered:** delete `--tinted-bg` entirely and inline
the colour math into the refresh helper.  Rejected — keep the
helper as a named, testable unit.

## Risks / Trade-offs

- **Theme-load timing** → if the user's init loads themes before
  `gfm-pretty-callouts` defines its tint faces, the first refresh
  runs against `+theme-default-background = nil` and tints come out
  wrong.  Mitigation: the existing refresh helper already guards
  against `nil` bg by short-circuiting (`when tint`); the new tint
  faces inherit the same guard.  After the user picks a real theme
  (`+theme-changed-hook` fires), tints resolve correctly.
- **Tint face order in `:inherit` list** → if a future refactor
  swaps the order (BORDER first), the border face's transparent
  `:background` would win over the tint face's and the panel would
  lose its tint.  Mitigation: regression test asserts the panel
  cell of a rendered callout has the tint face's bg, not the
  border face's.
- **Test surface churn** → existing callouts tests that assert the
  shape of `--apply-block-anchors` / `--apply-block-display` face
  specs (those that look for `:background <hex>`) need updating.
  Same mechanical change as blockquote-rectangle's test churn;
  scope is contained to the callouts test block.

## Open Questions

- Should the new tint faces be exposed for user customisation
  (`defface` with a meaningful default spec) or stay implementation-
  internal (`'((t))` and refreshed dynamically)?  Recommend the
  latter — they're derived state, not a customisation point.  Users
  who want a different tint can re-define
  `gfm-pretty-callouts--tinted-bg` or set
  `+theme-default-background` differently.
