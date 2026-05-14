## Context

`modules/lang-markdown` already carries four overlay-based GFM
decorators — callouts, code fences, tables, and a shared block-borders
helper — built around a uniform pattern: per-window overlay rendering,
post-command reveal, debounced rebuild, narrowing-resilient
discovery. Link decoration is the conspicuous gap. markdown-mode ships
`markdown-hide-urls`, but it leaves `[title]` wrapped in brackets and
collapses the URL to a single composition character, producing
`[title]▷`. The user wants link rendering pulled into the same
overlay-decorator family used by the rest of the module.

Two non-trivial entanglements drive this design out of "wedge in a
font-lock keyword" territory and into a dedicated module + a width
walker change:

1. The four existing decorators use overlays, which the table cell
   width measurement
   ([+gfm-tables.el#L152](/Users/chris/.config/emacs/modules/lang-markdown/lib/+gfm-tables.el#L152))
   does not currently consult — it walks text properties only.
2. markdown-mode's built-in `compose-region` URL collapse
   ([markdown-mode.el#L8324](/Users/chris/.config/emacs/elpaca/sources/markdown-mode/markdown-mode.el#L8324))
   would race / double-count against our overlays unless suppressed.

Both must be solved for link decoration to coexist correctly with
tables and the rest of the GFM stack.

## Goals / Non-Goals

**Goals:**

- Decorate every link shape where decoration adds reading value:
  inline `[t](url)`, reference full/collapsed/shortcut, autolinks
  `<url>`, GFM bare URLs, and wiki links (when
  `markdown-enable-wiki-links` is non-nil).
- Make the existing table width walker overlay-aware so link
  overlays inside cells measure correctly. Apply the same fix to
  any future overlay-based decorator that lands inside a cell.
- Reveal the whole link (title + URL) on cursor-on; restore on
  cursor-off. Match the reveal pattern in `gfm-callouts-mode` /
  `gfm-code-fences-mode`.
- Split RET (follow URL) from goto-definition (jump to ref-def
  line) so reference links have unambiguous keyboard semantics.
  Surface goto-definition through xref so `M-.` works without a
  new global binding.
- Surface the resolved URL through eldoc when point lands on a
  decorated link, without overriding the user's eldoc strategy.
- Defer all icon resolution to `nerd-icons` so the icon database
  is a single shared resource (already used by `nerd-icons-dired`,
  per [modules/dired/init.el#L62](/Users/chris/.config/emacs/modules/dired/init.el#L62)).

**Non-Goals:**

- Decorate image links `![alt](url)`. The filename/path is part of
  what the reader wants to see. Out of scope.
- Decorate footnote markers `[^1]`. Different semantic; existing
  markdown-mode behaviour suffices.
- Maintain a local URL → glyph alist. `nerd-icons-url-alist` is
  the user-extensible source of truth.
- Replace markdown-mode's link-following machinery. We reuse
  `markdown--browse-url`, `markdown-link-url`,
  `markdown-reference-definition`, and friends; we only add the
  spatial-scope keymap on overlays and an xref backend.
- Add a new axis. This change folds into the existing
  `lang-markdown` axis per the one-spec-per-axis rule.

## Decisions

### Decision: Overlay-based decoration, per-window, in a new `+gfm-links.el`

We use overlays — not font-lock text properties — for both the
title-side and URL-side decoration, mirroring the other four GFM
decorators. Overlays survive font-lock refresh, support per-window
display via the `window` property, and integrate naturally with the
post-command-hook reveal pattern already shared across this module.

A new file `modules/lang-markdown/lib/+gfm-links.el` owns the mode
and its overlay lifecycle. It does not extend the existing files;
link decoration is a peer to the others, not a sub-feature.

**Alternatives considered:**

- *Font-lock text properties via a new keyword*. Cheapest path; the
  table cell width walker would pick up `display` / `composition`
  props automatically. Rejected because cursor-reveal on text
  properties is awkward (requires toggling `invisible`), and the
  decorator family is overlay-based — diverging here would create
  two patterns for future maintainers to learn.
- *Hybrid: text props for URL side, overlays for title side*.
  Rejected as the worst of both — two reveal mechanisms, two
  rebuild paths, two test surfaces.

### Decision: Extend the table cell width walker to consult overlays

`gfm-tables--visible-width--compute`
([+gfm-tables.el#L152](/Users/chris/.config/emacs/modules/lang-markdown/lib/+gfm-tables.el#L152))
gains optional `(buffer beg end)` parameters. When provided, the
walker also examines overlays in BUFFER intersecting `[beg, end)`
and honours their `display`, `invisible`, and `composition` props
the same way it honours text properties today. Existing call sites
pass nil for the new args and behave identically.

`gfm-tables--fontify-cell` is restructured so width is measured on
the source-buffer cell *before* the cell text is copied to the
scratch buffer for fontification. The scratch buffer never sees
the source overlays, but it doesn't need to — width is a
source-buffer property of the cell region.

**Alternatives considered:**

- *Convert link overlays to text properties in the scratch buffer
  before fontify*. Rejected — fragile, runs once per cell rebuild,
  and degrades to "use text properties everywhere" in practice.
- *Diverge: keep the walker text-property-only, and forbid link
  decoration inside table cells*. Rejected — links inside tables
  are common in real markdown.

### Decision: Suppress the built-in compose path via around-advice

markdown-mode's `markdown-fontify-inline-links` and
`markdown-fontify-reference-links` call `compose-region` on the URL
chars when `markdown-hide-urls` is non-nil. To prevent double
rendering we install around-advice on both functions, gated by the
buffer-local `gfm-links-mode`:

```
(define-advice markdown-fontify-inline-links
    (:around (orig &rest args) gfm-links-suppress-compose)
  (if (bound-and-true-p gfm-links-mode)
      (let ((markdown-hide-urls nil)) (apply orig args))
    (apply orig args)))
```

The body of each fontifier still runs — faces apply, properties
propagate — only the `(when (and markdown-hide-urls url-start)
(compose-region …))` branch
([markdown-mode.el#L8323](/Users/chris/.config/emacs/elpaca/sources/markdown-mode/markdown-mode.el#L8323))
is skipped. Advice is installed at module load, gated per-buffer by
the mode variable, so there is no install / uninstall race window.

`markdown-hide-urls` and `gfm-links-mode` are independent
variables. Toggling `markdown-hide-urls` while `gfm-links-mode` is
on is a no-op to the user — our overlays own the appearance.

**Alternatives considered:**

- *Strip `composition` text props after font-lock runs* (via
  `font-lock-extend-region-functions` or jit-lock-after-change).
  Rejected — runs after the fact, fights font-lock refresh, and
  leaves a visible flash.
- *Bind `gfm-links-mode` to `markdown-hide-urls` so they toggle
  together*. Rejected — surprising semantics for users who expect
  the var to retain meaning independently.

### Decision: Icon resolution defers entirely to `nerd-icons`

The resolution branch is shallow:

```
URL  starts with http:// or https://        → nerd-icons-icon-for-url URL
URL  is a relative path or starts with file: → nerd-icons-icon-for-file (basename URL)
URL  is a same-doc anchor (`#…`)             → nerd-icons-icon-for-url URL  (globe fallback)
URL  any other scheme                        → nerd-icons-icon-for-url URL  (globe fallback)
```

No local scheme→glyph alist. `nerd-icons-url-alist` is the single
source of truth and is user-extensible. Reference links resolve
their definition URL first, then take the same branch.

We do not pass `:height` overrides — nerd-icons glyphs render at
character width and any height override would risk fractional
column widths in tables.

**Alternatives considered:**

- *Maintain a small project-local alist for schemes nerd-icons
  doesn't cover (mailto, tel, anchors)*. Rejected — divergent
  customisation surface. `nerd-icons-url-alist` extension is the
  blessed path.

### Decision: Spatially-scoped RET via overlay keymap; xref for goto-def

RET globally invokes `markdown-enter-key` for smart list-item
continuation. We do not want to hijack RET everywhere. Instead,
each title-side overlay carries a `keymap` property whose only
binding is `RET` → `gfm-links-follow-link-at-point`. The keymap
applies only when point is inside the overlay region; everywhere
else, the global RET binding wins.

Reference goto-definition is registered through
`xref-backend-functions`. A backend looks for ref-link match data
at point and returns a single `xref-item` pointing at the
`[label]: url` definition line. `M-.` works without a new binding;
`M-,` returns. No bespoke `gfm-links-goto-definition` command is
needed (we may add an alias for discoverability, but xref is the
load-bearing entry point).

### Decision: Eldoc via `eldoc-documentation-functions`; strategy untouched

We register `gfm-links--eldoc-function` on the buffer-local
`eldoc-documentation-functions`. It returns the resolved URL (and
the title attribute when present, e.g. `[title](url "tooltip")`)
when point is on a decorated link, and nil otherwise.

We deliberately do not set `eldoc-documentation-strategy` from the
mode. If the user has chosen `compose` globally, our function
participates in composition. If they prefer `default`, the first
non-nil contributor wins. Buffer-locally overriding the strategy
would clobber a user's deliberate choice; documentation in spec
points users to `compose` if they want guaranteed URL surfacing.

### Decision: Reference-def alist with debounced invalidation

Reference resolution requires a buffer-wide scan for
`^ {0,3}\[label\]: url` definitions. We cache this as a
buffer-local alist `gfm-links--ref-def-alist`, populated on mode
enable and on rebuild. Invalidation: any edit triggers a debounced
rebuild (same timer pattern as `gfm-callouts`); the alist is
recomputed at the start of each rebuild. The first definition for
a duplicate label wins (matching markdown-mode's behaviour in
`markdown-reference-definition`).

Broken references (label with no def) are not decorated — the raw
`[title][label]` shows through with `markdown-missing-link-face`
already applied by markdown-mode.

### Decision: Reveal the whole link region on cursor-on

The reveal hook (post-command-hook) walks the overlays at point
in the selected window and toggles their `display` properties to
nil for both the title-side and url-side overlays of the same
link. The two overlays are linked by a shared `gfm-links-id`
property so we can find the partner. Restoring on cursor-off uses
the standard pattern from `gfm-callouts--reveal`
([+gfm-callouts.el#L468](/Users/chris/.config/emacs/modules/lang-markdown/lib/+gfm-callouts.el#L468)).

### Decision: Mode follows `markdown-hide-urls`, but they remain independent

`gfm-links-mode` is enabled in `markdown-mode-hook` when
`markdown-hide-urls` is non-nil at mode-init time. A buffer-local
`add-variable-watcher` on `markdown-hide-urls` toggles the mode on
subsequent variable changes. The mode and the var stay
independent — code that calls `(setq markdown-hide-urls nil)` will
disable the mode, but the suppression advice continues to be
installed (gated by the mode var). No interference, no leftover
overlays.

## Risks / Trade-offs

- **[Width walker complexity grows]** → Mitigation: extension is
  additive and gated on the new optional args; existing callers
  unaffected. Add a focused test case for "link overlay inside
  table cell" and one for "wide title overflows column cap".
- **[Around-advice on third-party functions is fragile]** →
  Mitigation: advice is gated per-buffer by the mode var, so it is
  inert in buffers where we are not active. The advised functions
  are stable in markdown-mode (have not changed signature since
  v2.3, current is 2.6+). Pin a regression test that confirms the
  advice keeps face application intact.
- **[`nerd-icons-url-alist` matches are regex-based and unanchored
  for some entries]** → Mitigation: rely on nerd-icons' behaviour
  unchanged; document the dependency. If unhelpful matches surface,
  fix upstream rather than re-implement locally.
- **[xref backend collisions with other xref backends in markdown
  buffers]** → Mitigation: our backend only claims a match when
  point is on a reference-link region; otherwise returns nil and
  the next backend runs.
- **[Reveal flicker during fast cursor motion across multiple
  links]** → Mitigation: the same post-command-hook pattern works
  for callouts and code fences; reuse the debouncing strategy and
  selected-window guard.
- **[Eldoc composition surprise]** → If the user is on
  `eldoc-documentation-default`, a coincident diagnostic at the
  same point hides the URL. Documented in spec; user opts into
  `compose` for guaranteed surfacing.
- **[Wiki links require `markdown-enable-wiki-links` and have a
  different filename-resolution path]** → Mitigation: gate the
  wiki-link discovery path on the variable; resolve targets via
  `markdown-convert-wiki-link-to-filename` for consistent
  icon-for-file output.
