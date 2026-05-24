## Context

The `link-previews` decorator (lisp/gfm/gfm-pretty-link-previews.el)
landed as a clean extraction from `gfm-present.el`.  Reviewing it in
a real markdown buffer surfaced three rendering gaps and one
behaviour gap:

1. The display string the overlay carries always starts every body
   line at column 0.  When the overlay's link span starts after a
   list-item or blockquote marker (`- `, `> `, …), the box top is
   pushed right but the body lines and bottom border are not — the
   box looks like a step-shape.
2. The `gfm-pretty-blockquotes` decorator paints a `▌` rail on every
   line whose first character is `>`.  Its display overlays cover
   the two-char `> ` prefix.  The `link-previews` overlay covers
   the `[label](url)` span — a different, non-overlapping range on
   the same line.  Both decorators independently render their
   prefix, so the user sees `▌` followed by the preview box's
   `┌`.  Two left borders.
3. Diff bodies arrive as plain text.  Source bodies pass through
   `--fontify-source` (a temp-buffer + `font-lock-ensure` shape),
   so `+`/`-`/context lines need the same treatment with
   `diff-mode`.
4. Following a preview link out-of-band: inside
   `gfm-present-mode`, `gfm-present-follow-link` (bound to RET in
   `gfm-present-mode-map`) routes `path#L…` to `find-file` and
   `diff:…` to `magit-diff-range`.  In a plain
   `gfm-pretty-mode` buffer, RET falls through to `markdown-mode`,
   which tries to follow `diff:base...head[#path]` as a relative
   file path — never resolves.

The decorator already owns per-overlay overlays under the engine.
Each polish item is local to either
`lisp/gfm/gfm-pretty-link-previews.el` or
`lisp/gfm/gfm-pretty-blockquotes.el`.  No engine-protocol changes
needed.

## Goals / Non-Goals

**Goals:**

- Box top, body, and bottom border render at the same column
  inside list-item / blockquote preview lines.  Marker glyph
  remains visible to the left.
- Plain blockquote lines outside a preview keep their rail
  unchanged (no regression for the existing requirement).
- Diff body lines visibly distinguish added / removed / context
  via `diff-mode` faces, even when no theme customisation is
  loaded.
- RET on any preview overlay opens the underlying source range or
  diff range — same dispatch wherever previews render.

**Non-Goals:**

- Reskinning the box-border face palette.
- Inline-link previews (the gating rule does not change).
- Inline editing of the diff body, scroll-through-cap (still 10
  lines), or git-side caching.
- Replacing `markdown-mode`'s RET binding outside preview spans.

## Decisions

### D1. Marker-aware indent baked into the display string

`gfm-pretty-link-previews--apply-block` knows the link span's
`(link-start . link-end)`.  Compute
`indent = (link-start - (line-beginning-position))` and thread it
into `--box-display` as `:indent <n>`.  The renderer prepends
`(make-string indent ?\s)` to the top border, every body line, and
the bottom border before joining with `\n`.

This is preferable to using a `wrap-prefix` text property: the
display string already contains literal newlines, so each "line"
inside it starts a new visual row from column 0 unless we put the
indent inside the string.  `wrap-prefix` only fires on soft wraps,
not on literal newlines in a `display` value.

**Alternative considered:** make the overlay span BOL → EOL,
swallowing the marker.  Rejected — the marker is informative
(`-` vs `>` tells the reader the link's structural context) and
hiding it loses information.  The marker-and-indent approach
keeps the marker visible while aligning the box.

### D2. Blockquote rail suppression by overlap

In `gfm-pretty-blockquotes--apply-block-anchors` and
`--apply-block-display`, before laying a rail overlay on a given
line, check whether that line is fully covered by a
`gfm-pretty-link-previews` display overlay.  When it is, skip
the rail (anchor + display) for that line only.

Implementation: walk `(overlays-in lbeg lend)`; if any overlay
carries the `gfm-pretty-link-previews-display` tag (the registry
emits this property), skip.  Property name is the engine-derived
`(gfm-pretty--registry-display gfm-pretty-link-previews--registry)`
— stable, no string-matching.

This keeps the responsibility in blockquotes (the decorator that
owns rail painting decides when to defer), and avoids cross-
module ordering assumptions.

**Alternative considered:** extend the preview overlay's range to
cover the `> ` prefix as well.  Rejected for the same reason as
D1's "swallow the marker" — and additionally because two
decorators painting overlapping `display` properties yields
order-of-application-dependent rendering that's harder to reason
about.

### D3. Diff-mode fontification for the diff body

Mirror `--fontify-source`'s shape: temp buffer, activate
`diff-mode`, insert body lines joined by `\n`, run
`font-lock-ensure`, `buffer-substring` to capture `face` text
properties.  No `buffer-file-name` trick needed (`diff-mode`
doesn't tree-sit-route on file name).

The body is what `git diff` produced (already includes `diff --git`
header, hunk header, `+`/`-`/context lines) — `diff-mode`
fontifies all of those out of the box.

**Alternative considered:** hand-roll a `(when (eq (aref line 0)
?+) ...)` colourer.  Rejected — replicates `diff-mode`'s logic
poorly and skips hunk-header / file-header colouring.

### D4. RET dispatch via overlay `keymap`

Each preview overlay carries `(keymap . gfm-pretty-link-previews-overlay-map)`
where the keymap binds `RET` (and `<return>` for GUI Emacs) to
`gfm-pretty-link-previews-follow-link-at-point`.  The command:

- Find the overlay at point under the
  `gfm-pretty-link-previews-display` property.
- Read the overlay's `gfm-pretty-link-previews-kind` and
  the underlying URL (re-parse via the existing
  `--parse-source-link` / `--parse-diff-link`).
- Source → `find-file` the resolved path, `goto-line` the start,
  pulse the range with `pulsar-highlight-pulse` if available.
  This mirrors `gfm-present--follow-source-link` (lisp/gfm/
  gfm-present.el:210-223).
- Diff → require `magit` lazily; when `magit-diff-range` is
  `fboundp` call it with `<base>...<head>` and optional `(list
  path)`.  Otherwise fall back to `vc-diff-internal` or a plain
  `*Diff*` buffer via `(let ((default-directory worktree))
  (call-process "git" nil "*Diff*" nil "diff" "B...H" "--" P))`.

Reuse: the existing `gfm-present--follow-source-link` and
`gfm-present--follow-diff-link` are small enough to lift into the
decorator under the new prefix.  Present-mode keeps its RET
binding (`gfm-present-follow-link`) which delegates to the same
implementations — no behaviour change inside a slide.

**Alternative considered:** push `(define-key markdown-mode-map
[remap markdown-follow-link-at-point] …)` globally.  Rejected —
that affects every markdown buffer regardless of `gfm-pretty-mode`
state and over-reaches the decorator's scope.  Overlay `keymap`
fires only when point is inside a preview overlay.

### D5. Where the follow code lives

Move `--follow-source-link` and `--follow-diff-link` from
`gfm-present.el` into `gfm-pretty-link-previews.el` under the new
prefix.  `gfm-present-follow-link` keeps dispatching through the
new entry points (in-tree consumer; no obsolete aliases needed,
matching the precedent set in the prior change).

## Risks / Trade-offs

- **[Marker indent breaks under proportional fonts]** → the
  `(- link-start BOL)` count is character cells, accurate only
  under monospace.  Mitigation: every gfm-pretty decorator
  already assumes monospace; nothing to do.
- **[Blockquote-rail suppression races with rebuild ordering]** →
  if blockquotes rebuilds before link-previews, the rail is laid
  first, then link-previews overlays cover the link span without
  the blockquotes decorator getting a chance to skip.  Mitigation:
  the engine rebuilds in decorator registration order;
  blockquotes is registered first, so on the first apply it WILL
  draw the rail.  Add a no-op
  `:full-rebuild-required-p`-style refresh: when link-previews
  applies a new overlay covering a `> ` line, schedule a
  blockquotes rebuild for that line range.  Cheaper alternative:
  blockquotes' `overlays-in` check inspects the
  `gfm-pretty-link-previews--registry` overlays directly each
  apply call — since both decorators rebuild on engine ticks, by
  the time blockquotes lays new anchors the link-previews
  overlays are already in place.  Validate via the regression
  test.
- **[Diff-mode load cost]** → `(require 'diff-mode)` adds a few
  milliseconds the first time a diff preview renders.  Cached
  after that.  Acceptable.
- **[`magit-diff-range` absent on plain Emacs]** → the fallback
  spawns `git diff … | view-buffer`.  Less rich but functional.
  No new hard dependency.

## Migration Plan

No data migration; pure decorator behaviour change.  Existing
preview overlays simply re-render after the rebuild fires.
Anyone with `gfm-pretty-mode` on benefits automatically.

## Open Questions

- Should the suppression in D2 extend to the `callouts` decorator
  too?  Callouts treat a `> [!NOTE]` header line specially; a
  callout marker line containing a standalone preview link is an
  unusual but possible case.  Recommend: defer.  If a user hits
  it we add a regression test then.
