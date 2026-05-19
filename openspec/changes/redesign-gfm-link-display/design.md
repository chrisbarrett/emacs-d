## Context

`gfm-pretty-links` decorates Markdown / GFM links with two per-window
overlays per link: a title-side overlay rendering `[title]` as the
label text in `markdown-link-face`, and a url-side overlay rendering
the URL span as a single nerd-icons glyph. Both overlays carry a
shared `gfm-pretty-links-id`, and a `:reveal-fn` registered with the
engine hides BOTH on cursor entry so the raw `[title](url)` source
shows.

Three structural problems with that model:

1. **Reveal causes reflow.** As point moves through a paragraph the
   overlays for the link under point pop in and out — line breaks
   shift and the eye loses position. The reveal was modelled on
   org-mode's `org-link-descriptive`, but org doesn't reveal on point
   motion alone (only on edit), so the analogy doesn't carry.

2. **URL inspection requires reveal.** The eldoc handler at
   `lisp/gfm/gfm-pretty-links.el:598-608` returns only the resolved
   URL string. To see the full markup (title + URL together, with
   their scaffolding) the user must move point onto the link and read
   the now-revealed source. Eldoc has the link record on the overlay
   already — it just doesn't surface it.

3. **One face fits all targets.** `gfm-pretty-links-title-face`
   defaults to `markdown-link-face` and is applied uniformly at
   `lisp/gfm/gfm-pretty-links.el:404-409`. `markdown-link-face`
   inherits `link` which carries `:underline t`. The underline is a
   meaningful affordance for "this leaves the buffer / opens a
   browser" — but it currently appears on `#heading` and `./path`
   targets that don't leave the buffer at all. `RET` reinforces the
   confusion: `gfm-pretty-links-follow-link-at-point` always calls
   `markdown--browse-url`, which happens to handle `#anchor` by
   coincidence (markdown-mode wires that case) and outright fails on
   `./path` (browser handler treats it as a URL).

The decorator is the only consumer of the engine's `:reveal-fn`
hook (see `openspec/specs/gfm-pretty/spec.md:82`). Removing it does
not break engine contract; the engine's default reveal path keys off
the `<tag>-revealable` overlay property, and we simply stop setting
that property.

## Goals / Non-Goals

**Goals:**

- Decorated links stay decorated regardless of cursor position. No
  reflow on motion.
- Echo-area surface (eldoc) shows the literal `[title](url)` source
  with shadow on scaffolding and distinct faces on title vs URL —
  enough to read the link without disturbing rendering.
- Three target classes — **web**, **anchor**, **file** — each with
  distinct affordance: face, optional icon, RET behaviour.
- Reference-style links classify via their resolved URL, not their
  source shape. `[ops][tg-auth-sh]` with `[tg-auth-sh]: ./scripts/x`
  classifies as **file**.

**Non-Goals:**

- Restoring underline on local link faces via theme. The faces ship
  with `:underline nil`; theming can override but that's user choice.
- Editing affordance for the raw source. The user can disable
  `markdown-hide-urls` (or toggle the decorator) to edit; we don't
  provide an in-place edit mode.
- Image links, footnote markers (`[^id]`), wiki links — out of scope
  for this change. They continue to render as today.
- Honouring `markdown-link-make-anchor-fn` or generated heading slugs
  beyond what `markdown-mode` already exposes. We delegate anchor
  resolution to markdown-mode where one exists; otherwise the URL
  classifier just routes the prefix and RET calls into markdown-mode.

## Decisions

### Decision: URL classifier

URL strings classify into `web`, `anchor`, or `file` by prefix on
the *resolved* URL (after reference-def lookup, after autolink /
bare-URL extraction):

```
URL prefix matched by `rx`             →  class
─────────────────────────────────────  ─────────
bos "#" (+ any)                        →  anchor
bos (or "./" "../" "/")                →  file
bos (or "http://" "https://" …) / scheme   →  web   (default)
```

`file:` URLs map to **file** (extract the path). Wiki links route via
markdown-mode's existing handler — they aren't reclassified.

Alternative considered: classify by *source shape* (inline vs
reference). Rejected: the user's reference-style "footnotes" in
infrastructure-live mix web and file targets in one block; the source
shape says nothing about the affordance.

### Decision: Two new faces, both inheriting `markdown-link-face`

```elisp
(defface gfm-pretty-links-anchor-face
  '((t :inherit markdown-link-face :underline nil))
  "Title-side face for links whose target is a buffer-internal anchor.")

(defface gfm-pretty-links-file-face
  '((t :inherit markdown-link-face :underline nil))
  "Title-side face for links whose target is a relative or absolute path.")
```

Both default identical; themed independently if the user wants
divergent colour. Single shared face was an option — rejected per
user direction (room to tune separately later without breaking
muscle memory if one diverges).

### Decision: Url-side overlay suppressed for `anchor` / `file`

`gfm-pretty-links--decorate-link` constructs the title overlay
unconditionally, and the url overlay only when the url span is
non-empty (`lisp/gfm/gfm-pretty-links.el:443-444`). Add a class
guard: skip the url-side overlay when class ∈ `{anchor, file}`. The
title-side face is selected from the class.

Alternative considered: keep the icon, use anchor / file glyphs from
nerd-icons. Rejected per user direction — title-only sharpens the
local/external distinction visually and removes the visual noise of
icons next to short anchor labels.

### Decision: Remove reveal entirely

- Drop the `gfm-pretty-links-revealable` overlay property from both
  overlays at construction time.
- Unregister the `:reveal-fn` from the decorator definition.
- Delete `gfm-pretty-links--reveal`, `gfm-pretty-links--link-id-at`,
  `gfm-pretty-links--restore-overlay`, the shared
  `gfm-pretty-links-id` plumbing, and the engine state slot
  `hidden-ovs` reads of it.
- Keep `gfm-pretty-links-id` for *partner-overlay lookup* used by
  eldoc and RET? No — eldoc and RET each read the single overlay
  under point; they don't need to find the partner. `id` goes too.

Alternative considered: keep `id` and `revealable` as inert metadata
"in case we want reveal back later." Rejected — dead infrastructure
rots. The decorator-id is recoverable from git if reveal returns.

### Decision: Eldoc formatter returns propertised source string

Format used:

```
[Anthropic](https://anthropic.com)
```

with face properties:

| Span           | Face                   |
| :------------- | :--------------------- |
| `[`, `]`       | `shadow`               |
| title          | per-class title face   |
| `(`, `)`       | `shadow`               |
| URL            | `markdown-url-face`    |

Reference-shape links render as:

```
[name][label]    (eldoc)
```

with shadow on `[ ] [ ]` and the per-class face on `name` (and on
`label` as well, since the def is what classifies). Shortcut
references render as `[label]` with the same scheme.

Inline title attributes append ` — "title"` after the URL, italic.

The eldoc function pulls the URL, class, title-attr, ref-label, and
kind from the overlay properties already attached at construction
time — no re-parsing.

Alternative considered: `post-command-hook` `message`. Rejected per
user direction — fights with other minibuffer users (which-key,
eldoc itself, minibuffer prompts) and there's no clean way to
distinguish "I should clear" from "another consumer is using the
echo area now."

### Decision: RET dispatch by class

`gfm-pretty-links-follow-link-at-point` becomes a class switch:

```elisp
(pcase class
  ('anchor (markdown-do-link-jump))      ; or direct heading lookup
  ('file   (find-file (expand-file-name url
                       (file-name-directory (or buffer-file-name
                                                default-directory)))))
  (_       (markdown--browse-url url)))
```

`markdown-do-link-jump` reads the link at point in markdown-mode's
own terms. That fails when our overlay covers the source. Two
options:

1. **Call markdown-mode's lower-level helpers directly** —
   `markdown-jump-to-heading-text` for anchors using the captured
   anchor string.
2. **Temporarily hide our display overlay** at RET time so
   `markdown-do-link-jump` sees raw source, then restore.

Option 1 is cleaner — no overlay juggling, no need to know whether
markdown-mode reads the source. Use `markdown-link-url` /
`markdown-link-link` helpers where possible; for anchor jump, search
for a heading whose generated slug matches the captured anchor (uses
`markdown-heading-at-point` walked from `point-min`, or the existing
`markdown-toc` helpers if loaded). We already have the URL on the
overlay so no source read is needed.

For file paths: expand relative to `buffer-file-name`'s directory
(not `default-directory` — the latter wanders with `cd`). When the
buffer has no file (gfm scratch buffer), fall back to
`default-directory` with a warning via `user-error` if the path
doesn't resolve.

Alternative considered: route everything through
`markdown-follow-thing-at-point` after revealing the overlay around
point. Rejected — adds a moving part (reveal + restore) and depends
on markdown-mode's source-parsing reading correctly through our
overlay's `display` property.

### Decision: Reveal-removal does not affect other decorators

Callouts, fences, tables, hrule each register their own revealable
overlays. The engine's default reveal walker continues to find and
act on those. The change is strictly local to the `links` decorator.
Verified at `openspec/specs/gfm-pretty/spec.md:421-432`.

## Risks / Trade-offs

- **[Risk]** Eldoc display is throttled and only shows when point is
  on a decorated link. Users moving fast across links may not see the
  URL update in time. **→ Mitigation:** the URL is also available via
  `M-.` (xref) for reference-style links and via toggling the
  decorator. Tolerable.

- **[Risk]** Stripping reveal removes the only path to see / edit the
  raw URL without toggling. **→ Mitigation:** users who need to edit
  toggle `markdown-hide-urls` (already bound under the local leader
  at `modules/lang-markdown/init.el:201`).

- **[Trade-off]** File-path resolution relative to `buffer-file-name`
  fails for buffers without a backing file (e.g. `M-x gfm-mode` in
  `*scratch*-like` buffers). RET on a path link in those buffers
  raises `user-error`. Acceptable — the path is meaningless without a
  base.

- **[Trade-off]** Anchor resolution walks headings from `point-min`
  on each RET press. Linear in heading count; for documents with
  thousands of headings this is measurable but not interactive-slow.
  No caching layer.

- **[Risk]** Two new defcustoms-style faces add surface area for
  themes. **→ Mitigation:** both default to inheriting
  `markdown-link-face` with one override (`:underline nil`); themes
  that style `markdown-link-face` get sensible defaults for free.

## Migration Plan

The reveal removal is **BREAKING** in the technical sense — overlay
properties go away, the `:reveal-fn` deregisters — but no external
consumer reads those properties. Tests under
`lisp/gfm/gfm-pretty-tests.el` covering `Whole-link cursor reveal`
need updating in lockstep with the spec delta.

No deploy / rollback procedure — this is a personal Emacs
configuration. Rollback = `git revert`.

## Open Questions

- Anchor RET on a heading slug that doesn't exist: silent `user-error`
  or fall back to `markdown--browse-url url` (which will open the
  current file in a browser if there's a `file:` URL it generates)?
  Leaning toward `user-error` — silent failure is worse than a
  message.

- File RET when the target doesn't exist on disk: `find-file` will
  create the buffer pointing at a non-existent file. That's
  consistent with markdown-mode's `markdown-follow-thing-at-point`
  behaviour for the same case. Keep that.
