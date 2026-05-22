## Context

The preview code in `gfm-present.el` is structurally a single-pass
decorator: scan buffer for matching links, build display strings,
apply overlays.  It's been called from three present-mode-specific
sites:

- `gfm-present-mode` enable (line ~792): one-shot render of the
  whole buffer at narrow-in time.
- Slide-change hook (line ~733): re-render after a narrow swap.
- Anchor-jump callback (line ~721): re-render after a pretty-
  links RET jump.

Plus internal callers from edit hooks (lines 122, 137, 262, 912)
that re-render on buffer-modification events.

The `gfm-pretty` engine (`lisp/gfm/gfm-pretty-engine.el`) already
provides the per-buffer decorator lifecycle the previews need:

- `gfm-pretty-define-decorator` registers a decorator with
  `:collect-fn`, `:apply-block-fn`, `:on-enable-fn`,
  `:on-disable-fn`, `:full-rebuild-required-p`.
- `:apply-block-fn` is per-block per-window, with the engine
  handling window-configuration-change and after-change hooks.
- `:full-rebuild-required-p` lets the decorator escalate
  buffer-local edits to a full re-scan when needed.
- Overlay registry primitives
  (`gfm-pretty--make-anchor`, `--make-display`, `--register`,
  `--remove-overlays`) handle bulk teardown on disable.

So the work is mostly **moving** code, not designing new
mechanics.

## Goals / Non-Goals

**Goals:**

- One source of truth for link preview rendering — the new
  decorator module.  No duplicate rendering paths in
  `gfm-present.el`.
- `gfm-pretty-mode` in any `gfm-mode` buffer renders previews
  the same as a present-mode slide does today.
- Standalone-link gating, box display, sentinel rendering,
  truncation cap, project-relative path abbreviation, diff-
  command argv shape — all preserved verbatim from the archived
  `box-source-preview-overlays` change.
- Present mode's slide-change / anchor-jump refresh keeps
  working — but it calls into the decorator's rebuild entry
  point, not a private function.

**Non-Goals:**

- Designing new preview features (the user wants the existing
  feature in more places, not a different feature).
- Tuning rebuild performance.  The decorator inherits whatever
  the engine does today; if perf is an issue under
  `gfm-pretty-mode` we'll address it separately.
- Reworking the box-drawing helper (`--box-display`).
- Touching the diff `git diff` invocation shape.

## Decisions

### D1. New file `lisp/gfm/gfm-pretty-link-previews.el`

Lifts the preview helpers (~13 symbols, ~350 lines) into a
dedicated decorator module sibling to
`gfm-pretty-callouts.el`, `gfm-pretty-blockquotes.el`,
`gfm-pretty-tables.el`, etc.  The symbol prefix changes from
`gfm-present--` to `gfm-pretty-link-previews--`; obsolete
aliases (`define-obsolete-function-alias`) keep the old names
callable from out-of-tree config snippets the user might have.

**Alternative considered:** stash in `gfm-pretty-links.el`
(which already handles markdown link decoration via title-side
display).  Rejected — the existing links module deals with
inline link RENDERING (replacing `[label](url)` with a single
icon glyph); preview rendering is a different concern (replacing
the link with a multi-line code-block / diff body).  Mixing them
makes the links module harder to reason about.

### D2. Decorator registration via `gfm-pretty-define-decorator`

```elisp
(gfm-pretty-define-decorator 'link-previews
  :registry           gfm-pretty-link-previews--registry
  :collect-fn         #'gfm-pretty-link-previews--collect
  :range-fn           #'gfm-pretty-link-previews--block-range
  :apply-block-fn     #'gfm-pretty-link-previews--apply-block
  :full-rebuild-required-p
                      #'gfm-pretty-link-previews--full-rebuild-required-p
  :on-enable-fn       #'gfm-pretty-link-previews--on-enable
  :on-disable-fn      #'gfm-pretty-link-previews--on-disable)
```

The `:collect-fn` scans for standalone source-range / diff-range
links and returns one block per matched link.  `:apply-block-fn`
builds the preview display string and creates a single display
overlay covering the link span.  The decorator does NOT use the
anchor / display split — preview overlays are width-dependent
(box-width clamps on window width) so they're per-window display
overlays only.

**Alternative considered:** custom mode separate from
`gfm-pretty-define-decorator`.  Rejected — duplicates lifecycle
machinery the engine already provides.

### D3. Full-rebuild predicate

```elisp
(defun gfm-pretty-link-previews--full-rebuild-required-p (dirty)
  "Non-nil iff DIRTY overlaps any source/diff link or could create one."
  (or (cl-some (lambda (r)
                 (gfm-pretty--region-overlaps-p dirty r))
               (gfm-pretty-link-previews--link-line-ranges))
      ;; Any edit could turn a non-link line into a link.  Cheaper to
      ;; just always full-rebuild than to grep for `[' / `]'.
      t))
```

The decorator chooses to always-full-rebuild because the link
detection regex spans buffer positions that a single edit can
re-shape arbitrarily (`[fn](path#L1-L2)` can be assembled char-
by-char).  Always-rebuild is simpler and the cost is bounded by
the buffer's link count (typically <20 on a slide).

**Alternative considered:** track each link's source range and
only re-render the dirty ones.  Rejected — bookkeeping complexity
for marginal gain.

### D4. Present mode integration

`gfm-present-mode`'s slide-change hook
(`gfm-present--narrow-to-current-slide` and post-rebuild hooks)
calls `(gfm-pretty--rebuild (gfm-pretty--get 'link-previews))`
instead of the now-removed `gfm-present--render-link-previews`.

`gfm-present-mode` enable: turn on `gfm-pretty-mode` if not
already on (it's already required by other present features), and
ensure the `link-previews` decorator is enabled
(`gfm-pretty-toggle-decorator 'link-previews t` if a user has
disabled it).

`gfm-present-mode` disable: do NOT explicitly tear down preview
overlays.  If `gfm-pretty-mode` stays on, previews stay; if it
goes off too, the engine teardown removes them.  This matches the
behaviour of the other decorators on present-mode disable.

**Alternative considered:** keep present mode's slide-change
narrow re-render as a special-case fast-path.  Rejected — the
engine's after-change handler already debounces; running it
through the same path keeps the codebase simpler.

### D5. Obsolete aliases for the moved symbols

```elisp
(define-obsolete-function-alias 'gfm-present--render-link-previews
  'gfm-pretty-link-previews--rebuild "29.1")
(define-obsolete-function-alias 'gfm-present--source-preview-display
  'gfm-pretty-link-previews--source-display "29.1")
…
```

All previously-public-from-present-mode symbols get an alias.
Internal-only helpers (the ones whose name never escaped
`gfm-present.el`) don't need an alias — but adding them is cheap
and pre-empts future re-discovery.

**Alternative considered:** no aliases, rename cleanly.  Rejected
— the user has dotfiles that may bind keys to these functions.

## Risks / Trade-offs

- **[Always-full-rebuild on edits in long buffers]** → a 5000-
  line markdown buffer with `gfm-pretty-mode` on would re-scan
  for links on every after-change.  Mitigation: the engine
  debounces via idle-timer (`gfm-pretty--rebuild-timer`);
  measured cost is the regex scan + per-link git/file calls,
  bounded by link count.  If perf is an issue, drop to a
  position-tracking approach in a follow-up.

- **[Diff previews run `git` on every rebuild]** → no caching
  in the current implementation.  In a markdown buffer with
  many diff links the rebuild stalls behind subprocess calls.
  Mitigation: stays the same as today (the present-mode code
  has the same issue).  If the issue surfaces, add a
  content-hash-keyed cache in a follow-up.  Out of scope here.

- **[Project resolution for source links]** → the helper resolves
  paths relative to the buffer's `default-directory`.  Under
  presentation mode the buffer is the presentation file (e.g. in
  a `presentations/` dir), so source paths resolve against THAT
  directory.  Under plain `gfm-pretty-mode` the buffer is the
  authored markdown; same resolution rule.  No change needed.

- **[Preview overlay overlap with other decorators]** → preview
  overlays cover whole `[label](url)` ranges.  If the `links`
  decorator emits an icon over the same range, the two
  decorations stack.  Existing behaviour today (present mode
  composes with the links decorator); no change.

## Open Questions

- Should `gfm-pretty-mode` enable `link-previews` by default, or
  require an opt-in?  Recommend default-on — matches the other
  decorators in the engine.  Users who don't want previews
  toggle off via `gfm-pretty-toggle-decorator`.
