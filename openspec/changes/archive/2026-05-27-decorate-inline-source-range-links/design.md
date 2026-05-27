## Context

The `gfm-pretty-links` decorator and the `gfm-pretty-link-previews`
decorator both inspect bracketed markdown links. Pretty-links
prettifies the title and hides the URL span; link-previews replaces
the whole `[label](url)` span with a multi-line preview box for
source-range and diff URLs. To avoid stacked overlays on the same
span, pretty-links has refused all source-range/diff URLs since the
"skip source-range and diff URL forms" requirement was added
(`openspec/specs/gfm-pretty/spec.md` L2006-2069).

Two refinements to link-previews narrowed its claim of those URL
forms over time:

1. *Standalone gating* (spec L2446-2509). Link-previews only attaches
   when the URL span occupies a whole line, optionally inside one
   list-item or blockquote marker. Inline source-range links in
   prose are deliberately left alone.
2. *Bracketed-only shape*. Link-previews handles `[label](url)` and
   bare-line URL-only references; it never claims reference-style
   `[label][src]` links (spec L2511-2544).

Pretty-links' blanket skip predicate (`gfm-pretty-links--skip-url-p`,
`lisp/gfm/gfm-pretty-links.el:176-184`) hasn't tracked those
refinements. It excludes URL spans that link-previews will never
decorate, leaving them rendered raw — the case the user hit:

```
- If we hide it, should `RET` on the title still call `find-file`? — already
  wired,
  [/Users/chris/.config/emacs/lisp/gfm/gfm-pretty-links.el#L665-L674](/Users/chris/.config/emacs/lisp/gfm/gfm-pretty-links.el#L665-L674).
```

Both decorators run; neither claims; raw markdown bleeds through.

Two adjacent code-reality items surfaced during exploration:

- `gfm-pretty-links--icon-for-target`
  (`lisp/gfm/gfm-pretty-links.el:244-264`) calls
  `(file-name-nondirectory url)` then `nerd-icons-icon-for-file`.
  For `foo.el#L42` the basename is `foo.el#L42`, whose extension is
  `el#L42` — no nerd-icons match, generic file glyph instead of the
  elisp glyph. Fixing this is cheap and directly visible.
- File-class RET (`gfm-pretty-links` overlay keymap, behaviour spec
  L2287) is plain `find-file`. For source-range URLs the line range
  is right there; ignoring it is user-hostile.

## Goals / Non-Goals

**Goals:**

- Inline source-range links in prose render through `gfm-pretty-
  links` like any other `file` link: prettified title, file-icon
  URL overlay, hidden URL span.
- Reference / shortcut source-range links always render through
  pretty-links (never skipped).
- Single source of truth for "is this span standalone?" — extract
  the predicate into the engine so both decorators share it.
- File-class RET on a `#L<n>` URL jumps to line `<n>`.
- Icon resolver picks the right major-mode glyph for `foo.el#L42`.
- Narrowing-regression invariant preserved.

**Non-Goals:**

- Inline diff URLs (`diff:base...head`) — diff stays unconditionally
  skipped. Diff URLs have no `/` or `#` prefix and would classify as
  `web` under current rules, so RET would `browse-url` on them. Not
  worth widening the design.
- Label aesthetics when the label equals the URL (a long
  `/abs/path/foo.el#L665-L674` rendered as text). Agent-prose
  artefact; out of scope.
- Eldoc / xref behaviour changes. Both already read overlay metadata
  that the new decoration path populates.

## Decisions

### Decision: Tighten `--skip-url-p` instead of stacking overlays

Pretty-links' skip predicate becomes context-sensitive: skip only
when link-previews will actually claim the span. Concretely, skip
iff `(inline kind) AND (source-range URL) AND (standalone line)`.
Diff URLs stay unconditionally skipped.

Alternatives considered:

- **Stack overlays, rely on priority.** Let pretty-links decorate
  everything; have link-previews' overlays override on the standalone
  case. Rejected — the spec at L2023-2027 explicitly documents this
  path as garbling-prone and keymap-shadowing. Re-litigates a
  decision already made.
- **New URL sub-class (`source-range`, `diff-range`).** Decorator
  decides per class. Rejected — heavier spec churn, same behavioural
  result. The skip predicate already exists as the right hook.

### Decision: Extract `gfm-pretty-standalone-span-p` to the engine

Today's standalone check lives in
`gfm-pretty-link-previews--standalone-link-p`
(`lisp/gfm/gfm-pretty-link-previews.el:84-108`). Both decorators
need to call it. Options were:

- Add a forward `require` from `gfm-pretty-links` to
  `gfm-pretty-link-previews`. Awkward — pretty-links predates
  link-previews; the dependency direction is wrong.
- Duplicate the predicate. Drift risk on a load-bearing invariant.
- Extract to the engine. Chosen. "Is this span standalone on its
  line?" is a generic document-layout question; the engine is the
  natural home. Both decorators delegate.

New public helper `gfm-pretty-standalone-span-p (beg end)` returns
non-nil iff the line containing `[beg, end)` with the span removed
matches the standalone shape (whitespace plus at most one of
`- `, `* `, `+ `, `<n>. `, `> `). Existing predicate becomes a
thin wrapper.

### Decision: File-RET parses optional `#L<n>[-L<n>]`

The file-class follow handler reads the overlay's URL, splits on
`#`, calls `find-file` on the path part, then `goto-line` on the
first matching `#L<n>`. Range end is ignored — point goes to the
start line. Mirrors what gfm-present and standalone link-previews
RET do on the same URL shape.

Alternatives:

- Delegate to gfm-present's source-range follower. Cleaner in
  theory; requires pretty-links to know the gfm-present symbol
  exists. Rejected for coupling; the inline jump is ~5 lines.
- Plain `find-file`, ignore the line. Cheap but disappointing.
  Rejected.

### Decision: Icon resolver strips URL fragment before basename

`gfm-pretty-links--icon-for-target` gets a `url` argument cleanse
step for file/relative branches: split on `#`, take the head, then
`file-name-nondirectory`. `nerd-icons-icon-for-file` sees `foo.el`,
returns the elisp glyph. Fragment-stripping only applies to the
file-resolution branches; URL branches (`http:`, `#anchor`, other
schemes) pass through untouched.

### Decision: Reference-style source-range links always decorate

The skip predicate gates on `kind = inline` in addition to the
existing URL-shape and standalone checks. Reference-style
(`[label][src]` with `[src]: /p#L1`) and shortcut links resolve to
a source-range URL but their source shape never reaches
link-previews, so there is no ownership conflict to dodge. Existing
spec scenario *Reference link to source-range URL skipped* (L2052-
L2057) becomes the inverse assertion in the delta spec.

## Risks / Trade-offs

- **Risk:** Inline source-range link whose label happens to be very
  long renders a long prettified title. → Mitigation: out of scope.
  Pretty-links already strips wrapping backticks; further label
  shortening is its own change.
- **Risk:** File-RET's `goto-line` could surprise if a user pastes
  an inline link with a stale line number (file has changed). →
  Mitigation: same risk applies to the existing standalone-preview
  RET path; users tolerate it there.
- **Risk:** The new engine helper widens the public surface of
  `gfm-pretty.el` by one symbol. → Mitigation: the helper is small,
  pure, and documented as generic layout — exactly the kind of
  utility the engine should own.
- **Risk:** Narrowing edge cases when the inline link straddles a
  narrowed region. → Mitigation: covered by the narrowing-regression
  block per `AGENTS.md`; decorate path uses the same overlay
  lifecycle as the existing `file` path.
