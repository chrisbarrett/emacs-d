## Context

`gfm-pretty-links--decorate-link` (lisp/gfm/gfm-pretty-links.el:488-497)
creates two overlays per inline link — title-side and url-side — but
gates the url-side overlay on `(eq class 'web)`. For `anchor` and
`file` classes only the title-side overlay is created, leaving the
`(url)` span literal text visible.

`gfm-pretty-links--make-overlay` (gfm-pretty-links.el:449-474) computes
its `display` string from `side`: title side gets the propertized
label; url side gets a nerd-icons glyph or the empty string when the
glyph lookup returns nil. So the machinery to render an empty
url-side overlay already exists — only the class gate prevents it
firing for anchors.

The decorator's `:around` advice on
`markdown-fontify-inline-links` suppresses markdown-mode's
`markdown-hide-urls` compose-region collapse (gfm-pretty/spec.md:1774).
Without that advice in play, markdown-mode would collapse the URL
span via composition. The advice exists to give the decorator full
ownership of URL-side rendering — so the fix here is the decorator
taking that ownership for anchor links, not delegating back.

## Goals / Non-Goals

**Goals:**

- Anchor links render as label-only: `[The problem](#the-problem)` →
  `The problem`.
- Same overlay infrastructure as web links — no parallel hide
  mechanism (text-property invisibility, font-lock keyword, etc.).
- RET / eldoc / xref behaviour preserved (overlay metadata still
  attached).

**Non-Goals:**

- Changing rendering for `file` class links.
- Hiding the URL span via a separate `invisible` property — using
  `display ""` keeps one code path.
- Reworking the suppression-of-builtin-URL-composition advice.

## Decisions

### Decision: render empty url-side overlay for anchor class

Extend the class gate in `gfm-pretty-links--decorate-link` from
`(eq class 'web)` to `(memq class '(web anchor))`. Adjust
`gfm-pretty-links--make-overlay` so when `side` is `url` and `class`
is `anchor`, the display string is `""` (no icon lookup).

Alternatives considered:

- **`invisible t` overlay + invisibility-spec entry.** Rejected:
  introduces a new spec-symbol the user can toggle accidentally via
  `M-x outline` and similar consumers of `buffer-invisibility-spec`.
  Display-string replacement is the convention already used by every
  other gfm-pretty decorator.
- **Re-enable markdown-mode's `markdown-hide-urls` collapse for
  anchors only.** Rejected: the suppression advice is global to the
  decorator's enable bit; reverse-gating per-class adds a special
  case to an unrelated requirement.

### Decision: keep file class unchanged

The user asked specifically about anchor links. File-class URL spans
(`./scripts/x.sh`, `/path/to/file`) carry information the user often
wants to see at a glance (which file does this point to?). Hiding
them by default would surprise users. Out of scope here.

## Risks / Trade-offs

- **Risk:** Existing tests assert "anchor link omits overlay". They
  now need to assert "anchor link has empty-display overlay".
  → Mitigation: update tests as part of this change. Test inventory
  small (search for `anchor` in the test file).

- **Risk:** A user with `markdown-hide-urls` set to t expected the
  global collapse to handle this and might find the new overlay
  surprising under "disable links decorator".
  → Mitigation: existing requirement
  `Suppression of built-in URL composition` already states the
  collapse is decorator-gated. Disabling the decorator restores
  markdown-mode's behaviour. Unchanged here.

- **Trade-off:** Anchors that the slug-renderer cannot match (e.g.
  typo `[label](#typo)` with no heading `typo`) still render
  label-only — the user sees no slug clue. Acceptable: RET still
  raises `user-error` naming the bad slug, and the eldoc surface
  shows the URL.
