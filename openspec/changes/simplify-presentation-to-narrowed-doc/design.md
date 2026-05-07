## Context

The presentation module today carries a deck-of-slides architecture
designed for an "agent assembles slides over MCP" workflow.  After the
doc-first pivot, the only deck shape that gets used in practice is
"one narrative document plus a handful of supporting file slides".
The `pivot-presentation-to-document-mode` change accepted that shape
but kept the deck infrastructure to render the file slides.

This change observes that file slides are unnecessary too: a markdown
document can carry the same source views as inline link-previews,
with click-to-real-buffer as the escape hatch for users who want a
proper narrowed source view.  Once file slides are gone, every
remaining slide kind (`narrative` synthetic, `diff`, `layout`) was
already vestigial or unused, and the deck collapses to a single
markdown buffer.  At that point the entire MCP surface, the
session-key registry, the channel back-signal, and the tmux pane
management are all paying for a complexity budget the simplified
model doesn't need.

The replacement model is: one markdown file on disk, presented
via `(+present-markdown FILE)` from emacsclient, navigated heading-
by-heading via narrowing, with link-preview overlays for source and
diff references.  All state is buffer-local on the document.  The
caller (the agent's invocation context) handles tmux geometry by
choosing where it runs emacsclient; elisp does not own panes.

## Goals / Non-Goals

**Goals:**

- One public function (`+present-markdown FILE`); no MCP tools.
- All session state buffer-local on the doc buffer.  Killing the
  buffer ends the presentation.
- Heading-narrowing as the navigation primitive.  No deck array, no
  "current slide" pointer separate from buffer state.
- Live previews for `path#L<a>-L<b>` and `diff:<base>...<head>` link
  forms, refreshed on slide entry only.
- Click escapes to real buffers via standard `find-file` / `magit-
  diff-range`, with `push-mark` for back-jump.
- Robustness against in-place document edits (agent calls `Edit`
  while the user is presenting): re-narrow by heading slug, restore
  point + scroll via fingerprint search.
- Heading-text (`#some-heading`) in-doc links work in `+presentation-
  mode` and outside it (markdown-mode default).

**Non-Goals:**

- File-notify watchers on referenced files.
- HR (`---`) as soft slide-break inside an H1.
- Per-slide pane-layout reshaping; tmux geometry is the caller's
  problem.
- A registry / extension API for additional link kinds.  v1 hardcodes
  `path#L..` and `diff:..` in a `pcase`.
- Channel back-signal of any form.
- Backwards compatibility shims for the deleted MCP tools.
- Apheleia-style dynamic-programming point remap on revert; substring
  fingerprint is enough for v1.

## Decisions

### Single entry point: `+present-markdown FILE`

The function takes one argument: an absolute path to a markdown file.
It calls `find-file` on the path (so any existing buffer for that
file is reused), then enables `+presentation-mode` in that buffer,
which narrows to the first H1.  That is the entire public API.

```elisp
(defun +present-markdown (file)
  "Open FILE and enable presentation mode."
  (find-file file)
  (+presentation-mode 1))
```

Agent invocation pattern:

```
emacsclient -t -e '(+present-markdown "/path/to/doc.md")'
```

When the agent wants to put the doc in a fresh tmux pane, it composes
that itself:

```
tmux split-window -h -- emacsclient -t -e '(+present-markdown "/path/to/doc.md")'
```

This is the only place tmux is mentioned anywhere in the design, and
it is outside the elisp code.

Alternatives considered: making `+present-markdown` accept a worktree
+ slug + body and write the file itself (the old `present_document`
shape).  Rejected because the agent already has `Write` and can place
the file wherever it likes (`<worktree>/.claude/presentations/<slug>.md`
remains the recommended location, but conventions live in the rule
file, not the API).  Rejected because forcing the function to write
the file pulls in slug validation, path templating, and clock injection
— all of which are agent concerns, not Emacs concerns.

### Buffer-local minor mode owns navigation and lifecycle

`+presentation-mode` is a buffer-local minor mode.  Its keymap binds:

| Key            | Command                          |
| :------------- | :------------------------------- |
| `C-n` / `C-f`  | `+presentation-next-slide`       |
| `C-p` / `C-b`  | `+presentation-previous-slide`   |
| `C-c q`        | `+presentation-quit`             |
| `RET`          | `+presentation-follow-link`      |

`next-slide` and `previous-slide` widen, search for the next/previous
H1, and re-narrow to the region from that H1 to the next H1 (or
`point-max`).  `quit` disables the mode (which widens) and buries
the buffer; if the buffer was opened solely for the presentation
(detected via a buffer-local marker set on enable), `quit` kills it.

There is no global state.  No hash, no key.  The only state that
needs to survive across navigation is the doc buffer's narrowing,
which is already a buffer property.

Alternatives considered: keep a session key for symmetry with the
old API.  Rejected because there is nothing the key indexes.  No
external lookup needs to find this buffer.

### Slide = top-level H1 region

A slide is the buffer region from one `^# ` line up to (but not
including) the next `^# ` line, or `point-max`.  H2 / H3 / H4
headings flow inside the slide.  No HR-as-soft-break, no
frontmatter-driven slide demarcation.

Heading detection uses a tight regex (`^#[ \t]+\(.*\)$`) on lines that
are not inside fenced code blocks.  Fence detection is handled by the
existing `markdown-mode` machinery
(`markdown-code-block-at-point-p`); we delegate.

### Live previews replace links on display, not in storage

For each link in the current narrowing whose URL matches a previewable
form, an overlay covering the link's whole expression
(`[label](url)`) sets a `display` property to a multi-line string
containing a fenced code block.  The underlying buffer text is
unchanged — link follows continue to work, and writing the buffer to
disk writes the original markdown.

Overlays are managed in a buffer-local list
(`+presentation--preview-overlays`).  On every narrowing change
(`+presentation-mode-narrow`, `+presentation-next-slide`, etc.), the
list is cleared (`delete-overlay` on each member) and rebuilt by
scanning the new narrowed region.

The fence string is constructed as:

```
```<lang> <label> · <path>:<start>-<end>
<line 1>
<line 2>
...
<line 10>
```
+N more lines · click to open
```

`<lang>` is derived from the file extension (a small alist:
`.rs` → `rust`, `.el` → `elisp`, `.py` → `python`, etc.; default
`text`).  The footer line ("+N more lines · click to open") is
included only when the requested range exceeds 10 lines.  When the
file does not exist or the range is invalid, the overlay renders an
error fence (`text` lang, single line `(file not found: <path>)` /
`(invalid range)`) and the click action falls through to the same
error.

For diffs, the `<lang>` is always `diff`, and the preview body is
`git diff <base>...<head> [-- <path>]`'s first 10 lines.  When the
diff is empty, the overlay renders `(no changes)`.

Alternatives considered: text-replacement (modify the buffer to
insert the fence and remove on disable).  Rejected because it makes
`save-buffer` write the wrong contents.  Considered indirect-buffer
or polymode-style fence rendering; rejected as heavy machinery for a
single-overlay use case.

### Click semantics: push-mark, then standard navigation

`+presentation-follow-link` (bound to `RET` and optionally `mouse-1`)
identifies the link at point, parses its URL, and dispatches via a
single `pcase` on the URL form:

| URL form                                    | Action                                                |
| :------------------------------------------ | :---------------------------------------------------- |
| `#<heading-slug>`                           | `push-mark`; find heading; widen + re-narrow to its enclosing H1 |
| `<path>#L<a>` or `<path>#L<a>-L<b>`         | `push-mark`; `find-file` path; narrow to a..b; install focus + read-only restorer overlays |
| `diff:<base>...<head>` or `...#<path>`      | `push-mark`; `magit-diff-range` with parsed args      |
| `https:`, `mailto:`, plain path, etc.       | Pass through to `markdown-follow-link-at-point`       |

`push-mark` is called *before* navigation, so evil's `C-o` (and
`C-x C-SPC` in vanilla Emacs) returns to the link site.

`find-file` for `path#L..` clicks honours `other-window-prefix` —
the user controls split direction and reuse via the standard
prefix.  Same for the magit branch (`magit-diff-range` uses
`pop-to-buffer-same-window` by default but the prefix mechanism still
works through `display-buffer` overrides).

The narrowed-source view (post-click) re-uses the existing
`+presentation--render-file` renderer for the focus overlay, the
read-only toggle, and the restore-on-leave wiring.  It does NOT
enable `+presentation-mode` in the source buffer — the source is a
peek, not a slide.

Alternatives considered: open in a registered side-window slot.
Rejected because it imposes a layout that the user might not want and
is harder to dismiss with standard bindings; better to defer to
`other-window-prefix` and let the user choose.

### Revert resilience: slug + index, fingerprint point

Buffer-local hooks:

```
before-revert-hook  → +presentation--remember-position
after-revert-hook   → +presentation--restore-position
```

`remember-position` captures, into a buffer-local plist:

- `:slug` — slugified heading text of the current narrowing's H1
  (e.g. `"Auth flow"` → `"auth-flow"`).
- `:index` — ordinal of that H1 among all H1s (0-based).
- `:fingerprint` — a substring of up to 80 characters of buffer text
  starting at point (or fewer if near `point-max`).
- `:window-start-offset` — `(- (window-start) point)` for the
  selected window showing the buffer (or 0 when not displayed).

`restore-position` widens, then:

1. Searches for an H1 whose slugified text equals `:slug`.  If found,
   re-narrows to its region.
2. Else, narrows to the H1 at ordinal `:index` if one exists.  If
   the deck shrank, narrows to the last H1.
3. Else, narrows to the first H1 if one exists.  If no H1s, leaves
   the buffer widened and emits a one-shot message
   ("`+presentation-mode`: no headings to narrow to").
4. Searches for `:fingerprint` inside the new narrowing.  If found,
   sets point to the match start and `set-window-start` to
   `(+ point :window-start-offset)`.  Else, leaves point at the
   start of the narrowing.

The fingerprint window of 80 chars is a heuristic: long enough to be
specific in prose, short enough to survive small edits.  Slug
matching is the primary signal; fingerprint is just for in-slide
position.

Alternatives considered: apheleia-style dynamic-programming remap.
Rejected as overkill — fingerprinting handles the common cases
(point survives if the surrounding text wasn't edited) and degrades
gracefully (point-at-narrowing-start) when it doesn't.

### Heading slug normalisation

A heading slug is computed as: take the heading text after `# `,
downcase, replace any run of non-alphanumeric characters with a
single hyphen, strip leading/trailing hyphens.  The same algorithm
runs on the link target's `#fragment` and on every H1's text; equality
is string equality.  This matches GFM's heading-anchor convention
closely enough for in-doc links to behave intuitively, without
requiring perfect parity with GitHub's exact slugifier.

Edge case: duplicate headings.  In a doc with two `# Setup` headings,
both produce the same slug.  The first match wins.  Authors should
disambiguate (`# Setup (server)`, `# Setup (client)`) — this is a
markdown convention, not a presentation-mode constraint.

### Renderer reuse for click-to-source

`+presentation--render-file` (today: 50 lines) survives wholesale,
but is detached from the deck.  The function takes a buffer + range
+ optional focus + optional annotations and applies overlays /
read-only / narrowing with the same restore-on-cleanup discipline
as before.

Cleanup is triggered by a buffer-local `kill-buffer-hook` on the
source buffer (since there is no enclosing session to drive it).
When the user kills or reverts the source buffer, restorers run; on
re-narrow via another link click, the prior overlays are cleared
before new ones are applied.

Annotations are no longer constructible from outside the file —
there is no MCP path that supplies them.  The renderer keeps the
annotation rendering codepath but it is dormant in v1 (callable
from elisp if a future feature wants it; not exposed through any
link kind).

Alternative considered: delete the annotation renderers entirely.
Rejected because keeping them costs ~80 lines of tested code that we
might want for a future feature (e.g. a `[label](path#L42@warning)`
extension) and removing it now is harder to reverse than keeping it.

## Risks / Trade-offs

- **[Loss of agent-side observability]** → No channel back-signal
  means the agent cannot tell which slide the user is on.  Decided
  acceptable: the channel feature didn't work on the user's build
  anyway, and the doc-first model assumes the agent finishes its
  artefact and the conversation continues through the IDE chat
  surface, not through presentation-driven prompts.

- **[Loss of pre-baked layout reshaping]** → Without `pane_layout`,
  each user has to set up tmux geometry to taste before invoking
  `+present-markdown`.  Mitigation: the rule file documents
  recommended invocation patterns (`tmux split-window -h
  emacsclient ...` for "wide" feel; explicit `tmux select-layout
  main-vertical` for advanced users).

- **[Slug collisions]** → Two H1s with identical text both match the
  same `#slug` link.  First-match-wins is a footgun for poorly
  authored docs.  Mitigation: documentation guidance only; not worth
  enforcement.

- **[Fingerprint instability]** → If the agent's edit happens to
  rewrite the 80 chars at point, restore falls back to start-of-
  narrowing.  User notices a small jump.  Mitigation: keep the
  fingerprint short enough that it usually survives prose tweaks but
  long enough to be specific; revisit if user reports jitter.

- **[`other-window-prefix` discoverability]** → Users unfamiliar with
  the prefix mechanism may be surprised that clicking a link opens
  in the same window.  Mitigation: rule-file note;
  `display-buffer-alist` per-user customisation is the escape hatch.

- **[Magit unavailability at click time]** → If `magit` isn't loaded,
  diff-link clicks fail.  Mitigation: `(require 'magit nil t)` at
  click time; on failure, fall back to `vc-diff` or signal a
  user-error pointing the user at magit.  Decision deferred; the
  user has magit configured.

- **[Predecessor change still has open tasks]** → 3.10 and 5.2 of
  `pivot-presentation-to-document-mode` are unfinished smoke tests.
  This change deletes the surface those smoke tests would exercise.
  Mitigation: archive the predecessor with the smoke tests marked
  N/A (or run them quickly first), then merge this change.  Either
  is fine because the predecessor is shipped in code already.

## Migration Plan

There is no backward-compatibility surface to preserve.  Any agent
that calls the deleted MCP tools will see a "tool not found" error
from claude-code-ide.  The recommended pattern (encoded in the
external rule file) becomes:

1. Agent writes a markdown file via `Write` (typical location:
   `<worktree>/.claude/presentations/<ISO-date>T<HH-MM>-<slug>.md`).
2. Agent invokes `tmux split-window -h emacsclient -t -e
   '(+present-markdown PATH)'` (or a graphical-frame variant) via
   `Bash`.
3. Agent edits the markdown file via `Edit` as needed; the user's
   buffer auto-reverts and re-narrows.
4. User ends the presentation by killing the buffer or `C-c q`.

Rollback: this is a single change.  If the new shape proves wrong,
revert the merge commit and the previous `pivot-presentation-to-
document-mode` shape returns intact.

## Open Questions

None remaining; all material decisions resolved during the grilling
session that produced this proposal.
