## Context

Today's presentation module is structurally fine.  The problem is
*usage shape*: agents reach for many small slides because the API
makes it easy and there's no compelling alternative for "long-form
explanation with anchored source views".  We want to keep the
underlying machinery (frame, tmux, deck, file-slide annotations) and
shift the agent's default move to writing a single document and
pushing supporting file slides.

## Decision

### A document is just a narrative slide whose buffer comes from a file

The minimal API change is one optional field.  `narrative` already
renders markdown into a buffer; it just creates that buffer
synthetically.  Pointing it at a real file is a one-branch swap in
`+presentation--render-narrative`:

```elisp
(if path
    (find-file-noselect (expand-file-name path worktree))
  (+presentation--make-synthetic-narrative-buffer key markdown))
```

Real files are project-rooted, so claude-code-ide's session lookup
(hashed by `(project-root project)`) routes selections and chat to
the same session as the rest of the project — no new "attach this
file to that session" mechanism required.  The chat surface for
talking through the doc is the IDE's existing chat.

### Click-handler dispatches by URL scheme, scoped to narrative buffers

Two link forms route through the deck:

```
[label](slide:2)                        ; explicit slide reference
[label](modules/auth/init.el#L42-L67)   ; exact-match deck lookup
```

Plain paths and other schemes pass through to `markdown-mode`'s
default link handler.  The dispatch is scoped to narrative buffers
because file-slide buffers have no business containing markdown
links — they're source code under a presentation overlay.

For deck lookup, exact match means: same `:path` (after
`expand-file-name` against worktree), same `:start-line`, same
`:end-line`.  Approximate matches ("smallest containing range") were
considered and rejected: ambiguity becomes the agent's silent bug.
If a link fails to resolve to a deck slide, plain `find-file` +
`goto-line` is the fallback.  Authors get exactly what they author.

Implementation choice: advice on `markdown-follow-link-at-point` (or
the keymap binding it ends up on), guarded by `(eq major-mode
'markdown-mode)` AND `(bound-and-true-p +presentation--session-key)`.
Other markdown buffers in Emacs are unaffected.

### `present_document` is a thin convenience, not a new lifecycle

Composing the doc-first flow from existing tools is three calls:
`Write` → `start_presentation` → zero or more `push_slide`s.  The
agent is capable of stitching this, but encoding the convention as
one tool sets the *default move* unambiguously.

Argument shape:

```
{
  worktree:      string,                   // required
  tmux_session:  string,                   // required
  tmux_window:   string,                   // required
  slug:          string,                   // required (kebab-case)
  markdown:      string,                   // required (doc body)
  split:         "horizontal"|"vertical",  // optional
  file_slides:   [SlideSpec],              // optional, all kind="file"
}
```

Path is computed, not configurable:

```
<worktree>/.claude/presentations/<YYYY>-<MM>-<DD>T<HH>-<MM>-<slug>.md
```

ISO date + 24h time minute precision, dashes (not colons — Windows /
case-insensitive FS friendliness, even though the immediate target
is macOS), then slug.  Deterministic, sortable, unique to the minute.

Why force the path: the convention is the value.  Agents that skip
it (writing docs to `/tmp` or anywhere outside the project) lose
project-routing for chat/selections.  Removing the choice removes
the footgun.

`file_slides` is an array of slide specs of `kind: "file"`, validated
through the existing `+presentation--validate-slide` and pushed in
order via the existing push path.  Returns
`{ key, path, slide_count }` — the agent gets back the doc path so
it can pass it to `Edit` for iterative refinement.

### What about `update_document` / iterative refinement?

Considered, rejected for v1.  The agent already has `Edit` (or
`Write`) on the doc path returned by `present_document`.  When the
buffer is `find-file-noselect`'d, Emacs auto-reverts (or the user
can `revert-buffer`) on disk change.  No new tool needed.  If
auto-revert proves jarring in practice, we can add an explicit
`refresh_document` tool later — purely additive.

For mutating the supporting file slides, `replace_slide` and
`truncate_after` already exist.  The agent uses them.

### Discouraging multi-narrative decks

The current spec doesn't forbid multiple narrative slides; it just
doesn't recommend either way.  We add a non-binding spec note plus a
rule update to make the new norm explicit.  No code-level guard:
agents writing test decks with two narrative slides shouldn't get a
user-error.

## Risks / Trade-offs

- **Path convention forces a directory.**  If the worktree's
  `.claude/presentations/` doesn't exist, the tool creates it.
  Cheap.
- **Click-handler advice is brittle.**  `markdown-mode` changes
  bindings between releases.  Mitigation: bind in
  `+presentation-mode-map` (already takes precedence over the major
  mode for its keys) rather than advising — `RET` and `mouse-1` go
  through our keymap when the buffer-local mode is on.  Tests cover
  the dispatch logic against the deck-search helper, not the input
  binding.
- **Exact-match policy is strict.**  Slight authoring drift between
  the doc's `#L42-L67` and the slide's `:start-line 42 :end-line
  67` produces silent fall-through.  Acceptable: silent fall-through
  is `find-file` + jump, which is also useful.  An optional
  validator-side warning ("doc references `path#L42-L67` not in
  deck") could be added later if this proves error-prone.
- **`:path` vs `:markdown` ambiguity.**  We require exactly one.
  Both-given is an error rather than a precedence rule, because
  silent precedence ("file wins, markdown ignored") would mask
  agent bugs.

## Migration

None.  Existing slide payloads keep working.  The new field is
optional.  The new tool is additive.  No spec requirements are
removed.
