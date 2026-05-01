## Context

The `add-presentation-sessions` change shipped session lifecycle and a
single `narrative` slide carried inside the start payload.  Slide-level
ops (`push_slide` / `replace_slide` / `truncate_after`) were explicitly
deferred.  This change picks them up and adds three new slide kinds
plus inline overlay annotations.

The session module already follows an effect-interpreter pattern for
tmux interaction: pure planners emit effect records, a single runner
executes them.  Buffer rendering is in-process Emacs work, so we extend
the existing `+presentation--render-slide` dispatch rather than
introducing a second interpreter.

## Goals / Non-Goals

**Goals:**

- An indexed deck the agent can mutate (push/replace/truncate/goto).
- Three buffer-bearing slide kinds (`file`, `diff`, `layout`) plus the
  existing `narrative`, all sharing one render dispatch.
- Per-slide annotation overlays: `{ line, text, position }` records
  attached as Emacs overlays on the slide's buffer, wiped when the
  slide is left.
- Recoverability after agent compaction: `get_deck` returns enough
  state to resume.

**Non-Goals:**

- `await_feedback` and any bidirectional flow (deferred).
- Persistent annotations across slides.
- Custom diff backends (no magit dependency); plain `diff-mode` only.
- User-facing presentation minor-mode, navigation keybindings, or
  side-window layouts beyond the simple two-pane split.
- Image / mermaid / shell-output slide kinds.

## Decisions

### Deck data model

Each session plist gains:

| Key                     | Type           | Notes                                       |
| :---------------------- | :------------- | :------------------------------------------ |
| `:deck`                 | vector         | Vector of slide plists, indexed from 0.     |
| `:current-slide-index`  | integer / nil  | Index of the slide currently rendered, or nil before any slide. |
| `:render-state`         | plist          | Per-slide bookkeeping: spawned buffers, overlays, narrowing markers.  Cleared on slide change. |

Why a vector over a list: O(1) random access for `goto_slide` and
`replace_slide`; the deck is mutated in place by all four ops.  Lists
would force traversal on every op.

`initial_slide` from `start_presentation` becomes deck entry 0; the
existing splash placeholder is treated as "deck empty, current index
nil" rather than as a deck entry.  This keeps the index space clean.

### Slide schema

Every slide is a plist with `:kind` (string) plus kind-specific fields
and an optional `:annotations` array.  Schemas:

```
narrative
  :kind        "narrative"
  :markdown    string
  :annotations (optional) [ { :line N :text S :position before|after } ... ]

file
  :kind        "file"
  :path        string             ; relative to session :worktree, or absolute
  :start-line  integer (optional)
  :end-line    integer (optional) ; inclusive
  :focus       [start end] (optional) ; sub-range to recenter on
  :annotations (optional)

diff
  :kind        "diff"
  :path        string (optional)  ; scope to one path
  :base        string (optional)  ; git ref; default = working tree
  :head        string (optional)  ; git ref; required iff :base given
  :annotations (optional)

layout
  :kind        "layout"
  :split       "horizontal" | "vertical"
  :panes       [slide-spec, slide-spec]   ; exactly 2; recursion bans nested layout
```

Why MCP arg names use snake_case but elisp uses kebab-case: existing
convention in `init.el` already converts via `+presentation--alist-to-plist`.

### Render dispatch

Single entry point:

```elisp
(defun +presentation--render-slide (session slide))
```

dispatches on `:kind`, returns the buffer (or window-config) to be
shown in the session's frame.  Per-kind helpers:

| Kind        | Helper                                   | Buffer                                |
| :---------- | :--------------------------------------- | :------------------------------------ |
| `narrative` | `+presentation--render-narrative`        | `*presentation: KEY*` (existing)      |
| `file`      | `+presentation--render-file`             | `find-file-noselect` of resolved path |
| `diff`      | `+presentation--render-diff`             | `*presentation-diff: KEY*` (per-session) |
| `layout`    | `+presentation--render-layout`           | composite — splits frame's window     |

Frame display: the dispatcher computes the desired window-configuration
and applies it via `with-selected-frame frame ... set-window-buffer` /
`split-window`.  This bypasses `display-buffer` (consistent with the v1
display-buffer protection rule).

### `file` slide rendering

1. Resolve `:path` against `session :worktree` if relative.
2. `find-file-noselect` to load buffer; respects auto-mode-alist so the
   right major mode + LSP attaches.
3. If `:start-line` / `:end-line` given, `narrow-to-region` between the
   line bounds.  Stash the markers in `:render-state` so we can widen
   on slide-leave.
4. If `:focus` given, `recenter` on the focus midpoint and place point
   at focus start.  Add a `region`-faced overlay over the focus range
   so the eye lands on it.
5. Set `buffer-read-only t` for the duration of the slide.  Restore on
   slide-leave (only if we toggled it).

### `diff` slide rendering

1. Build argv: `git -C WORKTREE diff [BASE..HEAD] [-- PATH]`.
2. Run via the existing effect runner; capture stdout.
3. Insert into `*presentation-diff: KEY*`, `diff-mode`, read-only.
4. Annotations are line-indexed against the rendered diff buffer (not
   the source file) — the agent crafts annotations after seeing the
   diff hunks.

### `layout` slide rendering

1. Render each pane to its target buffer (recursive call).
2. In the frame: `delete-other-windows`, then `split-window` per
   `:split`, then `set-window-buffer` for each pane.
3. Recursion is depth-1: nested layout is rejected at validation.  Two
   panes only; richer layouts can come later.

### Annotation overlays

Annotations are stored on the slide spec, not the buffer, so they
travel with the slide.  Render path:

1. After per-kind rendering, iterate `:annotations`.
2. For each `{ :line N :text S :position P }`:
   - Compute buffer position via `goto-char (point-min); forward-line (1- N)`.
   - Make an overlay at that point.
   - Set `before-string` (P = `before`) or `after-string` (P = `after`,
     default).  String includes a leading/trailing newline as
     appropriate, with a face like `font-lock-comment-face` plus a
     left-margin sigil (e.g., `▸`) for visual grouping.
3. Overlay objects are pushed onto `:render-state :overlays`.

On slide-leave (next render or end-of-deck cleanup):

```elisp
(mapc #'delete-overlay (plist-get render-state :overlays))
```

This honours the "tied to slide" lifetime decision.  No accumulation
across slides; no risk of leaking overlays into user buffers (file
slides may share a buffer with the user, but overlays are owned by
this slide and explicitly removed).

### Deck mutation semantics

| Op                | Behaviour                                                   |
| :---------------- | :---------------------------------------------------------- |
| `push_slide`      | Append slide to `:deck`; render; set `:current-slide-index` to new index.  Returns the new index. |
| `replace_slide i` | Replace `(aref deck i)`.  If `i = current-slide-index`, re-render.  Otherwise just mutate. |
| `truncate_after i`| Drop slides at index `> i`.  If current index `> i`, render the slide at `i` and update `:current-slide-index`. |
| `goto_slide i`    | Render the slide at `i`; set `:current-slide-index`.       |

Bounds-check: `i` out of range signals a `user-error` and does not
mutate.  `replace_slide` on empty deck is a `user-error`.

### `get_presentation` extension

Existing payload gains two fields:

```
slide_count          integer       (vector length)
current_slide_index  integer / nil
```

This is additive; existing consumers ignore the new keys.

### `get_deck`

Returns:

```
{
  key: string,
  current_slide_index: integer | nil,
  slides: [
    { index: 0, kind: "narrative", title: "..." | null },
    ...
  ]
}
```

`title` is the slide's `:title` field if present, else nil.  Full slide
content (markdown body, file contents, annotations) is **not** echoed
back — agents already know what they pushed; this op is for recovery
of structure, not content.

### Validation

Slide validation is one pure function `+presentation--validate-slide`:
checks `:kind`, required fields per kind, refuses nested layout,
ensures `:annotations` line numbers are positive integers.  Called
before any deck mutation; failure raises a `user-error` without
mutation.

## Risks / Trade-offs

[Risk] User edits a `file` slide's buffer mid-presentation → narrowing
markers/overlays may end up in surprising positions when we widen on
slide-leave.
→ Mitigation: set `buffer-read-only t` while a `file` slide is current.
Restore prior read-only state on slide-leave.

[Risk] `diff` slide spawns long output for big trees, blocking the
runner.
→ Mitigation: rely on git's `--stat`-free default diff being fast
enough for review; document that very large diffs should be scoped
with `:path`.  No async work in v1.

[Risk] Frame deleted while a slide is mid-render (race with
`delete-frame-functions` hook).
→ Mitigation: every render path checks `(frame-live-p frame)` before
applying window-configuration; bails out cleanly if not live.

[Risk] Annotation lines referenced after `replace_slide` mutates
content but agent forgot to update annotations → overlays land on
wrong lines.
→ Mitigation: annotations are part of the slide spec, so an agent
replacing a slide replaces annotations atomically.  No partial-update
op.  Document in the tool description.

[Risk] `find-file-noselect` opens a buffer that shadows a buffer the
user already had open with unsaved changes.
→ Mitigation: `find-file-noselect` reuses the existing buffer if any.
Read-only toggle on slide-enter, restore on slide-leave.  If the user
had unsaved changes, our read-only toggle does not affect them; we
just never touch their buffer's contents.

## Migration Plan

Additive change.  Existing `start_presentation` / `get_presentation` /
`end_presentation` callers continue to work.  No data migration.
