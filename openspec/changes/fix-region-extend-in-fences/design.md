## Context

`gfm-pretty-fences` decorates fenced/YAML/indented code blocks with a
visible bounding box. Each body line carries a per-window display
overlay whose `after-string` draws the right-edge `│` border and pads
to the window's right edge. The pad/tail of that after-string ends
with `(propertize " " 'display '(space :align-to right) 'face 'default)`
— a stretch glyph painted in `default` face.

The trailing stretch glyph is **load-bearing for diff clipping**: the
C-level past-EOL face merge in `face_at_buffer_position` skips faces
with `:extend nil` rather than suppressing siblings' `:extend t`,
so an opt-out overlay cannot clip a `diff-added` text-property's
`:extend t` bg from leaking past the right border. The working idiom
established by `fix-gfm-extend-leak` is to physically paint the visual
line with default bg so there is no past-EOL region for `:extend t` to
colour. See `lisp/gfm/gfm-pretty-borders.el:106-144`.

Side-effect: the same stretch glyph also masks `region`'s `:extend t`
past-EOL fill on selected body lines. Vanilla `mark-active` and evil's
visual state both rely on the `region` face's `:extend t` to make a
`V`-line selection read as "whole line" to the right of the last text
column. Inside a src block, that signal is lost.

Evil-mode renders linewise/charwise visual state with an actual
overlay: `evil-states.el:630-636` calls `(make-overlay
evil-visual-beginning evil-visual-end)` with `face 'region` and
`priority 99`. Vanilla Emacs uses an internal mark-region face merge
keyed off `(use-region-p)`. Both result in `region`'s `:extend t`
painting past EOL, and both are masked the same way today.

Evil visual-block (`Ctrl-V`) is a separate path
(`evil-states.el:638-735`) that uses `evil-visual-block-overlays` — one
per row segment, no `:extend` extension. Not affected by the mask.

The fences engine already has prior art for a per-decorator
`post-command-hook` doing visible-only overlay bookkeeping:
`lisp/gfm/gfm-pretty-tables.el:1215-1232` installs
`gfm-pretty-tables--update-cursor-highlight` on enable, and
`lisp/gfm/gfm-pretty-engine.el:759-770` installs the engine-wide
`gfm-pretty--reveal` for cursor-position reveal across decorators.

## Goals / Non-Goals

**Goals:**

- Selected body lines inside fenced, indented, and YAML-helmet code
  blocks paint `region` bg from the buffer text through to the
  window's right edge — past the box's `│` border included.
- Unselected body lines keep current behaviour: default bg fills past
  EOL, `diff-added` / `diff-removed` `:extend t` bg is clipped at the
  right border.
- Rebuilds mid-selection produce overlays in the correct state — no
  stale-frame flicker after a buffer edit while V-selection is held.
- Both vanilla `mark-active` selection and evil's
  linewise/charwise visual state are handled by the same code path.

**Non-Goals:**

- Callouts decoration (`gfm-pretty-callouts.el`). Same mask shape but
  out of scope for this change; can mirror the fix later.
- Evil visual-block. Already correct — rectangle overlays don't
  `:extend` past their explicit width.
- Region inside non-decorated past-EOL areas. Not regressed; not
  changed.
- Any change to box geometry, border drawing, or non-region face
  behaviour.

## Decisions

### Decision 1: `post-command-hook` swap, not redisplay-time `(when ...)`

Two viable paths:

- **A. Conditional display spec.** Wrap the stretch glyph in
  `(when COND . SPEC)` where COND is evaluated at redisplay time with
  `position` bound to the overlay position. No hook needed.

  Rejected: when COND is false the `display` spec is suppressed but
  the underlying " " character still renders in `face 'default`,
  leaving one cell of default bg at EOL that interrupts `region`'s
  `:extend t` paint. No clean way to also gate the `face` text
  property — display specs accept `(space ...)` / `(image ...)` /
  etc. but not face forms. Would require splitting the after-string
  across multiple overlays with mutually exclusive `(when ...)`
  guards, which is fiddly and exactly the kind of past-EOL
  fragility the existing comments warn against
  (`gfm-pretty-borders.el:117-126`).

- **B. `post-command-hook` swaps after-string.** Precompute both
  variants ("masked", "bare") at body overlay creation time, store as
  overlay properties, swap `after-string` based on whether the
  overlay's range overlaps the active selection.

  Chosen: keeps after-strings fully static between commands, no
  redisplay-time computation, no past-EOL face fragility. On-pattern
  with `gfm-pretty-tables--update-cursor-highlight`
  (`lisp/gfm/gfm-pretty-tables.el:1215-1224`) and the engine's
  `gfm-pretty--reveal` (`lisp/gfm/gfm-pretty-engine.el:759-770`).

### Decision 2: Bare variant is the empty string

Two options for the bare variant:

- Empty string — body overlay contributes no past-EOL paint. The
  `region` overlay's `:extend t` (priority 99 from evil; priority via
  internal merge from vanilla) paints from EOL to window edge.
- Minimal string carrying only `│` and a non-`:extend` face — keep
  the box border visible inside the selection.

Chosen: **empty**. Matches the user-stated requirement of "no
clipping" on selected lines and "extend fully to RHS of window". The
box border disappearing under the selection paint reads as expected
selection feedback. Keeping `│` visible would re-introduce a column
of default bg breaking the region paint, regressing the original
complaint.

### Decision 3: Detect selection bounds via union helper

Single helper `gfm-pretty-fences--selection-bounds` returns
`(BEG . END)` of the active selection, or nil. Implementation:

```elisp
(defun gfm-pretty-fences--selection-bounds ()
  (cond
   ((and (bound-and-true-p evil-visual-state-p)
         (not (eq (bound-and-true-p evil-visual-selection) 'block))
         (markerp (bound-and-true-p evil-visual-beginning))
         (markerp (bound-and-true-p evil-visual-end)))
    (cons (marker-position evil-visual-beginning)
          (marker-position evil-visual-end)))
   ((use-region-p)
    (cons (region-beginning) (region-end)))))
```

Evil is optional; `bound-and-true-p` keeps the helper safe when evil
isn't loaded. Visual-block is opted out by name; its overlays are
unaffected by past-EOL `:extend t` paint anyway.

### Decision 4: Memoise selection bounds; no-op when unchanged

Buffer-local `gfm-pretty-fences--last-selection-bounds` holds the
previous `(BEG . END)` (or nil). Hook returns early on `equal` match.
Most commands don't move mark/point through region boundaries; the
fast path keeps the hook cheap on every keystroke.

### Decision 5: Visible-windows scoping

On change, walk only overlays in each window's `(window-start ..
window-end)` range, via the existing
`gfm-pretty--visible-window-ranges` helper
(`lisp/gfm/gfm-pretty-engine.el:362-372`). Body overlays outside
visible ranges don't need updating until they scroll into view; a
subsequent post-command-hook tick handles them.

A naive walk over all decorator overlays would be O(total fenced
lines × windows) per command; the visible-scoped walk is bounded by
on-screen content.

### Decision 6: Creation-time initial state

`--apply-bordered-display` and `--apply-indent-display` compute
`(after-masked, after-bare)` for each body line and apply the
selection-aware choice at creation time via
`gfm-pretty-fences--range-selected-p`. This handles the rebuild-
during-selection case without relying on the next post-command-hook
fire to re-sync.

Both strings are stored on the overlay (`'gfm-pretty-fences-after-masked`
and `'gfm-pretty-fences-after-bare`) so the post-command-hook never
recomputes — it only chooses.

## Risks / Trade-offs

- [Rebuild deletes overlay during active selection mid-frame] → Initial
  creation honours selection bounds (Decision 6), so the new overlay
  is created in the correct state. No frame of stale render.
- [`post-command-hook` cost on commands that don't touch region] →
  Memoised early-return on bounds equality. The unchanged-bounds path
  is a single `equal` call plus one accessor of
  `evil-visual-beginning` / `mark-active`. Negligible.
- [Overflow path] → `gfm-pretty--right-after-overflow`
  (`lisp/gfm/gfm-pretty-borders.el:194-228`) builds a wrapped-line
  variant of the masked after-string. The bare variant remains the
  empty string; selection paint past the wrapped visual line works
  via region's `:extend t` the same way as the non-overflow path.
- [`:extend` priority interaction with `diff-added` on selected lines]
  → Region overlay priority 99 (evil) or internal-mark (vanilla) wins
  over the text-property `diff-added` face's priority-0 contribution
  in the past-EOL merge, so the selection paint covers the diff bg
  past EOL. Verified by the existing
  `lang-markdown/gfm-pretty-fences-bg-fill-extend-line-bg` test
  pattern (face merge priority).
- [Theme reload while selected] → existing theme-reload pathway
  recomputes faces; after-strings rebuild on next decorator rebuild,
  which honours creation-time selection bounds (Decision 6).
- [Indirect buffers / edit-indirect] → not exercised by the hook;
  evil's overlay and `mark-active` are buffer-local. Same shape as
  the existing tables cursor-highlight handler.

## Migration Plan

Single decorator-internal change; no spec-version bump or external
contract change. Existing buffers re-render via the standard fences
rebuild path on the next edit / theme reload / window-config change.
No data migration. Revert by reverting the patch.

## Open Questions

- Should the same fix be mirrored into `gfm-pretty-callouts.el` and
  any other decorator using the past-EOL default-bg mask? Tracked as
  a follow-up; not blocking this change.
