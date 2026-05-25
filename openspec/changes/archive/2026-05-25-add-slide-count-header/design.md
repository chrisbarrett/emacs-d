## Context

`gfm-present-mode` (`lisp/gfm/gfm-present.el`) is a buffer-local minor
mode that narrows a markdown buffer to one H1 region at a time.  The
current slide index and total are derivable from
`gfm-present--all-h1-positions` (gfm-present.el:41) and
`gfm-present--current-h1-start` (gfm-present.el:104), but nothing
surfaces them to the user.

The mode keymap is a `defvar-keymap` (gfm-present.el:298), additionally
projected into evil normal/motion/visual via `evil-define-key*` and
`evil-make-overriding-map` (gfm-present.el:307-317).  Any new binding
must land in both places to be reachable from evil states.

## Goals / Non-Goals

**Goals:**

- Make slide position visible at all times without users running a
  command.
- Make `C-x n w` end the slide frame cleanly instead of leaving the
  mode active with a widened buffer.

**Non-Goals:**

- No mode-line lighter changes.  The existing ` Pres` lighter stays.
- No slide title in the header.  H1 is already on the first line of
  the narrowing.
- No redisplay-driven counter (`:eval` in `header-line-format`).
  Refresh only at known transitions.
- No anchor-jump-specific scenario in the spec; it shares the refresh
  helper with the link-follow path.

## Decisions

### Decision: Counter stored in a buffer-local var, formatted into `header-line-format`

`header-line-format` is set to a plain format string (e.g. `"2/5"`)
when the buffer is on a slide, or nil otherwise.  A single helper
`gfm-present--refresh-header` recomputes from
`gfm-present--all-h1-positions` and `cl-position` of the current H1.

Alternatives considered:

- `:eval` form recomputed every redisplay.  Rejected: pays the H1 scan
  on every keystroke for no observable gain; user explicitly preferred
  refresh-on-transition.
- Caching positions in a buffer-local list invalidated on text edits.
  Rejected: invalidation surface (post-command-hook,
  after-change-functions, revert) costs more than the linear scan it
  saves.  Revisit only if profiling shows lag on huge docs.

### Decision: `widen` remap binds to a thin disable command

The keymap gains `[remap widen] #'gfm-present--exit`, where
`gfm-present--exit` calls `(gfm-present-mode -1)`.  The mode-disable
branch already widens (gfm-present.el:351), so no extra widen call is
needed.

Alternatives considered:

- Bind `[remap widen]` directly to `(lambda () (interactive)
  (gfm-present-mode -1))`.  Rejected: anonymous command, harder to
  describe-key / which-key.
- Map `widen` to `gfm-present-quit`.  Rejected during grilling — quit
  buries/kills the buffer, widen should not.

### Decision: Refresh points enumerated explicitly

The helper is invoked from exactly:

1. `gfm-present-mode` enable branch (gfm-present.el:328-341).
2. `gfm-present-next-slide` after the new narrowing (gfm-present.el:120-124).
3. `gfm-present-previous-slide` after the new narrowing (gfm-present.el:135-139).
4. `gfm-present-follow-link` slug branch after `narrow-to-region`
   (gfm-present.el:240-243).
5. `gfm-present--after-anchor-jump` (gfm-present.el:261-268).
6. `gfm-present--restore-position` after the revert re-narrow
   (gfm-present.el:459-464).

The mode-disable branch clears `header-line-format` back to nil.

### Decision: Evil keymap projection mirrors the remap

`evil-define-key*` only sees explicit `(kbd …)` entries; `[remap …]`
on the underlying map flows through `evil-make-overriding-map`'s
keymap inheritance.  We rely on the overriding map for the remap and
do not add an explicit `(kbd "C-x n w")` entry in the
`evil-define-key*` block — adding one would create two divergent
binding paths.

## Risks / Trade-offs

- [Header line collides with another minor mode that owns
  `header-line-format`] → Buffer-local set/unset only on present-mode
  enable/disable.  We do not push/pop a stack; if another mode set
  the slot first, our enable overwrites it and our disable nils it.
  Acceptable: `gfm-present-mode` is meant to own the buffer's display
  for the duration of the walkthrough.
- [User triggers `widen` via `M-x widen RET`] → `[remap widen]` covers
  both the keybinding and named-command invocation, so this works.
- [Stale counter when buffer is edited mid-narrowing such that the H1
  set changes] → Acceptable.  Edits during a presentation are an
  unsupported flow; the next navigation command refreshes.
