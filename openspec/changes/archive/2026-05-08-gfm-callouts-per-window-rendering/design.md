## Context

`gfm-code-fences-mode` was rebuilt over several iterations into a
per-window decorator: anchor overlays carry width-independent props,
display overlays carry width-dependent props and are restricted to a
specific window via the `window` overlay property.  A debounced
scheduler reacts to edits and window-configuration changes; a
visible-first prioritised rebuilder keeps `C-x 3` transients
responsive.  See `openspec/specs/gfm-code-fence-rendering/spec.md`
for the full contract.

`gfm-callouts-mode` was written before that work and still uses a
single buffer-wide overlay set sized purely from content, with no
wrap simulation and no window-configuration listener.  Splitting a
markdown buffer with callouts across two windows of different widths
produces broken decorations; long body lines blow past the right
border in any window narrower than the content.

The decorators share most of their lower-level needs: a wrap
simulator, top/bottom border builders, a right-edge after-string
builder (with overflow path), an overlay registry tagged for bulk
cleanup, anchor/display overlay constructors, and a debounced
rebuild scheduler driven by `after-change-functions` and
`window-configuration-change-hook`.  Bringing callouts to feature
parity is the right point to extract the shared parts into
`+gfm-block-borders.el` rather than duplicating ~400 lines of glue.

## Goals / Non-Goals

**Goals:**

- Callouts render correctly in every window currently showing the
  buffer, sized to that window's `window-max-chars-per-line`.
- Long body lines wrap visually with a `wrap-prefix` and a
  right-aligned `│` whose padding accounts for the wrap.
- Window-configuration changes (split, delete, resize) reconcile
  per-window display overlays without rebuilding shared anchors.
- Edits inside one callout's body rebuild only that callout's
  overlays.  Edits to the marker line trigger a full rebuild.
- Reveal exposes source only in the selected window when the buffer
  is shown in multiple windows.
- Shared primitives live in `+gfm-block-borders.el`; both
  `+gfm-code-fences.el` and `+gfm-callouts.el` consume them.
- Existing `gfm-code-fences-mode` behaviour is preserved bit-for-bit
  through the refactor.

**Non-Goals:**

- Changing the visual identity of either decorator (border glyphs,
  faces, tinted background, label format).
- Adding language-icon resolution to callouts (callouts use type
  labels: NOTE/TIP/IMPORTANT/...; unchanged).
- Performance instrumentation for callouts in this pass — fences
  retains its `gfm-code-fences-stats`; an analogous
  `gfm-callouts-stats` is a follow-up.
- Indirect-buffer or polymode/mmm-mode integration beyond the
  existing `(buffer-base-buffer)` skip.

## Decisions

### Decision: Extract a single shared library `+gfm-block-borders.el`

Pull the box-drawing primitives, wrap simulator, anchor/display
overlay constructors, registry, scheduler primitives, and
window-state tracker into one file.  Both modes consume named
public functions; nothing in the shared lib references either
consumer's data shapes.

**Alternatives considered:**

- Two libs (`+block-borders.el` for drawing,
  `+block-rebuilder.el` for scheduling).  Cleaner separation but
  two modes only — premature.
- Keep duplication, refactor later.  Rejected: the duplication is
  ~400 lines and the second consumer is being written *now*; the
  cost of doing it once is lower than doing it twice with a
  follow-up consolidation PR.

### Decision: Anchor/display split mirrors fences exactly

Anchor overlays in callouts:

- Per-line tinted background face overlay (width-independent —
  paints buffer characters).
- `> ` → `│ ` substitution on each body line (covers 2 buffer chars,
  width-independent).
- `wrap-prefix` on the whole line for continuation visual rows.
- Marker-line "consume the leading `> [!TYPE]`" decoration is
  width-independent in its left half (the part covering the buffer
  chars) and width-dependent in its right half (the dash fill and
  closing `┐`).  Following fences, leading goes on the marker line
  as a `display` property (revealable), trailing goes on a
  zero-width display overlay at line-end.

Display overlays in callouts:

- Top-border leading (on marker line, revealable) — per window.
- Top-border trailing (after marker line-end) — per window.
- Per-body-line right-edge after-string — per window, with
  overflow path that simulates wrap.
- Bottom border on its own visual row — per window, attached as a
  display overlay starting after the last body line's end (mirrors
  the fences indent-block bottom-border treatment that already
  uses `\n` + bottom-string in an after-string).

### Decision: Box-width formula matches fences

`box-width = min(text-width, max(80, max-content + 4))`, where
`max-content` is the longest body line's visible width minus the
2-char `> ` prefix.  The `+ 4` accounts for `│ ` (2 cols) +
` │` (2 cols).  Current callouts uses `+ 2`, which is tight; we
adopt fences' formula for consistency.

### Decision: Reveal becomes selected-window aware

Match `gfm-code-fences--reveal`: only suppress display on
revealable overlays whose `window` property is `nil` (unrestricted)
or matches `(selected-window)`.  Per-buffer hidden-overlay list
stays a single list; suppression/restore is keyed on point + window.

### Decision: Scoped post-edit rebuild for callouts

Mirror fences' three-way fork:

- Edit overlaps a marker line (`> [!TYPE]`) → full rebuild.
- Edit overlaps a blank line bordering a blockquote (could change
  block boundaries) → full rebuild.
- Edit lies entirely inside one callout body → rebuild only that
  callout.
- Edit lies outside every callout → no rebuild.

The "blank line adjacent" check is simpler than fences' indent
case: callouts terminate at the next non-`>`-prefixed line, so any
edit that adds or removes a `> ` prefix in a body-adjacent line is
the trigger.  We treat this conservatively as: edit on any line
adjacent to a known callout that does not already match the body
pattern → full rebuild.

### Decision: Visible-first prioritised rebuild reused as-is

The fences' visible-first scheduler is generic.  Move it to the
shared lib parameterised by a `collect-blocks` callback and a
`render-block-for-window` callback.

### Decision: Performance instrumentation stays per-mode

`gfm-code-fences-stats` keeps its phase keys
(`find-fenced`/`find-yaml`/`find-indent`/...).  Callouts do not
gain stats in this pass; if needed later, the shared scheduler
will accept an optional stats-recorder hook.

## Risks / Trade-offs

- **Regression in fences during extraction** → cover the existing
  fences spec with a quick sweep before the extraction lands; gate
  the refactor on the existing fences test suite passing
  bit-identically.  Do the extraction in a series of mechanical
  moves, not a rewrite.
- **`window-configuration-change-hook` fires in the minibuffer
  too** → existing fences code already filters by checking whether
  rendering inputs actually changed.  The shared lib inherits that
  guard.
- **Callout marker line is `> [!TYPE]` (8+ chars), not 3 backticks
  (3 chars)** → top-border leading width must match marker
  buffer-width per-instance, exactly as fences does.  No new
  primitive needed; the existing `--top-strings` already accepts a
  `buffer-width` argument.
- **Callout bottom border currently rides last body's
  after-string via `\n`** → switching to a per-window bottom-border
  overlay risks ordering bugs on empty-body callouts (a NOTE with
  only a marker line).  Special-case empty body: bottom border
  attaches to the marker line's trailing after-string, matching
  current behaviour.
- **Reveal interaction with `gfm-callouts--apply-body-line` `> ` →
  `│ ` substitution** → the existing implementation already marks
  these revealable.  Per-window reveal must match `window` prop;
  existing-buffer reveal logic is the model.
- **Bg-tint face overlay extends rear-advance for typing-at-EOL**
  → preserve existing `(make-overlay ... t)` call to keep the
  background painting an inserted char at line end.  Don't change
  this in the refactor.

## Migration Plan

Implementation order (each step keeps tests green):

1. Create `+gfm-block-borders.el` with the existing fences
   primitives copied verbatim under a neutral prefix.
2. Switch `+gfm-code-fences.el` to consume the shared lib; delete
   the moved primitives.  Run fences tests.
3. Add unit coverage in the shared lib for the wrap simulator and
   width-clamp formula (these were exercised end-to-end before;
   pull them up).
4. Rewrite `+gfm-callouts.el` against the shared lib, anchor/display
   split first (no per-window restriction yet) — verify visual
   parity.
5. Add per-window restriction via the `window` overlay property.
6. Wire `window-configuration-change-hook` and the per-window
   reconciler.
7. Add the visible-first prioritised rebuilder.
8. Add the scoped post-edit rebuild.
9. Make reveal selected-window aware.

There is no runtime data to migrate; the change is purely
behavioural.

## Open Questions

- Should `gfm-callouts-mode`'s tinted-background face overlay also
  become per-window for consistency, even though the colour is
  width-independent?  Current plan: keep it as an anchor; revisit
  if it interacts badly with per-window reveal.
- Is the callout marker line ever wrapped natively (e.g. extremely
  narrow window)?  If yes, the marker buffer-width assumption may
  break.  Current plan: ignore for first pass; document as a known
  limitation if it matters.
