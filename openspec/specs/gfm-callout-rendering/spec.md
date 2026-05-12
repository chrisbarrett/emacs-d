# gfm-callout-rendering Specification

## Purpose
TBD - created by archiving change gfm-callouts-per-window-rendering. Update Purpose after archive.
## Requirements
### Requirement: Mode toggle

The system SHALL provide a buffer-local minor mode `gfm-callouts-mode`
that applies and removes the bordered-callout decoration overlays as
a unit.

#### Scenario: Enabling the mode in a markdown buffer

- **WHEN** `gfm-callouts-mode` is enabled in a buffer containing GFM
  callout blockquotes
- **THEN** every recognised callout (NOTE, TIP, IMPORTANT, WARNING,
  CAUTION, CRITICAL) is decorated with a curved-border box, a
  type-coloured top label, and a tinted background

#### Scenario: Disabling the mode

- **WHEN** `gfm-callouts-mode` is disabled in a buffer where it had
  decorated callouts
- **THEN** every overlay created by the mode is removed and the
  buffer visually matches its raw markdown source

### Requirement: Callout block discovery

The system SHALL identify a callout block as a marker line of the
form `> [!TYPE]` where `TYPE` is one of `NOTE`, `TIP`, `IMPORTANT`,
`WARNING`, `CAUTION`, or `CRITICAL`, followed by zero or more
contiguous blockquote lines beginning with `>`.

#### Scenario: Marker followed by body lines

- **WHEN** the buffer contains `> [!IMPORTANT]\n> Lorem ipsum.\n`
- **THEN** the system identifies one block of type `IMPORTANT`
  whose body covers `> Lorem ipsum.`

#### Scenario: Marker with no body

- **WHEN** the buffer contains `> [!NOTE]\n` followed by a blank
  line
- **THEN** the system identifies one block of type `NOTE` with no
  body lines

#### Scenario: Unknown marker is ignored

- **WHEN** the buffer contains `> [!FOO]\n> body\n`
- **THEN** the system does not identify a callout block

### Requirement: Bordered-block rendering

The system SHALL adorn every discovered callout with a curved
border composed of:

- a top border `┌─ TYPE ─…─┐` covering the marker line and
  extending across a window-sized width
- a left edge `│ ` substituting the `> ` prefix on every body line
- a right edge sized to the box width via an after-string
  containing `space :align-to`
- a bottom border `└─…─┘` on its own visual row beneath the last
  body line (or beneath the marker line when the body is empty)
- a `wrap-prefix` of `│ ` on continuation visual lines so wrapped
  content stays visually inside the box
- a tinted background face derived by blending the type face's
  foreground 10 % toward the default background, painted across
  every line of the callout

#### Scenario: Callout renders with curved box and label

- **WHEN** an `[!IMPORTANT]` callout with two body lines is
  decorated
- **THEN** the rendered output has a top border whose left segment
  reads `┌─ IMPORTANT ─` and right segment is dash fill ending in
  `┐`, body lines bracketed by `│ ` and a right-aligned `│`, and a
  matching bottom border on its own row

#### Scenario: Body-less callout renders with bottom border on marker line

- **WHEN** an `[!NOTE]` callout with no body lines is decorated
- **THEN** the bottom border attaches to the marker line's trailing
  after-string so the box still closes

### Requirement: Box width sizing

The system SHALL size each callout's border box width as
`min(text-width, max(80, max-content + 4))`, where `text-width` is
`window-max-chars-per-line` for the target window (falling back to
`fill-column` and then 80), and `max-content` is the longest body
line's visible width minus the 2-character `> ` prefix.  Body
lines whose visible width exceeds the inner content budget SHALL
wrap natively via the overlay's `wrap-prefix`.

#### Scenario: Box clamps to window width on a narrow split

- **WHEN** a callout whose natural content fits 100 columns is
  rendered in a window with `window-max-chars-per-line = 60`
- **THEN** the border box renders at width 60, body lines wrap with
  the `│ ` prefix, and the right-edge `│` aligns to column 60

#### Scenario: Box floors at 80 columns when content is short

- **WHEN** a callout's `max-content + 4 = 30` and `text-width = 120`
- **THEN** the border box renders at width 80

### Requirement: Wrapped right-edge alignment

The system SHALL pad the right-edge after-string of every body line
to the box width even when the line wraps, by simulating word-wrap
of `│ ` + the body content (account for the 2-column wrap-prefix
on continuation visual lines) and computing the residual padding
needed to reach `box-width - 1`.

#### Scenario: Long body line wraps and closes on the wrapped row

- **WHEN** a body line whose visible width exceeds `box-width - 4`
  is rendered
- **THEN** the line wraps with the `│ ` wrap-prefix on each
  continuation visual row and the right-edge `│` lands at column
  `box-width - 1` of the final wrapped row

### Requirement: Marker line and body prefix reveal on cursor entry

The system SHALL suppress the display of the marker-line top-border
overlay and the per-body-line `│ ` prefix overlay when point lies
on that line, exposing the underlying source `> [!TYPE]` or `> `
for editing.  The display SHALL be restored when point leaves the
line.

#### Scenario: Entering the marker line reveals source

- **WHEN** point moves onto the marker line of a decorated callout
- **THEN** the marker overlay's display is suppressed and the raw
  `> [!TYPE]` becomes visible

#### Scenario: Entering a body line reveals the `> ` prefix

- **WHEN** point moves onto a decorated body line within the
  callout
- **THEN** the `│ ` substitution overlay's display is suppressed
  and the raw `> ` becomes visible

#### Scenario: Leaving the line restores decoration

- **WHEN** point moves off a previously-revealed marker or body
  line
- **THEN** the overlay's display is restored

### Requirement: Per-window rendering

The system SHALL produce display overlays sized to each window
currently showing the buffer, so the same buffer split across
multiple windows (or shared between graphical and terminal frames)
renders at each window's own width.  Anchor overlays carry
width-independent props (background tint, `│ ` substitution,
`wrap-prefix`) and are shared across windows.  Display overlays
carry width-dependent props (top-border splits, right-edge
after-strings, bottom border) and SHALL be restricted to a single
window via Emacs's `window` overlay property.

When no window currently displays the buffer, the system SHALL
produce a single unrestricted display overlay set at a sane
fallback width and replace it with window-restricted overlays as
soon as a window starts showing the buffer.

#### Scenario: Buffer split across windows of different widths

- **WHEN** the buffer is shown in two windows whose
  `window-max-chars-per-line` differ
- **THEN** each window renders the callout sized to its own width,
  with one set of display overlays per window

#### Scenario: Anchor overlays are shared across windows

- **WHEN** the buffer is shown in N windows
- **THEN** each body line is covered by exactly one anchor overlay
  carrying the `│ ` substitution and one anchor overlay carrying
  the tinted background face, regardless of N

### Requirement: Per-window cursor reveal

The system SHALL suppress display only on the selected window's
revealable overlays when point enters a marker or body line.
Revealable overlays whose `window` property does not match the
selected window SHALL keep their display.

#### Scenario: Cursor in window A does not reveal source in window B

- **WHEN** the buffer is shown in two windows A and B with the
  selected window A and point on a marker line
- **THEN** window A shows the raw `> [!TYPE]` source while window
  B continues to display the bordered top label

### Requirement: Block-discovery cache

The system SHALL memoise `gfm-callouts--find-blocks` by
`buffer-chars-modified-tick` so repeat calls without an
intervening edit reuse the cached scan.  The cache SHALL be a pure
function of buffer contents — narrowing or widening the buffer
SHALL NOT invalidate or alter cached results.

#### Scenario: Repeated find-blocks calls without edits return cached result

- **WHEN** `gfm-callouts--find-blocks` is called twice with no
  buffer modification in between
- **THEN** both calls return `eq` block lists

#### Scenario: Edit invalidates cache

- **WHEN** the buffer is modified between two calls to
  `gfm-callouts--find-blocks`
- **THEN** the second call returns a fresh block list reflecting
  the new state

#### Scenario: Narrowing does not invalidate cache

- **WHEN** `gfm-callouts--find-blocks` is called once in the widened
  buffer and the buffer is then narrowed without any modification
- **THEN** the next call returns the same `eq` block list (the
  widened scan, including callouts outside the current restriction)

### Requirement: Narrowing-resilient discovery and teardown

The system SHALL discover callouts and tear down its overlays
identically regardless of the buffer's current restriction.
`gfm-callouts--find-blocks-1` SHALL widen the restriction for the
duration of its scan; the buffer-wide teardown path used by the
mode's full rebuild and by `gfm-callouts-mode` disable SHALL clear
all overlays tagged by the mode regardless of restriction.

#### Scenario: Narrowed rebuild does not duplicate overlays

- **WHEN** `gfm-callouts-mode` is enabled in a widened buffer
  containing callouts in two separate top-level regions
- **AND** the buffer is then narrowed to a region containing only
  the first callout
- **AND** `gfm-callouts--rebuild` is invoked
- **AND** the buffer is subsequently widened
- **THEN** the count of overlays tagged `gfm-callouts` on the
  buffer equals the length of `gfm-callouts--overlays`

#### Scenario: Cached block list is narrowing-independent

- **WHEN** `gfm-callouts--find-blocks` is called once in the
  widened buffer and once after narrowing to a sub-region
- **THEN** the second call returns the same data as the first for
  the same `buffer-chars-modified-tick`

#### Scenario: Full teardown leaves no off-narrowing zombies

- **WHEN** the buffer is narrowed and
  `gfm-callouts--remove-overlays` is invoked with no
  `BEG`/`END` arguments
- **AND** the buffer is subsequently widened
- **THEN** no overlay tagged `gfm-callouts` remains on the buffer

### Requirement: Debounced rebuild

The system SHALL respond to buffer modifications and window-state
changes via a 0.2-second idle timer, SHALL skip rebuilds in
indirect buffers (those with a non-nil `buffer-base-buffer`), and
SHALL react to window-state changes only when an actual rendering
input has changed (width of an existing window, or
arrival/departure of a window showing the buffer).

#### Scenario: Indirect buffer skipped

- **WHEN** the current buffer has a base buffer
- **THEN** the rebuild scheduler does not schedule a rebuild for
  the indirect buffer

#### Scenario: Window-config event with no width effect

- **WHEN** a window-configuration event fires that does not change
  any window's `window-max-chars-per-line` and does not add or
  remove a window showing the buffer (e.g. minibuffer activity,
  focus shift)
- **THEN** no rebuild is scheduled

### Requirement: Scoped post-edit rebuild

The system SHALL rebuild only the decorated callout whose source
range fully contains the dirty region when the dirty region
intersects exactly one callout's body.  The system SHALL fall
back to a full-buffer rebuild when:

- the changed region overlaps a marker line, or
- the changed region overlaps a line adjacent to a callout's first
  or last line (could create or destroy block boundaries), or
- the changed region intersects more than one callout.

When the changed region intersects no decorated callout and is
not adjacent to one, the system SHALL NOT rebuild.

#### Scenario: Edit inside one callout body

- **WHEN** the user modifies a body line inside one decorated
  callout and waits for idle
- **THEN** only that callout's overlays are rebuilt; other
  decorated callouts in the buffer retain their existing overlay
  objects unchanged

#### Scenario: Edit on the marker line triggers full rebuild

- **WHEN** the user inserts or deletes characters on the
  `> [!TYPE]` marker line of a callout and waits for idle
- **THEN** the system performs a full-buffer rebuild

#### Scenario: Edit on a line adjacent to a callout triggers full rebuild

- **WHEN** the user edits a line immediately above or below a
  callout's source range and waits for idle
- **THEN** the system performs a full-buffer rebuild

#### Scenario: Edit outside every decorated callout

- **WHEN** the user inserts text in a region that does not
  intersect or border any decorated callout and waits for idle
- **THEN** no overlays are rebuilt

### Requirement: Selective per-window reconciliation

The system SHALL act on a per-window diff when reconciling display
overlays after a window-state change: only added or resized
windows trigger fresh rendering, and only removed windows have
their display overlays deleted.  Display overlays belonging to
windows whose width is unchanged SHALL retain their existing
overlay objects.

#### Scenario: Resizing one of two windows leaves the other untouched

- **WHEN** the buffer is shown in two windows and only one
  window's width changes
- **THEN** the resized window's display overlays are replaced with
  fresh ones, while the other window's display overlay objects
  survive `eq` comparison

#### Scenario: Closing a window cleans up only its overlays

- **WHEN** a window showing the buffer is deleted
- **THEN** the deleted window's display overlays are removed at
  the next reconciliation, while overlays for surviving windows
  are not disturbed

#### Scenario: Opening a new window on the buffer renders just that window

- **WHEN** a fresh window starts showing a buffer that already has
  decorated callouts
- **THEN** display overlays for the new window are produced
  (visible callouts first, off-screen on the next idle); existing
  display overlays for already-shown windows are not rebuilt

### Requirement: Visible-first prioritised rebuild

The system SHALL prioritise visible-window callouts when
rebuilding display overlays in response to a window-state change,
rendering callouts that intersect any visible window range
immediately and deferring off-screen callouts to the next idle
tick.

#### Scenario: Visible callouts render before off-screen callouts

- **WHEN** a window resize fires a rebuild and the buffer contains
  callouts both inside and outside the visible window range
- **THEN** display overlays for callouts intersecting any visible
  window range are recreated synchronously, and overlays for the
  remaining callouts are recreated on a follow-up idle timer

