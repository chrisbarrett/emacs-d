## MODIFIED Requirements

### Requirement: Public block introspection

The system SHALL expose `gfm-pretty-block-at-point` and
`gfm-pretty-edit-block-at-point` as the only public block-aware
entry points. `gfm-pretty-block-at-point` SHALL return
`(DECORATOR-NAME . BLOCK)` for the first enabled decorator whose
`gfm-pretty-<name>--block-at-point` function returns non-nil, or nil
if none matches. `gfm-pretty-edit-block-at-point` SHALL dispatch to
the matched decorator's `gfm-pretty-<name>--edit-at-point` function.
Participation in this dispatch is a naming convention — a decorator
opts in by exporting both functions, with no registry slot involved.

No `gfm-pretty--*` private symbol SHALL be relied upon by code outside
the `gfm-pretty` module.

#### Scenario: Leader binding queries the public API

- **GIVEN** `modules/leader/init.el` provides the local-leader `t`
  binding for "edit block at point"
- **WHEN** point is inside a GFM table
- **THEN** the binding calls `gfm-pretty-edit-block-at-point`
- **AND** `gfm-pretty-tables--edit-at-point` is invoked
- **AND** no private `gfm-pretty--*` symbol is read

### Requirement: Rendering primitives are public

The system SHALL expose the following primitives for decorator authors:

- `gfm-pretty-available-width &optional WINDOW` — char width
  primitive.
- `gfm-pretty-top-border WIDTH FACE BUFFER-WIDTH &optional ICON` —
  top-border (LEADING . TRAILING) split.
- `gfm-pretty-bottom-border WIDTH FACE BUFFER-WIDTH` — bottom-border
  split.
- `gfm-pretty-right-after BOX-WIDTH FACE &optional BG` — right-edge
  after-string.
- `gfm-pretty-simulate-wrap TEXT WIDTH &optional CONT-PREFIX-W` —
  wrap-position simulator.
- `gfm-pretty-make-anchor BEG END &rest PROPS` — anchor-overlay
  factory bound to the calling decorator's registry.
- `gfm-pretty-make-display BEG END WINDOW &rest PROPS` —
  display-overlay factory restricted to WINDOW when non-nil.

Every other engine symbol SHALL be `gfm-pretty--` private.

#### Scenario: Decorator builds a bordered block

- **GIVEN** a decorator's `:apply-block-fn` callback
- **WHEN** it constructs a border for the current window
- **THEN** it calls `gfm-pretty-available-width` and
  `gfm-pretty-top-border` / `gfm-pretty-bottom-border` /
  `gfm-pretty-right-after`
- **AND** it does NOT reference `gfm-pretty--*` internal helpers

### Requirement: Debounced rebuild scheduler

The engine SHALL install exactly one of each lifecycle hook per
buffer when `gfm-pretty-mode` enables: one `after-change-functions`
handler, one `window-configuration-change-hook` handler, one
`post-command-hook` reveal handler, and one idle rebuild timer.

The timer SHALL fire after a 0.2 second idle delay following a
buffer modification, iterate every enabled decorator, and rebuild
within the buffer's accumulated dirty region. The engine SHALL
cancel and re-arm the timer on every modification so a burst of
edits produces one rebuild after the burst ends.

Decorators SHALL NOT install their own `after-change-functions`
or `post-command-hook` handlers for scheduling purposes. Decorators
MAY use `:on-enable-fn` / `:on-disable-fn` to install
decorator-specific hooks — for example, the tables decorator
installs its own buffer-local `window-configuration-change-hook`
handler for its bespoke per-window reconciler, and the links
decorator installs an xref backend.

#### Scenario: Burst of edits causes one rebuild

- **GIVEN** `gfm-pretty-mode` is enabled
- **WHEN** the user types ten characters in rapid succession
- **THEN** the engine SHALL cancel and re-arm the timer on each edit
- **AND** the rebuild SHALL run once, 0.2 s after the last edit
- **AND** every decorator's `:apply-block-fn` SHALL be invoked over
  the dirty region

#### Scenario: One handler per hook regardless of decorator count

- **GIVEN** a buffer with five enabled decorators
- **WHEN** the engine installs lifecycle hooks
- **THEN** `(length after-change-functions)` SHALL increase by one
  (engine handler only)
- **AND** `(length post-command-hook)` SHALL increase by one
- **AND** at most one buffer-local idle timer SHALL be live

### Requirement: Callout bordered-block rendering

The callouts decorator's `:apply-block-fn` SHALL render a bordered
callout box with:

- A top border using the type's coloured face and the type label as
  the upper-right caption.
- A `│ ` substitution for the `> ` prefix on each body line, using
  the border face.
- A right-edge `│` painted via `gfm-pretty-right-after` (or its
  overflow variant on wrapped body lines) so the right border
  aligns to the box width regardless of body-line wrapping.
- A bottom border using the type's coloured face.
- A tinted body background using the type's body-face background
  computed by `gfm-pretty-callouts--tint-bg` (10% from the type
  face foreground toward the theme background).

The decorator's `:apply-block-fn` SHALL call
`gfm-pretty-borders--apply-with-anchors` so anchor overlays
(carrying the body-face property and a `wrap-prefix` of `│ `) are
laid at most once per (block, rebuild pass) while per-window
display overlays (borders and right-edge after-strings) apply once
per window.

#### Scenario: NOTE callout

- **GIVEN** `> [!NOTE]\n> hello`
- **WHEN** the decorator renders
- **THEN** the top border shows `┌── NOTE ──┐` (right-aligned label)
- **AND** the body line shows `│ hello` with a blue-tinted background
- **AND** the bottom border shows `└──────────┘`

### Requirement: Callout marker line and body prefix reveal

The callouts decorator SHALL carry the engine's revealable property
(`gfm-pretty-callouts-revealable`) on the marker-line top and on
each per-line `> ` → `│ ` body-prefix display overlay. The engine's
reveal walker SHALL hide those overlays in the selected window when
point lies on them, exposing the raw `> [!TYPE]` or `> ` source.
The property name SHALL be derived from the callouts registry's
`tag`; the decorator SHALL NOT register it separately. Reveal SHALL
be scoped to the selected window.

#### Scenario: Point on marker

- **GIVEN** a NOTE callout with point on the marker line in W1
  (selected)
- **THEN** W1 shows the raw `> [!NOTE]` text
- **AND** the top-border decoration is visible
- **AND** other windows showing the buffer continue to show the
  marker decoration

### Requirement: Callout scoped post-edit rebuild

The callouts decorator SHALL register
`:full-rebuild-required-p` that returns non-nil when the dirty
region overlaps a `> [!TYPE]` marker line (structural-line case) or
overlaps a line directly above or below an existing callout
(adjacency case). The two conditions are OR-combined inside the
predicate. The engine's routing (see "Scoped post-edit rebuild
routing") SHALL use the predicate to choose between full rebuild
and single-block scoped rebuild.

When the dirty region intersects a callout body line (not the
marker and not adjacent to another callout), the engine SHALL
rebuild only the containing callout via its `:apply-block-fn`
(per displayed window). When the dirty region does not overlap any
callout block range and does not trigger the predicate, the
callouts decorator SHALL NOT contribute work to the rebuild
iteration.

#### Scenario: Edit inside one callout body

- **GIVEN** two non-adjacent callouts
- **WHEN** the user edits inside callout #1's body
- **THEN** only callout #1 is rebuilt by the engine
- **AND** callout #2's overlays are untouched

#### Scenario: Edit on a callout marker forces full rebuild

- **GIVEN** two callouts in a buffer
- **WHEN** the user edits callout #1's `> [!NOTE]` marker line
- **THEN** `:full-rebuild-required-p` returns non-nil for the dirty
  region
- **AND** the engine invokes the callouts decorator's `:rebuild-fn`
  (full rebuild)

### Requirement: Theme change responsiveness

The callouts decorator's `:on-enable-fn` SHALL add
`gfm-pretty-callouts--refresh-body-faces` to `+theme-changed-hook`.
`:on-disable-fn` SHALL remove it. The refresh function SHALL
recompute each callout body face's `:background` from the current
theme by tinting 10% toward the theme background.

#### Scenario: Theme switch

- **WHEN** the user switches from a light to a dark theme
- **THEN** `+theme-changed-hook` fires
- **AND** each `gfm-pretty-callouts-*-body-face` background is
  recomputed
- **AND** the next redisplay shows correctly tinted body backgrounds

### Requirement: Code-fence bordered-block rendering

The fences decorator's `:apply-block-fn` SHALL render a
curved-border box with:

- A top border using the border face and (when resolvable) a
  language icon at the upper-right.
- A `│ ` wrap-prefix and right-edge `│` so wrapped lines align to
  the box width.
- A bottom border.

The decorator's `:apply-block-fn` SHALL call
`gfm-pretty-borders--apply-with-anchors` so anchor overlays
(width-independent props such as the wrap-prefix and body
background fill) are laid at most once per (block, rebuild pass)
while per-window display overlays (borders and right-edge
after-strings, restricted to `WINDOW`) apply once per window.

#### Scenario: Fence with language

- **GIVEN** `\`\`\`bash\necho hi\n\`\`\``
- **THEN** the top border renders `┌──── …  ┐` with the bash icon
  right-aligned
- **AND** body line renders `│ echo hi` with a default-bg fill behind
  it
- **AND** bottom border `└──── …  ┘`

### Requirement: Code-fence marker line reveal

The fences decorator SHALL carry the engine's revealable property
(`gfm-pretty-fences-revealable`) on the opening and closing fence
marker display overlays. When point lies on a marker, the engine's
reveal walker SHALL hide those overlays in the selected window so
the raw `\`\`\`lang` shows. The property name SHALL be derived
from the fences registry's `tag`; the decorator SHALL NOT register
it separately.

#### Scenario: Point on opening fence

- **GIVEN** point on `\`\`\`bash`
- **THEN** the selected window shows `\`\`\`bash` (raw)
- **AND** other windows continue to show the top-border decoration

### Requirement: Code-fence scoped post-edit rebuild

The fences decorator SHALL register `:full-rebuild-required-p`
that returns non-nil when the dirty region overlaps any of:

- a fence opening line,
- a fence closing line,
- a YAML helmet's `---` marker line, or
- a blank line directly above or below an indent code block (where
  discovery is blank-line-gated).

The four conditions are OR-combined inside the predicate. The
engine's routing (see "Scoped post-edit rebuild routing") SHALL
use the predicate so an edit on any of those lines forces a full
fences rebuild; an edit fully contained in one block scopes to that
block; and edits outside every fences range produce no fences work.

#### Scenario: Edit inside fenced body

- **GIVEN** two fenced blocks
- **WHEN** the user edits inside block #1's body (not a marker)
- **THEN** only block #1 is rebuilt by the engine via the fences
  decorator's `:apply-block-fn`

#### Scenario: Blank line above an indent block becomes non-blank

- **GIVEN** an indent code block preceded by a blank line
- **WHEN** the user types on that preceding blank line
- **THEN** `:full-rebuild-required-p` returns non-nil for the dirty
  region
- **AND** the engine invokes the fences decorator's `:rebuild-fn`
  (full rebuild)

### Requirement: HR rendering

The hrule decorator's `:apply-block-fn` SHALL replace each
collected HR line's display with `(make-string WIDTH ?─)`
propertised with the hrule face. WIDTH comes from
`gfm-pretty-available-width` for the rendering window. The
overlay is per-window so two windows of different widths each see
a bar sized to their own width.

#### Scenario: HR in 100-cell window

- **GIVEN** an HR line in a 100-cell window
- **THEN** the display string is 100 `─` characters

### Requirement: HR cursor reveal

The hrule decorator SHALL carry the engine's revealable property
(`gfm-pretty-hrule-revealable`) on the HR display overlay. When
point lies on the HR line, the engine's reveal walker SHALL hide
the overlay so the raw `---` source shows in the selected window
only. The property name SHALL be derived from the hrule registry's
`tag`; the decorator SHALL NOT register it separately.

#### Scenario: Point on HR

- **WHEN** point moves to the HR line
- **THEN** the selected window shows `---` (raw)
- **AND** the unicode bar reappears when point leaves

### Requirement: Title-side overlay rendering

The links decorator's `:apply-block-fn` SHALL replace the
`[title]` span (brackets included) with the title text in
`gfm-pretty-links-title-face` (default `markdown-link-face`), per
window.

#### Scenario: Title rendering

- **GIVEN** `[Anthropic](https://anthropic.com)`
- **THEN** the title-side overlay displays `Anthropic` in
  `markdown-link-face`

### Requirement: URL-side icon rendering

The links decorator's `:apply-block-fn` SHALL replace the URL span
(`(url)`, `[label]`, the autolink span, …) with a single
nerd-icons glyph resolved from the target host or scheme. When
`nerd-icons` is unavailable, the URL-side overlay SHALL be omitted
(URL shows raw).

#### Scenario: Github URL

- **GIVEN** `[code](https://github.com/user/repo)` and `nerd-icons`
  available
- **THEN** the URL-side renders as the GitHub icon

#### Scenario: Unknown host

- **GIVEN** a link to an unrecognised host
- **THEN** the URL-side renders as a generic web icon

### Requirement: Whole-link cursor reveal

The links decorator SHALL carry the engine's revealable property
(`gfm-pretty-links-revealable`) on both the title-side and
URL-side display overlays, plus a shared `gfm-pretty-links-id`
property. The decorator SHALL register `:reveal-fn` so its bespoke
reveal handler — not the engine's default walker — runs for links.
When point lies anywhere inside either span, the handler SHALL
hide BOTH overlays in the selected window so the raw `[title](url)`
shows.

#### Scenario: Point inside title

- **WHEN** point is at the `t` in `[title](url)`
- **THEN** both overlays hide in the selected window
- **AND** other windows continue to show the decoration
