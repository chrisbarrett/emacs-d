## ADDED Requirements

### Requirement: Link mode toggle

The system SHALL provide a buffer-local minor mode `gfm-links-mode`
that applies and removes the link-decoration overlays as a unit. The
mode SHALL be enabled automatically in `markdown-mode-hook` whenever
`markdown-hide-urls` is non-nil at mode-init time, and SHALL track
subsequent changes to `markdown-hide-urls` via a buffer-local
variable watcher.

#### Scenario: Enabling the mode in a markdown buffer with hide-urls

- **WHEN** `markdown-mode` is initialised in a buffer with
  `markdown-hide-urls` set to t
- **THEN** `gfm-links-mode` is enabled in that buffer
- **AND** every recognised link form is decorated with the
  title-side and url-side overlays

#### Scenario: Toggling hide-urls off while the mode is on

- **WHEN** `markdown-hide-urls` is set to nil in a buffer where
  `gfm-links-mode` is on
- **THEN** `gfm-links-mode` is disabled
- **AND** every overlay created by the mode is removed
- **AND** the buffer visually matches its raw markdown source

#### Scenario: Disabling the mode directly

- **WHEN** `gfm-links-mode` is disabled in a buffer where it had
  decorated links
- **THEN** every overlay created by the mode is removed and the
  buffer visually matches its raw markdown source

### Requirement: Link shape discovery

The system SHALL discover and decorate the following link shapes:

- inline links `[title](url)` and `[title](url "title-attr")`
- reference links in full form `[title][label]`, collapsed form
  `[title][]`, and shortcut form `[title]` (the last only when a
  matching `[label]: url` definition exists in the buffer)
- autolinks `<url>` where `url` parses as an absolute URL
- GFM bare URLs (`https?://…` outside any other markup)
- wiki links `[[Page]]` and `[[alias|Page]]`, only when
  `markdown-enable-wiki-links` is non-nil

The system SHALL NOT decorate:

- image links `![alt](url)` or `![alt][label]`
- reference definition lines `[label]: url`
- footnote markers `[^id]` or footnote text lines

#### Scenario: Inline link with title attribute

- **WHEN** the buffer contains `[Anthropic](https://anthropic.com "official")`
- **THEN** the system decorates the link as one inline-link
  unit and the title attribute is available for eldoc surfacing

#### Scenario: Shortcut reference matches only with definition

- **WHEN** the buffer contains the prose `[design]` with no
  matching `[design]: …` definition elsewhere
- **THEN** the system does NOT decorate `[design]`
- **AND** the raw bracketed text remains visible

#### Scenario: Image link is left raw

- **WHEN** the buffer contains `![alt](./diagram.png)`
- **THEN** the system does not create any link-decoration overlay
  for that text
- **AND** the raw markdown remains visible

### Requirement: Title-side overlay rendering

The system SHALL replace the visible bracket scaffolding around the
title with an overlay whose `display` property is the title text in
`markdown-link-face`. For inline and reference links the overlay
covers the region `[title]` (brackets inclusive). For autolinks
`<url>` and bare URLs the overlay covers the URL span itself and
the visible label is the host extracted from the URL (the
`url-host` portion). For wiki links the overlay covers `[[…]]` and
the visible label is the page name (alias if present).

#### Scenario: Inline link title rendering

- **WHEN** an inline link `[Anthropic](https://anthropic.com)` is
  decorated
- **THEN** the visible text for the title region is the four
  characters `Anthropic` in `markdown-link-face`
- **AND** the surrounding square brackets are no longer visible

#### Scenario: Autolink host extraction

- **WHEN** an autolink `<https://anthropic.com/path>` is decorated
- **THEN** the visible label is `anthropic.com` in
  `markdown-link-face`

### Requirement: URL-side icon rendering

The system SHALL replace the URL-bearing region of each decorated
link with an overlay whose `display` property is a single icon
glyph. The URL-bearing region is `(url)` for inline links, the
trailing `[label]` (or its position) for reference links, the
autolink/bare-URL span itself, and the wiki-link target half for
wiki links.

The icon SHALL be resolved as follows, deferring entirely to
`nerd-icons`:

- if the URL starts with `http://` or `https://` (or is an
  unrecognised non-relative scheme), call `nerd-icons-icon-for-url`
  on the URL
- if the URL is a relative path or begins with `file:`, call
  `nerd-icons-icon-for-file` on the basename
- if the URL is a same-document anchor (`#…`), call
  `nerd-icons-icon-for-url` on the anchor (which yields the
  globe fallback)

The system SHALL NOT maintain its own URL→glyph table; users
extend `nerd-icons-url-alist` for customisation.

The system SHALL NOT pass a `:height` override to nerd-icons.

#### Scenario: GitHub URL resolves to the github octicon

- **WHEN** an inline link `[repo](https://github.com/foo/bar)` is
  decorated
- **THEN** the url-side overlay's display string is the glyph
  returned by `nerd-icons-icon-for-url`
- **AND** the user sees a github mark icon in the buffer

#### Scenario: Relative file path resolves via icon-for-file

- **WHEN** an inline link `[config](./modules/init.el)` is
  decorated
- **THEN** the url-side overlay's display string is the glyph
  returned by `nerd-icons-icon-for-file` applied to `init.el`

### Requirement: Reference link resolution

The system SHALL maintain a buffer-local reference-definition
alist mapping label → (url, title-attr). The alist SHALL be
populated by scanning the buffer for `^ {0,3}\[label\]: url`
definitions and SHALL be invalidated and recomputed at the start
of each rebuild. When duplicate definitions exist for a label,
the first definition in buffer order SHALL win, matching
`markdown-reference-definition`.

For each reference-style link the system SHALL look up the URL via
this alist and resolve the icon from that URL using the same
branches as URL-side icon rendering.

The system SHALL NOT decorate a reference link whose label has no
matching definition; the raw `[title][label]` text remains visible
and any `markdown-missing-link-face` applied by markdown-mode is
preserved.

#### Scenario: Collapsed reference resolves through alist

- **WHEN** the buffer contains `[design][]` and elsewhere
  `[design]: ./docs/adr-001.md`
- **THEN** the url-side icon is resolved through
  `nerd-icons-icon-for-file` on `adr-001.md`

#### Scenario: Broken reference is not decorated

- **WHEN** the buffer contains `[design][missing]` with no
  matching `[missing]: …` definition
- **THEN** no overlay is created for this link
- **AND** the raw bracketed text remains visible

### Requirement: Suppression of built-in URL composition

The system SHALL suppress markdown-mode's built-in
`compose-region` URL collapsing while `gfm-links-mode` is enabled
in a buffer, so that the built-in's URL-glyph composition does not
coexist with the new overlays.

The suppression SHALL be implemented via `:around` advice on
`markdown-fontify-inline-links` and
`markdown-fontify-reference-links`. The advice SHALL be installed
at module load and SHALL be inert (passing through to the original
function) in any buffer where `gfm-links-mode` is not active. When
`gfm-links-mode` is active, the advice SHALL invoke the original
function with `markdown-hide-urls` let-bound to nil, so all face
application proceeds normally but the `compose-region` branch is
skipped.

#### Scenario: Compose region branch skipped when mode is on

- **WHEN** `gfm-links-mode` is on and `markdown-fontify-inline-links`
  runs on a region containing a decorated link
- **THEN** no `composition` text property is applied to the URL
  region by the built-in fontifier

#### Scenario: Compose region branch runs in other buffers

- **WHEN** a buffer has `markdown-hide-urls` set to t but
  `gfm-links-mode` is not enabled
- **THEN** the built-in compose-region collapse proceeds unchanged

### Requirement: Whole-link cursor reveal

The system SHALL reveal the entire link (both the title-side and
url-side overlays together) to raw source whenever point lands
anywhere inside either overlay in the selected window, and SHALL
restore the decoration when point leaves both overlays. Title-side
and url-side overlays of the same link SHALL share a common
identifier so the reveal hook can find the partner overlay.

#### Scenario: Point on the title reveals the URL too

- **WHEN** point moves onto the visible label of a decorated
  inline link
- **THEN** the title-side overlay's display property is suppressed
- **AND** the url-side overlay's display property is also
  suppressed
- **AND** the raw `[title](url)` text is visible until point
  leaves the link region

#### Scenario: Reveal is per-window

- **WHEN** the same buffer is shown in two windows and point is
  inside a link in one window only
- **THEN** the link reveals only in that window
- **AND** the link remains decorated in the other window

### Requirement: RET follows the link when point is on the decoration

The system SHALL bind `RET` to `gfm-links-follow-link-at-point`
through the title-side overlay's `keymap` property, so the binding
applies only when point is inside a decorated link region. Off any
decorated link, `RET` SHALL fall through to the existing global
binding (`markdown-enter-key`). `gfm-links-follow-link-at-point`
SHALL invoke `markdown--browse-url` on the resolved URL.

#### Scenario: RET on a link follows the URL

- **WHEN** point is on a decorated inline link
  `[Anthropic](https://anthropic.com)` and the user presses RET
- **THEN** `markdown--browse-url` is invoked with
  `https://anthropic.com`

#### Scenario: RET off a link falls through to markdown-enter-key

- **WHEN** point is on a list item with no decorated link at point
  and the user presses RET
- **THEN** `markdown-enter-key` runs (smart list-item continuation)

### Requirement: Reference goto-definition via xref

The system SHALL register an `xref-backend-functions` entry for
markdown buffers with `gfm-links-mode` enabled. The backend SHALL
recognise reference-style links at point (full, collapsed, or
shortcut forms) and SHALL return an `xref-item` pointing at the
`[label]: …` definition line. The backend SHALL return nil for any
other position, allowing other xref backends to handle the call.

#### Scenario: M-. jumps to definition line

- **WHEN** point is on the title region of a reference link
  `[design][adr-001]` and the user invokes `xref-find-definitions`
- **THEN** point moves to the line beginning with `[adr-001]:`

#### Scenario: Backend defers off reference link

- **WHEN** point is on an inline link (not a reference link) and
  the user invokes `xref-find-definitions`
- **THEN** the gfm-links xref backend returns nil
- **AND** other xref backends are consulted

### Requirement: Eldoc URL exposure

The system SHALL register a buffer-local
`eldoc-documentation-functions` entry that returns a one-line
string of the resolved URL (and the inline title attribute when
present) whenever point is on a decorated link, and returns nil
otherwise. The system SHALL NOT modify
`eldoc-documentation-strategy`; composition with other providers
follows the user's existing strategy.

#### Scenario: Eldoc surfaces the URL when point is on a link

- **WHEN** point is on a decorated inline link
  `[Anthropic](https://anthropic.com "official")`
- **THEN** the eldoc function returns a string including
  `https://anthropic.com` and `official`

#### Scenario: Eldoc returns nil when point is off any link

- **WHEN** point is on plain prose with no decorated link
- **THEN** the eldoc function returns nil
- **AND** other eldoc providers are not blocked

### Requirement: Overlay decoration does not skew column widths

The system SHALL extend `gfm-tables--visible-width--compute` so
its caller can pass an optional source buffer and `(beg, end)`
range over which overlays in that buffer are also consulted for
width-affecting properties. When source-buffer arguments are
provided, the walker SHALL honour `display`, `invisible`, and
`composition` properties on overlays intersecting the range using
the same rules it currently applies to text properties. When the
new arguments are nil, the walker SHALL behave exactly as before.

`gfm-tables--fontify-cell` (and any code path that measures a
cell's visible width) SHALL measure cell width against the source
buffer overlays, so a table cell containing a decorated link
renders with a column width that matches the decoration's visible
width — not the underlying raw markdown width.

#### Scenario: Cell containing a decorated link aligns with the box

- **WHEN** a table cell contains a decorated inline link
  `[Anthropic](https://anthropic.com)` and the cell is rendered
  inside a decorated table
- **THEN** the column width is computed against the visible
  width of `Anthropic ` plus the URL icon — not against the
  raw markdown length
- **AND** the row's closing `│` aligns with the box's right edge

#### Scenario: Existing callers without overlay args still work

- **WHEN** code outside the new module calls
  `gfm-tables--visible-width` with the old single-argument form
- **THEN** the function returns the same result as before this
  change

### Requirement: Per-window link rendering

The system SHALL render link decoration overlays per-window using
the same window-property mechanism the other GFM decorators use,
so multiple windows showing the same buffer at different points
each see independent decoration and reveal state. Title-side and
url-side overlays SHALL each carry a `window` property scoped to
one window.

#### Scenario: Two windows show different reveal state

- **WHEN** the same markdown buffer is shown in two windows, with
  point on a link in window A and elsewhere in window B
- **THEN** window A shows the raw link source
- **AND** window B shows the decorated link

### Requirement: Link debounced rebuild

The system SHALL rebuild link overlays on a debounced timer
following buffer changes, mirroring the debounced-rebuild pattern
in `gfm-callouts-mode`. The rebuild SHALL recompute the
reference-definition alist before re-creating overlays. The
rebuild SHALL be narrowing-resilient: rebuilding within a
narrowed region SHALL produce the same final overlay set as
rebuilding the widened buffer, with no leaked overlays outside
the narrowed range.

#### Scenario: Rebuild after editing a definition line

- **WHEN** a `[label]: url` definition is edited in the buffer
- **THEN** the debounced rebuild fires
- **AND** every reference link to that label re-resolves to the
  new URL

#### Scenario: Rebuild while narrowed leaves no zombies

- **WHEN** the buffer is narrowed to a region containing some but
  not all decorated links, the rebuild runs, and the buffer is
  widened and rebuilt again
- **THEN** the overlay set after the widen-rebuild matches the
  overlay set produced by a clean widened rebuild
- **AND** no overlays from the narrowed-rebuild remain outside
  the narrowed range
