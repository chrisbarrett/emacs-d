## ADDED Requirements

### Requirement: URL target classification

The links decorator SHALL classify every link's *resolved* URL into
one of three target classes: `web`, `anchor`, or `file`. The
classification SHALL drive title-side face selection, url-side
overlay presence, and RET dispatch.

Classification rules, applied in order:

| Prefix on resolved URL                  | Class    |
| :-------------------------------------- | :------- |
| `#`                                     | `anchor` |
| `./`, `../`, `/`, or `file:`            | `file`   |
| any other scheme (`http://`, `https://`, …) or any other content | `web`    |

Reference-style links (full, collapsed, shortcut) SHALL classify via
the URL recorded in their resolved `[label]:` definition, not via
the source shape. Autolinks and bare URLs SHALL classify via their
extracted target.

The classifier SHALL be a pure function of the URL string; the
resulting class SHALL be recorded on both the title-side and (when
present) url-side overlays as an overlay property so consumers
(RET, eldoc) can read it without re-classifying.

#### Scenario: Inline anchor link

- **GIVEN** `[Setup](#setup)`
- **THEN** the link classifies as `anchor`

#### Scenario: Reference link to a relative path

- **GIVEN** `[ops][tg-auth-sh]` with a definition line
  `[tg-auth-sh]: ./_scripts/tg-auth.sh`
- **THEN** the link classifies as `file`

#### Scenario: Reference link to a web URL

- **GIVEN** `[catalog][cat]` with `[cat]: https://github.com/x/y`
- **THEN** the link classifies as `web`

#### Scenario: Absolute path

- **GIVEN** `[etc](/etc/hostname)`
- **THEN** the link classifies as `file`

### Requirement: Local-link face customisation points

The links decorator SHALL expose two faces for local target classes:
`gfm-pretty-links-anchor-face` and `gfm-pretty-links-file-face`. Both
SHALL default to inheriting `markdown-link-face` with `:underline
nil` so local link titles are distinguishable from external
underlined links. Themes MAY override either independently.

#### Scenario: Default faces strip underline

- **WHEN** the decorator loads with no theme overrides
- **THEN** `gfm-pretty-links-anchor-face` and
  `gfm-pretty-links-file-face` resolve to no underline

## MODIFIED Requirements

### Requirement: Title-side overlay rendering

The links decorator's `:apply-block-fn` SHALL replace the
`[title]` span (brackets included) with the title text in a face
chosen by the link's target class:

| Class    | Face                            |
| :------- | :------------------------------ |
| `web`    | `gfm-pretty-links-title-face`   |
| `anchor` | `gfm-pretty-links-anchor-face`  |
| `file`   | `gfm-pretty-links-file-face`    |

`gfm-pretty-links-title-face` SHALL continue to default to
`markdown-link-face`. The overlay is per-window.

#### Scenario: Web link uses title face

- **GIVEN** `[Anthropic](https://anthropic.com)`
- **THEN** the title-side overlay displays `Anthropic` in
  `gfm-pretty-links-title-face`

#### Scenario: Anchor link uses anchor face

- **GIVEN** `[Setup](#setup)`
- **THEN** the title-side overlay displays `Setup` in
  `gfm-pretty-links-anchor-face`

#### Scenario: File link uses file face

- **GIVEN** `[ops](./scripts/x.sh)`
- **THEN** the title-side overlay displays `ops` in
  `gfm-pretty-links-file-face`

### Requirement: URL-side icon rendering

The links decorator's `:apply-block-fn` SHALL replace the URL span
(`(url)`, `[label]`, the autolink span, …) with a single
nerd-icons glyph resolved from the target host or scheme, ONLY when
the link's target class is `web`. For `anchor` and `file` classes
the url-side overlay SHALL be omitted; the URL span renders raw (or
collapses via markdown-mode's own composition if the decorator's
advice is not active). When `nerd-icons` is unavailable, the
URL-side overlay SHALL be omitted for `web` links too (URL shows
raw).

#### Scenario: Github URL

- **GIVEN** `[code](https://github.com/user/repo)` and `nerd-icons`
  available
- **THEN** the URL-side renders as the GitHub icon

#### Scenario: Unknown host

- **GIVEN** a link to an unrecognised host (web class)
- **THEN** the URL-side renders as a generic web icon

#### Scenario: Anchor link omits icon

- **GIVEN** `[Setup](#setup)`
- **THEN** no url-side overlay is created
- **AND** the `(#setup)` span is left to markdown-mode's own
  rendering

#### Scenario: File link omits icon

- **GIVEN** `[ops](./scripts/x.sh)`
- **THEN** no url-side overlay is created

### Requirement: RET follows the link when point is on the decoration

The links decorator SHALL install an overlay keymap binding `RET` to
a class-dispatched follow handler. The handler SHALL read the link's
target class and URL from the overlay under point and act per class:

| Class    | Action                                                                  |
| :------- | :---------------------------------------------------------------------- |
| `web`    | call `markdown--browse-url` on the URL                                  |
| `anchor` | jump to the heading whose generated slug matches the anchor portion    |
| `file`   | `find-file` on the URL, expanded relative to `buffer-file-name`'s directory (or `default-directory` when the buffer has no file) |

The binding SHALL fire when point is on the rendered overlay.

#### Scenario: RET on web link

- **WHEN** point is on a rendered `web` link's title and user presses
  `RET`
- **THEN** `markdown--browse-url` opens the URL

#### Scenario: RET on anchor link

- **GIVEN** a heading `## Setup Steps` in the buffer
- **AND** point is on a rendered `[go](#setup-steps)` link
- **WHEN** user presses `RET`
- **THEN** point moves to the `Setup Steps` heading

#### Scenario: RET on anchor with no matching heading

- **GIVEN** a rendered `[go](#missing)` link
- **AND** no heading in the buffer has slug `missing`
- **WHEN** user presses `RET`
- **THEN** the handler raises `user-error` with a message naming the
  missing anchor

#### Scenario: RET on file link

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer
  whose file lives at `/repo/README.md`
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/repo/scripts/x.sh`

#### Scenario: RET on file link in fileless buffer

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer with
  no `buffer-file-name`
- **WHEN** user presses `RET`
- **THEN** `find-file` resolves the path against `default-directory`

### Requirement: Eldoc link exposure

The links decorator's `:on-enable` SHALL install an eldoc
documentation function that surfaces the *formatted source* of the
link under point — not the bare URL. The returned string SHALL be a
propertised reconstruction of the link's raw markdown source, with:

- `shadow` face on scaffolding characters (`[`, `]`, `(`, `)`)
- the class-appropriate title face on the title span (`web` →
  `gfm-pretty-links-title-face`, `anchor` →
  `gfm-pretty-links-anchor-face`, `file` →
  `gfm-pretty-links-file-face`)
- `markdown-url-face` on the URL span

For reference-shape links, the reconstruction SHALL be `[title][label]`
(or `[label]` for shortcut references) with `shadow` on brackets and
the class face on both title and label spans. Inline title
attributes (`"Anthropic Home"`) SHALL render after the URL, in
italics.

The function SHALL return nil off any decorated link so other eldoc
providers are not blocked.

#### Scenario: Eldoc on inline web link

- **GIVEN** point on a rendered `[Anthropic](https://anthropic.com)`
  link
- **THEN** eldoc displays `[Anthropic](https://anthropic.com)` with
  `shadow` on the brackets and parens,
  `gfm-pretty-links-title-face` on `Anthropic`, and
  `markdown-url-face` on `https://anthropic.com`

#### Scenario: Eldoc on reference file link

- **GIVEN** point on `[ops][tg-auth-sh]` with definition
  `[tg-auth-sh]: ./scripts/tg-auth.sh`
- **THEN** eldoc displays `[ops][tg-auth-sh]` with `shadow` on the
  brackets and `gfm-pretty-links-file-face` on `ops` and
  `tg-auth-sh`

## REMOVED Requirements

### Requirement: Whole-link cursor reveal

**Reason:** Cursor-driven reveal causes layout reflow as point
moves across paragraphs, fighting reading flow. The same need
(inspect the URL without disturbing rendering) is now served by the
formatted eldoc surface.

**Migration:** The `gfm-pretty-links-revealable` overlay property is
no longer set; the `gfm-pretty-links-id` partner-overlay identifier
is no longer set; the links decorator no longer registers a
`:reveal-fn` with the engine. Consumers needing the raw source
toggle `markdown-hide-urls` (already bound under the local leader)
or disable `gfm-pretty-mode`.
