# lang-markdown Specification

## Purpose

Compose GitHub Flavored Markdown editing into the running Emacs config:
file associations, `major-mode-remap-alist` mapping, the
`markdown-code-lang-modes` enumeration shared with `gfm-pretty`,
`apheleia` formatter wiring for `gfm-mode-local-vars-hook`, the
`+markdown-tab-dwim` insert-state binding, two `markdown-mode` advices
(`+markdown--memoise-lang-mode`, `+markdown--clamp-extend-region`),
and local-leader bindings for `markdown-toggle-url-hiding`,
`markdown-insert-footnote`, and `markdown-narrow-to-subtree`.

Overlay decoration, font-lock callout fontification, callout faces,
header-face weight styling, and `markdown-blockquote-face`
neutralisation are out of scope here — see `gfm-pretty/spec.md`.

---

## Requirements

### Requirement: Visual behaviour delegated to gfm-pretty

The `lang-markdown` axis SHALL NOT define visual-behaviour
requirements for GFM buffers.  Overlay decoration, font-lock callout
fontification, callout faces, header-face weight styling, and
`markdown-blockquote-face` neutralisation are defined exclusively
under the `gfm-pretty` axis.

`modules/lang-markdown/` retains responsibility for:

- File associations (`auto-mode-alist`) routing `.md` / `.markdown` /
  `.mkd` / `.mdown` / `.mkdn` and `/prompt` files to `gfm-mode`.
- `major-mode-remap-alist` mapping `markdown-mode` → `gfm-mode`.
- `markdown-code-lang-modes` enumeration (used by `markdown-mode` for
  native fontification AND read by the `gfm-pretty` fences decorator
  for icon lookup; the variable lives in `markdown-mode` but its value
  is configured here).
- `apheleia` formatter wiring for `gfm-mode-local-vars-hook`
  (`deno-markdown` or `prettier-markdown`).
- `+markdown-tab-dwim` command and its insert-state binding.
- `markdown-get-lang-mode` memoise advice (`+markdown--memoise-lang-mode`).
- `markdown-syntax-propertize-extend-region` clamp advice
  (`+markdown--clamp-extend-region`).
- Local-leader bindings for `markdown-toggle-url-hiding`,
  `markdown-insert-footnote`, and `markdown-narrow-to-subtree`.

#### Scenario: Opening a markdown file routes to gfm-mode

- **WHEN** the user opens a file named `notes.md`
- **THEN** `gfm-mode` is the major mode
- **AND** `gfm-pretty-mode` is enabled via `gfm-mode-hook` (defined in
  `gfm-pretty`)

#### Scenario: Visual decoration concerns are out of scope here

- **GIVEN** a proposal adding a new GFM overlay decorator
- **WHEN** spec placement is decided
- **THEN** the requirement SHALL be filed under
  `openspec/specs/gfm-pretty/spec.md`
- **AND** `openspec/specs/lang-markdown/spec.md` SHALL NOT gain a
  visual-behaviour requirement

#### Scenario: Formatter wiring stays here

- **GIVEN** an edit to the `apheleia-formatters` registration for
  markdown
- **WHEN** spec placement is decided
- **THEN** the change targets `openspec/specs/lang-markdown/spec.md`

### Requirement: Adaptive wrap-prefix on visual-line continuation rows

GFM buffers SHALL enable `adaptive-wrap-prefix-mode` such that wrapped visual lines inherit the originating line's leading-whitespace indent.

The mode is activated via `gfm-mode-hook` alongside `visual-line-mode`. The `adaptive-wrap` package is declared as a project dependency in `modules/lang-markdown/packages.eld`.

Overlay-set `wrap-prefix` properties installed by `gfm-pretty` decorators (callouts, fences, tables) MUST continue to take effect inside their respective block ranges; `adaptive-wrap`'s text-property `wrap-prefix` applies only where no overlay supersedes it.

#### Scenario: Nested list item continuation aligns under content column

- **GIVEN** a GFM buffer containing a nested list item whose body text is hard-wrapped to a width narrower than the window and indented to the bullet's content column in the source
- **WHEN** the buffer is rendered in a window narrower than the source's hard-wrap column, forcing visual-line wrapping
- **THEN** continuation visual lines of that list item SHALL be prefixed to match the source line's leading-whitespace column, not column 0

#### Scenario: Blockquote continuation preserves quote-marker indent

- **GIVEN** a GFM buffer containing a blockquote line that exceeds the window width
- **WHEN** the line wraps under `visual-line-mode`
- **THEN** the continuation row SHALL be indented to match the blockquote line's leading whitespace

#### Scenario: gfm-pretty decorator wrap-prefix is unaffected

- **GIVEN** a GFM buffer containing a callout and a fenced code block — each producing overlay `wrap-prefix` via its `gfm-pretty` decorator
- **WHEN** `adaptive-wrap-prefix-mode` is enabled in the same buffer
- **THEN** the callout left bar and fence borders SHALL continue to render with their decorator-supplied wrap-prefix on continuation rows

#### Scenario: Disabling adaptive-wrap restores flush continuation

- **GIVEN** a GFM buffer with `adaptive-wrap-prefix-mode` enabled
- **WHEN** the user toggles `adaptive-wrap-prefix-mode` off
- **THEN** continuation visual lines outside `gfm-pretty` decorator ranges SHALL fall back to column 0
- **AND** decorator-managed ranges SHALL continue to render their overlay `wrap-prefix` unchanged
