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
- **AND** `gfm-pretty-mode` is enabled via `gfm-mode-hook`

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
