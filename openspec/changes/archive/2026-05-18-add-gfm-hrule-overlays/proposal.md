## Why

GFM markdown thematic breaks (`---` on a line by themselves, 3+
dashes) currently render as raw dashes, which is visually weak and
inconsistent with the rest of the configuration's typographic
treatment of markdown.  Callouts, code fences, and tables already
draw with unicode box-drawing characters at a window-aware width;
horizontal rules should join them.  A full-width unicode separator
makes section boundaries unambiguous at a glance in long markdown
documents (release notes, change logs, presentation slides).

## What Changes

- New minor mode `gfm-hrule-mode` (sibling to `gfm-callouts-mode`,
  `gfm-code-fences-mode`, `gfm-tables-mode`) that replaces qualifying
  dash-only HR lines with a single unicode horizontal bar.
- Per-window display: the bar's width tracks each window currently
  showing the buffer, reusing the
  `gfm-block-borders--available-width` helper used by callouts and
  tables.
- Discovery only matches lines that are *bona fide* GFM thematic
  breaks: 3+ consecutive `-` characters at BOL with optional trailing
  whitespace, where markdown-mode has already determined the line is
  not a setext-2 heading underline and not inside a fenced or
  indented code block.  Use the `markdown-hr` text property set by
  `markdown-match-hr` as the source of truth so we don't re-implement
  that classification.
- Cursor-aware reveal: when point lands on the HR line, the overlay
  is suppressed so the source `---` is editable.  Display restores
  on leaving the line.
- Auto-enable: hook `gfm-hrule-mode` into `gfm-mode-hook` alongside
  the other GFM decorators.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `lang-markdown`: adds a new family of requirements covering HR
  discovery, rendering, per-window width, reveal-on-cursor,
  debounced/scoped rebuild, and narrowing safety — mirroring the
  shape of the existing callout and code-fence sections.

## Impact

- New module file: `modules/lang-markdown/lib/+gfm-hrule.el`.
- `modules/lang-markdown/init.el` — hook `gfm-hrule-mode` into
  `gfm-mode-hook`; declare a face customisation if needed.
- `modules/lang-markdown/tests.el` — new test group for HR
  rendering, reveal, per-window width, narrowing safety.
- Reuses shared primitives from
  `modules/lang-markdown/lib/+gfm-block-borders.el` (overlay
  registry, scheduler, reconciler, width helper).
- No external dependencies.
