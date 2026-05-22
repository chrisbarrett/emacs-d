## Why

Plain GFM blockquotes (`> ` lines without a `[!TYPE]` marker) render with the bare `>` character plus `markdown-mode`'s `wrap-prefix "> "` text property, so wrapped continuation rows show an extra `>` on the left. The effect reads as visual clutter rather than the continuous left rail that modern email and chat clients use to mark quoted prose. Callouts already solve this for typed blockquotes; plain blockquotes deserve the same treatment.

## What Changes

- Add a new `blockquotes` decorator under `gfm-pretty` that draws a single-character left rail (`│`) over plain blockquote blocks.
- Per source line, swap the leading `> ` (or bare `>`) with `│ ` (or `│`) via a display overlay.
- Per source line, override `markdown-mode`'s `wrap-prefix "> "` with an overlay `wrap-prefix "│ "` so wrapped visual rows continue the rail.
- Detect plain blockquote blocks by subtracting callout block ranges from the set of maximal `^>`-prefixed runs, so the callouts decorator keeps ownership of typed blockquotes.
- Reveal the source `> ` per window when point or the active region overlaps the block (mirrors callouts' reveal contract).
- Adjacent blockquote paragraphs separated by a blank line render as two distinct rails.
- Add a `gfm-pretty-blockquotes-rail-face` inheriting `shadow` for the rail glyph.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: adds blockquotes decorator registration, discovery, rendering, reveal, narrowing-resilient teardown, and scoped post-edit rebuild requirements alongside the existing callouts / fences / tables decorators.

## Impact

- New file `lisp/gfm/gfm-pretty-blockquotes.el` (decorator).
- New tests in `lisp/gfm/gfm-pretty-tests.el` (or sibling file).
- `modules/lang-markdown/init.el` wires the decorator on `gfm-mode` / `markdown-mode` enable.
- No change to `gfm-pretty-engine.el`, `gfm-pretty-borders.el`, or other decorators — the new decorator plugs in via the existing `gfm-pretty-define-decorator` seam.
- No keybinding changes. No new external dependencies.
