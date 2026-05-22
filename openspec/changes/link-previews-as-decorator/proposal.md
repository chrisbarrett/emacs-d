## Why

Source-range (`path#L<a>-L<b>`) and diff-range (`diff:B...H[#P]`)
link previews currently live entirely in `gfm-present-mode` —
they render when a slide is narrowed-in, refresh on anchor jumps,
and disappear when present mode is off.  The decoration is useful
outside presentation mode too: any markdown buffer authored as a
working doc (PR descriptions, design notes, READMEs) benefits from
inline source / diff previews.

The rendering helpers, parsers, and overlay management are already
shaped like a `gfm-pretty` decorator (per-buffer overlay list,
standalone-link gating, idle-suitable rebuild).  Extract them into
a decorator so the previews fire wherever `gfm-pretty-mode` is on.

## What Changes

- Define a new `link-previews` decorator under `gfm-pretty`.  When
  `gfm-pretty-mode` is enabled the decorator scans the buffer for
  standalone source-range / diff-range links and renders preview
  overlays — the same shape as today's present-mode output.
- Move the preview helpers (`--source-preview-display`,
  `--diff-preview-display`, `--render-link-previews`,
  `--parse-source-link`, `--parse-diff-link`,
  `--standalone-link-p`, `--abbrev-source-path`,
  `--abbrev-diff-refs`, `--read-line-range`, `--fontify-source`,
  `--box-display`, `--preview-cap`) from `gfm-present.el` to a new
  file `lisp/gfm/gfm-pretty-link-previews.el`.  Rename the
  `gfm-present--` prefix on those symbols to
  `gfm-pretty-link-previews--`; keep obsolete aliases for the
  prior names so any out-of-tree callers don't break.
- Wire decorator lifecycle through the existing
  `gfm-pretty-engine` seams: `:collect-fn` returns the standalone
  link positions; `:apply-block-fn` builds the preview overlay
  per link; `:full-rebuild-required-p` fires on any edit that
  could change the link list; refresh ties into the engine's
  generic after-change + window-configuration-change machinery
  rather than the present-mode-specific slide-change hook.
- `gfm-present-mode` no longer owns preview rendering directly.
  Its `on-enable` enables `gfm-pretty-mode` (if not already on)
  and the link-previews decorator; its `on-disable` does not
  forcibly tear down previews — they stay if `gfm-pretty-mode`
  remains on.  The slide-change / anchor-jump hooks in present
  mode call into the decorator's rebuild entry point instead of
  the soon-to-be-removed `gfm-present--render-link-previews`.
- The `gfm-pretty-toggle-decorator 'link-previews` lever lets
  users disable previews in `gfm-pretty-mode` buffers if they
  prefer raw link text.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: gains a `link-previews` decorator that renders
  source-range and diff-range link previews wherever
  `gfm-pretty-mode` is enabled.  Standalone-link gating, box
  rendering, file-not-found / invalid-range sentinel behaviour
  are preserved from the present-mode implementation.

## Impact

- New file: `lisp/gfm/gfm-pretty-link-previews.el` (the
  extracted module).  Roughly 350 lines moved from
  `gfm-present.el`.
- `lisp/gfm/gfm-present.el`: shrinks; preview helpers replaced by
  `(require 'gfm-pretty-link-previews)` and calls to the
  decorator's rebuild entry point on slide change / anchor jump.
  Obsolete aliases for the old `gfm-present--` symbols.
- `lisp/gfm/gfm-pretty-tests.el` (or a new
  `gfm-pretty-link-previews-tests.el`): existing source-preview
  / diff-preview tests rename their function calls; new tests
  cover the decorator-protocol integration (enable / disable /
  toggle / rebuild on after-change).
- `openspec/specs/gfm-pretty/spec.md`: delta adds the
  `link-previews` decorator-registration requirement, the
  rendering contract (lifted from the archived
  `box-source-preview-overlays` change), and the rebuild
  semantics.
- No new external dependencies; no `gfm-pretty-engine` changes
  needed (the existing decorator protocol covers everything).
