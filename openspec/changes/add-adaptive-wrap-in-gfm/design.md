## Context

`gfm-mode` enables `visual-line-mode` via `gfm-mode-hook` (`modules/lang-markdown/init.el:34`). With visual-line wrapping on, continuation rows flush to column 0 — destroying the indent that makes nested lists, blockquotes, and indented list-item bodies legible. Existing `gfm-pretty` decorators (callouts at `lisp/gfm/gfm-pretty-callouts.el:310`, fences at `lisp/gfm/gfm-pretty-fences.el:349`) compute bespoke overlay `wrap-prefix` values inside their own block ranges, but no machinery handles wrap-prefix for plain paragraphs, list items, or blockquote bodies.

`adaptive-wrap` (GNU ELPA, present in `elpaca/cache/gnu-elpa.eld`) provides `adaptive-wrap-prefix-mode`, a buffer-local minor mode that sets the `wrap-prefix` text property line-by-line to match each line's leading-whitespace prefix. It is implemented as a `jit-lock` consumer; it does not own block-level state.

## Goals / Non-Goals

**Goals:**

- Continuation visual lines of paragraphs, list items, and blockquote bodies inherit their source line's leading-whitespace indent.
- Zero regression to `gfm-pretty` decorator rendering (callout left bar, fence borders, table borders, hrule, link folding).
- Trivial implementation surface: one hook entry + one package declaration.

**Non-Goals:**

- Virtually joining soft newlines inside a paragraph so it reflows to window width. This is the harder "Layer 2" problem and stays out of scope here.
- Custom paragraph / list / blockquote block discovery. `adaptive-wrap` operates per-line; no block model needed.
- Editing/motion command changes (`evil-respect-visual-line-mode`, etc.). Tracked as a possible follow-up; the indent feature is independent of motion semantics.

## Decisions

### Decision: Adopt `adaptive-wrap-prefix-mode` rather than build a paragraph decorator

`adaptive-wrap` is a 200-line library on GNU ELPA that already does what Layer 1 needs. Building an equivalent inside `gfm-pretty` would mean: new block collector (paragraphs / lists / blockquotes), engine integration, narrowing-safety review, tests — for a result that materially duplicates the upstream package.

**Alternatives considered:**

- _Build a `gfm-pretty-reflow` decorator now._ Rejected: large surface, none of which the indent half of the problem actually needs. Defer until Layer 2 (paragraph join) is on the table — at that point, a custom decorator is justified because `adaptive-wrap` cannot hide newlines.
- _Set `wrap-prefix` text properties from a custom `gfm-mode-hook` function._ Rejected: ends up reimplementing `adaptive-wrap` for no gain.

### Decision: Enable on `gfm-mode-hook`, not buffer-wide

`adaptive-wrap-prefix-mode` is buffer-local. Wiring it on `gfm-mode-hook` matches the existing pattern for `visual-line-mode` and `gfm-pretty-mode` in `modules/lang-markdown/init.el:33-36`. No other major mode picks up the behaviour, which avoids surprising interactions with code, configs, or org files.

### Decision: Rely on overlay-`wrap-prefix` winning over text-property `wrap-prefix`

`gfm-pretty` decorators set `wrap-prefix` as an overlay property over their block ranges. `adaptive-wrap` sets `wrap-prefix` as a text property via `jit-lock`. The Emacs display engine resolves `wrap-prefix` per character: overlay properties take precedence over text properties (`(elisp) Overlay Properties`; see `xdisp.c` `handle_single_display_spec` / wrap-prefix lookup path). Outcome:

- Inside callout / fence / table ranges → overlay `wrap-prefix` wins → existing rendering preserved.
- Everywhere else → text-property `wrap-prefix` from `adaptive-wrap` applies → new indent behaviour active.

No coordination code required.

### Decision: Keep `adaptive-wrap-extra-indent` at its default (0)

Markdown source already uses content-column indent (e.g. list item bodies live at the bullet's content column). Extra indent would over-shift. Leave the user-option at default; revisit only if real-world buffers reveal a problem.

## Risks / Trade-offs

- **Risk**: `adaptive-wrap`'s `jit-lock` participation conflicts with `markdown-mode`'s own `jit-lock` consumers, causing flicker or stale `wrap-prefix` during rapid edits. → **Mitigation**: confirm visually on a buffer with rapid edits before merging. `adaptive-wrap` has shipped on GNU ELPA for years; conflicts with `markdown-mode` are not in the bug tracker as of this write. If it bites, the rollback is one-line hook removal.

- **Risk**: Overlay-precedence assumption is wrong for some Emacs version / display configuration, breaking callout / fence / table wrap. → **Mitigation**: visual smoke-test all three decorators after enabling. The precedence rule is stable across Emacs 28+; we run 29.

- **Risk**: A future `gfm-pretty` paragraph decorator overlaps `adaptive-wrap`'s domain. → **Mitigation**: when Layer 2 lands, decide then whether `adaptive-wrap` stays (overlay > text means decorator overlays win on their ranges anyway) or is replaced. Not a concern for this change.

- **Trade-off**: Adopting an external micro-dependency vs. carrying the logic inline. Worth it: `adaptive-wrap` is on GNU ELPA, well-maintained, and 5x smaller than the inline equivalent.
