## Context

`gfm-pretty-link-previews` (in `lisp/gfm/gfm-pretty-link-previews.el`) renders box-bordered previews for standalone markdown source-range / diff-range links.  Its collector (`--collect-blocks`) scans for `[label](url)` matches, parses each URL via `--parse-source-link` / `--parse-diff-link`, and gates with `--standalone-link-p` (whole line is the link plus an optional list/blockquote marker).  Each surviving match becomes a `--block` struct whose `range` covers the `[label](url)` span; `--apply-block` lays a per-window display overlay over that range.

Agents and humans regularly paste references without brackets — e.g. `/abs/path/main.tf#L52-L93` standing alone on a line.  These bypass the collector entirely.  The user wants such lines to render the same preview, except when the line is inside a code fence, indented code block, or inline-code wrap.

Constraints that shape the design:

- Engine memoises `:collect-fn` by `buffer-chars-modified-tick` per decorator (`gfm-pretty--collect`, `lisp/gfm/gfm-pretty-engine.el:379-396`).  Adding a second pass to the existing collector keeps the cache singular.
- `gfm-pretty--make-display` (`engine.el:303`) is window-keyed; bare-line overlays slot in via the same machinery used for bracketed links.
- The blockquote rail decorator coexists with previews via the marker-aware continuation prefix already in `--continuation-prefix` (`link-previews.el:267-293`).  Same code path serves bare-line matches because the prefix logic keys off the line's leading characters, not the URL form.
- Fence-range detection has a prior art: `gfm-pretty-fences--open-re` / `--close-re` (`gfm-pretty-fences.el:39-50`).  Re-using the same regexes keeps the bare-link recogniser's notion of "fenced" identical to the fences decorator's.

## Goals / Non-Goals

**Goals:**

- Bare-line `<path>#L<a>[-L<b>]` and `diff:<base>...<head>[#<path>]` references render identical previews to their bracketed equivalents.
- Decoration respects markdown's preformatted contexts (``` fences, 4-space indented code blocks, inline-code wrap on a standalone line) — bare references inside them are inert.
- Marker-aware prefixes (list / blockquote) work for bare references via the existing `--continuation-prefix`.
- All behaviour rides the existing decorator's overlay registry, rebuild lifecycle, and RET-follow dispatch — no second decorator, no duplicated renderer.

**Non-Goals:**

- Decorating bare paths without `#L<n>` anchors (no preview content to show).
- Decorating bare references mid-prose (e.g. `See /abs/path#L1 for details.`) — the standalone-line gate excludes these by design, mirroring the bracketed form's behaviour.
- Exact GFM-correct indented-code-block detection (including list-continuation indents).  An "indent ≥ 4 ⇒ preformatted" approximation is acceptable.
- Sharing the fence-range cache with `gfm-pretty-fences`.  The engine's memoisation is per-decorator; cross-decorator sharing would couple modules that don't otherwise depend on each other.

## Decisions

### Decision: Augment `link-previews` collector, no new decorator

Add a second pass to `gfm-pretty-link-previews--collect-blocks`.  After the existing `[label](url)` scan, walk the buffer line-by-line and try to match each line against a bare-line shape; push matching blocks with the same payload tag (`source` or `diff`).

**Alternatives considered:**

- *Separate decorator (`bare-link-previews`)*: clean separation, but doubles overlay registries, rebuild walks, and follow-dispatch wiring.  No payoff for a feature whose rendering is byte-identical to the existing one.
- *Single regex covering both shapes*: hard to express cleanly (bracketed vs bare have different anchor characters and require different "standalone" predicates).  Two passes is easier to read and easier to gate against preformatted contexts (only bare needs the gate).

### Decision: Bare-line shape

Match against a per-line regex anchored to `bol`:

```elisp
(rx bol (* blank)
    (? (or "- " "* " "+ " "> " (: (+ digit) ". ")))
    (* blank)
    (group (+ (not (any blank "\n"))))
    (* blank) eol)
```

Group 1 is the candidate URL token.  The collector then tries `--parse-source-link` / `--parse-diff-link` on the token; the line matches only when one parser succeeds.

For source-range tokens, an extra guard requires the path component (the part before `#L`) to contain at least one `/` — this rules out basename-only false positives like `auth.rs#L1-L5` that look syntactically valid but are likely an accidental match on prose.

The leading marker is consumed inside the regex (not via `--standalone-link-p` post-stripping) because for bare lines there's no `[label](url)` span to subtract — the regex either matches the whole line or it doesn't.

### Decision: Preformatted-context exclusion shape

Build two range sets per collect, both computed once on the widened buffer:

1. **Fence ranges**: scan for matching pairs of `gfm-pretty-fences--open-re` / `--close-re`, capturing `(open-bol . close-eol)` inclusive.  Re-implement locally (~15 lines) rather than `require`-ing `gfm-pretty-fences`, so the bare-link decorator doesn't depend on the fences decorator being loaded or enabled.
2. **Indented-code-block lines**: walk lines whose leading-whitespace indent is ≥ 4 columns.  No list-continuation analysis — flat approximation.

For inline-code wrap on a standalone line, a per-line check is enough: if the line trimmed matches `` `…` `` with no other content (modulo leading marker), skip.

A bare-line candidate's line is rejected if it's inside any fence range, has indent ≥ 4, or is an inline-code-wrap line.

### Decision: Overlay range covers URL token only

The bare-line overlay's `range` is `(token-start . token-end)`, not the whole line.  This mirrors the bracketed form's range (`[label](url)` span only) and keeps the leading marker rendering in its native form to the left of the box.  The marker-aware `--continuation-prefix` then aligns the box's continuation rows under the `┌`.

### Decision: Collect-pass ordering

Run the bracketed pass first, then the bare-line pass.  Because the two regexes start with `[` (bracketed) vs `(+ (not (any blank ...)))` (bare), they're mutually exclusive on a given match — but ordering keeps the dependency clear: the bracketed pass is authoritative; the bare pass is supplementary.  No de-dup logic needed at the line level because the regexes can't both match the same span.

## Risks / Trade-offs

- **Indented-code-block approximation misses list-continuation contexts** → A 6-space-indented bare reference under a list item would be skipped (treated as code).  Mitigation: agent output rarely indents references past 4 spaces; if it becomes a real annoyance, tighten by checking whether the preceding non-blank line started a list-item with a smaller marker indent.
- **Bare-line scan adds one buffer pass per `:collect-fn` call** → The decorator's `--full-rebuild-required-p` is already `t`, so collects fire on every dirty rebuild.  The pass is O(lines × regex) — same shape as the bracketed pass.  Engine memoisation by buffer tick covers steady state.
- **Worktree-internal absolute paths break when read from another worktree** → Existing `[broken preview] … file not found` sentinel handles it.  Not a new failure mode.
- **Tree-sitter fence detection would be more robust than regex** → But it ties the decorator to `markdown-ts-mode`, which the rest of `gfm-pretty` does not require.  Regex parity with `gfm-pretty-fences` is the right level of fidelity here.
- **Inline-code recogniser is per-line, not span-aware** → A bare reference followed by `` ` `` on the same line would not match the standalone-line gate anyway, so the per-line wrap check only needs to handle the `` `…` `` -only-on-its-own-line case.  Trade simplicity for narrow coverage that matches the user's stated motivation.

## Migration Plan

None — additive feature.  Existing bracketed previews continue to work unchanged; bare-line previews come online the moment `gfm-pretty-mode` recomputes blocks (any buffer edit, or a `gfm-pretty-link-previews--rebuild` call).
