## Context

`lisp/gfm/gfm-pretty-link-previews.el` collects preview blocks in `--collect-blocks` via two passes over the widened buffer:

1. **Pass 1** — `--md-link-rx` matches `[label](url)` and parses the `url`.
2. **Pass 2** — `--bare-line-rx` matches a single non-whitespace token alone on a line (optional indent / list / blockquote marker), then `--parse-source-link` / `--parse-diff-link` parse the token.

`--parse-source-link` anchors its regexp `bos … eos`. A token wrapped in `[ ]` or `< >` therefore fails to parse: the leading delimiter is swallowed into the path group and the trailing delimiter breaks `eos`. Verified live — `[…/pattern.rs#L234-L267]` → `nil`, same token unwrapped → `("…/pattern.rs" 234 . 267)`.

Both wrapper shapes are non-standard markdown (a bare `[foo]` is a shortcut-reference link with no referent; `<…>` is an autolink only for recognised URI schemes), so GFM renders them as literal text. The decorator already recognises one non-standard shape — the unwrapped bare-line token — so wrapped shapes are a natural extension of the same convention-recognition stance.

## Goals / Non-Goals

**Goals:**

- Recognise `[<url>]` and `<<url>>` standalone references and render the same box preview as the unwrapped bare-line form.
- Cover the full wrapper (including delimiters) with the overlay so the literal `[ ]` / `< >` is replaced by the box.
- Reuse the existing standalone gate, `/`-in-path gate, preformatted-context exclusion, payload, render pipeline, RET-follow dispatch, and sentinels.

**Non-Goals:**

- Paren-wrapped `(url)` references — too collision-prone with prose.
- References without a `#L<n>` anchor (bare path hyperlinking).
- Any change to the unwrapped bare-line or `[label](url)` behaviour.

## Decisions

### Decision: unwrap in Pass 2 via a token-unwrap helper, not new regexps

Pass 2 already captures the whole bare-line token (group 1 of `--bare-line-rx`). The token for a wrapped reference is the *entire* `[…]` / `<…>` run — `--bare-line-rx` captures it verbatim because it is a contiguous non-whitespace run. So the only addition needed is: before handing the token to the parsers, try stripping one matched delimiter pair.

Add a helper — `--unwrap-token` — returning `(INNER . (START-OFFSET . END-OFFSET))` or nil, where the offsets let the caller widen the existing token bounds to keep covering the delimiters (the offsets are 0 for an unwrapped token, 1/-1 for a wrapped one). Pass 2 then parses `INNER`; on a match it builds the block with the *original* token bounds (which already include the delimiters, since `--bare-line-rx` captured them).

Rationale: keeps a single discovery path. No second scan, no new top-level regexp, no risk of double-claiming a span. The overlay-covers-wrapper requirement falls out for free because the captured token bounds already span the delimiters.

Alternative considered — a dedicated `--wrapped-line-rx` second sub-pass. Rejected: duplicates the marker-stripping and standalone logic already in Pass 2, and risks the two sub-passes both claiming the same line.

### Decision: only matched pairs, `[ ]` and `< >`

`--unwrap-token` accepts a token iff it starts with `[` and ends with `]`, or starts with `<` and ends with `>`. Mismatched (`[…>`) or single-sided tokens are left unwrapped and fall through to the existing unwrapped parse (which will reject them). This keeps the false-positive surface minimal and matches the spec's mismatched-delimiter scenario.

### Decision: `/`-gate and standalone gate apply to INNER / full token unchanged

The `/`-in-path gate (`--bare-source-path-ok-p`) runs on the parsed inner path, so a wrapped basename-only token is rejected exactly as the unwrapped form is. The standalone gate already operates on the whole captured token removed from the line, so a wrapped token in prose (`See [a/b.rs#L1-L2] here`) is non-standalone and rejected. No new gating code.

### Decision: link-reference-definition non-collision is structural, not special-cased

`[a/b.rs#L1-L2]: https://…` is a markdown LRD. `--bare-line-rx` requires the captured token to be the line's sole significant content (`(group …) (* blank) eol`). The trailing `: https://…` means the line is not token-only, so the rx never matches it. No explicit LRD check needed; a regression test pins the behaviour.

### Decision: bracketed-link precedence is preserved trivially

Pass 1 (`[label](url)`) still runs first and claims `[label](url)` spans. A wrapped reference has no `(url)`, so Pass 1 never claims it, and Pass 2's existing already-claimed-span skip is untouched.

## Risks / Trade-offs

- **Angle-wrapped false positives on generics / comparisons** (`<a/b.rs#L1-L2>` is unambiguous, but agents rarely write angle wrappers) → the inner-token must still parse as a source/diff URL *and* pass the `/`-gate and standalone gate, so a stray `<…>` in prose is rejected on multiple grounds. Net new surface is small.
- **Token bounds vs. delimiter offsets** → the overlay must cover the delimiters; getting the bounds wrong would leave a stray `[` or `>` beside the box. Mitigated by asserting overlay-covers-full-token in tests, and by reusing the already-correct `--bare-line-rx` token bounds rather than recomputing.
- **Inner token containing the delimiter char** (e.g. a path with `>` in it) → paths with `[`/`]`/`<`/`>` are pathological and already unsupported by the bare token rx; no regression.
