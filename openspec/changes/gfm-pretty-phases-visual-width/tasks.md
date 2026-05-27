## 1. Engine schema + visual-width primitives

- [x] 1.1 Add `:phase` slot to the decorator struct in
      `lisp/gfm/gfm-pretty-engine.el`; accept `:phase` keyword in
      `gfm-pretty-define-decorator`; default to `containers` when
      unspecified; validate value is one of
      `(atoms containers overlays)`
- [x] 1.2 Add `gfm-pretty--decorators-by-phase` helper that returns the
      decorator list phase-ordered (`atoms → containers → overlays`)
      with intra-phase order unspecified
- [x] 1.3 Route every `dolist` over `gfm-pretty--decorators` /
      `gfm-pretty--enabled-decorators` through the phase-ordered
      helper (scheduled rebuild, after-change dirty propagation,
      lifecycle enable/disable, prioritised window rebuild)
- [x] 1.4 Implement `gfm-pretty--visual-line-width LBEG LEND` walking
      via `next-overlay-change` + `next-property-change`; honour
      overlay/text-prop `display` (strings + cell-derivable stretch
      glyphs) and `invisible`; fall back to `string-width` of buffer
      chars on unknown specs
- [x] 1.5 Implement `gfm-pretty--visual-max-line-width BEG END
      &optional INDENT` iterating lines from BEG to END (does not
      cross newlines mid-line)
- [x] 1.6 Implement `gfm-pretty--visualised-string LBEG LEND` mirroring
      the walker but emitting a propertised string with
      `gfm-pretty-atomic` copied forward from contributing
      overlays/text-props
- [x] 1.7 Update `gfm-pretty--simulate-wrap` in
      `lisp/gfm/gfm-pretty-borders.el` to treat space characters
      inside a `gfm-pretty-atomic` text-prop span as non-wrap-points
- [x] 1.8 Promote the three new helpers to public names per the spec:
      `gfm-pretty-visual-line-width`,
      `gfm-pretty-visual-max-line-width`,
      `gfm-pretty-visualised-string`

## 2. Atoms phase: `links`

- [x] 2.1 Declare `:phase 'atoms` on the `gfm-pretty-define-decorator
      'links` form in `lisp/gfm/gfm-pretty-links.el`
- [x] 2.2 Tag every `display`-bearing overlay / text-property
      contributed by links with `gfm-pretty-atomic t` over the
      visually-atomic extent (icon glyph + label string; whole-link
      hidden-URL display)

## 3. Containers phase: `callouts`

- [x] 3.1 Declare `:phase 'containers` on the
      `gfm-pretty-define-decorator 'callouts` form in
      `lisp/gfm/gfm-pretty-callouts.el`
- [x] 3.2 Replace `gfm-pretty--max-line-width body-beg-pos end 2` in
      `gfm-pretty-callouts--apply-block-display` with
      `gfm-pretty-visual-max-line-width body-beg-pos end 2`
- [x] 3.3 Replace the per-body-line `line-content-w` raw-char
      computation with `gfm-pretty-visual-line-width lbeg lend` minus
      the 2-cell `> ` prefix; recompute `overflow-p` from the visual
      value
- [x] 3.4 In `gfm-pretty-callouts--right-after-overflow`, build the
      input string via `gfm-pretty-visualised-string lbeg lend`
      instead of `buffer-substring-no-properties`, prepending the
      `│ ` before-string contribution
- [x] 3.5 Drop the wrap-simulation call in the non-overflow branch
      (visual width ≤ content budget); rely on
      `gfm-pretty-callouts--right-after` for fixed `align-to`
      placement
- [x] 3.6 Audit `gfm-pretty-callouts--callout-top-strings` /
      `gfm-pretty-callouts--callout-bottom-string` for residual
      raw-char measurements (`string-width title`); keep where they
      operate on already-known glyph strings

## 4. Containers phase: `fences`

- [x] 4.1 Declare `:phase 'containers` on the
      `gfm-pretty-define-decorator 'fences` form in
      `lisp/gfm/gfm-pretty-fences.el`
- [x] 4.2 Audit fences for `gfm-pretty--max-line-width` /
      `last-visual-col` / `buffer-substring-no-properties` width
      use; switch to visual primitives where the input is buffer
      content (not already a known glyph string)
- [x] 4.3 Where the fence body uses `gfm-pretty--simulate-wrap`, feed
      it `gfm-pretty-visualised-string` of the body range

## 5. Containers phase: `tables`

- [x] 5.1 Declare `:phase 'containers` on the
      `gfm-pretty-define-decorator 'tables` form in
      `lisp/gfm/gfm-pretty-tables.el`
- [x] 5.2 Audit tables' existing visual-width walker (currently
      hand-rolled) for overlaps with `gfm-pretty-visual-line-width`;
      consolidate onto the engine primitive where shapes match;
      retain table-specific cell-walk logic where it does not

## 6. Overlays phase: `blockquotes`, `hrule`, `link-previews`

- [x] 6.1 Declare `:phase 'overlays` on
      `gfm-pretty-define-decorator 'blockquotes`
- [x] 6.2 Declare `:phase 'overlays` on
      `gfm-pretty-define-decorator 'hrule`
- [x] 6.3 Declare `:phase 'overlays` on
      `gfm-pretty-define-decorator 'link-previews`

## 7. Delete the raw-char width helper

- [x] 7.1 `git grep gfm-pretty--max-line-width` and confirm no
      remaining callers
- [x] 7.2 Remove `gfm-pretty--max-line-width` from
      `lisp/gfm/gfm-pretty-engine.el`
- [x] 7.3 Byte-compile the gfm-pretty modules; resolve any caller
      that surfaces

## 8. Tests

- [x] 8.1 Add a test to the callouts test suite that opens a buffer
      with a callout body line dominated by a prettified inline
      link, enables `gfm-pretty-mode`, and asserts the box width is
      `min(text-width, max(80, visual-width + 4))`
- [x] 8.2 Add a test that asserts the right-edge `│` of the same
      body line sits at `align-to (box-width - 2)`, exercising the
      non-overflow short-circuit
- [x] 8.3 Add a test for the overflow path: a body line whose
      visual width exceeds the content budget; assert the `│` lands
      at the box's right edge on the final wrapped visual row
- [x] 8.4 Add an engine-level test that registers two stub
      decorators (one atom, one container) and asserts the atom's
      `:apply-block-fn` runs before the container's during a single
      rebuild pass
- [x] 8.5 Add a wrap-simulator test: input a string with
      `gfm-pretty-atomic` over a `"icon label phrase"` span and a
      width that lands inside the span; assert the returned wrap
      position is at the span boundary, not inside
- [x] 8.6 Add a window-resize test: a callout-with-link buffer
      resized narrower; assert the box rebuilds at the new
      `text-width` while links overlays survive

## 9. Verification

- [x] 9.1 Run `make test`
- [x] 9.2 Open `openspec/changes/gfm-pretty-phases-visual-width/`
      contents in a markdown buffer with `gfm-pretty-mode` enabled
      and visually confirm the repro case from the proposal renders
      correctly
- [x] 9.3 Run `openspec validate gfm-pretty-phases-visual-width`
