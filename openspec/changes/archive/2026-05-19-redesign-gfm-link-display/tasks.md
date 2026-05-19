## 1. URL classifier (test-first)

- [x] 1.1 Write failing tests in `lisp/gfm/gfm-pretty-tests.el` for a
      pure `gfm-pretty-links--classify-url` returning `'web`,
      `'anchor`, or `'file` against the prefix matrix in
      design.md (cases: `#x`, `./x`, `../x`, `/x`, `file:x`,
      `https://x`, `http://x`, `mailto:x`, autolink-extracted)
- [x] 1.2 Implement `gfm-pretty-links--classify-url` using `rx`
- [x] 1.3 Extend the `gfm-pretty-links--link` cl-struct with a
      `:class` slot; thread the classifier through every scan
      site (`scan-inline`, `scan-reference`, `scan-shortcut`,
      `scan-autolinks`, `scan-bare-urls`, `scan-wiki`)

## 2. Local-link faces

- [x] 2.1 Write a failing test asserting both
      `gfm-pretty-links-anchor-face` and
      `gfm-pretty-links-file-face` resolve with `:underline nil`
      and inherit `markdown-link-face`
- [x] 2.2 Define both faces with `defface`

## 3. Title-side face selection

- [x] 3.1 Write failing tests asserting the title overlay's
      `display` string carries the per-class face (web, anchor,
      file)
- [x] 3.2 In `gfm-pretty-links--make-overlay`, pick the face from
      `(gfm-pretty-links--link-class record)`; record class on the
      overlay as the `gfm-pretty-links-class` property

## 4. URL-side suppression for local links

- [x] 4.1 Write failing tests asserting anchor and file links
      produce only one overlay per link (title-side), while web
      links produce two
- [x] 4.2 Add a class guard in `gfm-pretty-links--decorate-link`
      skipping `make-url-overlay` when class ∈ `{anchor, file}`

## 5. Remove cursor-driven reveal

- [x] 5.1 Delete reveal-coverage tests under `Whole-link cursor
      reveal` in `lisp/gfm/gfm-pretty-tests.el`
- [x] 5.2 Remove `gfm-pretty-links--reveal`,
      `gfm-pretty-links--link-id-at`,
      `gfm-pretty-links--restore-overlay`,
      `gfm-pretty-links--id-counter`, and
      `gfm-pretty-links--next-id`
- [x] 5.3 Stop setting `gfm-pretty-links-revealable`,
      `gfm-pretty-links-id`, and `gfm-pretty-links-saved-display`
      properties on overlays
- [x] 5.4 Drop `:reveal-fn` from the `gfm-pretty-define-decorator`
      call
- [x] 5.5 Update the eldoc / xref helpers
      (`gfm-pretty-links--overlay-at-point`) to find the overlay
      by its `gfm-pretty-links-class` property (or a new
      `gfm-pretty-links-overlay` marker prop) instead of
      `gfm-pretty-links-revealable`

## 6. RET class dispatch

- [x] 6.1 Write failing tests for
      `gfm-pretty-links-follow-link-at-point` covering: web URL →
      `markdown--browse-url`; anchor with matching heading → point
      moves; anchor with no match → `user-error`; file path with
      file-backed buffer → `find-file` with correct expanded path;
      file path in fileless buffer → `find-file` against
      `default-directory`
- [x] 6.2 Implement class-dispatched RET handler in
      `gfm-pretty-links-follow-link-at-point`; helper
      `gfm-pretty-links--jump-to-anchor` walks headings via
      `markdown-heading-at-point` from `point-min`

## 7. Eldoc formatted source

- [x] 7.1 Write failing tests for the eldoc handler asserting:
      inline web link returns `[title](url)` with `shadow` on
      `[`/`]`/`(`/`)`, title face on title, `markdown-url-face`
      on URL; inline anchor link uses anchor face on title;
      reference link returns `[title][label]` with shadow on
      brackets and class face on both spans; shortcut reference
      returns `[label]`; title-attr appends ` — "x"` in italic
- [x] 7.2 Rewrite `gfm-pretty-links--eldoc-function` to build the
      propertised string from the overlay's metadata (`kind`,
      `class`, `url`, `title-attr`, `ref-label`, and the title
      string read from the title-side overlay's `display`)

## 8. Verification

- [x] 8.1 `make test-quick` passes
- [x] 8.2 `make test` (full suite) passes
- [x] 8.3 Manual check in a real markdown buffer: web, anchor, and
      `./path` links render with correct faces; url-side icon
      appears only for web; cursor motion no longer causes
      reflow; eldoc surface shows formatted source; RET dispatches
      to browser / heading / file respectively
- [x] 8.4 Confirm narrowing-regression tag still passes (`make
      test-integration` or selective run on
      `'(narrowing-regression)`)
