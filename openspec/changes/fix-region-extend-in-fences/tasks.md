## 1. Helpers and state

- [ ] 1.1 Add buffer-local variable `gfm-pretty-fences--last-selection-bounds`
      (initial value nil) to `lisp/gfm/gfm-pretty-fences.el`.
- [ ] 1.2 Add `gfm-pretty-fences--selection-bounds` returning `(BEG . END)`
      of the active selection, or nil. Handle evil
      linewise/charwise visual state via `bound-and-true-p` guards;
      treat evil visual-block (`(eq evil-visual-selection 'block)`)
      as no selection. Fall back to `(use-region-p)` →
      `(cons (region-beginning) (region-end))`.
- [ ] 1.3 Add `gfm-pretty-fences--range-selected-p (beg end)` that
      reads `gfm-pretty-fences--selection-bounds` and returns non-nil
      iff `[beg, end)` overlaps the selection.

## 2. Body overlay creation — fenced path

- [ ] 2.1 In `gfm-pretty-fences--apply-bordered-display`
      (`lisp/gfm/gfm-pretty-fences.el`), compute the masked
      `after-string` exactly as today and bind it to `after-masked`.
- [ ] 2.2 Compute the bare variant `after-bare` as the empty string
      `""`.
- [ ] 2.3 Stash both on the body overlay via
      `'gfm-pretty-fences-after-masked` and
      `'gfm-pretty-fences-after-bare` properties.
- [ ] 2.4 Set the initial `'after-string` based on
      `gfm-pretty-fences--range-selected-p lbeg lend`.
- [ ] 2.5 Confirm the `body-bg-inset` masking overlay path is
      unchanged.

## 3. Body overlay creation — indent path

- [ ] 3.1 Apply the same change shape inside
      `gfm-pretty-fences--apply-indent-display`. The
      last-line body overlay's after-string still concatenates `bot-str`;
      for the bare variant on the last line, the bot-str must still
      render so the box's bottom border survives selection — bare
      variant for the last line is `(concat "" "\n" bot-str)` (i.e.
      drop only the pad/pipe/tail prefix, keep the bottom border).
- [ ] 3.2 Verify visually that mid-block lines (non-last) use a
      plain empty bare variant.

## 4. Selection-aware after-string swap

- [ ] 4.1 Add `gfm-pretty-fences--update-selection` to walk every
      overlay returned by `(overlays-in s e)` for each
      `(s . e)` from `gfm-pretty--visible-window-ranges`. Filter to
      `(eq (overlay-get ov 'gfm-pretty-fences-kind) 'body)`.
- [ ] 4.2 For each matching overlay, read both stashed variants and
      set `'after-string` to bare iff the overlay's range overlaps
      current selection bounds; masked otherwise.
- [ ] 4.3 Memoise: early-return when current bounds `equal` last
      stored bounds; update `gfm-pretty-fences--last-selection-bounds`
      after the walk.

## 5. Hook wiring

- [ ] 5.1 Add `gfm-pretty-fences--on-enable` and
      `gfm-pretty-fences--on-disable` (or extend existing
      enable/disable callbacks if present) to install and remove
      `gfm-pretty-fences--update-selection` on
      buffer-local `post-command-hook`.
- [ ] 5.2 Wire the on-enable / on-disable callbacks into the fences
      decorator registration so they fire alongside the existing
      lifecycle (mirror
      `gfm-pretty-tables.el:1215-1232`).
- [ ] 5.3 Clear `gfm-pretty-fences--last-selection-bounds` on
      disable.

## 6. Tests

- [ ] 6.1 Loosen
      `lang-markdown/gfm-pretty--right-after-tail-fills-to-window-edge`
      and its overflow sibling so they remain accurate for the
      unselected path. (No call-site change to
      `gfm-pretty--right-after`; the masked variant still uses it.)
- [ ] 6.2 Add `lang-markdown/gfm-pretty-fences-body-after-string-bare-on-selection`:
      enable fences, place body inside a fenced block, set
      `mark-active` + region covering the body, run the selection
      update, and assert the body overlay's `after-string` equals the
      stashed bare variant (empty string for fenced).
- [ ] 6.3 Add `lang-markdown/gfm-pretty-fences-body-after-string-restored-after-deselect`:
      same setup, then deactivate the mark, run update, assert
      `after-string` equals the stashed masked variant.
- [ ] 6.4 Add `lang-markdown/gfm-pretty-fences-body-overlay-stashes-both-variants`:
      assert every fenced body overlay carries non-nil
      `gfm-pretty-fences-after-masked` and
      `gfm-pretty-fences-after-bare` overlay properties.
- [ ] 6.5 Add `lang-markdown/gfm-pretty-fences-rebuild-honours-active-selection`:
      with mark active over a body range, trigger a fenced rebuild
      (e.g. `gfm-pretty--rebuild-block`) and assert the new body
      overlays' `after-string` is the bare variant from the start.
- [ ] 6.6 Add `lang-markdown/gfm-pretty-fences-selection-bounds-honours-evil-visual`:
      stub `evil-visual-state-p` to t, set
      `evil-visual-beginning` / `evil-visual-end` markers, leave
      `mark-active` nil, and assert
      `gfm-pretty-fences--selection-bounds` returns the evil markers'
      range. Stub `evil-visual-selection` to `'block` and assert nil.

## 7. Verification

- [ ] 7.1 Run `make test-quick` and confirm green.
- [ ] 7.2 Run `make test` (full suite incl. narrowing-regression) and
      confirm green.
- [ ] 7.3 Open `/Users/chris/src/chrisbarrett/may-i/REFERENCE.md` in a
      live Emacs session, place a `V`-line selection across several
      fenced-body lines (including at least one diff src block), and
      visually confirm the `region` bg spans EOL to the window's
      right edge on every selected line. Confirm unselected diff
      lines still clip diff bg at the right border.
- [ ] 7.4 Repeat 7.3 with vanilla `set-mark` + line-stretch selection
      to verify the non-evil path.
