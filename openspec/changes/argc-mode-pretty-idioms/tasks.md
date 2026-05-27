## 1. Tests first (red)

- [ ] 1.1 In `lisp/argc-mode/argc-mode-tests.el`, replace the existing
      box-width scenarios (80-col floor; expand for 100-char lines)
      with a scenario asserting the per-line right-edge after-string
      `display` spec contains `(space :align-to right)` or
      `(space :align-to (- right N))`.
- [ ] 1.2 Add a scenario asserting per-line overlays carry a
      `wrap-prefix` property whose value (string) contains the rail
      `│ `.
- [ ] 1.3 Add a scenario asserting the per-line after-string ends
      with a `(space :align-to right)` segment painted in the
      `default` face (mask for past-EOL `:extend t` leaks).
- [ ] 1.4 Add a scenario asserting the border face spec used by the
      box explicitly sets `:slant normal`, `:underline nil`,
      `:overline nil`, `:strike-through nil`, `:box nil`, and
      `:background "unspecified-bg"`.
- [ ] 1.5 Run `make test-quick`; confirm the four new scenarios fail
      (red) and the existing function-label / single-block /
      no-comments scenarios still pass.

## 2. Implementation (green)

- [ ] 2.1 Add a `argc--normalised-box-face` helper in
      `lisp/argc-mode/argc-mode.el` returning a face spec that
      inherits `argc-box-face` and clears text-styling attrs +
      pins `:background "unspecified-bg"`. Model on
      `gfm-pretty--normalised-border-face`
      (lisp/gfm/gfm-pretty-borders.el:52-68).
- [ ] 2.2 Replace the literal `'face 'argc-box-face' propertize calls
      in `argc--make-border` and `argc--apply-box-overlays` with the
      normalised face spec from 2.1.
- [ ] 2.3 Rewrite `argc--make-border` (and inline its top/bottom
      callers) so the border builds as `┌` + dashes + (optional
      label positioned via `(space :align-to (- right LABEL-W 2))`)
      + ` ┐` for the top, and the symmetric form with `└`/`┘` for
      the bottom. Strip the `width` / `box-width` / `max-col` /
      `argc--block-max-col` machinery — sizing is window-relative
      now.
- [ ] 2.4 In `argc--apply-box-overlays`, swap the per-line
      `after = (concat pad right-border)` construction (where `pad`
      is `(space :align-to align-col)`) for one whose `pad` uses
      `(space :align-to (- right 2))` so `│` lands at column
      `right - 1`. Append a trailing `(space :align-to right)` in
      the `default` face after `│` to mask past-EOL `:extend t`
      leaks.
- [ ] 2.5 Add `'wrap-prefix` to each per-line overlay carrying the
      left rail string `│ ` painted in the normalised border face.
- [ ] 2.6 Run `make test-quick`; confirm all box scenarios pass
      (green), including the four new ones from §1.

## 3. Manual verification

- [ ] 3.1 In a sandbox emacs daemon (see `CLAUDE.md` —
      `emacs -Q --bg-daemon`), open a shell-script file with argc
      directives, enable `argc-mode`, and visually confirm the box
      hugs the window's right edge at multiple window widths
      (`C-x 3`, then shrink/grow with `C-x }` / `C-x {`).
- [ ] 3.2 Enable `visual-line-mode` in the same sandbox buffer,
      widen a directive description so it soft-wraps, and confirm
      the left `│ ` rail continues on continuation visual rows.
- [ ] 3.3 Move point into the box and confirm `hl-line-mode`
      highlight does NOT bleed past `│` to the window edge.
- [ ] 3.4 Place point on a region that overlaps a directive line;
      confirm the `region` face does not paint past `│`.
- [ ] 3.5 Add a stylistic font-lock face (e.g. `font-lock-comment-
      face` with `:slant italic`) inside the box and confirm border
      glyphs render upright (not slanted).

## 4. Verification

- [ ] 4.1 Run `make test` to confirm all checks pass.
- [ ] 4.2 Run `openspec verify --change argc-mode-pretty-idioms` to
      confirm artifacts validate.
