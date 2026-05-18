;;; gfm-pretty-borders.el --- Graphics toolkit for GFM block decorators -*- lexical-binding: t; -*-

;;; Commentary:

;; Decorator-neutral graphics primitives shared across the gfm-pretty
;; decorators (callouts, fences, tables, hrule, links).  Hosts the
;; box-drawing primitives (top/bottom border builders, right-edge
;; after-strings, wrap simulator), width helpers, a normalised border
;; face, and the in-range/region-overlaps predicates.
;;
;; The lifecycle engine — overlay registry, scheduler primitives,
;; window-state tracking, and the reconciler — lives in
;; `gfm-pretty-engine.el'.  Files that need only the graphics
;; primitives require this module directly; files that drive
;; rebuilds require `gfm-pretty-engine.el'.

;;; Code:

(require 'cl-lib)

(defgroup gfm-pretty-borders nil
  "Shared border-drawing primitives for GFM block decorators."
  :group 'markdown-faces)

(defface gfm-pretty-border-face
  '((((background dark))  :foreground "#6c7086")
    (((background light)) :foreground "#b9b2a3")
    (t :inherit shadow))
  "Face for overlay borders around fenced code blocks and tables."
  :group 'markdown-faces)

(define-obsolete-face-alias '+markdown-overlay-border-face
  'gfm-pretty-border-face "29.1")

(defconst gfm-pretty--wrap-prefix-w 2
  "Visual width of the wrap-prefix shown on continuation visual lines.")

(defcustom gfm-pretty--icon-gui-nudge 0.25
  "Fractional columns to shift the language icon leftward on GUI frames.
Compensates for icon-font glyphs whose pixel width exceeds the
`string-width' cell count.  Ignored on TTY frames."
  :type 'number
  :group 'gfm-pretty-borders)

;;; Range helpers

(defun gfm-pretty--in-ranges-p (pos ranges)
  "Non-nil if POS lies in any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-pretty--region-overlaps-p (a b)
  "Non-nil if (BEG . END) ranges A and B overlap."
  (and (<= (car a) (cdr b)) (>= (cdr a) (car b))))

;;; Width helpers

(defun gfm-pretty--available-width (&optional window)
  "Return the available char width for a block in WINDOW.
Falls back to a window currently showing the buffer, then to
`fill-column' or 80."
  (let ((win (or window
                 (get-buffer-window (current-buffer))
                 (get-buffer-window (current-buffer) t))))
    (or (and win (window-max-chars-per-line win))
        fill-column
        80)))

(defun gfm-pretty--text-width (&optional window)
  "Return WINDOW's max chars per visual line.
Compatibility alias for `gfm-pretty--available-width'."
  (gfm-pretty--available-width window))

(defun gfm-pretty--max-line-width (beg end &optional indent)
  "Maximum line width between BEG and END, subtracting INDENT from each line."
  (let ((max-col 0)
        (indent (or indent 0)))
    (save-excursion
      (goto-char beg)
      (while (and (not (eobp)) (<= (line-beginning-position) end))
        (let* ((lbeg (line-beginning-position))
               (lend (line-end-position))
               (len (max 0 (- (- lend lbeg) indent))))
          (setq max-col (max max-col len)))
        (forward-line 1)))
    max-col))

;;; Border primitives

(defun gfm-pretty--normalised-border-face (face)
  "Return a face spec that inherits FACE but resets text-styling attrs.
Border glyphs share buffer regions with prose whose font-lock face
carries `:slant italic', `:underline t', etc.  Without an explicit
override, those attrs leak through face composition on GUI frames
and visually slant the box edges.

`:background' is explicitly pinned to `\"unspecified-bg\"' — the
literal Emacs marker for the system / frame background — to stop
the buffer position's text-property `:background' (e.g. `diff-added'
on the newline at the body line's end) from bleeding through into
border / before-string / after-string chars whose `:background'
would otherwise be unspecified and inherit from below."
  `(:inherit ,face
    :slant normal :weight normal
    :underline nil :overline nil :strike-through nil :box nil
    :background "unspecified-bg"))

(defun gfm-pretty--top-strings (width face buffer-width &optional icon)
  "Return (LEADING . TRAILING) split of the top border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols (matching the marker line's char count) so
the buffer text shows in place of LEADING when it is revealed; TRAILING
covers the remaining decoration. ICON, if non-nil, is right-aligned."
  (let* ((face (gfm-pretty--normalised-border-face face))
         (l (propertize "┌" 'face face))
         (r (propertize "┐" 'face face))
         (gap (propertize " " 'face face))
         (leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat l (propertize (make-string leading-dash-w ?─)
                                        'face face))))
    (ignore gap)
    (cond
     (icon
      (let* ((icon-w (string-width icon))
             (nudge (if (display-graphic-p)
                        (max 0 (min 0.99 gfm-pretty--icon-gui-nudge))
                      0))
             (total-fill-w (max 1 (- width 4 icon-w)))
             (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
             (rem-fill (propertize (make-string rem-fill-w ?─) 'face face))
             (icon-pad (propertize " " 'display
                                   `(space :align-to ,(- width 2 icon-w nudge))
                                   'face face))
             (snap (propertize " " 'display `(space :align-to ,(1- width))
                               'face face)))
        (cons leading (concat rem-fill icon-pad icon snap r))))
     (t
      (let* ((total-fill-w (max 1 (- width 2)))
             (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
             (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
        (cons leading (concat rem-fill r)))))))

(defun gfm-pretty--bottom-strings (width face buffer-width)
  "Return (LEADING . TRAILING) split of the bottom border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols matching the marker line's char count."
  (let* ((face (gfm-pretty--normalised-border-face face))
         (leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat (propertize "└" 'face face)
                          (propertize (make-string leading-dash-w ?─)
                                      'face face)))
         (total-fill-w (max 1 (- width 2)))
         (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
         (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
    (cons leading (concat rem-fill (propertize "┘" 'face face)))))

(defun gfm-pretty--right-after (box-width face &optional bg)
  "Build the after-string that draws the right border at column BOX-WIDTH.
When BG is non-nil, the padding before the border `│' is painted
with `:background BG' so a body line's highlight band fills up to
the right-side inner gap; nil leaves it on the border face.

When BG is active, the band ends two columns short of `│' (a 2-col
default-bg gap); when nil, the band ends one column short.  The
extra column of internal padding when bg-fill is active makes the
band visibly inset from the right border rather than abutting it.

The after-string ends with `(space :align-to right)' painted in the
default face — this fills the visual line from `│' to the window's
right edge with the default background, masking any `:extend t'
past-EOL fill that an underlying face (`diff-added' / `diff-removed'
text-property faces, `hl-line' / `region' overlay faces) would
otherwise paint past the border.  `:extend nil' on an overlay does
NOT clip such a leak — the C-level past-EOL face merge in
`face_at_buffer_position' simply skips faces that opt out, leaving
the leaking face's background intact.  Filling the visual line so
there is no past-EOL region to fill is the working idiom."
  (let* ((face (gfm-pretty--normalised-border-face face))
         ;; `face' already pins `:background "unspecified-bg"' so the
         ;; border / sep / pipe paint the system bg.  When BG is non-nil
         ;; the pad replaces that pin with the highlight colour, and
         ;; the band runs all the way to the col before `│' (no
         ;; default-bg sep).
         (pad-face (if bg (plist-put (copy-sequence face) :background bg)
                     face))
         (align-col (if bg (- box-width 1) (- box-width 2)))
         (pad (propertize " " 'display `(space :align-to ,align-col)
                          'face pad-face))
         (sep (propertize (if bg "" " ") 'face face))
         (pipe (propertize "│" 'face face))
         (tail (propertize " " 'display '(space :align-to right)
                           'face 'default))
         (str (concat pad sep pipe tail)))
    (put-text-property 0 1 'cursor t str)
    str))

(defun gfm-pretty--simulate-wrap (text width &optional cont-prefix-w)
  "Simulate word-wrap of TEXT in a WIDTH-col window.
CONT-PREFIX-W is the width of the wrap-prefix shown on continuation
visual lines (default 0).  Returns (LAST-COL . WRAP-POSITIONS).

The per-iteration `line-width' is clamped to at least 1 so the position
counter advances on every loop body, even when WIDTH ≤ CONT-PREFIX-W —
this keeps the function terminating during tiny-window transients
\(e.g. `C-x 3' splits where one window briefly drops below the wrap-prefix
width)."
  (let* ((cont-prefix-w (or cont-prefix-w 0))
         (pos 0)
         (col 0)
         (len (length text))
         (first-line t)
         (wraps nil))
    (while (< pos len)
      (let* ((raw-line-width (if first-line width (- width cont-prefix-w)))
             (line-width (max 1 raw-line-width))
             (remaining (- len pos))
             (space-left (max 1 (- line-width col))))
        (cond
         ((<= remaining space-left)
          (setq col (+ col remaining)
                pos len))
         (t
          (let* ((slice (substring text pos (+ pos space-left)))
                 (wrap-at (cl-position ?\s slice :from-end t))
                 (next-pos (if wrap-at (+ pos wrap-at 1) (+ pos space-left))))
            (push next-pos wraps)
            (setq pos next-pos
                  col 0
                  first-line nil))))))
    (cons (if first-line col (+ col cont-prefix-w))
          (nreverse wraps))))

(defun gfm-pretty--last-visual-col (text width &optional cont-prefix-w)
  "Estimate last visual column TEXT reaches.
See `gfm-pretty--simulate-wrap'."
  (car (gfm-pretty--simulate-wrap text width cont-prefix-w)))

(defun gfm-pretty--wrap-prefix (face &optional glyph)
  "Wrap-prefix string for continuation lines using FACE.
GLYPH defaults to `⋱ ' (fences).  Callers wanting the box's left edge
on continuation rows pass `│ '."
  (propertize (or glyph "⋱ ") 'face
              (gfm-pretty--normalised-border-face face)))

(defun gfm-pretty--right-after-overflow (face line-text window
                                                     &optional cont-prefix-w bg)
  "After-string padding the right border to WINDOW's edge for a wrapped line.
LINE-TEXT is the line's buffer content; the function simulates word-wrap
to work out where the line ends visually, accounting for the wrap-prefix
on continuation lines.  WINDOW selects the width; nil falls back to a
sane default.  CONT-PREFIX-W defaults to
`gfm-pretty--wrap-prefix-w'.  When BG is non-nil the padding is
painted with `:background BG' so the highlight band fills the gap up
to the border.

A trailing `(space :align-to right)' in the default face fills the
last wrapped visual row from `│' to the window's right edge — see
`gfm-pretty--right-after' for why."
  (let* ((face (gfm-pretty--normalised-border-face face))
         (pad-face (if bg (plist-put (copy-sequence face) :background bg)
                     face))
         (text-width (gfm-pretty--available-width window))
         (cpw (or cont-prefix-w gfm-pretty--wrap-prefix-w))
         ;; +2 for the `│ ' before-string contribution to the first visual line.
         (visual-col (gfm-pretty--last-visual-col
                      (concat "│ " line-text) text-width cpw))
         ;; Pad runs to the col before `│' regardless of BG; the
         ;; non-overflow path collapses its 1-col `sep' when BG is
         ;; active, so the wrapped path matches by always reaching
         ;; `target-col = text-width - 1'.
         (target-col (- text-width 1))
         (pad-len (max 0 (- target-col visual-col)))
         (pad (propertize (make-string pad-len ?\s) 'face pad-face))
         (pipe (propertize "│" 'face face))
         (tail (propertize " " 'display '(space :align-to right)
                           'face 'default))
         (str (concat pad pipe tail)))
    (put-text-property 0 1 'cursor t str)
    str))

(provide 'gfm-pretty-borders)

;;; gfm-pretty-borders.el ends here
