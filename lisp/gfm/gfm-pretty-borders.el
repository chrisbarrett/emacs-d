;;; gfm-pretty-borders.el --- Box-drawing primitives for GFM decorators -*- lexical-binding: t; -*-

;;; Commentary:

;; Decorator-neutral box-drawing primitives shared across the
;; gfm-pretty decorators: top / bottom border builders, right-edge
;; after-string builders, wrap simulator, wrap-prefix factory, and the
;; normalised border face.
;;
;; Width helpers, range / overlap predicates, the window-list helper,
;; and the wrap-prefix width constant live in `gfm-pretty-engine.el'.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty-engine)

(defgroup gfm-pretty-borders nil
  "Shared border-drawing primitives for GFM block decorators."
  :group 'markdown-faces)

(defface gfm-pretty-border-face
  '((((background dark))  :foreground "#6c7086" :weight light)
    (((background light)) :foreground "#b9b2a3" :weight light)
    (t :inherit shadow :weight light))
  "Face for overlay borders around fenced code blocks and tables."
  :group 'markdown-faces)

(define-obsolete-face-alias '+markdown-overlay-border-face
  'gfm-pretty-border-face "29.1")

(defcustom gfm-pretty--icon-gui-nudge 0.25
  "Fractional columns to shift the language icon leftward on GUI frames.
Compensates for icon-font glyphs whose pixel width exceeds the
`string-width' cell count.  Ignored on TTY frames."
  :type 'number
  :group 'gfm-pretty-borders)

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
    :slant normal :weight light
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
GLYPH defaults to `↪ ' (fences).  Callers wanting the box's left edge
on continuation rows pass `│ '."
  (propertize (or glyph "↪ ") 'face
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

;;; Selection-aware decoration variant swap
;;
;; Decoration overlays (display strings, before-/after-strings, faces)
;; that paint an opaque `:background' to mask past-EOL `:extend t' leaks
;; from sibling overlays (e.g. `region', `hl-line', `diff-added') also
;; block the `region' face from showing through inside the decoration's
;; visible cells.  To make active selections paint across the box edges
;; the cleanest way, each decorator stashes BOTH a masked variant (the
;; opaque rendering) AND a bare variant (region face baked in front of
;; every face plist) on each decoration overlay.  A buffer-local
;; post-command-hook walks the visible overlays and swaps each one's
;; live property between the two variants based on whether the overlay
;; falls inside the active V-line selection or the interior lines of
;; an active charwise / vanilla region.

(defvar-local gfm-pretty--last-selection-bounds nil
  "Memoised `(BOUNDS . VISIBLE-RANGES)' tuple from the last walker pass.
Keyed on visible window ranges so a pure scroll (selection bounds
unchanged) invalidates the cache and re-walks overlays that have just
entered the visible range.")

(defconst gfm-pretty--variant-props
  '((display      . gfm-pretty-display)
    (after-string . gfm-pretty-after)
    (before-string . gfm-pretty-before)
    (face         . gfm-pretty-face))
  "Overlay props whose live value swaps between masked and bare variants.
Each entry is (PROP . STASH-PREFIX); the masked / bare values live on
`STASH-PREFIX-masked' / `STASH-PREFIX-bare' overlay properties.")

(defun gfm-pretty--interior-line-bounds (beg end)
  "Return `(IBEG . IEND)' covering lines strictly between line(BEG) and line(END).
Returns nil when no interior lines exist (single-line selection or two
adjacent lines).  Used for charwise selections, where the start and
end lines are partially-selected exceptions and only fully-enclosed
interior lines get full-width region paint."
  (let ((first-line (line-number-at-pos beg))
        (last-line (line-number-at-pos end)))
    (when (> last-line (1+ first-line))
      (save-excursion
        (let ((ibeg (progn (goto-char beg) (forward-line 1)
                           (line-beginning-position)))
              (iend (progn (goto-char end) (line-beginning-position))))
          (cons ibeg iend))))))

(defun gfm-pretty--selection-bounds ()
  "Return `(BEG . END)' of the decoration-paintable selection range, or nil.

V-line (evil `linewise' visual): every line in the selection paints
full-width across the window — return the full marker range.

v charwise (evil `char' visual) and vanilla `mark-active' regions:
only interior lines (those strictly between the start and end lines)
get full-width paint; the start and end lines stay masked so Emacs's
native char-level region paint sits naturally inside the text.

Visual-block (evil `block') and the no-selection state return nil.

Evil's `evil-visual-state-p' is a function (not a variable), so
detection runs through the `evil-state' variable instead."
  (cond
   ((and (eq (bound-and-true-p evil-state) 'visual)
         (markerp (bound-and-true-p evil-visual-beginning))
         (markerp (bound-and-true-p evil-visual-end)))
    (let ((sel (bound-and-true-p evil-visual-selection))
          (vb (marker-position evil-visual-beginning))
          (ve (marker-position evil-visual-end)))
      (cond
       ((eq sel 'line) (cons vb ve))
       ((eq sel 'char) (gfm-pretty--interior-line-bounds vb ve)))))
   ((use-region-p)
    (gfm-pretty--interior-line-bounds (region-beginning) (region-end)))))

(defun gfm-pretty--pos-in-selection-p (pos bounds)
  "Non-nil iff POS is on a line covered by BOUNDS.
BOUNDS is `(BEG . END)' from `gfm-pretty--selection-bounds'.  V-line
bounds are line-aligned and interior bounds are bol-aligned, so a
position in the half-open [BEG, END) range belongs to a selected line."
  (and bounds (<= (car bounds) pos) (< pos (cdr bounds))))

(defun gfm-pretty--range-selected-p (beg _end)
  "Non-nil iff the line at BEG is covered by the active selection range.
Decorators call this at overlay creation to choose the initial
variant; the second argument (overlay end) is unused since V-line
semantics paint by line and the overlay's start position alone
determines selection."
  (gfm-pretty--pos-in-selection-p
   beg (gfm-pretty--selection-bounds)))

(defun gfm-pretty--with-region-face (face)
  "Return a face spec that paints `region' bg on top of FACE.
Emacs does not merge a `region' overlay's face into a sibling
overlay's display/before-/after-string, so to make decoration glyphs
pick up the active selection bg we bake `region' into the string's
own face property, ahead of FACE so its `:background' wins."
  (cond
   ((null face) 'region)
   ((symbolp face) (list 'region face))
   ((and (consp face) (keywordp (car face))) (list 'region face))
   ((consp face) (cons 'region face))
   (t (list 'region face))))

(defun gfm-pretty--str-with-region-bg (s)
  "Return a copy of string S with `region' merged in front of every face.
Region's `:background' then paints the chars regardless of the
masked face's `:background \"unspecified-bg\"' pin."
  (cond
   ((null s) nil)
   ((not (stringp s)) s)
   ((string-empty-p s) s)
   (t
    (let ((s (copy-sequence s))
          (pos 0)
          (len (length s)))
      (while (< pos len)
        (let ((next (or (next-single-property-change pos 'face s) len))
              (face (get-text-property pos 'face s)))
          (put-text-property pos next 'face
                             (gfm-pretty--with-region-face face)
                             s)
          (setq pos next)))
      s))))

(defun gfm-pretty--region-tail ()
  "Return a stretch-glyph tail painting `region' bg out to the window edge.
Marker-line after-strings end at the corner glyph with no past-EOL
fill of their own, so the bare variant appends this so the selection
visually reaches the window's right edge."
  (propertize " " 'display '(space :align-to right) 'face 'region))

(defun gfm-pretty--apply-variant (ov variant)
  "Apply VARIANT (`bare' or `masked') to OV's stashed swap properties.
For each entry in `gfm-pretty--variant-props' that OV stashes, copies
the chosen variant onto OV's live property.  `display' swaps are
reveal-aware: when reveal has hidden the overlay (display=nil and the
shared `gfm-pretty-saved-display' prop is non-nil), the saved value is
updated instead so the next reveal-restore picks up the right variant."
  (dolist (entry gfm-pretty--variant-props)
    (let* ((prop (car entry))
           (base (symbol-name (cdr entry)))
           (masked-key (intern (concat base "-masked"))))
      (when (overlay-get ov masked-key)
        (let* ((var-key (intern (concat base "-" (symbol-name variant))))
               (value (overlay-get ov var-key))
               (reveal-hidden (and (eq prop 'display)
                                   (null (overlay-get ov 'display))
                                   (overlay-get ov 'gfm-pretty-saved-display))))
          (if reveal-hidden
              (overlay-put ov 'gfm-pretty-saved-display value)
            (overlay-put ov prop value)))))))

(defun gfm-pretty--update-selection ()
  "Swap each visible decoration overlay between masked and bare variants.
Walks overlays in every window's visible range and, for each overlay
carrying any stashed masked variant from `gfm-pretty--variant-props',
chooses the bare variant when the overlay's start lies inside the
active selection range and masked otherwise.

Memoised on `(bounds . visible-ranges)' so pure scrolls (selection
unchanged) still re-walk overlays that have just entered the visible
range; the previous walk left those untouched and their cached
variant could be stale."
  (let* ((bounds (gfm-pretty--selection-bounds))
         (ranges (or (gfm-pretty--visible-window-ranges)
                     (list (cons (point-min) (point-max)))))
         (key (cons bounds ranges)))
    (unless (equal key gfm-pretty--last-selection-bounds)
      (dolist (range ranges)
        (dolist (ov (overlays-in (car range) (cdr range)))
          (let* ((selected (gfm-pretty--pos-in-selection-p
                            (overlay-start ov) bounds))
                 (variant (if selected 'bare 'masked)))
            (gfm-pretty--apply-variant ov variant))))
      (setq gfm-pretty--last-selection-bounds key))))

;;; Anchor / display split helper

(cl-defun gfm-pretty-borders--apply-with-anchors
    (block window &key registry range anchors-fn display-fn)
  "Apply WINDOW's overlays for BLOCK with the anchor / display split.
ANCHORS-FN is `(block)' and applies width-independent overlays shared
across every window; it runs at most once per (decorator, RANGE) per
rebuild pass.  DISPLAY-FN is `(block window)' and applies the
per-window display overlays; it runs unconditionally.

REGISTRY is the decorator's `gfm-pretty--registry' (used to locate
the decorator's state slot).  RANGE is the block's (BEG . END) source
range; it doubles as the anchors-laid sentinel key and lets the
engine prune the sentinel when overlapping ranges are torn down."
  (let* ((name (gfm-pretty--registry-name registry))
         (laid (gfm-pretty--state-get name 'anchors-laid)))
    (unless (member range laid)
      (when anchors-fn (funcall anchors-fn block))
      (gfm-pretty--state-set name 'anchors-laid (cons range laid)))
    (when display-fn (funcall display-fn block window))))

(provide 'gfm-pretty-borders)

;;; gfm-pretty-borders.el ends here
