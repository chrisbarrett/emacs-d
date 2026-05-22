;;; gfm-pretty-callouts.el --- Box overlays for GitHub Flavored Markdown callouts -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that draws box overlays around GitHub Flavored Markdown
;; callout blockquotes:
;;
;;   > [!IMPORTANT]
;;   > Lorem ipsum.
;;
;; Mirrors the fences decorator on the Path C anchor / display split:
;;
;; - Anchor overlays carry width-independent props (tinted background
;;   face, `wrap-prefix' for continuation rows).  Shared across windows.
;; - Display overlays carry width-dependent props (top/bottom borders,
;;   the `> ' → `│ ' substitution, per-body-line right-edge after
;;   strings).  One set per window currently showing the buffer; each
;;   set is restricted to its window via the `window' overlay
;;   property so a buffer split across two windows of different widths
;;   renders at each window's own width.
;;
;; Box width clamps to `(min text-width (max 80 (+ max-content 4)))'
;; per window, where `text-width' comes from `window-max-chars-per-line'
;; and `max-content' is the longest body line's visible width minus
;; the 2-char `> ' prefix.  Long body lines wrap natively via
;; `wrap-prefix'; the right-edge `│' is padded by simulating wrap.
;;
;; Window-configuration changes drive a per-window reconciler:
;; added/resized windows render fresh display overlays, removed
;; windows have theirs deleted, unchanged windows keep theirs.
;; Visible callouts render first; off-screen ones defer one idle
;; tick.  Scoped post-edit rebuild restricts work to the dirty
;; region's containing callout when possible.  Reveal exposes source
;; only in the selected window.
;;
;; Shared primitives (overlay registry, scheduler, reconciler, border
;; builders, wrap simulator) live in `gfm-pretty-borders.el'.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty-borders)
(require 'gfm-pretty-engine)

(defgroup gfm-pretty-callouts nil
  "Box overlays for GitHub Flavored Markdown callouts."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-box-face
  '((t :inherit shadow))
  "Fallback face for callout box-drawing characters."
  :group 'gfm-pretty-callouts)

(defconst gfm-pretty-callouts--type-faces
  '(("NOTE"      . gfm-pretty-callouts-note-face)
    ("TIP"       . gfm-pretty-callouts-tip-face)
    ("IMPORTANT" . gfm-pretty-callouts-important-face)
    ("WARNING"   . gfm-pretty-callouts-warning-face)
    ("CAUTION"   . gfm-pretty-callouts-caution-face)
    ("CRITICAL"  . gfm-pretty-callouts-caution-face))
  "Map of callout type to face used for box border and label.")

(defconst gfm-pretty-callouts--marker-re
  (rx bol "> " "[!"
      (group (or "NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
      "]" (* space) eol)
  "Regexp matching a callout marker line. Group 1 captures the type.")

(defconst gfm-pretty-callouts--blockquote-line-re
  (rx bol ">")
  "Regexp matching any blockquote continuation line.")

;;; Block discovery

(defun gfm-pretty-callouts--find-blocks ()
  "Scan the buffer for callout blocks (uncached).
Each entry is (BEG END TYPE) where BEG is BOL of the marker line, END
is EOL of the last blockquote line, and TYPE is the callout type
string.

Widens for the duration of its body so the cache key
\(`buffer-chars-modified-tick') is a pure function of buffer contents
regardless of any current narrowing.  See fix-gfm-narrowing-safety."
  (let (blocks)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward gfm-pretty-callouts--marker-re nil t)
            (let ((block-beg (line-beginning-position))
                  (block-end (line-end-position))
                  (type (match-string-no-properties 1)))
              (forward-line 1)
              (while (and (not (eobp))
                          (looking-at gfm-pretty-callouts--blockquote-line-re))
                (setq block-end (line-end-position))
                (forward-line 1))
              (push (list block-beg block-end type) blocks))))))
    (nreverse blocks)))

;;; Tinted background for the callout panel

(defun gfm-pretty-callouts--tinted-bg (face)
  "Blend FACE's foreground 10% toward `+theme-default-background'.
Returns a hex colour string, or nil if either colour is unresolvable."
  (require 'color)
  (let* ((fg (face-foreground face nil t))
         (bg (and (boundp '+theme-default-background)
                  +theme-default-background))
         (fg-rgb (and fg (color-name-to-rgb fg)))
         (bg-rgb (and bg (color-name-to-rgb bg))))
    (when (and fg-rgb bg-rgb)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (b f) (+ b (* 0.1 (- f b))))
                                bg-rgb fg-rgb)
                     '(2))))))

;;; Overlay registry

(defconst gfm-pretty-callouts--registry
  (gfm-pretty--registry-for 'callouts 'gfm-pretty-callouts)
  "Shared overlay-registry context for callouts.")

(defsubst gfm-pretty-callouts--make-anchor (beg end &rest props)
  "Make an anchor overlay over [BEG, END] with PROPS."
  (apply #'gfm-pretty--make-anchor
         gfm-pretty-callouts--registry beg end props))


(defsubst gfm-pretty-callouts--make-display (beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW with PROPS."
  (apply #'gfm-pretty--make-display
         gfm-pretty-callouts--registry beg end window props))

(defsubst gfm-pretty-callouts--remove-overlays (&optional beg end)
  "Remove all gfm-pretty-callouts overlays between BEG and END."
  (gfm-pretty--remove-overlays gfm-pretty-callouts--registry beg end))

(defsubst gfm-pretty-callouts--prune-dead-overlays ()
  "Drop overlays from the registry whose buffer is gone."
  (gfm-pretty--prune-dead-overlays gfm-pretty-callouts--registry))

(defsubst gfm-pretty-callouts--register (ov)
  "Tag OV as a callout overlay and remember it for bulk cleanup."
  (gfm-pretty--register gfm-pretty-callouts--registry ov))

;;; Block enumeration

(cl-defstruct (gfm-pretty-callouts--block
               (:constructor gfm-pretty-callouts--make-block)
               (:copier nil))
  "Tagged callout block for unified rebuild dispatch.
RANGE is (LINE-BEG . LINE-END+1) covering the full source range,
used for visibility + scoped-rebuild containment.  PAYLOAD is the
raw (BEG END TYPE) tuple."
  range payload)

(defun gfm-pretty-callouts--collect-blocks ()
  "Uncached widened scan returning tagged callout blocks.
The engine memoises this via `gfm-pretty--collect'."
  (mapcar (lambda (b)
            (gfm-pretty-callouts--make-block
             :range (cons (nth 0 b) (1+ (nth 1 b)))
             :payload b))
          (gfm-pretty-callouts--find-blocks)))

;;; Rendering

(defun gfm-pretty-callouts--upright (str face &optional bg weight)
  "Propertize STR with FACE forced upright; optionally set background BG.
The display string sits over buffer text whose face may carry
`:slant italic'; unspecified attributes leak from the underlying face,
so anchor `:slant normal'.  BG paints the box's tinted background onto
each decoration char.  WEIGHT, when non-nil, pins `:weight' (use
`light' for hairline box chars; leave nil for the title so its weight
inherits from FACE)."
  (let* ((spec `(:inherit ,face :slant normal))
         (spec (if bg (append spec (list :background bg)) spec))
         (spec (if weight (append spec (list :weight weight)) spec)))
    (propertize str 'face spec)))

(defun gfm-pretty-callouts--callout-top-strings (width title face buffer-width &optional bg)
  "Build (LEADING . TRAILING) split for the top border.

Layout: `┌─ TITLE ─...─┐'.  WIDTH is total box width, TITLE the type
label, FACE the border colour, BUFFER-WIDTH the marker line's char
count, BG the optional tinted background.  LEADING covers exactly
BUFFER-WIDTH columns so it can be set as the marker overlay's
`display' (matching the buffer footprint); TRAILING is hung off the
line-end as an after-string."
  (let* ((title-w (string-width title))
         ;; Layout: `┌─ TITLE ─...─┐'.  Decorations occupy 5 cols
         ;; (`┌', `─', ` ', ` ' after title, `┐'); the rest of the
         ;; line is the title and trailing dash fill.
         (dash-fill (max 1 (- width 5 title-w)))
         (full (concat
                (gfm-pretty-callouts--upright "┌─ " face bg 'light)
                (gfm-pretty-callouts--upright title face bg)
                (gfm-pretty-callouts--upright " " face bg)
                (gfm-pretty-callouts--upright (make-string dash-fill ?─) face bg 'light)
                (gfm-pretty-callouts--upright "┐" face bg 'light)))
         (full-len (length full))
         (split-at (min buffer-width full-len)))
    (cons (substring full 0 split-at)
          (substring full split-at))))

(defun gfm-pretty-callouts--callout-bottom-string (width face &optional bg)
  "Build the bottom border string of WIDTH cols, tinted with BG."
  (concat (gfm-pretty-callouts--upright "└" face bg 'light)
          (gfm-pretty-callouts--upright (make-string (max 1 (- width 2)) ?─) face bg 'light)
          (gfm-pretty-callouts--upright "┘" face bg 'light)))

(defun gfm-pretty-callouts--right-after (box-width face bg)
  "Build the body-line right-edge after-string.
Pads with `space :align-to' to BOX-WIDTH-2, then a tinted gap, then
`│'.  Carries the `cursor' text property so cursor motion crosses it.

A trailing `(space :align-to right)' in the default face fills the
visual line from `│' to the window's right edge, masking any
`:extend t' past-EOL fill that a foreign overlay (`hl-line',
`region') would otherwise paint past the border — see
`gfm-pretty--right-after' for the rationale."
  (let* ((align-face (let ((spec `(:inherit ,face :slant normal)))
                       (if bg (append spec (list :background bg)) spec)))
         (str (concat
               (propertize " "
                           'display `(space :align-to ,(- box-width 2))
                           'face align-face)
               (gfm-pretty-callouts--upright " " face bg)
               (gfm-pretty-callouts--upright "│" face bg 'light)
               (propertize " "
                           'display '(space :align-to right)
                           'face 'default))))
    (put-text-property 0 1 'cursor t str)
    str))

(defun gfm-pretty-callouts--right-after-overflow (face bg line-text window)
  "Build the right-edge after-string for a wrapped body line.
Simulates word-wrap of `│ ' + LINE-TEXT in WINDOW's width to compute
how much padding is needed before the closing `│' on the final wrapped
visual row.  A trailing `(space :align-to right)' in the default face
extends the last wrapped row to the window edge, suppressing past-EOL
`:extend' leaks — see `gfm-pretty-callouts--right-after'."
  (let* ((text-width (gfm-pretty--available-width window))
         (visual-col (gfm-pretty--last-visual-col
                      (concat "│ " line-text) text-width
                      gfm-pretty--wrap-prefix-w))
         (target-col (1- text-width))
         (pad-len (max 0 (- target-col visual-col)))
         (face-spec (let ((s `(:inherit ,face :slant normal)))
                      (if bg (append s (list :background bg)) s)))
         (pad (propertize (make-string pad-len ?\s) 'face face-spec))
         (pipe (gfm-pretty-callouts--upright "│" face bg 'light))
         (tail (propertize " " 'display '(space :align-to right)
                           'face 'default))
         (str (concat pad pipe tail)))
    (put-text-property 0 1 'cursor t str)
    str))

(defun gfm-pretty-callouts--apply-block-anchors (block)
  "Apply width-independent anchor overlays for BLOCK.
Paints the per-line tinted background and the per-body-line
`wrap-prefix' so wrapped content stays inside the box.  Anchors are
shared across windows; reveal does not touch them.

Widens for the duration of the apply so a BLOCK whose source range lies
outside the current restriction (e.g. another slide under
`gfm-present-mode') can still be parsed and decorated.  Without this,
the per-line walk would clamp at the narrowing's `point-max' and loop
forever.  Display under narrowing is naturally clipped by Emacs'
overlay engine."
  (save-restriction
   (widen)
  (cl-destructuring-bind (beg end type) (gfm-pretty-callouts--block-payload block)
    (let* ((type-face (alist-get type gfm-pretty-callouts--type-faces
                                  nil nil #'string=))
           (border-face (or type-face 'gfm-pretty-callouts-box-face))
           (tint (gfm-pretty-callouts--tinted-bg border-face))
           ;; Anchor face specifies only background tint and `:extend
           ;; t'; all other attributes left unspecified so emphasis
           ;; faces on buffer text (bold, italic, link, inline code)
           ;; merge through.  Blockquote-italic suppression now lives
           ;; at the face layer — see the `set-face-attribute' for
           ;; `markdown-blockquote-face' in `lang-markdown/init.el'.
           (bg-face (if tint
                        `(:background ,tint :extend t)
                      '(:extend t)))
           (wrap (propertize "│ " 'face
                             (let ((s `(:inherit ,border-face
                                        :slant normal :weight light)))
                               (if tint
                                   (append s (list :background tint))
                                 s)))))
      ;; Per-line anchor: tinted background + wrap-prefix on body lines.
      ;; Iterate via position math — `forward-line' inside the
      ;; overlay-creation loop interacts with cursor-intangible /
      ;; display props (set by the fences decorator on the same
      ;; buffer) and can stall mid-block; see the matching note in
      ;; `gfm-pretty-fences.el'.  Combining face + wrap-prefix on a
      ;; single anchor per line keeps Emacs's redisplay from picking
      ;; only one of two competing anchors at the same buffer range.
      (let ((marker-line-end (save-excursion
                               (goto-char beg) (line-end-position)))
            (p beg))
        (while (< p (1+ end))
          (let* ((lbeg p)
                 (lend (save-excursion
                         (goto-char p) (line-end-position)))
                 (is-body (> lbeg marker-line-end)))
            (apply #'gfm-pretty-callouts--make-anchor
                   lbeg lend
                   'face bg-face
                   (and is-body (list 'wrap-prefix wrap)))
            (setq p (min (1+ end) (1+ lend))))))))))

(defun gfm-pretty-callouts--apply-block-display (block window)
  "Apply per-WINDOW display overlays for BLOCK.
Top border (leading on marker line + trailing after), per-body-line
`> ' → `│ ' substitution + right-edge after-string, bottom border
hung off the last body line's end (or the marker's trailing piece for
a body-less callout).
See `gfm-pretty-callouts--apply-block-anchors' for the widening rationale."
  (save-restriction
   (widen)
  (cl-destructuring-bind (beg end type) (gfm-pretty-callouts--block-payload block)
    (let* ((type-face (alist-get type gfm-pretty-callouts--type-faces
                                  nil nil #'string=))
           (border-face (or type-face 'gfm-pretty-callouts-box-face))
           (tint (gfm-pretty-callouts--tinted-bg border-face))
           (text-width (gfm-pretty--available-width window))
           (marker-line-end (save-excursion
                              (goto-char beg) (line-end-position)))
           (marker-buf-w (- marker-line-end beg))
           (body-beg-pos (save-excursion
                           (goto-char marker-line-end)
                           (forward-line 1)
                           (point)))
           (has-body (and (<= body-beg-pos end)
                          (not (= body-beg-pos (1+ end)))
                          (< marker-line-end end)))
           ;; max-content excludes the 2-char `> ' prefix.
           (max-content (if has-body
                            (gfm-pretty--max-line-width
                             body-beg-pos end 2)
                          0))
           (box-width (min text-width (max 80 (+ max-content 4))))
           (content-budget (- box-width 4))
           (top-split (gfm-pretty-callouts--callout-top-strings
                       box-width type border-face marker-buf-w tint))
           (bottom-str (gfm-pretty-callouts--callout-bottom-string
                        box-width border-face tint))
           (edge (gfm-pretty-callouts--upright "│ " border-face tint))
           (last-right-after-ov nil))
      ;; Top — leading on marker line, trailing after.
      (let* ((top-display-masked (car top-split))
             (top-display-bare
              (gfm-pretty--str-with-region-bg top-display-masked))
             (selected (gfm-pretty--range-selected-p beg marker-line-end)))
        (gfm-pretty-callouts--make-display
         beg marker-line-end window
         'gfm-pretty-callouts-kind 'top-leading
         'gfm-pretty-callouts-revealable t
         'evaporate t
         'gfm-pretty-display-masked top-display-masked
         'gfm-pretty-display-bare top-display-bare
         'display (if selected top-display-bare top-display-masked)))
      (let* ((top-after-masked (cdr top-split))
             (top-after-bare
              (concat (gfm-pretty--str-with-region-bg top-after-masked)
                      (gfm-pretty--region-tail)))
             (selected (gfm-pretty--range-selected-p
                        marker-line-end marker-line-end)))
        (gfm-pretty-callouts--make-display
         marker-line-end marker-line-end window
         'gfm-pretty-callouts-kind 'top-trailing
         'gfm-pretty-after-masked top-after-masked
         'gfm-pretty-after-bare top-after-bare
         'after-string (if selected top-after-bare top-after-masked))
        ;; Body lines.  Iterate via explicit text-position math, not
        ;; `forward-line': inside this overlay-creation loop,
        ;; `forward-line' interacts with cursor-intangible / display
        ;; props (set by the fences decorator on the same buffer)
        ;; and can stall mid-block, spinning forever — see the
        ;; matching note in `gfm-pretty-fences.el'.
        (when has-body
          (let ((p body-beg-pos))
            (while (< p (1+ end))
              (let* ((lbeg p)
                     (lend (save-excursion
                             (goto-char p) (line-end-position)))
                     (last-body (>= lend end))
                     (line-content-w (max 0 (- (- lend lbeg) 2)))
                     (overflow-p (> line-content-w content-budget))
                     (line-text (buffer-substring-no-properties
                                 (min (+ lbeg 2) lend) lend))
                     (right-after
                      (if overflow-p
                          (gfm-pretty-callouts--right-after-overflow
                           border-face tint line-text window)
                        (gfm-pretty-callouts--right-after
                         box-width border-face tint))))
                ;; `> ' / bare `>' → `│ ' substitution as a per-window
                ;; display so reveal in window A doesn't expose the
                ;; source in B.  Source range is 2 chars for `> ',
                ;; 1 char for a bare `>' continuation line.
                (let* ((edge-bare (gfm-pretty--str-with-region-bg edge))
                       (line-selected
                        (gfm-pretty--range-selected-p lbeg lend)))
                  (cond
                   ((and (>= (- lend lbeg) 2)
                         (eq (char-after lbeg) ?>)
                         (eq (char-after (1+ lbeg)) ?\s))
                    (gfm-pretty-callouts--make-display
                     lbeg (+ lbeg 2) window
                     'gfm-pretty-callouts-kind 'body-prefix
                     'gfm-pretty-callouts-revealable t
                     'evaporate t
                     'gfm-pretty-display-masked edge
                     'gfm-pretty-display-bare edge-bare
                     'display (if line-selected edge-bare edge)))
                   ((and (= (- lend lbeg) 1)
                         (eq (char-after lbeg) ?>))
                    (gfm-pretty-callouts--make-display
                     lbeg (1+ lbeg) window
                     'gfm-pretty-callouts-kind 'body-prefix
                     'gfm-pretty-callouts-revealable t
                     'evaporate t
                     'gfm-pretty-display-masked edge
                     'gfm-pretty-display-bare edge-bare
                     'display (if line-selected edge-bare edge))))
                  ;; Right-edge of the body line.
                  (let* ((right-after-bare
                          (concat (gfm-pretty--str-with-region-bg right-after)
                                  (gfm-pretty--region-tail))))
                    (setq last-right-after-ov
                          (gfm-pretty-callouts--make-display
                           lend lend window
                           'gfm-pretty-callouts-kind 'body-rhs
                           'gfm-pretty-after-masked right-after
                           'gfm-pretty-after-bare right-after-bare
                           'after-string (if line-selected
                                             right-after-bare
                                           right-after)))
                    ;; Bottom border (last body line only).  Rendered as
                    ;; a separate overlay so its selection check can use
                    ;; the line BELOW the box: bare iff the selection
                    ;; covers position (1+ lend) (= bol of line after
                    ;; the callout).  Without this split, painting the
                    ;; bottom in the bare variant on the last body line
                    ;; would bleed region bg past the box's bottom edge
                    ;; when only the last body line is selected.
                    (when last-body
                      ;; The leading newline of body-bottom's after-string
                      ;; lands as the trailing cell of the LAST body
                      ;; line's visual row (the after-string is appended
                      ;; to that row).  Propertize it with `region' so
                      ;; the trailing cell shares the bare variant's bg
                      ;; — otherwise it renders with the default face,
                      ;; leaving a 1-col "notch" against the box's right
                      ;; border.
                      (let* ((bottom-masked (concat "\n" bottom-str))
                             (bottom-bare
                              (concat (propertize "\n" 'face 'region)
                                      (gfm-pretty--str-with-region-bg
                                       bottom-str)
                                      (gfm-pretty--region-tail)))
                             (select-range (cons (1+ lend) (1+ lend)))
                             (bottom-selected
                              (and (gfm-pretty--selection-bounds)
                                   (<= (car (gfm-pretty--selection-bounds))
                                       (1+ lend))
                                   (< (1+ lend)
                                      (cdr (gfm-pretty--selection-bounds))))))
                        (gfm-pretty-callouts--make-display
                         lend lend window
                         'gfm-pretty-callouts-kind 'body-bottom
                         'gfm-pretty-select-range select-range
                         'gfm-pretty-after-masked bottom-masked
                         'gfm-pretty-after-bare bottom-bare
                         'after-string (if bottom-selected
                                           bottom-bare
                                         bottom-masked))))))
                (setq p (min (1+ end) (1+ lend)))))))
        ;; Body-less callout: bottom border attaches to the marker
        ;; line, but its selection logic still belongs to the line
        ;; BELOW the box.  Use a separate overlay so the bottom-only
        ;; selection check sees the right range.
        (unless has-body
          (let* ((bottom-masked (concat "\n" bottom-str))
                 (bottom-bare
                  (concat (propertize "\n" 'face 'region)
                          (gfm-pretty--str-with-region-bg bottom-str)
                          (gfm-pretty--region-tail)))
                 (select-range (cons (1+ marker-line-end)
                                     (1+ marker-line-end)))
                 (bottom-selected
                  (and (gfm-pretty--selection-bounds)
                       (<= (car (gfm-pretty--selection-bounds))
                           (1+ marker-line-end))
                       (< (1+ marker-line-end)
                          (cdr (gfm-pretty--selection-bounds))))))
            (gfm-pretty-callouts--make-display
             marker-line-end marker-line-end window
             'gfm-pretty-callouts-kind 'body-bottom
             'gfm-pretty-select-range select-range
             'gfm-pretty-after-masked bottom-masked
             'gfm-pretty-after-bare bottom-bare
             'after-string (if bottom-selected bottom-bare bottom-masked))))
        (ignore last-right-after-ov))))))

;;; Visibility helper

(defun gfm-pretty-callouts--block-visible-p (block ranges)
  "Non-nil if BLOCK's source range overlaps any range in RANGES."
  (gfm-pretty--block-visible-p
   block ranges #'gfm-pretty-callouts--block-range))

;;; Per-block apply (engine seam)

(defun gfm-pretty-callouts--apply-block (block window)
  "Engine `:apply-block-fn' — apply WINDOW's overlays for BLOCK.
Routes through `gfm-pretty-borders--apply-with-anchors' so width-
independent anchors are laid at most once per (block, rebuild pass)."
  (gfm-pretty-borders--apply-with-anchors
   block window
   :registry gfm-pretty-callouts--registry
   :range (gfm-pretty-callouts--block-range block)
   :anchors-fn #'gfm-pretty-callouts--apply-block-anchors
   :display-fn #'gfm-pretty-callouts--apply-block-display))

;;; Full-rebuild-required predicate (engine seam)

(defun gfm-pretty-callouts--full-rebuild-required-p (dirty)
  "Engine `:full-rebuild-required-p' — fold structural + adjacency checks.
Non-nil when DIRTY overlaps a `> [!TYPE]' marker line (structural)
or a line directly above / below a callout (adjacency)."
  (or (cl-some (lambda (r) (gfm-pretty--region-overlaps-p dirty r))
               (gfm-pretty-callouts--marker-line-ranges))
      (gfm-pretty-callouts--region-adjacent-to-callout-p dirty)))

;;; Structural-line + edit-adjacency helpers

(defun gfm-pretty-callouts--marker-line-ranges ()
  "Return per-line (BEG . END) ranges for every `> [!TYPE]' line."
  (mapcar (lambda (b)
            (let ((beg (nth 0 b)))
              (cons beg (save-excursion
                          (goto-char beg) (line-end-position)))))
          (gfm-pretty-callouts--find-blocks)))

(defun gfm-pretty-callouts--region-adjacent-to-callout-p (region)
  "Non-nil if REGION overlaps a line directly above or below a callout.
Edits there can create or destroy block boundaries."
  (cl-some
   (lambda (b)
     (let* ((beg (nth 0 b))
            (end (nth 1 b))
            (before-beg (save-excursion
                          (goto-char beg) (forward-line -1)
                          (line-beginning-position)))
            (before-end (save-excursion
                          (goto-char beg) (forward-line -1)
                          (line-end-position)))
            (after-beg (save-excursion
                         (goto-char end) (forward-line 1)
                         (line-beginning-position)))
            (after-end (save-excursion
                         (goto-char end) (forward-line 1)
                         (line-end-position))))
       (or (and (>= before-end (point-min))
                (gfm-pretty--region-overlaps-p
                 region (cons before-beg before-end)))
           (and (<= after-beg (point-max))
                (gfm-pretty--region-overlaps-p
                 region (cons after-beg after-end))))))
   (gfm-pretty-callouts--find-blocks)))

;;; Lifecycle hooks delegated to engine

(defun gfm-pretty-callouts--on-enable ()
  "Per-decorator setup invoked on enable.
Font-lock / face refresh are installed at load time."
  nil)

(defun gfm-pretty-callouts--on-disable ()
  "Per-decorator teardown invoked on disable.
Engine handles overlay teardown."
  nil)

;;; Callout faces (moved from modules/lang-markdown/lib.el)

(defface gfm-pretty-callouts-note-face
  '((((background dark))  :foreground "#89b4fa" :slant normal)
    (((background light)) :foreground "#1e66f5" :slant normal)
    (t :inherit font-lock-keyword-face :slant normal))
  "Header face for [!NOTE] callouts (blue)."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-tip-face
  '((((background dark))  :foreground "#a6e3a1" :slant normal)
    (((background light)) :foreground "#40a02b" :slant normal)
    (t :inherit success :slant normal))
  "Header face for [!TIP] callouts (green)."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-important-face
  '((((background dark))  :foreground "#cba6f7" :slant normal)
    (((background light)) :foreground "#8839ef" :slant normal)
    (t :inherit font-lock-keyword-face :slant normal))
  "Header face for [!IMPORTANT] callouts (purple)."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-warning-face
  '((((background dark))  :foreground "#fab387" :slant normal)
    (((background light)) :foreground "#fe640b" :slant normal)
    (t :inherit warning :slant normal))
  "Header face for [!WARNING] callouts (orange)."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-caution-face
  '((((background dark))  :foreground "#f38ba8" :slant normal)
    (((background light)) :foreground "#d20f39" :slant normal)
    (t :inherit error :slant normal))
  "Header face for [!CAUTION]/[!CRITICAL] callouts (red)."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-header-face
  '((t :weight semibold))
  "Face merged onto callout marker lines on top of the block face."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-note-body-face
  '((t))
  "Body face for [!NOTE] callouts; `:background' set dynamically from theme.
Default spec is intentionally empty so inline-markup emphasis (italic,
bold, underline, link, inline code) merged in from `markdown-italic-face'
et al. is not clobbered when this face is prepended to body chars."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-tip-body-face
  '((t))
  "Body face for [!TIP] callouts; `:background' set dynamically from theme.
See `gfm-pretty-callouts-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-important-body-face
  '((t))
  "Body face for [!IMPORTANT] callouts; `:background' set dynamically from theme.
See `gfm-pretty-callouts-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-warning-body-face
  '((t))
  "Body face for [!WARNING] callouts; `:background' set dynamically from theme.
See `gfm-pretty-callouts-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-caution-body-face
  '((t))
  "Body face for [!CAUTION]/[!CRITICAL] callouts.
`:background' is set dynamically from the active theme.  See
`gfm-pretty-callouts-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface gfm-pretty-callouts-prettier-ignore-comment-face
  '((t :inherit shadow :weight light))
  "Face for prettier-ignore comments."
  :group 'markdown-faces)

;; Backwards-compatible face aliases for the old `+markdown-' names.
(define-obsolete-face-alias '+markdown-gfm-callout-note-face
  'gfm-pretty-callouts-note-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-tip-face
  'gfm-pretty-callouts-tip-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-important-face
  'gfm-pretty-callouts-important-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-warning-face
  'gfm-pretty-callouts-warning-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-caution-face
  'gfm-pretty-callouts-caution-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-header-face
  'gfm-pretty-callouts-header-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-note-body-face
  'gfm-pretty-callouts-note-body-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-tip-body-face
  'gfm-pretty-callouts-tip-body-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-important-body-face
  'gfm-pretty-callouts-important-body-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-warning-body-face
  'gfm-pretty-callouts-warning-body-face "29.1")
(define-obsolete-face-alias '+markdown-gfm-callout-caution-body-face
  'gfm-pretty-callouts-caution-body-face "29.1")
(define-obsolete-face-alias '+markdown-prettier-ignore-comment-face
  'gfm-pretty-callouts-prettier-ignore-comment-face "29.1")

;;;###autoload
(defun +markdown-style-header-faces ()
  "Set markdown header faces to bold on TTY, semi-bold on graphic frames.
TTY fonts often lack a true semi-bold weight, so fall back to plain
bold there to keep the headings legible."
  (let ((spec '((((type tty)) :weight bold)
                (t :weight semi-bold))))
    (dolist (face '(markdown-header-face
                    markdown-header-face-1
                    markdown-header-face-2
                    markdown-header-face-3
                    markdown-header-face-4
                    markdown-header-face-5
                    markdown-header-face-6))
      (face-spec-set face spec))))

(defconst gfm-pretty-callouts--type-face-alist
  '(("NOTE"      . gfm-pretty-callouts-note-face)
    ("TIP"       . gfm-pretty-callouts-tip-face)
    ("IMPORTANT" . gfm-pretty-callouts-important-face)
    ("WARNING"   . gfm-pretty-callouts-warning-face)
    ("CAUTION"   . gfm-pretty-callouts-caution-face)
    ("CRITICAL"  . gfm-pretty-callouts-caution-face))
  "Map of GFM callout type label to its header face.")

(defconst gfm-pretty-callouts--type-body-face-alist
  '(("NOTE"      . gfm-pretty-callouts-note-body-face)
    ("TIP"       . gfm-pretty-callouts-tip-body-face)
    ("IMPORTANT" . gfm-pretty-callouts-important-body-face)
    ("WARNING"   . gfm-pretty-callouts-warning-body-face)
    ("CAUTION"   . gfm-pretty-callouts-caution-body-face)
    ("CRITICAL"  . gfm-pretty-callouts-caution-body-face))
  "Map of GFM callout type label to its body (tinted background) face.")

(defun gfm-pretty-callouts--font-lock-tint-bg (face)
  "Return a hex colour 10% from FACE's foreground toward the theme bg.
Mirrors `gfm-pretty-callouts--tinted-bg' so body face background matches the
overlay's tinted panel.  Returns nil if either colour is unresolvable."
  (require 'color)
  (let* ((fg (face-foreground face nil t))
         (bg (or (and (boundp '+theme-default-background)
                      +theme-default-background)
                 (face-background 'default nil t)))
         (fg-rgb (and fg (color-name-to-rgb fg)))
         (bg-rgb (and bg (color-name-to-rgb bg))))
    (when (and fg-rgb bg-rgb)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (b f) (+ b (* 0.1 (- f b))))
                                bg-rgb fg-rgb)
                     '(2))))))

;;;###autoload
(defun gfm-pretty-callouts--refresh-body-faces (&rest _)
  "Recompute `:background' on each callout body face from the current theme.
Also clears `:slant', `:weight', and `:underline' on each body face.
Body faces are prepended to body chars via `font-lock-prepend-text-property',
so any attribute they specify shadows the markdown emphasis faces beneath
them in the merge — emphasis would silently disappear inside callout
bodies."
  (dolist (entry gfm-pretty-callouts--type-body-face-alist)
    (let* ((type (car entry))
           (body-face (cdr entry))
           (header-face (alist-get type gfm-pretty-callouts--type-face-alist
                                   nil nil #'string=))
           (tint (and header-face
                      (gfm-pretty-callouts--font-lock-tint-bg header-face))))
      (set-face-attribute body-face nil
                          :slant 'unspecified
                          :weight 'unspecified
                          :underline 'unspecified)
      (when tint
        (set-face-background body-face tint)))))

(gfm-pretty-callouts--refresh-body-faces)

(add-hook '+theme-changed-hook #'gfm-pretty-callouts--refresh-body-faces)

(defun gfm-pretty-callouts--font-lock-block-end (marker-eol)
  "Return EOL of the last `>'-prefixed line following MARKER-EOL."
  (save-excursion
    (goto-char marker-eol)
    (let ((end marker-eol))
      (forward-line 1)
      (while (and (not (eobp))
                  (eq (char-after) ?>))
        (setq end (line-end-position))
        (forward-line 1))
      end)))

(defun gfm-pretty-callouts--font-lock-matcher (limit)
  "Font-lock matcher that spans full callout blocks up to LIMIT.
Match data: group 0 = whole block, group 1 = type label, group 2 =
marker line.  Sets `font-lock-multiline' on the matched region so
edits inside the block trigger refontification of the entire block."
  (when (re-search-forward gfm-pretty-callouts--marker-re limit t)
    (let* ((mbeg (match-beginning 0))
           (mend (match-end 0))
           (tbeg (match-beginning 1))
           (tend (match-end 1))
           (block-end (gfm-pretty-callouts--font-lock-block-end mend)))
      (with-silent-modifications
        (put-text-property mbeg block-end 'font-lock-multiline t))
      (set-match-data (list mbeg block-end tbeg tend mbeg mend))
      t)))

(defun gfm-pretty-callouts--font-lock-paint-body (type block-beg block-end)
  "Apply TYPE's body face to body content between BLOCK-BEG and BLOCK-END.
Skips newlines so the tinted background does not bleed one column past
the right border on body lines (the trailing newline character would
otherwise pick up `:background')."
  (when-let* ((body-face (alist-get type
                                    gfm-pretty-callouts--type-body-face-alist
                                    nil nil #'string=)))
    (save-excursion
      (goto-char block-beg)
      (forward-line 1)
      (while (< (point) block-end)
        (let ((lbeg (line-beginning-position))
              (lend (min block-end (line-end-position))))
          (when (< lbeg lend)
            (font-lock-prepend-text-property lbeg lend 'face body-face)))
        (forward-line 1)))))

(defun gfm-pretty-callouts--font-lock-extend-region ()
  "Extend `font-lock-beg' / `font-lock-end' to cover any callout block.
If the region overlaps a `> [!TYPE]' marker line or any of its
`>'-prefixed continuation lines, widen so the whole block is refontified
together.  Without this, typing on a new continuation line refontifies
only that line, missing the multi-line matcher's anchor."
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (let ((changed nil))
    (save-excursion
      (goto-char font-lock-beg)
      (forward-line 0)
      (while (and (not (bobp))
                  (save-excursion
                    (forward-line -1)
                    (looking-at-p (rx bol ">"))))
        (forward-line -1))
      (when (looking-at-p gfm-pretty-callouts--marker-re)
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point)
                changed t))
        (let ((block-end (gfm-pretty-callouts--font-lock-block-end
                          (line-end-position))))
          (when (> block-end font-lock-end)
            (setq font-lock-end block-end
                  changed t)))))
    changed))

;;;###autoload
(defun gfm-pretty-callouts-install-font-lock ()
  "Add font-lock keywords for GFM callout syntax."
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            #'gfm-pretty-callouts--font-lock-extend-region nil t)
  (font-lock-add-keywords
   nil
   `((gfm-pretty-callouts--font-lock-matcher
      (2 (alist-get (match-string-no-properties 1)
                    gfm-pretty-callouts--type-face-alist
                    nil nil #'string=)
         prepend)
      (2 'gfm-pretty-callouts-header-face prepend)
      (0 (progn (gfm-pretty-callouts--font-lock-paint-body
                 (match-string-no-properties 1)
                 (match-beginning 0)
                 (match-end 0))
                nil)))
     (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-start" (1+ space) "-->")
      0 'gfm-pretty-callouts-prettier-ignore-comment-face prepend)
     (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-end" (1+ space) "-->")
      0 'gfm-pretty-callouts-prettier-ignore-comment-face prepend)
     (,(rx (group "$" (or (seq "{" (1+ (any alnum "_")) "}")
                          (seq (any upper "_") (1+ (any upper digit "_"))))))
      1 'font-lock-constant-face prepend))))

;; Strip `:background' and `:extend' from `markdown-blockquote-face' so
;; the face does not paint a coloured stripe through blockquote text or
;; past EOL.  Themes (e.g. catppuccin) set these attributes directly on
;; the face — `:inherit'-only fixes don't suffice, every value must be
;; cleared.  Run on `+theme-changed-hook' too so theme switches don't
;; re-introduce the stripe; the original neutralisation only ran once
;; at markdown-mode load time and lost out whenever the theme reapplied
;; afterwards.  Italic and foreground are left alone — they're
;; user-visible markdown emphasis on plain blockquotes and the rail-
;; only treatment doesn't conflict with them.
(defun gfm-pretty-callouts--strip-blockquote-face-bg (&rest _)
  "Clear `:background' and `:extend' on `markdown-blockquote-face'."
  (when (facep 'markdown-blockquote-face)
    (set-face-attribute 'markdown-blockquote-face nil
                        :background 'unspecified
                        :extend 'unspecified)))

(with-eval-after-load 'markdown-mode
  (gfm-pretty-callouts--strip-blockquote-face-bg))
(add-hook '+theme-changed-hook #'gfm-pretty-callouts--strip-blockquote-face-bg)

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'callouts
    :registry           gfm-pretty-callouts--registry
    :collect-fn         #'gfm-pretty-callouts--collect-blocks
    :range-fn           #'gfm-pretty-callouts--block-range
    :apply-block-fn     #'gfm-pretty-callouts--apply-block
    :full-rebuild-required-p #'gfm-pretty-callouts--full-rebuild-required-p
    :on-enable-fn       #'gfm-pretty-callouts--on-enable
    :on-disable-fn      #'gfm-pretty-callouts--on-disable))

(provide 'gfm-pretty-callouts)

;;; gfm-pretty-callouts.el ends here
