;;; +gfm-callouts.el --- Box overlays for GitHub Flavored Markdown callouts -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that draws box overlays around GitHub Flavored Markdown
;; callout blockquotes:
;;
;;   > [!IMPORTANT]
;;   > Lorem ipsum.
;;
;; Mirrors `gfm-code-fences-mode' on the Path C anchor / display split:
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
;; builders, wrap simulator) live in `+gfm-block-borders.el'.

;;; Code:

(require 'cl-lib)
(require '+gfm-block-borders)

(defgroup gfm-callouts nil
  "Box overlays for GitHub Flavored Markdown callouts."
  :group 'markdown-faces)

(defface gfm-callouts-box-face
  '((t :inherit shadow))
  "Fallback face for callout box-drawing characters."
  :group 'gfm-callouts)

(defcustom gfm-callouts-slow-rebuild-threshold 0.05
  "Threshold in seconds above which a single rebuild emits a warning."
  :type 'number
  :group 'gfm-callouts)

(defconst gfm-callouts--type-faces
  '(("NOTE"      . +markdown-gfm-callout-note-face)
    ("TIP"       . +markdown-gfm-callout-tip-face)
    ("IMPORTANT" . +markdown-gfm-callout-important-face)
    ("WARNING"   . +markdown-gfm-callout-warning-face)
    ("CAUTION"   . +markdown-gfm-callout-caution-face)
    ("CRITICAL"  . +markdown-gfm-callout-caution-face))
  "Map of callout type to face used for box border and label.")

(defconst gfm-callouts--marker-re
  (rx bol "> " "[!"
      (group (or "NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
      "]" (* space) eol)
  "Regexp matching a callout marker line. Group 1 captures the type.")

(defconst gfm-callouts--blockquote-line-re
  (rx bol ">")
  "Regexp matching any blockquote continuation line.")

;;; Block discovery (cached)

(defvar-local gfm-callouts--blocks-cache nil
  "Pair (TICK . BLOCKS) memoising `gfm-callouts--find-blocks'.
TICK is `buffer-chars-modified-tick' at the time of scan.")

(defun gfm-callouts--find-blocks-1 ()
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
          (while (re-search-forward gfm-callouts--marker-re nil t)
            (let ((block-beg (line-beginning-position))
                  (block-end (line-end-position))
                  (type (match-string-no-properties 1)))
              (forward-line 1)
              (while (and (not (eobp))
                          (looking-at gfm-callouts--blockquote-line-re))
                (setq block-end (line-end-position))
                (forward-line 1))
              (push (list block-beg block-end type) blocks))))))
    (nreverse blocks)))

(defun gfm-callouts--find-blocks ()
  "Return all callout blocks in the current buffer.
Memoised by `buffer-chars-modified-tick' so repeat calls without an
intervening edit reuse the cached scan."
  (let ((tick (buffer-chars-modified-tick)))
    (cond
     ((and gfm-callouts--blocks-cache
           (= tick (car gfm-callouts--blocks-cache)))
      (cdr gfm-callouts--blocks-cache))
     (t
      (let ((blocks (gfm-callouts--find-blocks-1)))
        (setq gfm-callouts--blocks-cache (cons tick blocks))
        blocks)))))

;;; Tinted background for the callout panel

(defun gfm-callouts--tinted-bg (face)
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

;;; Performance instrumentation (lightweight)

(defvar-local gfm-callouts--stats nil
  "Per-buffer alist of rebuild stats: (rebuild-count total-time last-time).")

(defun gfm-callouts--init-stats ()
  "Reset the per-buffer rebuild stats to zero."
  (setq gfm-callouts--stats
        (list (cons 'rebuild-count 0)
              (cons 'total-time 0.0)
              (cons 'last-time 0.0))))

(defun gfm-callouts--record-stats (duration)
  "Record DURATION (seconds) for one rebuild."
  (unless gfm-callouts--stats (gfm-callouts--init-stats))
  (cl-incf (alist-get 'rebuild-count gfm-callouts--stats))
  (cl-incf (alist-get 'total-time gfm-callouts--stats) duration)
  (setf (alist-get 'last-time gfm-callouts--stats) duration)
  (when (> duration gfm-callouts-slow-rebuild-threshold)
    (message "gfm-callouts: slow rebuild in %s: %.3fs"
             (buffer-name) duration)))

;;; Overlay registry

(defvar gfm-callouts-mode)

(defvar-local gfm-callouts--overlays nil
  "All callout overlays currently in this buffer.")

(defvar-local gfm-callouts--hidden-ovs nil
  "Revealable overlays whose display is currently suppressed.")

(defconst gfm-callouts--registry
  (gfm-block-borders-registry-for
   'gfm-callouts
   'gfm-callouts--overlays
   'gfm-callouts--hidden-ovs)
  "Shared overlay-registry context for callouts.")

(defsubst gfm-callouts--make-anchor (beg end &rest props)
  "Make an anchor overlay over [BEG, END] with PROPS."
  (apply #'gfm-block-borders--make-anchor
         gfm-callouts--registry beg end props))


(defsubst gfm-callouts--make-display (beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW with PROPS."
  (apply #'gfm-block-borders--make-display
         gfm-callouts--registry beg end window props))

(defsubst gfm-callouts--remove-overlays (&optional beg end)
  "Remove all gfm-callouts overlays between BEG and END."
  (gfm-block-borders--remove-overlays gfm-callouts--registry beg end))

(defsubst gfm-callouts--prune-dead-overlays ()
  "Drop overlays from the registry whose buffer is gone."
  (gfm-block-borders--prune-dead-overlays gfm-callouts--registry))

(defsubst gfm-callouts--register (ov)
  "Tag OV as a callout overlay and remember it for bulk cleanup."
  (gfm-block-borders--register gfm-callouts--registry ov))

;;; Block enumeration

(cl-defstruct (gfm-callouts--block
               (:constructor gfm-callouts--make-block)
               (:copier nil))
  "Tagged callout block for unified rebuild dispatch.
RANGE is (LINE-BEG . LINE-END+1) covering the full source range,
used for visibility + scoped-rebuild containment.  PAYLOAD is the
raw (BEG END TYPE) tuple."
  range payload)

(defun gfm-callouts--collect-blocks ()
  "Return tagged callout blocks for the buffer."
  (mapcar (lambda (b)
            (gfm-callouts--make-block
             :range (cons (nth 0 b) (1+ (nth 1 b)))
             :payload b))
          (gfm-callouts--find-blocks)))

;;; Rendering

(defun gfm-callouts--upright (str face &optional bg)
  "Propertize STR with FACE forced upright; optionally set background BG.
The display string sits over buffer text whose face may carry
`:slant italic'; unspecified attributes leak from the underlying face,
so anchor `:slant normal'.  BG paints the box's tinted background onto
each decoration char."
  (let* ((spec `(:inherit ,face :slant normal))
         (spec (if bg (append spec (list :background bg)) spec)))
    (propertize str 'face spec)))

(defun gfm-callouts--callout-top-strings (width title face buffer-width &optional bg)
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
                (gfm-callouts--upright "┌─ " face bg)
                (gfm-callouts--upright title face bg)
                (gfm-callouts--upright " " face bg)
                (gfm-callouts--upright (make-string dash-fill ?─) face bg)
                (gfm-callouts--upright "┐" face bg)))
         (full-len (length full))
         (split-at (min buffer-width full-len)))
    (cons (substring full 0 split-at)
          (substring full split-at))))

(defun gfm-callouts--callout-bottom-string (width face &optional bg)
  "Build the bottom border string of WIDTH cols, tinted with BG."
  (concat (gfm-callouts--upright "└" face bg)
          (gfm-callouts--upright (make-string (max 1 (- width 2)) ?─) face bg)
          (gfm-callouts--upright "┘" face bg)))

(defun gfm-callouts--right-after (box-width face bg)
  "Build the body-line right-edge after-string.
Pads with `space :align-to' to BOX-WIDTH-2, then a tinted gap, then
`│'.  Carries the `cursor' text property so cursor motion crosses it.

A trailing `(space :align-to right)' in the default face fills the
visual line from `│' to the window's right edge, masking any
`:extend t' past-EOL fill that a foreign overlay (`hl-line',
`region') would otherwise paint past the border — see
`gfm-block-borders--right-after' for the rationale."
  (let* ((align-face (let ((spec `(:inherit ,face :slant normal)))
                       (if bg (append spec (list :background bg)) spec)))
         (str (concat
               (propertize " "
                           'display `(space :align-to ,(- box-width 2))
                           'face align-face)
               (gfm-callouts--upright " " face bg)
               (gfm-callouts--upright "│" face bg)
               (propertize " "
                           'display '(space :align-to right)
                           'face 'default))))
    (put-text-property 0 1 'cursor t str)
    str))

(defun gfm-callouts--right-after-overflow (face bg line-text window)
  "Build the right-edge after-string for a wrapped body line.
Simulates word-wrap of `│ ' + LINE-TEXT in WINDOW's width to compute
how much padding is needed before the closing `│' on the final wrapped
visual row.  A trailing `(space :align-to right)' in the default face
extends the last wrapped row to the window edge, suppressing past-EOL
`:extend' leaks — see `gfm-callouts--right-after'."
  (let* ((text-width (gfm-block-borders--available-width window))
         (visual-col (gfm-block-borders--last-visual-col
                      (concat "│ " line-text) text-width
                      gfm-block-borders--wrap-prefix-w))
         (target-col (1- text-width))
         (pad-len (max 0 (- target-col visual-col)))
         (face-spec (let ((s `(:inherit ,face :slant normal)))
                      (if bg (append s (list :background bg)) s)))
         (pad (propertize (make-string pad-len ?\s) 'face face-spec))
         (pipe (gfm-callouts--upright "│" face bg))
         (tail (propertize " " 'display '(space :align-to right)
                           'face 'default))
         (str (concat pad pipe tail)))
    (put-text-property 0 1 'cursor t str)
    str))

(defun gfm-callouts--apply-block-anchors (block)
  "Apply width-independent anchor overlays for BLOCK.
Paints the per-line tinted background and the per-body-line
`wrap-prefix' so wrapped content stays inside the box.  Anchors are
shared across windows; reveal does not touch them.

Widens for the duration of the apply so a BLOCK whose source range lies
outside the current restriction (e.g. another slide under
`+presentation-mode') can still be parsed and decorated.  Without this,
the per-line walk would clamp at the narrowing's `point-max' and loop
forever.  Display under narrowing is naturally clipped by Emacs'
overlay engine."
  (save-restriction
   (widen)
  (cl-destructuring-bind (beg end type) (gfm-callouts--block-payload block)
    (let* ((type-face (alist-get type gfm-callouts--type-faces
                                  nil nil #'string=))
           (border-face (or type-face 'gfm-callouts-box-face))
           (tint (gfm-callouts--tinted-bg border-face))
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
                             (let ((s `(:inherit ,border-face :slant normal)))
                               (if tint
                                   (append s (list :background tint))
                                 s)))))
      ;; Per-line anchor: tinted background + wrap-prefix on body lines.
      ;; Iterate via position math — `forward-line' inside the
      ;; overlay-creation loop interacts with cursor-intangible /
      ;; display props (set by `gfm-code-fences-mode' on the same
      ;; buffer) and can stall mid-block; see the matching note in
      ;; `+gfm-code-fences.el'.  Combining face + wrap-prefix on a
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
            (apply #'gfm-callouts--make-anchor
                   lbeg lend
                   'face bg-face
                   (and is-body (list 'wrap-prefix wrap)))
            (setq p (min (1+ end) (1+ lend))))))))))

(defun gfm-callouts--apply-block-display (block window)
  "Apply per-WINDOW display overlays for BLOCK.
Top border (leading on marker line + trailing after), per-body-line
`> ' → `│ ' substitution + right-edge after-string, bottom border
hung off the last body line's end (or the marker's trailing piece for
a body-less callout).
See `gfm-callouts--apply-block-anchors' for the widening rationale."
  (save-restriction
   (widen)
  (cl-destructuring-bind (beg end type) (gfm-callouts--block-payload block)
    (let* ((type-face (alist-get type gfm-callouts--type-faces
                                  nil nil #'string=))
           (border-face (or type-face 'gfm-callouts-box-face))
           (tint (gfm-callouts--tinted-bg border-face))
           (text-width (gfm-block-borders--available-width window))
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
                            (gfm-block-borders--max-line-width
                             body-beg-pos end 2)
                          0))
           (box-width (min text-width (max 80 (+ max-content 4))))
           (content-budget (- box-width 4))
           (top-split (gfm-callouts--callout-top-strings
                       box-width type border-face marker-buf-w tint))
           (bottom-str (gfm-callouts--callout-bottom-string
                        box-width border-face tint))
           (edge (gfm-callouts--upright "│ " border-face tint))
           (last-right-after-ov nil))
      ;; Top — leading on marker line, trailing after.
      (gfm-callouts--make-display
       beg marker-line-end window
       'gfm-callouts-kind 'top-leading
       'gfm-callouts-revealable t
       'evaporate t
       'display (car top-split))
      (let ((trailing-ov
             (gfm-callouts--make-display
              marker-line-end marker-line-end window
              'gfm-callouts-kind 'top-trailing
              'after-string (cdr top-split))))
        ;; Body lines.  Iterate via explicit text-position math, not
        ;; `forward-line': inside this overlay-creation loop,
        ;; `forward-line' interacts with cursor-intangible / display
        ;; props (set by `gfm-code-fences-mode' on the same buffer)
        ;; and can stall mid-block, spinning forever — see the
        ;; matching note in `+gfm-code-fences.el'.
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
                          (gfm-callouts--right-after-overflow
                           border-face tint line-text window)
                        (gfm-callouts--right-after
                         box-width border-face tint)))
                     (after-with-bottom
                      (cond
                       (last-body
                        (let ((s (concat right-after "\n" bottom-str)))
                          (put-text-property 0 1 'cursor t s)
                          s))
                       (t right-after))))
                ;; `> ' → `│ ' substitution as a per-window display so
                ;; reveal in window A doesn't expose the source in B.
                (when (and (>= (- lend lbeg) 2)
                           (eq (char-after lbeg) ?>)
                           (eq (char-after (1+ lbeg)) ?\s))
                  (gfm-callouts--make-display
                   lbeg (+ lbeg 2) window
                   'gfm-callouts-kind 'body-prefix
                   'gfm-callouts-revealable t
                   'evaporate t
                   'display edge))
                ;; Right-edge (and bottom on the last line).
                (setq last-right-after-ov
                      (gfm-callouts--make-display
                       lend lend window
                       'gfm-callouts-kind 'body-rhs
                       'after-string after-with-bottom))
                (setq p (min (1+ end) (1+ lend)))))))
        ;; Body-less callout: bottom border attaches to marker trailing.
        (unless has-body
          (let* ((existing (overlay-get trailing-ov 'after-string))
                 (new (concat existing "\n" bottom-str)))
            (put-text-property 0 1 'cursor t new)
            (overlay-put trailing-ov 'after-string new)))
        (ignore last-right-after-ov))))))

(defun gfm-callouts--apply-overlays ()
  "Create overlays for every callout block in the buffer.
Anchors shared, displays per-window.  Returns the block count."
  (save-excursion
    (let* ((blocks (gfm-callouts--collect-blocks))
           (windows (or (gfm-block-borders--display-windows) (list nil))))
      (dolist (block blocks)
        (gfm-callouts--apply-block-anchors block))
      (dolist (window windows)
        (dolist (block blocks)
          (gfm-callouts--apply-block-display block window)))
      (length blocks))))

;;; Cursor-driven reveal (selected-window aware)

(defun gfm-callouts--reveal ()
  "Suppress display on the selected window's revealable overlays at point.
Restores overlays no longer at point.  Per-window: only overlays
whose `window' property is nil or matches the selected window are
toggled, so cursor in window A does not expose source in window B."
  (let ((pos (point))
        (win (selected-window)))
    (setq gfm-callouts--hidden-ovs
          (cl-loop for ov in gfm-callouts--hidden-ovs
                   if (and (overlay-buffer ov)
                           (>= pos (overlay-start ov))
                           (<= pos (overlay-end ov)))
                   collect ov
                   else do (when (overlay-buffer ov)
                             (overlay-put ov 'display
                                          (overlay-get ov 'gfm-callouts-saved-display))
                             (overlay-put ov 'gfm-callouts-saved-display nil))))
    (dolist (ov (overlays-in pos (1+ pos)))
      (when (and (overlay-get ov 'gfm-callouts-revealable)
                 (overlay-get ov 'display)
                 (let ((w (overlay-get ov 'window)))
                   (or (null w) (eq w win)))
                 (not (memq ov gfm-callouts--hidden-ovs)))
        (overlay-put ov 'gfm-callouts-saved-display
                     (overlay-get ov 'display))
        (overlay-put ov 'display nil)
        (push ov gfm-callouts--hidden-ovs)))))

;;; Rebuild scheduler state

(defvar-local gfm-callouts--last-window-state nil
  "Snapshot of the windows showing the buffer at the last rebuild.")

(defvar-local gfm-callouts--dirty-region nil
  "Buffer-local (BEG . END) covering all unrebuilt edits.")

(defvar-local gfm-callouts--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defun gfm-callouts--rebuild ()
  "Remove and recreate all gfm-callouts overlays."
  (let ((start (current-time)))
    (gfm-callouts--remove-overlays)
    (setq gfm-callouts--dirty-region nil)
    (gfm-callouts--apply-overlays)
    (gfm-callouts--record-stats (float-time (time-since start)))
    (setq gfm-callouts--last-window-state
          (gfm-block-borders--window-state))))

(defun gfm-callouts--rebuild-block (block)
  "Tear down BLOCK's overlays and re-apply just that block."
  (let* ((start (current-time))
         (range (gfm-callouts--block-range block)))
    (gfm-callouts--remove-overlays (car range) (cdr range))
    (gfm-callouts--apply-block-anchors block)
    (dolist (window (or (gfm-block-borders--display-windows) (list nil)))
      (gfm-callouts--apply-block-display block window))
    (gfm-callouts--record-stats (float-time (time-since start)))))

(defun gfm-callouts--rebuild-blocks (blocks)
  "Tear down each block in BLOCKS and re-apply them in one pass."
  (let ((start (current-time))
        (windows (or (gfm-block-borders--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (gfm-callouts--block-range block)))
        (gfm-callouts--remove-overlays (car range) (cdr range)))
      (gfm-callouts--apply-block-anchors block)
      (dolist (window windows)
        (gfm-callouts--apply-block-display block window)))
    (gfm-callouts--record-stats (float-time (time-since start)))))

(defconst gfm-callouts--reconciler
  (gfm-block-borders-make-reconciler
   :registry gfm-callouts--registry
   :state-symbol 'gfm-callouts--last-window-state
   :dirty-region-symbol 'gfm-callouts--dirty-region
   :timer-symbol 'gfm-callouts--rebuild-timer
   :mode-symbol 'gfm-callouts-mode
   :collect-fn #'gfm-callouts--collect-blocks
   :range-fn #'gfm-callouts--block-range
   :apply-anchors-fn #'gfm-callouts--apply-block-anchors
   :apply-display-fn #'gfm-callouts--apply-block-display
   :rebuild-fn #'gfm-callouts--rebuild)
  "Shared reconciler context for callouts.")

(defun gfm-callouts--rebuild-block-for-window (block window)
  "Replace WINDOW's display overlays for BLOCK at the current width."
  (gfm-block-borders--rebuild-block-for-window
   gfm-callouts--reconciler block window))

;;; Visible-first prioritised rebuild

(defun gfm-callouts--block-visible-p (block ranges)
  "Non-nil if BLOCK's source range overlaps any range in RANGES."
  (gfm-block-borders--block-visible-p
   block ranges #'gfm-callouts--block-range))

(defun gfm-callouts--rebuild-prioritised ()
  "Rebuild visible-window blocks first; defer off-screen ones one idle tick."
  (let ((ranges (gfm-block-borders--visible-window-ranges)))
    (cond
     ((null ranges) (gfm-callouts--rebuild))
     (t
      (let* ((blocks (gfm-callouts--collect-blocks))
             (visible (cl-remove-if-not
                       (lambda (b) (gfm-callouts--block-visible-p b ranges))
                       blocks))
             (offscreen (cl-set-difference blocks visible)))
        (when visible
          (gfm-callouts--rebuild-blocks visible))
        (setq gfm-callouts--dirty-region nil
              gfm-callouts--last-window-state
              (gfm-block-borders--window-state))
        (when offscreen
          (run-with-idle-timer
           0 nil
           (lambda (buf bs)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when gfm-callouts-mode
                   (gfm-callouts--rebuild-blocks bs)))))
           (current-buffer) offscreen)))))))

(defun gfm-callouts--reconcile-windows ()
  "Reconcile display overlays with current window state."
  (gfm-block-borders--reconcile-windows gfm-callouts--reconciler))

;;; Scoped post-edit rebuild

(defsubst gfm-callouts--extend-dirty-region (beg end)
  "Extend the buffer's dirty region to cover BEG..END."
  (gfm-block-borders--extend-dirty-region
   'gfm-callouts--dirty-region beg end))

(defun gfm-callouts--marker-line-ranges ()
  "Return per-line (BEG . END) ranges for every `> [!TYPE]' line."
  (mapcar (lambda (b)
            (let ((beg (nth 0 b)))
              (cons beg (save-excursion
                          (goto-char beg) (line-end-position)))))
          (gfm-callouts--find-blocks)))

(defun gfm-callouts--region-overlaps-marker-line-p (region)
  "Non-nil if REGION overlaps any callout marker line."
  (cl-some (lambda (r) (gfm-block-borders--region-overlaps-p region r))
           (gfm-callouts--marker-line-ranges)))

(defun gfm-callouts--region-adjacent-to-callout-p (region)
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
                (gfm-block-borders--region-overlaps-p
                 region (cons before-beg before-end)))
           (and (<= after-beg (point-max))
                (gfm-block-borders--region-overlaps-p
                 region (cons after-beg after-end))))))
   (gfm-callouts--find-blocks)))

(defun gfm-callouts--block-fully-contains-p (block region)
  "Non-nil if REGION lies inside BLOCK's source range."
  (let ((br (gfm-callouts--block-range block)))
    (and (>= (car region) (car br))
         (<= (cdr region) (cdr br)))))

(defun gfm-callouts--rebuild-scoped ()
  "Rebuild only what `gfm-callouts--dirty-region' demands."
  (let ((dirty gfm-callouts--dirty-region))
    (setq gfm-callouts--dirty-region nil)
    (cond
     ((null dirty) nil)
     ((gfm-callouts--region-overlaps-marker-line-p dirty)
      (gfm-callouts--rebuild))
     ((gfm-callouts--region-adjacent-to-callout-p dirty)
      (gfm-callouts--rebuild))
     (t
      (let* ((blocks (gfm-callouts--collect-blocks))
             (matching (cl-loop for b in blocks
                                when (gfm-block-borders--region-overlaps-p
                                      dirty
                                      (gfm-callouts--block-range b))
                                collect b)))
        (cond
         ((null matching) nil)
         ((and (null (cdr matching))
               (gfm-callouts--block-fully-contains-p (car matching) dirty))
          (gfm-callouts--rebuild-block (car matching)))
         (t (gfm-callouts--rebuild))))))))

;;; Schedulers

(defsubst gfm-callouts--arm-rebuild-timer (callback)
  "Cancel any pending rebuild timer and schedule CALLBACK after idle."
  (gfm-block-borders--arm-rebuild-timer
   'gfm-callouts--rebuild-timer 'gfm-callouts-mode callback))

(defun gfm-callouts--schedule-rebuild (&optional beg end _len)
  "Merge BEG..END into the dirty region and arm the rebuild timer."
  (unless (buffer-base-buffer)
    (when (and beg end)
      (gfm-callouts--extend-dirty-region beg end))
    (gfm-callouts--arm-rebuild-timer #'gfm-callouts--rebuild-scoped)))

(defun gfm-callouts--schedule-full-rebuild (&rest _)
  "Schedule a window reconciliation if window state has changed."
  (unless (buffer-base-buffer)
    (let ((state (gfm-block-borders--window-state)))
      (unless (equal state gfm-callouts--last-window-state)
        (gfm-callouts--arm-rebuild-timer
         #'gfm-callouts--reconcile-windows)))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-callouts-mode
  "Render GFM callout blockquotes as boxes."
  :lighter " gfm-cb"
  (if gfm-callouts-mode
      (progn
        (gfm-callouts--init-stats)
        (gfm-callouts--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-callouts--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-callouts--schedule-full-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-callouts--reveal nil t))
    (remove-hook 'after-change-functions
                 #'gfm-callouts--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-callouts--schedule-full-rebuild t)
    (remove-hook 'post-command-hook #'gfm-callouts--reveal t)
    (when (timerp gfm-callouts--rebuild-timer)
      (cancel-timer gfm-callouts--rebuild-timer))
    (gfm-callouts--remove-overlays)))

(provide '+gfm-callouts)

;;; +gfm-callouts.el ends here
