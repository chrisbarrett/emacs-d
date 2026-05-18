;;; gfm-pretty-callouts.el --- Box overlays for GitHub Flavored Markdown callouts -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that draws box overlays around GitHub Flavored Markdown
;; callout blockquotes:
;;
;;   > [!IMPORTANT]
;;   > Lorem ipsum.
;;
;; Mirrors `gfm-pretty-fences-mode' on the Path C anchor / display split:
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

(defcustom gfm-pretty-callouts-slow-rebuild-threshold 0.05
  "Threshold in seconds above which a single rebuild emits a warning."
  :type 'number
  :group 'gfm-pretty-callouts)

(defconst gfm-pretty-callouts--type-faces
  '(("NOTE"      . +markdown-gfm-callout-note-face)
    ("TIP"       . +markdown-gfm-callout-tip-face)
    ("IMPORTANT" . +markdown-gfm-callout-important-face)
    ("WARNING"   . +markdown-gfm-callout-warning-face)
    ("CAUTION"   . +markdown-gfm-callout-caution-face)
    ("CRITICAL"  . +markdown-gfm-callout-caution-face))
  "Map of callout type to face used for box border and label.")

(defconst gfm-pretty-callouts--marker-re
  (rx bol "> " "[!"
      (group (or "NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
      "]" (* space) eol)
  "Regexp matching a callout marker line. Group 1 captures the type.")

(defconst gfm-pretty-callouts--blockquote-line-re
  (rx bol ">")
  "Regexp matching any blockquote continuation line.")

;;; Block discovery (cached)

(defvar-local gfm-pretty-callouts--blocks-cache nil
  "Pair (TICK . BLOCKS) memoising `gfm-pretty-callouts--find-blocks'.
TICK is `buffer-chars-modified-tick' at the time of scan.")

(defun gfm-pretty-callouts--find-blocks-1 ()
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

(defun gfm-pretty-callouts--find-blocks ()
  "Return all callout blocks in the current buffer.
Memoised by `buffer-chars-modified-tick' so repeat calls without an
intervening edit reuse the cached scan."
  (let ((tick (buffer-chars-modified-tick)))
    (cond
     ((and gfm-pretty-callouts--blocks-cache
           (= tick (car gfm-pretty-callouts--blocks-cache)))
      (cdr gfm-pretty-callouts--blocks-cache))
     (t
      (let ((blocks (gfm-pretty-callouts--find-blocks-1)))
        (setq gfm-pretty-callouts--blocks-cache (cons tick blocks))
        blocks)))))

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

;;; Performance instrumentation (lightweight)

(defvar-local gfm-pretty-callouts--stats nil
  "Per-buffer alist of rebuild stats: (rebuild-count total-time last-time).")

(defun gfm-pretty-callouts--init-stats ()
  "Reset the per-buffer rebuild stats to zero."
  (setq gfm-pretty-callouts--stats
        (list (cons 'rebuild-count 0)
              (cons 'total-time 0.0)
              (cons 'last-time 0.0))))

(defun gfm-pretty-callouts--record-stats (duration)
  "Record DURATION (seconds) for one rebuild."
  (unless gfm-pretty-callouts--stats (gfm-pretty-callouts--init-stats))
  (cl-incf (alist-get 'rebuild-count gfm-pretty-callouts--stats))
  (cl-incf (alist-get 'total-time gfm-pretty-callouts--stats) duration)
  (setf (alist-get 'last-time gfm-pretty-callouts--stats) duration)
  (when (> duration gfm-pretty-callouts-slow-rebuild-threshold)
    (message "gfm-pretty-callouts: slow rebuild in %s: %.3fs"
             (buffer-name) duration)))

;;; Overlay registry

(defvar gfm-pretty-callouts-mode)

(defvar-local gfm-pretty-callouts--overlays nil
  "All callout overlays currently in this buffer.")

(defvar-local gfm-pretty-callouts--hidden-ovs nil
  "Revealable overlays whose display is currently suppressed.")

(defconst gfm-pretty-callouts--registry
  (gfm-pretty--registry-for
   'gfm-pretty-callouts
   'gfm-pretty-callouts--overlays
   'gfm-pretty-callouts--hidden-ovs)
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
  "Return tagged callout blocks for the buffer."
  (mapcar (lambda (b)
            (gfm-pretty-callouts--make-block
             :range (cons (nth 0 b) (1+ (nth 1 b)))
             :payload b))
          (gfm-pretty-callouts--find-blocks)))

;;; Rendering

(defun gfm-pretty-callouts--upright (str face &optional bg)
  "Propertize STR with FACE forced upright; optionally set background BG.
The display string sits over buffer text whose face may carry
`:slant italic'; unspecified attributes leak from the underlying face,
so anchor `:slant normal'.  BG paints the box's tinted background onto
each decoration char."
  (let* ((spec `(:inherit ,face :slant normal))
         (spec (if bg (append spec (list :background bg)) spec)))
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
                (gfm-pretty-callouts--upright "┌─ " face bg)
                (gfm-pretty-callouts--upright title face bg)
                (gfm-pretty-callouts--upright " " face bg)
                (gfm-pretty-callouts--upright (make-string dash-fill ?─) face bg)
                (gfm-pretty-callouts--upright "┐" face bg)))
         (full-len (length full))
         (split-at (min buffer-width full-len)))
    (cons (substring full 0 split-at)
          (substring full split-at))))

(defun gfm-pretty-callouts--callout-bottom-string (width face &optional bg)
  "Build the bottom border string of WIDTH cols, tinted with BG."
  (concat (gfm-pretty-callouts--upright "└" face bg)
          (gfm-pretty-callouts--upright (make-string (max 1 (- width 2)) ?─) face bg)
          (gfm-pretty-callouts--upright "┘" face bg)))

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
               (gfm-pretty-callouts--upright "│" face bg)
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
         (pipe (gfm-pretty-callouts--upright "│" face bg))
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
                             (let ((s `(:inherit ,border-face :slant normal)))
                               (if tint
                                   (append s (list :background tint))
                                 s)))))
      ;; Per-line anchor: tinted background + wrap-prefix on body lines.
      ;; Iterate via position math — `forward-line' inside the
      ;; overlay-creation loop interacts with cursor-intangible /
      ;; display props (set by `gfm-pretty-fences-mode' on the same
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
      (gfm-pretty-callouts--make-display
       beg marker-line-end window
       'gfm-pretty-callouts-kind 'top-leading
       'gfm-pretty-callouts-revealable t
       'evaporate t
       'display (car top-split))
      (let ((trailing-ov
             (gfm-pretty-callouts--make-display
              marker-line-end marker-line-end window
              'gfm-pretty-callouts-kind 'top-trailing
              'after-string (cdr top-split))))
        ;; Body lines.  Iterate via explicit text-position math, not
        ;; `forward-line': inside this overlay-creation loop,
        ;; `forward-line' interacts with cursor-intangible / display
        ;; props (set by `gfm-pretty-fences-mode' on the same buffer)
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
                  (gfm-pretty-callouts--make-display
                   lbeg (+ lbeg 2) window
                   'gfm-pretty-callouts-kind 'body-prefix
                   'gfm-pretty-callouts-revealable t
                   'evaporate t
                   'display edge))
                ;; Right-edge (and bottom on the last line).
                (setq last-right-after-ov
                      (gfm-pretty-callouts--make-display
                       lend lend window
                       'gfm-pretty-callouts-kind 'body-rhs
                       'after-string after-with-bottom))
                (setq p (min (1+ end) (1+ lend)))))))
        ;; Body-less callout: bottom border attaches to marker trailing.
        (unless has-body
          (let* ((existing (overlay-get trailing-ov 'after-string))
                 (new (concat existing "\n" bottom-str)))
            (put-text-property 0 1 'cursor t new)
            (overlay-put trailing-ov 'after-string new)))
        (ignore last-right-after-ov))))))

(defun gfm-pretty-callouts--apply-overlays ()
  "Create overlays for every callout block in the buffer.
Anchors shared, displays per-window.  Returns the block count."
  (save-excursion
    (let* ((blocks (gfm-pretty-callouts--collect-blocks))
           (windows (or (gfm-pretty--display-windows) (list nil))))
      (dolist (block blocks)
        (gfm-pretty-callouts--apply-block-anchors block))
      (dolist (window windows)
        (dolist (block blocks)
          (gfm-pretty-callouts--apply-block-display block window)))
      (length blocks))))

;;; Cursor-driven reveal (selected-window aware)

(defun gfm-pretty-callouts--reveal ()
  "Suppress display on the selected window's revealable overlays at point.
Restores overlays no longer at point.  Per-window: only overlays
whose `window' property is nil or matches the selected window are
toggled, so cursor in window A does not expose source in window B."
  (let ((pos (point))
        (win (selected-window)))
    (setq gfm-pretty-callouts--hidden-ovs
          (cl-loop for ov in gfm-pretty-callouts--hidden-ovs
                   if (and (overlay-buffer ov)
                           (>= pos (overlay-start ov))
                           (<= pos (overlay-end ov)))
                   collect ov
                   else do (when (overlay-buffer ov)
                             (overlay-put ov 'display
                                          (overlay-get ov 'gfm-pretty-callouts-saved-display))
                             (overlay-put ov 'gfm-pretty-callouts-saved-display nil))))
    (dolist (ov (overlays-in pos (1+ pos)))
      (when (and (overlay-get ov 'gfm-pretty-callouts-revealable)
                 (overlay-get ov 'display)
                 (let ((w (overlay-get ov 'window)))
                   (or (null w) (eq w win)))
                 (not (memq ov gfm-pretty-callouts--hidden-ovs)))
        (overlay-put ov 'gfm-pretty-callouts-saved-display
                     (overlay-get ov 'display))
        (overlay-put ov 'display nil)
        (push ov gfm-pretty-callouts--hidden-ovs)))))

;;; Rebuild scheduler state

(defvar-local gfm-pretty-callouts--last-window-state nil
  "Snapshot of the windows showing the buffer at the last rebuild.")

(defvar-local gfm-pretty-callouts--dirty-region nil
  "Buffer-local (BEG . END) covering all unrebuilt edits.")

(defvar-local gfm-pretty-callouts--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defun gfm-pretty-callouts--rebuild ()
  "Remove and recreate all gfm-pretty-callouts overlays."
  (let ((start (current-time)))
    (gfm-pretty-callouts--remove-overlays)
    (setq gfm-pretty-callouts--dirty-region nil)
    (gfm-pretty-callouts--apply-overlays)
    (gfm-pretty-callouts--record-stats (float-time (time-since start)))
    (setq gfm-pretty-callouts--last-window-state
          (gfm-pretty--window-state))))

(defun gfm-pretty-callouts--rebuild-block (block)
  "Tear down BLOCK's overlays and re-apply just that block."
  (let* ((start (current-time))
         (range (gfm-pretty-callouts--block-range block)))
    (gfm-pretty-callouts--remove-overlays (car range) (cdr range))
    (gfm-pretty-callouts--apply-block-anchors block)
    (dolist (window (or (gfm-pretty--display-windows) (list nil)))
      (gfm-pretty-callouts--apply-block-display block window))
    (gfm-pretty-callouts--record-stats (float-time (time-since start)))))

(defun gfm-pretty-callouts--rebuild-blocks (blocks)
  "Tear down each block in BLOCKS and re-apply them in one pass."
  (let ((start (current-time))
        (windows (or (gfm-pretty--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (gfm-pretty-callouts--block-range block)))
        (gfm-pretty-callouts--remove-overlays (car range) (cdr range)))
      (gfm-pretty-callouts--apply-block-anchors block)
      (dolist (window windows)
        (gfm-pretty-callouts--apply-block-display block window)))
    (gfm-pretty-callouts--record-stats (float-time (time-since start)))))

(defconst gfm-pretty-callouts--reconciler
  (gfm-pretty--make-reconciler
   :registry gfm-pretty-callouts--registry
   :state-symbol 'gfm-pretty-callouts--last-window-state
   :dirty-region-symbol 'gfm-pretty-callouts--dirty-region
   :timer-symbol 'gfm-pretty-callouts--rebuild-timer
   :mode-symbol 'gfm-pretty-callouts-mode
   :collect-fn #'gfm-pretty-callouts--collect-blocks
   :range-fn #'gfm-pretty-callouts--block-range
   :apply-anchors-fn #'gfm-pretty-callouts--apply-block-anchors
   :apply-display-fn #'gfm-pretty-callouts--apply-block-display
   :rebuild-fn #'gfm-pretty-callouts--rebuild)
  "Shared reconciler context for callouts.")

(defun gfm-pretty-callouts--rebuild-block-for-window (block window)
  "Replace WINDOW's display overlays for BLOCK at the current width."
  (gfm-pretty--rebuild-block-for-window
   gfm-pretty-callouts--reconciler block window))

;;; Visible-first prioritised rebuild

(defun gfm-pretty-callouts--block-visible-p (block ranges)
  "Non-nil if BLOCK's source range overlaps any range in RANGES."
  (gfm-pretty--block-visible-p
   block ranges #'gfm-pretty-callouts--block-range))

(defun gfm-pretty-callouts--rebuild-prioritised ()
  "Rebuild visible-window blocks first; defer off-screen ones one idle tick."
  (let ((ranges (gfm-pretty--visible-window-ranges)))
    (cond
     ((null ranges) (gfm-pretty-callouts--rebuild))
     (t
      (let* ((blocks (gfm-pretty-callouts--collect-blocks))
             (visible (cl-remove-if-not
                       (lambda (b) (gfm-pretty-callouts--block-visible-p b ranges))
                       blocks))
             (offscreen (cl-set-difference blocks visible)))
        (when visible
          (gfm-pretty-callouts--rebuild-blocks visible))
        (setq gfm-pretty-callouts--dirty-region nil
              gfm-pretty-callouts--last-window-state
              (gfm-pretty--window-state))
        (when offscreen
          (run-with-idle-timer
           0 nil
           (lambda (buf bs)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when gfm-pretty-callouts-mode
                   (gfm-pretty-callouts--rebuild-blocks bs)))))
           (current-buffer) offscreen)))))))

(defun gfm-pretty-callouts--reconcile-windows ()
  "Reconcile display overlays with current window state."
  (gfm-pretty--reconcile-windows gfm-pretty-callouts--reconciler))

;;; Scoped post-edit rebuild

(defsubst gfm-pretty-callouts--extend-dirty-region (beg end)
  "Extend the buffer's dirty region to cover BEG..END."
  (gfm-pretty--extend-dirty-region
   'gfm-pretty-callouts--dirty-region beg end))

(defun gfm-pretty-callouts--marker-line-ranges ()
  "Return per-line (BEG . END) ranges for every `> [!TYPE]' line."
  (mapcar (lambda (b)
            (let ((beg (nth 0 b)))
              (cons beg (save-excursion
                          (goto-char beg) (line-end-position)))))
          (gfm-pretty-callouts--find-blocks)))

(defun gfm-pretty-callouts--region-overlaps-marker-line-p (region)
  "Non-nil if REGION overlaps any callout marker line."
  (cl-some (lambda (r) (gfm-pretty--region-overlaps-p region r))
           (gfm-pretty-callouts--marker-line-ranges)))

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

(defun gfm-pretty-callouts--block-fully-contains-p (block region)
  "Non-nil if REGION lies inside BLOCK's source range."
  (let ((br (gfm-pretty-callouts--block-range block)))
    (and (>= (car region) (car br))
         (<= (cdr region) (cdr br)))))

(defun gfm-pretty-callouts--rebuild-scoped ()
  "Rebuild only what `gfm-pretty-callouts--dirty-region' demands."
  (let ((dirty gfm-pretty-callouts--dirty-region))
    (setq gfm-pretty-callouts--dirty-region nil)
    (cond
     ((null dirty) nil)
     ((gfm-pretty-callouts--region-overlaps-marker-line-p dirty)
      (gfm-pretty-callouts--rebuild))
     ((gfm-pretty-callouts--region-adjacent-to-callout-p dirty)
      (gfm-pretty-callouts--rebuild))
     (t
      (let* ((blocks (gfm-pretty-callouts--collect-blocks))
             (matching (cl-loop for b in blocks
                                when (gfm-pretty--region-overlaps-p
                                      dirty
                                      (gfm-pretty-callouts--block-range b))
                                collect b)))
        (cond
         ((null matching) nil)
         ((and (null (cdr matching))
               (gfm-pretty-callouts--block-fully-contains-p (car matching) dirty))
          (gfm-pretty-callouts--rebuild-block (car matching)))
         (t (gfm-pretty-callouts--rebuild))))))))

;;; Schedulers

(defsubst gfm-pretty-callouts--arm-rebuild-timer (callback)
  "Cancel any pending rebuild timer and schedule CALLBACK after idle."
  (gfm-pretty--arm-rebuild-timer
   'gfm-pretty-callouts--rebuild-timer 'gfm-pretty-callouts-mode callback))

(defun gfm-pretty-callouts--schedule-rebuild (&optional beg end _len)
  "Merge BEG..END into the dirty region and arm the rebuild timer."
  (unless (buffer-base-buffer)
    (when (and beg end)
      (gfm-pretty-callouts--extend-dirty-region beg end))
    (gfm-pretty-callouts--arm-rebuild-timer #'gfm-pretty-callouts--rebuild-scoped)))

(defun gfm-pretty-callouts--schedule-full-rebuild (&rest _)
  "Schedule a window reconciliation if window state has changed."
  (unless (buffer-base-buffer)
    (let ((state (gfm-pretty--window-state)))
      (unless (equal state gfm-pretty-callouts--last-window-state)
        (gfm-pretty-callouts--arm-rebuild-timer
         #'gfm-pretty-callouts--reconcile-windows)))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-pretty-callouts-mode
  "Render GFM callout blockquotes as boxes."
  :lighter " gfm-cb"
  (if gfm-pretty-callouts-mode
      (progn
        (gfm-pretty-callouts--init-stats)
        (gfm-pretty-callouts--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-pretty-callouts--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-pretty-callouts--schedule-full-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-pretty-callouts--reveal nil t))
    (remove-hook 'after-change-functions
                 #'gfm-pretty-callouts--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-pretty-callouts--schedule-full-rebuild t)
    (remove-hook 'post-command-hook #'gfm-pretty-callouts--reveal t)
    (when (timerp gfm-pretty-callouts--rebuild-timer)
      (cancel-timer gfm-pretty-callouts--rebuild-timer))
    (gfm-pretty-callouts--remove-overlays)))

;;; Callout faces (moved from modules/lang-markdown/lib.el)

(defface +markdown-gfm-callout-note-face
  '((((background dark))  :foreground "#89b4fa" :slant normal)
    (((background light)) :foreground "#1e66f5" :slant normal)
    (t :inherit font-lock-keyword-face :slant normal))
  "Header face for [!NOTE] callouts (blue)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-tip-face
  '((((background dark))  :foreground "#a6e3a1" :slant normal)
    (((background light)) :foreground "#40a02b" :slant normal)
    (t :inherit success :slant normal))
  "Header face for [!TIP] callouts (green)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-important-face
  '((((background dark))  :foreground "#cba6f7" :slant normal)
    (((background light)) :foreground "#8839ef" :slant normal)
    (t :inherit font-lock-keyword-face :slant normal))
  "Header face for [!IMPORTANT] callouts (purple)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-warning-face
  '((((background dark))  :foreground "#fab387" :slant normal)
    (((background light)) :foreground "#fe640b" :slant normal)
    (t :inherit warning :slant normal))
  "Header face for [!WARNING] callouts (orange)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-caution-face
  '((((background dark))  :foreground "#f38ba8" :slant normal)
    (((background light)) :foreground "#d20f39" :slant normal)
    (t :inherit error :slant normal))
  "Header face for [!CAUTION]/[!CRITICAL] callouts (red)."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-header-face
  '((t :weight semibold))
  "Face merged onto callout marker lines on top of the block face."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-note-body-face
  '((t))
  "Body face for [!NOTE] callouts; `:background' set dynamically from theme.
Default spec is intentionally empty so inline-markup emphasis (italic,
bold, underline, link, inline code) merged in from `markdown-italic-face'
et al. is not clobbered when this face is prepended to body chars."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-tip-body-face
  '((t))
  "Body face for [!TIP] callouts; `:background' set dynamically from theme.
See `+markdown-gfm-callout-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-important-body-face
  '((t))
  "Body face for [!IMPORTANT] callouts; `:background' set dynamically from theme.
See `+markdown-gfm-callout-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-warning-body-face
  '((t))
  "Body face for [!WARNING] callouts; `:background' set dynamically from theme.
See `+markdown-gfm-callout-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface +markdown-gfm-callout-caution-body-face
  '((t))
  "Body face for [!CAUTION]/[!CRITICAL] callouts.
`:background' is set dynamically from the active theme.  See
`+markdown-gfm-callout-note-body-face' for the empty-spec rationale."
  :group 'markdown-faces)

(defface +markdown-prettier-ignore-comment-face
  '((t :inherit shadow :weight light))
  "Face for prettier-ignore comments."
  :group 'markdown-faces)

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

(defconst +markdown-gfm-callout-type-face-alist
  '(("NOTE"      . +markdown-gfm-callout-note-face)
    ("TIP"       . +markdown-gfm-callout-tip-face)
    ("IMPORTANT" . +markdown-gfm-callout-important-face)
    ("WARNING"   . +markdown-gfm-callout-warning-face)
    ("CAUTION"   . +markdown-gfm-callout-caution-face)
    ("CRITICAL"  . +markdown-gfm-callout-caution-face))
  "Map of GFM callout type label to its header face.")

(defconst +markdown-gfm-callout-type-body-face-alist
  '(("NOTE"      . +markdown-gfm-callout-note-body-face)
    ("TIP"       . +markdown-gfm-callout-tip-body-face)
    ("IMPORTANT" . +markdown-gfm-callout-important-body-face)
    ("WARNING"   . +markdown-gfm-callout-warning-body-face)
    ("CAUTION"   . +markdown-gfm-callout-caution-body-face)
    ("CRITICAL"  . +markdown-gfm-callout-caution-body-face))
  "Map of GFM callout type label to its body (tinted background) face.")

(defun +markdown-gfm-callout--tint-bg (face)
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
(defun +markdown-gfm-callout-refresh-body-faces (&rest _)
  "Recompute `:background' on each callout body face from the current theme.
Also clears `:slant', `:weight', and `:underline' on each body face.
Body faces are prepended to body chars via `font-lock-prepend-text-property',
so any attribute they specify shadows the markdown emphasis faces beneath
them in the merge — emphasis would silently disappear inside callout
bodies."
  (dolist (entry +markdown-gfm-callout-type-body-face-alist)
    (let* ((type (car entry))
           (body-face (cdr entry))
           (header-face (alist-get type +markdown-gfm-callout-type-face-alist
                                   nil nil #'string=))
           (tint (and header-face
                      (+markdown-gfm-callout--tint-bg header-face))))
      (set-face-attribute body-face nil
                          :slant 'unspecified
                          :weight 'unspecified
                          :underline 'unspecified)
      (when tint
        (set-face-background body-face tint)))))

(+markdown-gfm-callout-refresh-body-faces)

(add-hook '+theme-changed-hook #'+markdown-gfm-callout-refresh-body-faces)

(defconst +markdown-gfm-callout--marker-re
  (rx bol "> " "[!"
      (group (or "NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
      "]" (* space) eol)
  "Regexp matching a GFM callout marker line. Group 1 is the type.")

(defun +markdown-gfm-callout--block-end (marker-eol)
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

(defun +markdown-gfm-callout--matcher (limit)
  "Font-lock matcher that spans full callout blocks up to LIMIT.
Match data: group 0 = whole block, group 1 = type label, group 2 =
marker line.  Sets `font-lock-multiline' on the matched region so
edits inside the block trigger refontification of the entire block."
  (when (re-search-forward +markdown-gfm-callout--marker-re limit t)
    (let* ((mbeg (match-beginning 0))
           (mend (match-end 0))
           (tbeg (match-beginning 1))
           (tend (match-end 1))
           (block-end (+markdown-gfm-callout--block-end mend)))
      (with-silent-modifications
        (put-text-property mbeg block-end 'font-lock-multiline t))
      (set-match-data (list mbeg block-end tbeg tend mbeg mend))
      t)))

(defun +markdown-gfm-callout--paint-body (type block-beg block-end)
  "Apply TYPE's body face to body content between BLOCK-BEG and BLOCK-END.
Skips newlines so the tinted background does not bleed one column past
the right border on body lines (the trailing newline character would
otherwise pick up `:background')."
  (when-let* ((body-face (alist-get type
                                    +markdown-gfm-callout-type-body-face-alist
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

(defun +markdown-gfm-callout--extend-region ()
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
      (when (looking-at-p +markdown-gfm-callout--marker-re)
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point)
                changed t))
        (let ((block-end (+markdown-gfm-callout--block-end
                          (line-end-position))))
          (when (> block-end font-lock-end)
            (setq font-lock-end block-end
                  changed t)))))
    changed))

;;;###autoload
(defun +markdown-fontify-gfm-pretty-callouts ()
  "Add font-lock keywords for GFM callout syntax."
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            #'+markdown-gfm-callout--extend-region nil t)
  (font-lock-add-keywords
   nil
   `((+markdown-gfm-callout--matcher
      (2 (alist-get (match-string-no-properties 1)
                    +markdown-gfm-callout-type-face-alist
                    nil nil #'string=)
         prepend)
      (2 '+markdown-gfm-callout-header-face prepend)
      (0 (progn (+markdown-gfm-callout--paint-body
                 (match-string-no-properties 1)
                 (match-beginning 0)
                 (match-end 0))
                nil)))
     (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-start" (1+ space) "-->")
      0 '+markdown-prettier-ignore-comment-face prepend)
     (,(rx bol (* space) "<!--" (1+ space) "prettier-ignore-end" (1+ space) "-->")
      0 '+markdown-prettier-ignore-comment-face prepend)
     (,(rx (group "$" (or (seq "{" (1+ (any alnum "_")) "}")
                          (seq (any upper "_") (1+ (any upper digit "_"))))))
      1 'font-lock-constant-face prepend))))

;; Neutralise `markdown-blockquote-face' so the face contributes nothing
;; — no italic, no theme-imposed foreground/background, no extend.  The
;; default face wins for plain blockquote chars; emphasis faces merge
;; through cleanly inside callout boxes (see
;; `gfm-pretty-callouts--apply-block-anchors').  Themes like catppuccin set
;; `:foreground'/`:background'/`:slant' directly on this face, so
;; `:inherit'-only fixes don't suffice — every attribute must be cleared.
(with-eval-after-load 'markdown-mode
  (dolist (attr '(:family :foundry :width :height :weight :slant
                  :underline :overline :strike-through :box
                  :inverse-video :foreground :background
                  :stipple :extend :inherit))
    (set-face-attribute 'markdown-blockquote-face nil attr 'unspecified)))

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty
  (gfm-pretty-define-decorator 'callouts
    :enable-fn    (lambda () (gfm-pretty-callouts-mode 1))
    :disable-fn   (lambda () (gfm-pretty-callouts-mode -1))
    :enabled-p-fn (lambda () (bound-and-true-p gfm-pretty-callouts-mode))))

(provide 'gfm-pretty-callouts)

;;; gfm-pretty-callouts.el ends here
