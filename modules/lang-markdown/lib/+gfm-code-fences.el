;;; +gfm-code-fences.el --- Visual treatment for fenced code blocks -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that adorns GFM fenced code blocks, leading YAML frontmatter,
;; and 4-space / tab-indented code blocks with a curved border, language
;; icons, and reveal-on-cursor for fence markers.
;;
;;   ┌────────
;;   │ echo hello
;;   │ echo world
;;   └────────
;;
;; Per-window display overlays (Path C — anchor / display split) keep
;; width-independent props on shared anchor overlays and width-dependent
;; rendering on per-window display overlays, so a buffer split across
;; two windows of different widths renders at each window's own width.
;; Discovery is memoised per `buffer-chars-modified-tick'; rebuilds are
;; scoped to the dirty region when possible and prioritised
;; visible-window-first when widths change.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'nerd-icons nil t)

(defgroup gfm-code-fences nil
  "Visual treatment for GFM fenced code blocks."
  :group 'markdown-faces)

(defcustom gfm-code-fences-slow-rebuild-threshold 0.05
  "Threshold in seconds above which a single rebuild emits a warning."
  :type 'number
  :group 'gfm-code-fences)

(defcustom gfm-code-fences-icon-gui-nudge 0.25
  "Fractional columns to shift the language icon leftward on GUI frames.
Compensates for icon-font glyphs whose pixel width exceeds the
`string-width' cell count.  Ignored on TTY frames."
  :type 'number
  :group 'gfm-code-fences)

(defconst gfm-code-fences--open-re
  (rx bol (* blank)
      (group "```" (* "`"))
      (* blank)
      (? "{") (* blank)
      (? (group (+ (any alnum ?_ ?-))))
      (* nonl) eol)
  "Regexp for an opening fence. Group 1: backticks. Group 2: language.")

(defconst gfm-code-fences--close-re
  (rx bol (* blank) (group "```" (* "`")) (* blank) eol)
  "Regexp for a closing fence. Group 1: backticks.")

(defconst gfm-code-fences--yaml-marker-re
  (rx bol "---" (* blank) eol)
  "Line consisting of `---' (with optional trailing whitespace).")

(defconst gfm-code-fences--border-face '+markdown-overlay-border-face
  "Face for the box border around pre blocks.")

(defconst gfm-code-fences--wrap-prefix-w 2
  "Visual width of the wrap-prefix shown on continuation visual lines.")

;;; Language resolution

(defun gfm-code-fences--lang-mode (lang)
  "Best-guess major mode symbol for LANG.
Consults `markdown-code-lang-modes' case-insensitively without the
`fboundp' filter `markdown-get-lang-mode' applies, so an icon can still
be picked even when the corresponding mode is not installed."
  (or (alist-get lang markdown-code-lang-modes nil nil
                 (lambda (a b) (string-equal (downcase a) (downcase b))))
      (let ((ts (intern (concat lang "-ts-mode"))))
        (and (fboundp ts) ts))
      (intern (concat lang "-mode"))))

(defun gfm-code-fences--icon-for-lang (lang)
  "Return the nerd-icons icon string for LANG, or nil."
  (when (and lang (fboundp 'nerd-icons-icon-for-mode))
    (let ((icon (ignore-errors
                  (nerd-icons-icon-for-mode (gfm-code-fences--lang-mode lang)))))
      (and (stringp icon) icon))))

;;; Range helpers

(defun gfm-code-fences--in-ranges-p (pos ranges)
  "Non-nil if POS lies in any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-code-fences--region-overlaps-p (a b)
  "Non-nil if (BEG . END) ranges A and B overlap."
  (and (<= (car a) (cdr b)) (>= (cdr a) (car b))))

;;; Block discovery — fenced

(defvar-local gfm-code-fences--fenced-blocks-cache nil
  "Pair (TICK . BLOCKS) memoising `gfm-code-fences--find-blocks'.
TICK is `buffer-chars-modified-tick' at the time of scan.")

(defun gfm-code-fences--find-blocks-1 ()
  "Scan the buffer for fenced code blocks (uncached).
Each entry is (OPEN-BEG OPEN-END CLOSE-BEG CLOSE-END LANG LANG-BEG LANG-END)."
  (let (blocks)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward gfm-code-fences--open-re nil t)
          (let* ((open-beg (match-beginning 1))
                 (open-end (match-end 1))
                 (lang (match-string-no-properties 2))
                 (lang-beg (match-beginning 2))
                 (lang-end (match-end 2))
                 (backticks (match-string-no-properties 1)))
            (forward-line 1)
            (let (close-beg close-end)
              (while (and (not (eobp)) (not close-beg))
                (cond
                 ((and (looking-at gfm-code-fences--close-re)
                       (>= (length (match-string 1)) (length backticks)))
                  (setq close-beg (match-beginning 1)
                        close-end (match-end 1)))
                 (t (forward-line 1))))
              (when close-beg
                (push (list open-beg open-end close-beg close-end
                            lang lang-beg lang-end)
                      blocks)
                (goto-char (line-end-position))))))))
    (nreverse blocks)))

(defun gfm-code-fences--find-blocks ()
  "Return all fenced code blocks in the current buffer.
Memoised by `buffer-chars-modified-tick' so repeat calls without an
intervening edit reuse the cached scan.  Each entry is (OPEN-BEG
OPEN-END CLOSE-BEG CLOSE-END LANG LANG-BEG LANG-END)."
  (let ((tick (buffer-chars-modified-tick)))
    (cond
     ((and gfm-code-fences--fenced-blocks-cache
           (= tick (car gfm-code-fences--fenced-blocks-cache)))
      (cdr gfm-code-fences--fenced-blocks-cache))
     (t
      (let ((blocks (gfm-code-fences--find-blocks-1)))
        (setq gfm-code-fences--fenced-blocks-cache (cons tick blocks))
        blocks)))))

;;; Block discovery — YAML helmet

(defvar-local gfm-code-fences--yaml-helmet-cache nil
  "Pair (TICK . HELMET-OR-NIL) memoising `gfm-code-fences--find-yaml-helmet'.")

(defun gfm-code-fences--find-yaml-helmet-1 ()
  "Scan the buffer for a leading YAML helmet (uncached).
Returns (OPEN-BEG OPEN-END CLOSE-BEG CLOSE-END) or nil."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (looking-at gfm-code-fences--yaml-marker-re)
        (let ((open-beg (match-beginning 0))
              (open-end (match-end 0)))
          (forward-line 1)
          (let (close-beg close-end)
            (while (and (not (eobp)) (not close-beg))
              (cond
               ((looking-at gfm-code-fences--yaml-marker-re)
                (setq close-beg (match-beginning 0)
                      close-end (match-end 0)))
               (t (forward-line 1))))
            (when close-beg
              (list open-beg open-end close-beg close-end))))))))

(defun gfm-code-fences--find-yaml-helmet ()
  "Return the leading YAML helmet, memoised by chars-modified tick."
  (let ((tick (buffer-chars-modified-tick)))
    (cond
     ((and gfm-code-fences--yaml-helmet-cache
           (= tick (car gfm-code-fences--yaml-helmet-cache)))
      (cdr gfm-code-fences--yaml-helmet-cache))
     (t
      (let ((helmet (gfm-code-fences--find-yaml-helmet-1)))
        (setq gfm-code-fences--yaml-helmet-cache (cons tick helmet))
        helmet)))))

;;; Block discovery — indent

(defun gfm-code-fences--line-indent ()
  "Return code-block indent width on the current line, or nil.
4 spaces or 1 tab counts as a markdown indented code block opener."
  (cond ((looking-at "    ") 4)
        ((looking-at "\t") 1)))

(defun gfm-code-fences--blank-line-p ()
  "Non-nil if the current line is blank."
  (looking-at-p "[[:blank:]]*$"))

(defvar-local gfm-code-fences--indent-blocks-cache nil
  "Pair (TICK . BLOCKS) memoising the unfiltered indent-block scan.
Cache key is `buffer-chars-modified-tick' only; the call-site
EXCLUDED-RANGES parameter is applied as a post-filter so its content
does not invalidate cached scans.")

(defun gfm-code-fences--find-indent-blocks-1 (excluded-ranges)
  "Return indented code blocks not overlapping EXCLUDED-RANGES (uncached).
Each block is (BLOCK-BEG BLOCK-END INDENT-WIDTH)."
  (let (blocks
        (prev-blank t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((lbeg (line-beginning-position))
               (blank (gfm-code-fences--blank-line-p))
               (excluded (gfm-code-fences--in-ranges-p lbeg excluded-ranges))
               (indent (and (not blank) (not excluded)
                            (gfm-code-fences--line-indent))))
          (cond
           ((and indent prev-blank)
            (let ((block-beg lbeg)
                  (block-end (line-end-position))
                  (block-indent indent)
                  (continue t))
              (forward-line 1)
              (while (and continue (not (eobp)))
                (let ((ll-beg (line-beginning-position)))
                  (cond
                   ((or (gfm-code-fences--in-ranges-p ll-beg excluded-ranges)
                        (gfm-code-fences--blank-line-p)
                        (not (gfm-code-fences--line-indent)))
                    (setq continue nil))
                   (t (setq block-end (line-end-position))
                      (forward-line 1)))))
              (push (list block-beg block-end block-indent) blocks)
              (setq prev-blank nil)))
           (t
            (setq prev-blank blank)
            (forward-line 1))))))
    (nreverse blocks)))

(defun gfm-code-fences--find-indent-blocks (excluded-ranges)
  "Return indented code blocks, memoised by chars-modified tick.
EXCLUDED-RANGES filters at scan time but is not part of the cache key:
in practice callers always pass the buffer's fenced ranges for the same
tick, so the result is deterministic from the tick alone."
  (let ((tick (buffer-chars-modified-tick)))
    (cond
     ((and gfm-code-fences--indent-blocks-cache
           (= tick (car gfm-code-fences--indent-blocks-cache)))
      (cdr gfm-code-fences--indent-blocks-cache))
     (t
      (let ((blocks (gfm-code-fences--find-indent-blocks-1 excluded-ranges)))
        (setq gfm-code-fences--indent-blocks-cache (cons tick blocks))
        blocks)))))

;;; Width helpers

(defun gfm-code-fences--available-width (&optional window)
  "Return the available char width for a block in WINDOW.
Falls back to a window currently showing the buffer, then to
`fill-column' or 80."
  (let ((win (or window
                 (get-buffer-window (current-buffer))
                 (get-buffer-window (current-buffer) t))))
    (or (and win (window-max-chars-per-line win))
        fill-column
        80)))

(defun gfm-code-fences--text-width (&optional window)
  "Return WINDOW's max chars per visual line.
Compatibility alias for `gfm-code-fences--available-width'."
  (gfm-code-fences--available-width window))

(defun gfm-code-fences--display-windows ()
  "Return windows currently displaying the buffer."
  (get-buffer-window-list (current-buffer) nil t))

(defun gfm-code-fences--max-line-width (beg end &optional indent)
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

(defun gfm-code-fences--normalised-border-face (face)
  "Return a face spec that inherits FACE but resets text-styling attrs.
Border glyphs share buffer regions with prose whose font-lock face
carries `:slant italic', `:underline t', etc.  Without an explicit
override, those attrs leak through face composition on GUI frames
and visually slant the box edges."
  `(:inherit ,face
    :slant normal :weight normal
    :underline nil :overline nil :strike-through nil :box nil))

(defun gfm-code-fences--top-strings (width face buffer-width &optional icon)
  "Return (LEADING . TRAILING) split of the top border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols (matching the marker line's char count) so
the buffer text shows in place of LEADING when it is revealed; TRAILING
covers the remaining decoration. ICON, if non-nil, is right-aligned."
  (let* ((face (gfm-code-fences--normalised-border-face face))
         (l (propertize "┌" 'face face))
         (r (propertize "┐" 'face face))
         (gap (propertize " " 'face face))
         (leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat l (propertize (make-string leading-dash-w ?─)
                                        'face face))))
    (cond
     (icon
      (let* ((icon-w (string-width icon))
             (nudge (if (display-graphic-p)
                        (max 0 (min 0.99 gfm-code-fences-icon-gui-nudge))
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

(defun gfm-code-fences--bottom-strings (width face buffer-width)
  "Return (LEADING . TRAILING) split of the bottom border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols matching the marker line's char count."
  (let* ((face (gfm-code-fences--normalised-border-face face))
         (leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat (propertize "└" 'face face)
                          (propertize (make-string leading-dash-w ?─)
                                      'face face)))
         (total-fill-w (max 1 (- width 2)))
         (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
         (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
    (cons leading (concat rem-fill (propertize "┘" 'face face)))))

(defun gfm-code-fences--right-after (box-width face)
  "Build the after-string that draws the right border at column BOX-WIDTH."
  (let* ((face (gfm-code-fences--normalised-border-face face))
         (align-col (- box-width 2))
         (pad (propertize " " 'display `(space :align-to ,align-col)
                          'face face))
         (sep (propertize " " 'face face))
         (pipe (propertize "│" 'face face))
         (str (concat pad sep pipe)))
    (put-text-property 0 1 'cursor t str)
    str))

(defun gfm-code-fences--simulate-wrap (text width &optional cont-prefix-w)
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

(defun gfm-code-fences--last-visual-col (text width &optional cont-prefix-w)
  "Estimate last visual column TEXT reaches.
See `gfm-code-fences--simulate-wrap'."
  (car (gfm-code-fences--simulate-wrap text width cont-prefix-w)))

(defun gfm-code-fences--wrap-prefix (face)
  "Wrap-prefix string used on continuation lines."
  (propertize "⋱ " 'face (gfm-code-fences--normalised-border-face face)))

(defun gfm-code-fences--right-after-overflow (face line-text window)
  "After-string padding the right border to WINDOW's edge for a wrapped line.
LINE-TEXT is the line's buffer content; the function simulates word-wrap
to work out where the line ends visually, accounting for the wrap-prefix
on continuation lines.  WINDOW selects the width; nil falls back to a
sane default."
  (let* ((face (gfm-code-fences--normalised-border-face face))
         (text-width (gfm-code-fences--available-width window))
         ;; +2 for the `│ ' before-string contribution to the first visual line.
         (visual-col (gfm-code-fences--last-visual-col
                      (concat "│ " line-text) text-width
                      gfm-code-fences--wrap-prefix-w))
         (target-col (1- text-width))
         (pad-len (max 0 (- target-col visual-col)))
         (pad (propertize (make-string pad-len ?\s) 'face face))
         (pipe (propertize "│" 'face face))
         (str (concat pad pipe)))
    (put-text-property 0 1 'cursor t str)
    str))

;;; Performance instrumentation

(defvar-local gfm-code-fences--stats nil
  "Per-buffer alist of rebuild stats.")

(defconst gfm-code-fences--phase-keys
  '(find-fenced find-yaml find-indent compose-borders compose-overflow apply)
  "Keys used in `gfm-code-fences--stats' `phase-totals' alist, in display order.")

(defun gfm-code-fences--init-stats ()
  "Reset the per-buffer rebuild stats to zero."
  (setq gfm-code-fences--stats
        (list (cons 'rebuild-count 0)
              (cons 'total-time 0.0)
              (cons 'last-time 0.0)
              (cons 'max-time 0.0)
              (cons 'block-count 0)
              (cons 'phase-totals
                    (mapcar (lambda (k) (cons k 0.0))
                            gfm-code-fences--phase-keys)))))

(defun gfm-code-fences--accum-phase (phase delta)
  "Accumulate DELTA seconds into PHASE in `gfm-code-fences--stats'."
  (when gfm-code-fences--stats
    (let ((totals (alist-get 'phase-totals gfm-code-fences--stats)))
      (when totals
        (setf (alist-get phase totals)
              (+ delta (or (alist-get phase totals) 0.0)))
        (setf (alist-get 'phase-totals gfm-code-fences--stats) totals)))))

(defmacro gfm-code-fences--time-phase (phase &rest body)
  "Run BODY, accumulating its wall-time into PHASE on the buffer's stats."
  (declare (indent 1) (debug (form body)))
  (let ((start (make-symbol "start")))
    `(let ((,start (current-time)))
       (prog1 (progn ,@body)
         (gfm-code-fences--accum-phase
          ,phase (float-time (time-since ,start)))))))

(defun gfm-code-fences--record-stats (duration block-count)
  "Update stats with DURATION and BLOCK-COUNT from one rebuild."
  (unless gfm-code-fences--stats (gfm-code-fences--init-stats))
  (setf (alist-get 'rebuild-count gfm-code-fences--stats)
        (1+ (alist-get 'rebuild-count gfm-code-fences--stats)))
  (setf (alist-get 'total-time gfm-code-fences--stats)
        (+ duration (alist-get 'total-time gfm-code-fences--stats)))
  (setf (alist-get 'last-time gfm-code-fences--stats) duration)
  (setf (alist-get 'max-time gfm-code-fences--stats)
        (max duration (alist-get 'max-time gfm-code-fences--stats)))
  (setf (alist-get 'block-count gfm-code-fences--stats) block-count)
  (when (> duration gfm-code-fences-slow-rebuild-threshold)
    (message "gfm-code-fences: slow rebuild in %s: %.3fs"
             (buffer-name) duration)))

(defun gfm-code-fences--format-phase-totals (totals)
  "Return a phase-by-phase summary string for TOTALS, sorted by total desc."
  (let ((sorted (sort (copy-sequence totals)
                      (lambda (a b) (> (cdr a) (cdr b))))))
    (mapconcat (lambda (p) (format "%s=%.3fs" (car p) (cdr p)))
               sorted " ")))

(defun gfm-code-fences-stats ()
  "Display the current buffer's gfm-code-fences rebuild statistics."
  (interactive)
  (if (not gfm-code-fences--stats)
      (message "gfm-code-fences: no stats yet")
    (let-alist gfm-code-fences--stats
      (message
       "gfm-code-fences [%s]: rebuilds=%d total=%.3fs last=%.3fs max=%.3fs blocks=%d | %s"
       (buffer-name)
       .rebuild-count .total-time .last-time .max-time .block-count
       (gfm-code-fences--format-phase-totals .phase-totals)))))

;;; Overlay registry

(defvar gfm-code-fences-mode)

(defvar-local gfm-code-fences--overlays nil
  "All fence overlays currently in this buffer.")

(defvar-local gfm-code-fences--hidden-ovs nil
  "Revealable overlays whose display is currently suppressed.")

(defun gfm-code-fences--register (ov)
  "Tag OV as a fence overlay and remember it for bulk cleanup."
  (overlay-put ov 'gfm-code-fences t)
  (push ov gfm-code-fences--overlays)
  ov)

(defun gfm-code-fences--remove-overlays (&optional beg end)
  "Remove all gfm-code-fences overlays between BEG and END."
  (remove-overlays (or beg (point-min)) (or end (point-max))
                   'gfm-code-fences t)
  (cond
   ((or beg end)
    (setq gfm-code-fences--overlays
          (cl-remove-if-not #'overlay-buffer gfm-code-fences--overlays)))
   (t
    (setq gfm-code-fences--overlays nil
          gfm-code-fences--hidden-ovs nil))))

(defun gfm-code-fences--prune-dead-overlays ()
  "Drop overlays from the registry whose buffer is gone.
Called once after a batch of overlay deletions (e.g. window
reconciliation), not per block — `cl-remove-if-not' on the registry is
O(n) and the registry can hold thousands of overlays on heavy buffers."
  (setq gfm-code-fences--overlays
        (cl-remove-if-not #'overlay-buffer gfm-code-fences--overlays)))

(defun gfm-code-fences--remove-display-overlays-in-range (beg end window)
  "Delete display overlays in [BEG, END] for WINDOW.
WINDOW non-nil matches only that window's overlays; nil matches every
display overlay (the unrestricted fallback set).  The global registry
is NOT pruned here — call `gfm-code-fences--prune-dead-overlays' once
after a batch."
  (dolist (ov (overlays-in beg end))
    (when (and (overlay-get ov 'gfm-code-fences-display)
               (or (null window)
                   (eq (overlay-get ov 'window) window)))
      (delete-overlay ov))))

(defun gfm-code-fences--remove-display-overlays-for-window (window)
  "Delete every display overlay restricted to WINDOW across the buffer."
  (dolist (ov gfm-code-fences--overlays)
    (when (and (overlay-buffer ov)
               (overlay-get ov 'gfm-code-fences-display)
               (eq (overlay-get ov 'window) window))
      (delete-overlay ov)))
  (gfm-code-fences--prune-dead-overlays))

;;; Anchor + display overlay constructors

(defun gfm-code-fences--make-anchor (beg end &rest props)
  "Make an anchor overlay over [BEG, END] with width-independent PROPS.
PROPS is a property list applied to the overlay (e.g.
`before-string', `wrap-prefix', `cursor-intangible')."
  (let ((ov (make-overlay beg end nil nil t)))
    (overlay-put ov 'gfm-code-fences t)
    (overlay-put ov 'gfm-code-fences-anchor t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    (push ov gfm-code-fences--overlays)
    ov))

(defun gfm-code-fences--make-display (beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW with PROPS.
WINDOW non-nil restricts the overlay to that window only."
  (let ((ov (make-overlay beg end nil nil t)))
    (overlay-put ov 'gfm-code-fences t)
    (overlay-put ov 'gfm-code-fences-display t)
    (when window (overlay-put ov 'window window))
    (while props
      (overlay-put ov (pop props) (pop props)))
    (push ov gfm-code-fences--overlays)
    ov))

;;; Bordered rendering — fenced + YAML (shared)

(defun gfm-code-fences--apply-bordered-anchors
    (_open-line-end _close-line-beg _face)
  "No-op: fenced/yaml body decoration lives entirely on per-window
display overlays.

Two zero-width overlays at the same buffer position render in an
order Emacs does not guarantee: an anchor carrying just the
`│ ' before-string and a sibling display overlay carrying the
right-edge `│' after-string can render after-string-first on empty
body lines, painting the left border at the right position.
Collapsing both strings onto a single overlay sidesteps the
ambiguity at the cost of duplicating the `│ ' string per window —
acceptable, mirrors gfm-tables row decoration."
  nil)

(defun gfm-code-fences--apply-bordered-display
    (window open-line-beg open-line-end close-line-beg close-line-end face label)
  "Build per-WINDOW display overlays for a fenced/yaml block.
Borders sized to WINDOW's width; FACE colours; LABEL right-aligned in
the top border (icon string for fenced, `meta' for YAML, or nil)."
  (let* ((body-beg (save-excursion
                     (goto-char open-line-end) (forward-line 1) (point)))
         (body-end (max body-beg (1- close-line-beg)))
         (max-content (gfm-code-fences--max-line-width body-beg body-end))
         (text-width (gfm-code-fences--available-width window))
         (box-width (min text-width (max 80 (+ max-content 4))))
         (content-budget (- box-width 4))
         (open-buf-width (- open-line-end open-line-beg))
         (close-buf-width (- close-line-end close-line-beg))
         (top-split (gfm-code-fences--time-phase 'compose-borders
                      (gfm-code-fences--top-strings box-width face
                                                    open-buf-width label)))
         (bot-split (gfm-code-fences--time-phase 'compose-borders
                      (gfm-code-fences--bottom-strings box-width face
                                                       close-buf-width))))
    ;; Top — leading on the marker line, trailing after.
    (gfm-code-fences--make-display
     open-line-beg open-line-end window
     'gfm-code-fences-kind 'top-leading
     'gfm-code-fences-revealable t
     'evaporate t
     'display (car top-split))
    (gfm-code-fences--make-display
     open-line-end open-line-end window
     'gfm-code-fences-kind 'top-trailing
     'after-string (cdr top-split))
    ;; Body lines — single per-window overlay carrying before/wrap/after.
    ;; Iterate via explicit text-position math, not `forward-line': inside
    ;; this overlay-creation loop, `forward-line' interacts with our
    ;; cursor-intangible / display props and can stall mid-block,
    ;; spinning on the same line forever (bisect 2026-05-08).
    (let ((lhs (propertize "│ " 'face
                           (gfm-code-fences--normalised-border-face face)))
          (p body-beg))
      (while (< p close-line-beg)
        (let* ((lbeg p)
               (lend (save-excursion (goto-char p) (line-end-position)))
               (after (gfm-code-fences--time-phase 'compose-overflow
                        (if (> (- lend lbeg) content-budget)
                            (gfm-code-fences--right-after-overflow
                             face (buffer-substring-no-properties lbeg lend)
                             window)
                          (gfm-code-fences--right-after box-width face)))))
          (gfm-code-fences--make-display
           lbeg lend window
           'gfm-code-fences-kind 'body
           'before-string lhs
           'wrap-prefix (gfm-code-fences--wrap-prefix face)
           'after-string after)
          (setq p (min close-line-beg (1+ lend))))))
    ;; Bottom — leading on the marker line, trailing after.
    (gfm-code-fences--make-display
     close-line-beg close-line-end window
     'gfm-code-fences-kind 'bottom-leading
     'gfm-code-fences-revealable t
     'evaporate t
     'display (car bot-split))
    (gfm-code-fences--make-display
     close-line-end close-line-end window
     'gfm-code-fences-kind 'bottom-trailing
     'after-string (cdr bot-split))))

;;; Indent block rendering

(defun gfm-code-fences--apply-indent-anchors (beg end indent-width face)
  "Build width-independent anchors for an indent block at [BEG, END]."
  (let ((lhs (propertize "│ " 'face
                         (gfm-code-fences--normalised-border-face face)))
        (first t)
        (p beg))
    (while (<= p end)
      (let* ((lbeg p)
             (lend (save-excursion (goto-char p) (line-end-position)))
             (cover-end (min (+ lbeg indent-width) lend)))
        ;; Cover indent chars with `│ ' display; carry cursor-intangible.
        (gfm-code-fences--make-anchor
         lbeg cover-end
         'gfm-code-fences-kind 'indent-body
         'gfm-code-fences-indent-first first
         'cursor-intangible t
         'display lhs)
        ;; Wrap-prefix on the whole line (continuation lines).
        (gfm-code-fences--make-anchor
         lbeg lend
         'gfm-code-fences-kind 'indent-wrap
         'wrap-prefix (gfm-code-fences--wrap-prefix face))
        (setq first nil)
        (setq p (1+ lend))))))

(defun gfm-code-fences--apply-indent-display (window beg end indent-width face)
  "Build per-WINDOW display overlays for an indent block at [BEG, END].
INDENT-WIDTH is the buffer indent width; FACE colours the borders."
  (let* ((max-content (gfm-code-fences--max-line-width beg end indent-width))
         (text-width (gfm-code-fences--available-width window))
         (box-width (min text-width (max 80 (+ max-content 4))))
         (content-budget (- box-width 4))
         (top-split (gfm-code-fences--time-phase 'compose-borders
                      (gfm-code-fences--top-strings box-width face 0 nil)))
         (bot-split (gfm-code-fences--time-phase 'compose-borders
                      (gfm-code-fences--bottom-strings box-width face 0)))
         (top-str (concat (car top-split) (cdr top-split)))
         (bot-str (concat (car bot-split) (cdr bot-split))))
    (let ((first t)
          (p beg))
      (while (<= p end)
        (let* ((lbeg p)
               (lend (save-excursion (goto-char p) (line-end-position)))
               (last-line (>= lend end))
               (line-content-w (max 0 (- (- lend lbeg) indent-width)))
               (line-text (buffer-substring-no-properties
                           (min (+ lbeg indent-width) lend) lend))
               (overflow-p (> line-content-w content-budget))
               (after (gfm-code-fences--time-phase 'compose-overflow
                        (if overflow-p
                            (gfm-code-fences--right-after-overflow
                             face line-text window)
                          (gfm-code-fences--right-after box-width face)))))
          (when first
            (gfm-code-fences--make-display
             lbeg lbeg window
             'gfm-code-fences-kind 'indent-top
             'before-string (concat top-str "\n")))
          (gfm-code-fences--make-display
           lend lend window
           'gfm-code-fences-kind 'indent-rhs
           'after-string (if last-line (concat after "\n" bot-str) after))
          (setq first nil)
          (setq p (1+ lend)))))))

;;; Block adapters

(defun gfm-code-fences--fenced-line-positions (block)
  "Return (OPEN-LINE-BEG OPEN-LINE-END CLOSE-LINE-BEG CLOSE-LINE-END) for BLOCK."
  (let ((open-beg (nth 0 block))
        (close-beg (nth 2 block))
        (close-end (nth 3 block)))
    (list (save-excursion (goto-char open-beg) (line-beginning-position))
          (save-excursion (goto-char open-beg) (line-end-position))
          (save-excursion (goto-char close-beg) (line-beginning-position))
          close-end)))

(defun gfm-code-fences--apply-fenced-block-anchors (block)
  "Apply width-independent anchors for fenced BLOCK."
  (cl-destructuring-bind (_olb ole _clb _cle)
      (gfm-code-fences--fenced-line-positions block)
    (gfm-code-fences--apply-bordered-anchors
     ole (nth 2 (gfm-code-fences--fenced-line-positions block))
     gfm-code-fences--border-face)))

(defun gfm-code-fences--apply-fenced-block-display (block window)
  "Apply per-WINDOW display overlays for fenced BLOCK."
  (let* ((face gfm-code-fences--border-face)
         (lang (nth 4 block))
         (icon (and lang (gfm-code-fences--icon-for-lang lang)))
         (positions (gfm-code-fences--fenced-line-positions block)))
    (cl-destructuring-bind (olb ole clb cle) positions
      (gfm-code-fences--apply-bordered-display
       window olb ole clb cle face icon))))

(defun gfm-code-fences--yaml-line-positions (helmet)
  "Return (OLB OLE CLB CLE) line positions for HELMET."
  (cl-destructuring-bind (open-beg _open-end close-beg close-end) helmet
    (list (save-excursion (goto-char open-beg) (line-beginning-position))
          (save-excursion (goto-char open-beg) (line-end-position))
          (save-excursion (goto-char close-beg) (line-beginning-position))
          close-end)))

(defun gfm-code-fences--apply-yaml-block-anchors (helmet)
  "Apply width-independent anchors for YAML HELMET, including body fontification."
  (cl-destructuring-bind (_olb ole clb _cle)
      (gfm-code-fences--yaml-line-positions helmet)
    (gfm-code-fences--apply-bordered-anchors ole clb 'font-lock-constant-face))
  ;; Fontify body (face overlays — rebuilt per anchor pass; not per window).
  (cl-destructuring-bind (open-beg open-end close-beg _close-end) helmet
    (let* ((body-beg (min close-beg
                          (save-excursion
                            (goto-char open-end) (forward-line 1) (point))))
           (body-end (max body-beg (1- close-beg))))
      (ignore open-beg)
      (gfm-code-fences--fontify-yaml-body body-beg body-end))))

(defun gfm-code-fences--apply-yaml-block-display (helmet window)
  "Apply per-WINDOW display overlays for YAML HELMET."
  (let* ((face 'font-lock-constant-face)
         (label (propertize "meta" 'face `(:inherit (bold ,face))))
         (positions (gfm-code-fences--yaml-line-positions helmet)))
    (cl-destructuring-bind (olb ole clb cle) positions
      (gfm-code-fences--apply-bordered-display
       window olb ole clb cle face label))))

(defun gfm-code-fences--apply-indent-block-anchors (block)
  "Apply width-independent anchors for indent BLOCK."
  (cl-destructuring-bind (beg end indent-width) block
    (gfm-code-fences--apply-indent-anchors
     beg end indent-width gfm-code-fences--border-face)))

(defun gfm-code-fences--apply-indent-block-display (block window)
  "Apply per-WINDOW display overlays for indent BLOCK."
  (cl-destructuring-bind (beg end indent-width) block
    (gfm-code-fences--apply-indent-display
     window beg end indent-width gfm-code-fences--border-face)))

;;; YAML body fontification

(defun gfm-code-fences--yaml-mode ()
  "Return a yaml major-mode symbol that's currently fboundp, or nil."
  (cond ((and (fboundp 'yaml-ts-mode)
              (fboundp 'treesit-language-available-p)
              (treesit-language-available-p 'yaml))
         'yaml-ts-mode)
        ((fboundp 'yaml-mode) 'yaml-mode)))

(defun gfm-code-fences--fontify-yaml-body (start end)
  "Apply YAML font-lock face overlays to buffer region [START, END]."
  (when-let* ((lang-mode (gfm-code-fences--yaml-mode))
              ((> end start)))
    (let ((string (buffer-substring-no-properties start end))
          (target-buffer (current-buffer))
          pos next)
      (with-current-buffer
          (get-buffer-create
           (format " *gfm-code-fences-yaml-fontification:%s*"
                   (symbol-name lang-mode)))
        (let ((inhibit-modification-hooks nil))
          (delete-region (point-min) (point-max))
          (insert string " "))
        (unless (eq major-mode lang-mode)
          (funcall lang-mode))
        (font-lock-ensure)
        (setq pos (point-min))
        (while (setq next (next-single-property-change pos 'face))
          (when-let* ((val (get-text-property pos 'face)))
            (let ((ov (make-overlay (+ start (1- pos))
                                    (1- (+ start next))
                                    target-buffer)))
              (overlay-put ov 'face val)
              (gfm-code-fences--register ov)))
          (setq pos next))))))

;;; Block enumeration & ranges

(cl-defstruct (gfm-code-fences--block
               (:constructor gfm-code-fences--make-block)
               (:copier nil))
  "Tagged source block for unified rebuild dispatch.
KIND is `fenced', `yaml', or `indent'.  RANGE is (LINE-BEG . LINE-END+1)
covering the block's full source range, used for visibility checks and
scoped-rebuild containment.  PAYLOAD is the kind-specific data tuple."
  kind range payload)

(defun gfm-code-fences--collect-blocks ()
  "Return tagged blocks discovered in the current buffer.
Order is: yaml (if any), then fenced in source order, then indent in
source order.  Indent discovery excludes fenced + yaml ranges."
  (let* ((helmet (gfm-code-fences--time-phase 'find-yaml
                   (gfm-code-fences--find-yaml-helmet)))
         (fenced (gfm-code-fences--time-phase 'find-fenced
                   (gfm-code-fences--find-blocks)))
         (excluded
          (append
           (when helmet
             (cl-destructuring-bind (olb _ole _clb cle)
                 (gfm-code-fences--yaml-line-positions helmet)
               (list (cons olb cle))))
           (mapcar (lambda (b)
                     (cl-destructuring-bind (olb _ole _clb cle)
                         (gfm-code-fences--fenced-line-positions b)
                       (cons olb cle)))
                   fenced)))
         (indent (gfm-code-fences--time-phase 'find-indent
                   (gfm-code-fences--find-indent-blocks excluded)))
         result)
    (when helmet
      (cl-destructuring-bind (olb _ole _clb cle)
          (gfm-code-fences--yaml-line-positions helmet)
        (push (gfm-code-fences--make-block
               :kind 'yaml :range (cons olb (1+ cle)) :payload helmet)
              result)))
    (dolist (b fenced)
      (cl-destructuring-bind (olb _ole _clb cle)
          (gfm-code-fences--fenced-line-positions b)
        (push (gfm-code-fences--make-block
               :kind 'fenced :range (cons olb (1+ cle)) :payload b)
              result)))
    (dolist (b indent)
      (cl-destructuring-bind (beg end _iw) b
        (push (gfm-code-fences--make-block
               :kind 'indent :range (cons beg (1+ end)) :payload b)
              result)))
    (nreverse result)))

(defun gfm-code-fences--apply-block-anchors (block)
  "Apply anchor overlays for tagged BLOCK."
  (gfm-code-fences--time-phase 'apply
    (cl-case (gfm-code-fences--block-kind block)
      (fenced (gfm-code-fences--apply-fenced-block-anchors
               (gfm-code-fences--block-payload block)))
      (yaml (gfm-code-fences--apply-yaml-block-anchors
             (gfm-code-fences--block-payload block)))
      (indent (gfm-code-fences--apply-indent-block-anchors
               (gfm-code-fences--block-payload block))))))

(defun gfm-code-fences--apply-block-display (block window)
  "Apply per-WINDOW display overlays for tagged BLOCK."
  (gfm-code-fences--time-phase 'apply
    (cl-case (gfm-code-fences--block-kind block)
      (fenced (gfm-code-fences--apply-fenced-block-display
               (gfm-code-fences--block-payload block) window))
      (yaml (gfm-code-fences--apply-yaml-block-display
             (gfm-code-fences--block-payload block) window))
      (indent (gfm-code-fences--apply-indent-block-display
               (gfm-code-fences--block-payload block) window)))))

(defun gfm-code-fences--apply-overlays ()
  "Create overlays for every block in the buffer.
Anchors are shared across windows; display overlays are produced once
per window currently showing the buffer (or one unrestricted set when
no window does).  Returns the block count."
  (save-excursion
    (let* ((blocks (gfm-code-fences--collect-blocks))
           (windows (or (gfm-code-fences--display-windows) (list nil))))
      (dolist (block blocks)
        (gfm-code-fences--apply-block-anchors block))
      (dolist (window windows)
        (dolist (block blocks)
          (gfm-code-fences--apply-block-display block window)))
      (length blocks))))

;;; Cursor-driven reveal

(defun gfm-code-fences--reveal ()
  "Suppress display on the selected window's revealable overlays at point.
With per-window display overlays each window owns its own copy of the
top/bottom marker overlays, so reveal toggles only the selected
window's overlay (matched on the `window' overlay property; nil
matches the unrestricted fallback)."
  (let ((pos (point))
        (win (selected-window)))
    ;; Restore overlays no longer at point.
    (setq gfm-code-fences--hidden-ovs
          (cl-loop for ov in gfm-code-fences--hidden-ovs
                   if (and (overlay-buffer ov)
                           (>= pos (overlay-start ov))
                           (<= pos (overlay-end ov)))
                   collect ov
                   else do (when (overlay-buffer ov)
                             (overlay-put ov 'display
                                          (overlay-get ov 'gfm-code-fences-saved-display))
                             (overlay-put ov 'gfm-code-fences-saved-display nil))))
    ;; Hide revealable overlays at point in the selected window.
    (dolist (ov (overlays-in pos (1+ pos)))
      (when (and (overlay-get ov 'gfm-code-fences-revealable)
                 (overlay-get ov 'display)
                 (let ((w (overlay-get ov 'window)))
                   (or (null w) (eq w win)))
                 (not (memq ov gfm-code-fences--hidden-ovs)))
        (overlay-put ov 'gfm-code-fences-saved-display
                     (overlay-get ov 'display))
        (overlay-put ov 'display nil)
        (push ov gfm-code-fences--hidden-ovs)))))

;;; Rebuild scheduler state

(defvar gfm-code-fences-mode)

(defvar-local gfm-code-fences--last-window-state nil
  "Snapshot of the windows showing the buffer at the last rebuild.
List of (WINDOW . MAX-CHARS-PER-LINE) pairs.")

(defvar-local gfm-code-fences--dirty-region nil
  "Buffer-local (BEG . END) covering all unrebuilt edits, or nil if clean.")

(defvar-local gfm-code-fences--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defun gfm-code-fences--window-state ()
  "Return the (WINDOW . WIDTH) snapshot used to detect rendering drift."
  (mapcar (lambda (w) (cons w (gfm-code-fences--available-width w)))
          (gfm-code-fences--display-windows)))

(defun gfm-code-fences--rebuild ()
  "Remove and recreate all gfm-code-fences overlays."
  (let ((start (current-time)))
    (gfm-code-fences--remove-overlays)
    (setq gfm-code-fences--dirty-region nil)
    (let ((n (gfm-code-fences--apply-overlays)))
      (gfm-code-fences--record-stats (float-time (time-since start)) n))
    (setq gfm-code-fences--last-window-state
          (gfm-code-fences--window-state))))

(defun gfm-code-fences--rebuild-block (block)
  "Tear down BLOCK's overlays and re-apply just that block.
BLOCK is a `gfm-code-fences--block' struct."
  (let* ((start (current-time))
         (range (gfm-code-fences--block-range block)))
    (gfm-code-fences--remove-overlays (car range) (cdr range))
    (gfm-code-fences--apply-block-anchors block)
    (dolist (window (or (gfm-code-fences--display-windows) (list nil)))
      (gfm-code-fences--apply-block-display block window))
    (gfm-code-fences--record-stats (float-time (time-since start)) 1)))

(defun gfm-code-fences--rebuild-blocks (blocks)
  "Tear down each block in BLOCKS and re-apply them in one pass."
  (let ((start (current-time))
        (windows (or (gfm-code-fences--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (gfm-code-fences--block-range block)))
        (gfm-code-fences--remove-overlays (car range) (cdr range)))
      (gfm-code-fences--apply-block-anchors block)
      (dolist (window windows)
        (gfm-code-fences--apply-block-display block window)))
    (gfm-code-fences--record-stats (float-time (time-since start))
                                   (length blocks))))

(defun gfm-code-fences--rebuild-block-for-window (block window)
  "Replace WINDOW's display overlays for BLOCK at the current width."
  (let ((range (gfm-code-fences--block-range block)))
    (gfm-code-fences--remove-display-overlays-in-range
     (car range) (cdr range) window))
  (gfm-code-fences--apply-block-display block window)
  (gfm-code-fences--prune-dead-overlays))

;;; Visible-first prioritised rebuild

(defun gfm-code-fences--block-visible-p (block ranges)
  "Non-nil if BLOCK's source range overlaps any range in RANGES."
  (let ((br (gfm-code-fences--block-range block)))
    (cl-some (lambda (r)
               (and (<= (car br) (cdr r))
                    (>= (cdr br) (car r))))
             ranges)))

(defun gfm-code-fences--visible-window-ranges ()
  "Return (VSTART . VEND) pairs for every window currently showing this buffer.
Uses cached `window-end' (no force-update) since forcing redisplay on
a brand-new split window with stale overlays is the dominant cost
during a `C-x 3' transient.  Windows whose end is nil are dropped —
their blocks fall through to the off-screen-deferred path."
  (delq nil
        (mapcar (lambda (w)
                  (let ((s (window-start w))
                        (e (window-end w)))
                    (and s e (cons s e))))
                (get-buffer-window-list (current-buffer) nil t))))

(defun gfm-code-fences--rebuild-prioritised ()
  "Rebuild visible-window blocks first; defer off-screen blocks one idle tick."
  (let ((ranges (gfm-code-fences--visible-window-ranges)))
    (cond
     ((null ranges)
      (gfm-code-fences--rebuild))
     (t
      (let* ((blocks (gfm-code-fences--collect-blocks))
             (visible (cl-remove-if-not
                       (lambda (b) (gfm-code-fences--block-visible-p b ranges))
                       blocks))
             (offscreen (cl-set-difference blocks visible)))
        (when visible
          (gfm-code-fences--rebuild-blocks visible))
        (setq gfm-code-fences--dirty-region nil
              gfm-code-fences--last-window-state
              (gfm-code-fences--window-state))
        (when offscreen
          (run-with-idle-timer
           0 nil
           (lambda (buf bs)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when gfm-code-fences-mode
                   (gfm-code-fences--rebuild-blocks bs)))))
           (current-buffer) offscreen)))))))

(defun gfm-code-fences--rebuild-window-prioritised (window)
  "Per-block, idle-paced rebuild of WINDOW's display overlays.
Visible blocks are rendered first, then off-screen ones, but rendering
proceeds one block at a time across separate idle ticks so user input
between blocks is responsive.  `window-end' is read WITHOUT the
force-redisplay flag — forcing redisplay on a freshly-split window
with parent-sized overlays is itself expensive."
  (when (window-live-p window)
    (let* ((blocks (gfm-code-fences--collect-blocks))
           (vstart (window-start window))
           (vend (window-end window))
           (ranges (and vstart vend (list (cons vstart vend))))
           (visible (and ranges
                         (cl-remove-if-not
                          (lambda (b)
                            (gfm-code-fences--block-visible-p b ranges))
                          blocks)))
           (offscreen (cl-set-difference blocks visible))
           (queue (append visible offscreen)))
      (when queue
        (gfm-code-fences--pace-window-rebuild
         (current-buffer) queue window)))))

(defun gfm-code-fences--pace-window-rebuild (buf queue window)
  "Render the next block in QUEUE for WINDOW in BUF, then re-arm idle.
One block per idle tick keeps `C-x 3' / resize transients responsive
even on buffers with many fenced blocks."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (when (and (buffer-live-p buf) (window-live-p window))
       (with-current-buffer buf
         (when gfm-code-fences-mode
           (let ((b (car queue))
                 (rest (cdr queue)))
             (gfm-code-fences--rebuild-block-for-window b window)
             (cond
              (rest
               (gfm-code-fences--pace-window-rebuild buf rest window))
              (t
               (gfm-code-fences--prune-dead-overlays))))))))))

(defun gfm-code-fences--reconcile-windows ()
  "Reconcile display overlays with current window state.
Removed windows have their display overlays deleted synchronously
\(fast — pure deletion).  Added or resized windows have their rebuild
deferred to the next idle tick so the `C-x 3' / resize transient
itself never blocks: even visible-block rendering pays the cost of
forcing a redisplay on a freshly-split window whose existing overlays
were sized for the parent's width, which on a buffer with many
fenced blocks is the dominant cost."
  (cond
   ((or (null gfm-code-fences--last-window-state)
        (null (cl-some (lambda (o) (overlay-get o 'gfm-code-fences-anchor))
                       gfm-code-fences--overlays)))
    (gfm-code-fences--rebuild))
   (t
    (let* ((prev gfm-code-fences--last-window-state)
           (curr (gfm-code-fences--window-state))
           (prev-keys (mapcar #'car prev))
           (curr-keys (mapcar #'car curr))
           (added (cl-remove-if (lambda (e) (memq (car e) prev-keys)) curr))
           (removed (cl-remove-if (lambda (w) (memq w curr-keys)) prev-keys))
           (resized (cl-remove-if-not
                     (lambda (e)
                       (let ((old (assq (car e) prev)))
                         (and old (not (eql (cdr e) (cdr old))))))
                     curr))
           (touched-windows (mapcar #'car (append added resized))))
      (dolist (w removed)
        (gfm-code-fences--remove-display-overlays-for-window w))
      (gfm-code-fences--prune-dead-overlays)
      (setq gfm-code-fences--last-window-state curr)
      (when touched-windows
        (run-with-idle-timer
         0 nil
         (lambda (buf wins)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when gfm-code-fences-mode
                 (dolist (w wins)
                   (when (window-live-p w)
                     (gfm-code-fences--rebuild-window-prioritised w)))))))
         (current-buffer) touched-windows))))))

;;; Scoped post-edit rebuild

(defun gfm-code-fences--extend-dirty-region (beg end)
  "Extend the buffer's dirty region to cover BEG..END."
  (cond
   ((null gfm-code-fences--dirty-region)
    (setq gfm-code-fences--dirty-region (cons beg end)))
   (t
    (setcar gfm-code-fences--dirty-region
            (min (car gfm-code-fences--dirty-region) beg))
    (setcdr gfm-code-fences--dirty-region
            (max (cdr gfm-code-fences--dirty-region) end)))))

(defun gfm-code-fences--fence-line-ranges ()
  "Return per-line (BEG . END) ranges for every fence opening / closing line.
Includes YAML helmet markers as well so edits to those lines invalidate."
  (let (ranges)
    (dolist (b (gfm-code-fences--find-blocks))
      (let ((open-beg (nth 0 b))
            (close-end (nth 3 b)))
        (push (cons (save-excursion
                      (goto-char open-beg) (line-beginning-position))
                    (save-excursion
                      (goto-char open-beg) (line-end-position)))
              ranges)
        (push (cons (save-excursion
                      (goto-char close-end) (line-beginning-position))
                    (save-excursion
                      (goto-char close-end) (line-end-position)))
              ranges)))
    (when-let* ((helmet (gfm-code-fences--find-yaml-helmet)))
      (cl-destructuring-bind (open-beg _open-end close-beg close-end) helmet
        (push (cons (save-excursion
                      (goto-char open-beg) (line-beginning-position))
                    (save-excursion
                      (goto-char open-beg) (line-end-position)))
              ranges)
        (push (cons (save-excursion
                      (goto-char close-beg) (line-beginning-position))
                    close-end)
              ranges)))
    ranges))

(defun gfm-code-fences--region-overlaps-fence-line-p (region)
  "Non-nil if REGION overlaps a fence opening/closing line."
  (cl-some (lambda (r) (gfm-code-fences--region-overlaps-p region r))
           (gfm-code-fences--fence-line-ranges)))

(defun gfm-code-fences--blank-line-adjacent-to-indent-p (region)
  "Non-nil if REGION overlaps a blank line adjacent to an indent block.
Indent-block discovery is gated by blank-line adjacency, so an edit
that synthesises or dissolves an adjacent blank line could create or
destroy a block."
  (let* ((excluded
          (append
           (when-let* ((h (gfm-code-fences--find-yaml-helmet)))
             (cl-destructuring-bind (olb _ole _clb cle)
                 (gfm-code-fences--yaml-line-positions h)
               (list (cons olb cle))))
           (mapcar (lambda (b)
                     (cl-destructuring-bind (olb _ole _clb cle)
                         (gfm-code-fences--fenced-line-positions b)
                       (cons olb cle)))
                   (gfm-code-fences--find-blocks))))
         (blocks (gfm-code-fences--find-indent-blocks excluded)))
    (cl-some
     (lambda (b)
       (cl-destructuring-bind (beg end _iw) b
         (let* ((before-beg
                 (save-excursion
                   (goto-char beg)
                   (forward-line -1)
                   (line-beginning-position)))
                (before-end
                 (save-excursion
                   (goto-char beg)
                   (forward-line -1)
                   (line-end-position)))
                (after-beg
                 (save-excursion
                   (goto-char end)
                   (forward-line 1)
                   (line-beginning-position)))
                (after-end
                 (save-excursion
                   (goto-char end)
                   (forward-line 1)
                   (line-end-position))))
           (or (and (>= before-end (point-min))
                    (gfm-code-fences--region-overlaps-p
                     region (cons before-beg before-end)))
               (and (<= after-beg (point-max))
                    (gfm-code-fences--region-overlaps-p
                     region (cons after-beg after-end)))))))
     blocks)))

(defun gfm-code-fences--block-fully-contains-p (block region)
  "Non-nil if REGION lies inside BLOCK's source range."
  (let ((br (gfm-code-fences--block-range block)))
    (and (>= (car region) (car br))
         (<= (cdr region) (cdr br)))))

(defun gfm-code-fences--rebuild-scoped ()
  "Rebuild only what `gfm-code-fences--dirty-region' demands."
  (let ((dirty gfm-code-fences--dirty-region))
    (setq gfm-code-fences--dirty-region nil)
    (cond
     ((null dirty) nil)
     ((gfm-code-fences--region-overlaps-fence-line-p dirty)
      (gfm-code-fences--rebuild))
     ((gfm-code-fences--blank-line-adjacent-to-indent-p dirty)
      (gfm-code-fences--rebuild))
     (t
      (let* ((blocks (gfm-code-fences--collect-blocks))
             (matching (cl-loop for b in blocks
                                when (gfm-code-fences--region-overlaps-p
                                      dirty
                                      (gfm-code-fences--block-range b))
                                collect b)))
        (cond
         ((null matching) nil)
         ((and (null (cdr matching))
               (gfm-code-fences--block-fully-contains-p (car matching) dirty))
          (gfm-code-fences--rebuild-block (car matching)))
         (t
          (gfm-code-fences--rebuild))))))))

;;; Schedulers

(defun gfm-code-fences--arm-rebuild-timer (callback)
  "Cancel any pending rebuild timer and schedule CALLBACK after idle."
  (when (timerp gfm-code-fences--rebuild-timer)
    (cancel-timer gfm-code-fences--rebuild-timer))
  (setq gfm-code-fences--rebuild-timer
        (run-with-idle-timer
         0.2 nil
         (lambda (buf cb)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when gfm-code-fences-mode
                 (funcall cb)))))
         (current-buffer) callback)))

(defun gfm-code-fences--schedule-rebuild (&optional beg end _len)
  "Merge BEG..END into the dirty region and arm the rebuild timer.
Skips indirect buffers since base-buffer overlays already cover them."
  (unless (buffer-base-buffer)
    (when (and beg end)
      (gfm-code-fences--extend-dirty-region beg end))
    (gfm-code-fences--arm-rebuild-timer #'gfm-code-fences--rebuild-scoped)))

(defun gfm-code-fences--schedule-full-rebuild (&rest _)
  "Schedule a reconciliation on next idle if window state has changed."
  (unless (buffer-base-buffer)
    (let ((state (gfm-code-fences--window-state)))
      (unless (equal state gfm-code-fences--last-window-state)
        (gfm-code-fences--arm-rebuild-timer
         #'gfm-code-fences--reconcile-windows)))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-code-fences-mode
  "Adorn fenced code blocks with curved dashed borders and language icons."
  :lighter " gfm-cf"
  (if gfm-code-fences-mode
      (progn
        (cursor-intangible-mode 1)
        (gfm-code-fences--init-stats)
        (gfm-code-fences--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-code-fences--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-code-fences--schedule-full-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-code-fences--reveal nil t))
    (remove-hook 'after-change-functions
                 #'gfm-code-fences--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-code-fences--schedule-full-rebuild t)
    (remove-hook 'post-command-hook #'gfm-code-fences--reveal t)
    (when (timerp gfm-code-fences--rebuild-timer)
      (cancel-timer gfm-code-fences--rebuild-timer))
    (gfm-code-fences--remove-overlays)
    (cursor-intangible-mode -1)))

(provide '+gfm-code-fences)

;;; +gfm-code-fences.el ends here
