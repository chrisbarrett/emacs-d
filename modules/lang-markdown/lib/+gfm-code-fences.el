;;; +gfm-code-fences.el --- Visual treatment for fenced code blocks -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that adorns GFM fenced code blocks:
;;
;;   ╭┄┄┄
;;   │ echo hello
;;   │ echo world
;;   ╰┄┄┄
;;
;; The buffer ```` ``` ```` lines stay in place but are display-replaced with a
;; curved dashed border that joins the left edge.  The language tag on the
;; opening fence is replaced by the icon for that language's major mode (via
;; `nerd-icons-icon-for-mode'); the icon overlay evaporates on edit so typing
;; over the language tag reveals the text immediately.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)
(require 'nerd-icons nil t)

(defgroup gfm-code-fences nil
  "Visual treatment for GFM fenced code blocks."
  :group 'markdown-faces)

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

(defun gfm-code-fences--lang-mode (lang)
  "Best-guess major mode symbol for LANG."
  (or (cdr (assoc lang markdown-code-lang-modes))
      (let ((ts (intern (concat lang "-ts-mode"))))
        (and (fboundp ts) ts))
      (intern (concat lang "-mode"))))

(defun gfm-code-fences--icon-for-lang (lang)
  "Return the nerd-icons icon string for LANG, or nil."
  (when (and lang (fboundp 'nerd-icons-icon-for-mode))
    (let ((icon (ignore-errors
                  (nerd-icons-icon-for-mode (gfm-code-fences--lang-mode lang)))))
      (and (stringp icon) icon))))

;;; Block discovery

(defun gfm-code-fences--find-blocks ()
  "Return all fenced code blocks in the current buffer.
Each entry is (OPEN-BEG OPEN-END CLOSE-BEG CLOSE-END LANG LANG-BEG LANG-END)
where any of LANG, LANG-BEG, LANG-END may be nil."
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

;;; Indented (4-space / tab) pre block discovery

(defun gfm-code-fences--line-indent ()
  "Return code-block indent width on the current line, or nil.
4 spaces or 1 tab counts as a markdown indented code block opener."
  (cond ((looking-at "    ") 4)
        ((looking-at "\t") 1)))

(defun gfm-code-fences--blank-line-p ()
  "Non-nil if the current line is blank."
  (looking-at-p "[[:blank:]]*$"))

(defun gfm-code-fences--in-ranges-p (pos ranges)
  "Non-nil if POS lies in any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-code-fences--find-indent-blocks (excluded-ranges)
  "Return indented code blocks not overlapping EXCLUDED-RANGES.
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

;;; Border face & primitives

(defconst gfm-code-fences--border-face 'parenthesis
  "Face for the box border around pre blocks.")

(defun gfm-code-fences--text-width ()
  "Return the current window's max chars per visual line, with a sane fallback.
Uses `window-max-chars-per-line', which already excludes the continuation
column, so it stays stable across mode-init / window-config-change rebuilds."
  (or (when-let* ((win (or (get-buffer-window (current-buffer))
                           (get-buffer-window (current-buffer) t))))
        (window-max-chars-per-line win))
      fill-column
      80))

(defun gfm-code-fences--box-cap ()
  "Return the maximum allowable box-width.
One column is reserved as headroom so a fitting block's right border never
touches the window edge; overflow lines may use the full text width."
  (1- (gfm-code-fences--text-width)))

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

(defun gfm-code-fences--top-strings (width face buffer-width &optional icon)
  "Return (LEADING . TRAILING) split of the top border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols (matching the marker line's char count) so
the buffer text shows in place of LEADING when it is revealed; TRAILING
covers the remaining decoration. ICON, if non-nil, is right-aligned."
  (let* ((l (propertize "┌" 'face face))
         (r (propertize "┐" 'face face))
         (gap (propertize " " 'face face))
         (leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat l (propertize (make-string leading-dash-w ?─)
                                        'face face))))
    (cond
     (icon
      (let* ((icon-w (string-width icon))
             (total-fill-w (max 1 (- width 4 icon-w)))
             (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
             (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
        (cons leading (concat rem-fill gap icon gap r))))
     (t
      (let* ((total-fill-w (max 1 (- width 2)))
             (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
             (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
        (cons leading (concat rem-fill r)))))))

(defun gfm-code-fences--bottom-strings (width face buffer-width)
  "Return (LEADING . TRAILING) split of the bottom border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols matching the marker line's char count."
  (let* ((leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat (propertize "└" 'face face)
                          (propertize (make-string leading-dash-w ?─)
                                      'face face)))
         (total-fill-w (max 1 (- width 2)))
         (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
         (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
    (cons leading (concat rem-fill (propertize "┘" 'face face)))))

(defun gfm-code-fences--right-after (box-width face)
  "Build the after-string that draws the right border at column BOX-WIDTH."
  (let* ((align-col (- box-width 2))
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
visual lines (default 0). Returns (LAST-COL . WRAP-POSITIONS) where
LAST-COL is the visual column the text reaches on its final visual line
(including the wrap-prefix on continuations) and WRAP-POSITIONS is the
list of TEXT positions at which a wrap occurs."
  (let* ((cont-prefix-w (or cont-prefix-w 0))
         (pos 0)
         (col 0)
         (len (length text))
         (first-line t)
         (wraps nil))
    (while (< pos len)
      (let* ((line-width (if first-line width (- width cont-prefix-w)))
             (remaining (- len pos))
             (space-left (- line-width col)))
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

(defconst gfm-code-fences--wrap-prefix-w 2
  "Visual width of the wrap-prefix shown on continuation visual lines.")

(defun gfm-code-fences--wrap-prefix (face)
  "Wrap-prefix string used on continuation lines."
  (propertize "⋱ " 'face face))


(defun gfm-code-fences--right-after-overflow (face line-text)
  "After-string padding the right border to the window edge for a wrapped line.
LINE-TEXT is the line's buffer content; the function simulates word-wrap to
work out where the line ends visually, accounting for the wrap-prefix on
continuation lines."
  (let* ((text-width (gfm-code-fences--text-width))
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

;;; Overlay application

(defvar-local gfm-code-fences--overlays nil
  "All fence overlays currently in this buffer.")

(defun gfm-code-fences--register (ov)
  "Tag OV as a fence overlay and remember it for bulk cleanup."
  (overlay-put ov 'gfm-code-fences t)
  (push ov gfm-code-fences--overlays)
  ov)

(defun gfm-code-fences--apply-bordered-block
    (open-line-beg open-line-end close-line-beg close-line-end face label)
  "Wrap a line-bounded block in a box.
OPEN-LINE-BEG/END mark the opening marker line, CLOSE-LINE-BEG/END the
closing marker line. FACE colours the border. LABEL, if non-nil, is
right-aligned in the top border.

The opening/closing marker lines are split into:
- a leading overlay (display covers the marker chars; evaporative + revealable)
- a trailing overlay (after-string with the rest of the border decoration;
  preserved even when the leading overlay is revealed)."
  (let* ((open-line-beg (save-excursion
                          (goto-char open-line-beg) (line-beginning-position)))
         (open-line-end (save-excursion
                          (goto-char open-line-end) (line-end-position)))
         (close-line-beg (save-excursion
                           (goto-char close-line-beg) (line-beginning-position)))
         (close-line-end (save-excursion
                           (goto-char close-line-end) (line-end-position)))
         (body-beg (save-excursion
                     (goto-char open-line-end) (forward-line 1) (point)))
         (body-end (max body-beg (1- close-line-beg)))
         (max-content (gfm-code-fences--max-line-width body-beg body-end))
         (text-width (gfm-code-fences--text-width))
         (box-width (min text-width (max 80 (+ max-content 4))))
         (content-budget (- box-width 4))
         (open-buf-width (- open-line-end open-line-beg))
         (close-buf-width (- close-line-end close-line-beg))
         (top-split (gfm-code-fences--top-strings box-width face
                                                  open-buf-width label))
         (bot-split (gfm-code-fences--bottom-strings box-width face
                                                     close-buf-width))
         (lhs (propertize "│ " 'face face)))
    ;; Top — leading on the marker line, trailing after.
    (let ((ov (make-overlay open-line-beg open-line-end)))
      (overlay-put ov 'gfm-code-fences-kind 'top-leading)
      (overlay-put ov 'gfm-code-fences-revealable t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display (car top-split))
      (gfm-code-fences--register ov))
    (let ((ov (make-overlay open-line-end open-line-end nil nil t)))
      (overlay-put ov 'gfm-code-fences-kind 'top-trailing)
      (overlay-put ov 'after-string (cdr top-split))
      (gfm-code-fences--register ov))
    ;; Body lines.
    (save-excursion
      (goto-char body-beg)
      (while (< (line-beginning-position) close-line-beg)
        (let* ((lbeg (line-beginning-position))
               (lend (line-end-position))
               (ov (make-overlay lbeg lend nil nil t)))
          (overlay-put ov 'gfm-code-fences-kind 'body)
          (overlay-put ov 'before-string lhs)
          (overlay-put ov 'wrap-prefix (gfm-code-fences--wrap-prefix face))
          (overlay-put ov 'after-string
                       (if (> (- lend lbeg) content-budget)
                           (gfm-code-fences--right-after-overflow
                            face (buffer-substring-no-properties lbeg lend))
                         (gfm-code-fences--right-after box-width face)))
          (gfm-code-fences--register ov))
        (forward-line 1)))
    ;; Bottom — leading on the marker line, trailing after.
    (let ((ov (make-overlay close-line-beg close-line-end)))
      (overlay-put ov 'gfm-code-fences-kind 'bottom-leading)
      (overlay-put ov 'gfm-code-fences-revealable t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display (car bot-split))
      (gfm-code-fences--register ov))
    (let ((ov (make-overlay close-line-end close-line-end nil nil t)))
      (overlay-put ov 'gfm-code-fences-kind 'bottom-trailing)
      (overlay-put ov 'after-string (cdr bot-split))
      (gfm-code-fences--register ov))))

(defun gfm-code-fences--apply-fenced-block (block fenced-ranges)
  "Adorn one fenced BLOCK and push its range onto FENCED-RANGES."
  (cl-destructuring-bind
      (open-beg _open-end close-beg close-end lang _lang-beg _lang-end) block
    (let* ((face gfm-code-fences--border-face)
           (open-line-beg (save-excursion
                            (goto-char open-beg) (line-beginning-position)))
           (open-line-end (save-excursion
                            (goto-char open-beg) (line-end-position)))
           (close-line-beg (save-excursion
                             (goto-char close-beg) (line-beginning-position)))
           (close-line-end close-end)
           (icon (and lang (gfm-code-fences--icon-for-lang lang))))
      (push (cons open-line-beg close-line-end) (cdr fenced-ranges))
      (gfm-code-fences--apply-bordered-block
       open-line-beg open-line-end close-line-beg close-line-end face icon))))

;;; YAML frontmatter

(defconst gfm-code-fences--yaml-marker-re "^---[[:blank:]]*$"
  "Line consisting of `---' (with optional trailing whitespace).")

(defun gfm-code-fences--find-yaml-helmet ()
  "Return (OPEN-BEG OPEN-END CLOSE-BEG CLOSE-END) for leading YAML, or nil."
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

(defun gfm-code-fences--apply-yaml-helmet (fenced-ranges)
  "Adorn leading YAML frontmatter and add its range to FENCED-RANGES."
  (when-let* ((helmet (gfm-code-fences--find-yaml-helmet)))
    (cl-destructuring-bind (open-beg open-end close-beg close-end) helmet
      (let* ((face 'font-lock-constant-face)
             (label (propertize "meta" 'face `(:inherit (bold ,face))))
             (open-line-beg (save-excursion
                              (goto-char open-beg) (line-beginning-position)))
             (open-line-end (save-excursion
                              (goto-char open-beg) (line-end-position))))
        (push (cons open-line-beg close-end) (cdr fenced-ranges))
        (gfm-code-fences--apply-bordered-block
         open-line-beg open-line-end close-beg close-end face label)))))

(defun gfm-code-fences--apply-indent-block (beg end indent-width)
  "Adorn the indented code block at [BEG, END] with a full curved box.
INDENT-WIDTH is the buffer indent (4 for spaces, 1 for tab)."
  (save-excursion
    (goto-char beg)
    (let* ((face gfm-code-fences--border-face)
           (max-content (gfm-code-fences--max-line-width beg end indent-width))
           (text-width (gfm-code-fences--text-width))
           (box-width (min text-width (max 80 (+ max-content 4))))
           (content-budget (- box-width 4))
           ;; Indent blocks have no marker line; use a 0-width split so the
           ;; full top/bottom border lives in the trailing piece.
           (top-split (gfm-code-fences--top-strings box-width face 0 nil))
           (bot-split (gfm-code-fences--bottom-strings box-width face 0))
           (top-str (concat (car top-split) (cdr top-split)))
           (bot-str (concat (car bot-split) (cdr bot-split)))
           (lhs (propertize "│ " 'face face))
           (first t))
      (while (and (not (eobp)) (<= (line-beginning-position) end))
        (let* ((lbeg (line-beginning-position))
               (lend (line-end-position))
               (cover-end (min (+ lbeg indent-width) lend))
               (last-line (>= lend end))
               (line-content-w (max 0 (- (- lend lbeg) indent-width)))
               (body-ov (make-overlay lbeg cover-end))
               (rhs-ov (make-overlay lend lend nil nil t))
               (line-text (buffer-substring-no-properties cover-end lend))
               (overflow-p (> line-content-w content-budget))
               (after (if overflow-p
                          (gfm-code-fences--right-after-overflow face line-text)
                        (gfm-code-fences--right-after box-width face))))
          (overlay-put body-ov 'gfm-code-fences-kind 'indent-body)
          (overlay-put body-ov 'cursor-intangible t)
          (overlay-put body-ov 'display lhs)
          (when first
            (overlay-put body-ov 'before-string (concat top-str "\n")))
          (gfm-code-fences--register body-ov)
          (let ((wrap-ov (make-overlay lbeg lend nil nil t)))
            (overlay-put wrap-ov 'gfm-code-fences-kind 'indent-wrap)
            (overlay-put wrap-ov 'wrap-prefix (gfm-code-fences--wrap-prefix face))
            (gfm-code-fences--register wrap-ov))
          (overlay-put rhs-ov 'gfm-code-fences-kind 'indent-rhs)
          (overlay-put rhs-ov 'after-string
                       (if last-line (concat after "\n" bot-str) after))
          (gfm-code-fences--register rhs-ov)
          (setq first nil))
        (forward-line 1)))))

(defun gfm-code-fences--apply-overlays ()
  "Create overlays for YAML frontmatter, fenced and indented code blocks."
  (save-excursion
    (let ((fenced-ranges (cons nil nil)))
      (gfm-code-fences--apply-yaml-helmet fenced-ranges)
      (dolist (block (gfm-code-fences--find-blocks))
        (gfm-code-fences--apply-fenced-block block fenced-ranges))
      (dolist (block (gfm-code-fences--find-indent-blocks (cdr fenced-ranges)))
        (cl-destructuring-bind (beg end indent-width) block
          (gfm-code-fences--apply-indent-block beg end indent-width))))))

;;; Cursor-driven reveal

(defvar-local gfm-code-fences--hidden-ovs nil
  "Revealable overlays whose display is currently suppressed.")

(defun gfm-code-fences--reveal ()
  "Suppress display on revealable overlays containing point; restore others."
  (let ((pos (point)))
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
    ;; Hide revealable overlays at point.
    (dolist (ov (overlays-in pos (1+ pos)))
      (when (and (overlay-get ov 'gfm-code-fences-revealable)
                 (overlay-get ov 'display)
                 (not (memq ov gfm-code-fences--hidden-ovs)))
        (overlay-put ov 'gfm-code-fences-saved-display
                     (overlay-get ov 'display))
        (overlay-put ov 'display nil)
        (push ov gfm-code-fences--hidden-ovs)))))


;;; Overlay management

(defun gfm-code-fences--remove-overlays (&optional beg end)
  "Remove all gfm-code-fences overlays between BEG and END."
  (remove-overlays (or beg (point-min)) (or end (point-max))
                   'gfm-code-fences t)
  (unless (or beg end)
    (setq gfm-code-fences--overlays nil
          gfm-code-fences--hidden-ovs nil)))

(defun gfm-code-fences--rebuild ()
  "Remove and recreate all gfm-code-fences overlays."
  (gfm-code-fences--remove-overlays)
  (gfm-code-fences--apply-overlays))

(defvar-local gfm-code-fences--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defun gfm-code-fences--schedule-rebuild (&rest _)
  "Schedule a debounced overlay rebuild."
  (unless (buffer-base-buffer)
    (when (timerp gfm-code-fences--rebuild-timer)
      (cancel-timer gfm-code-fences--rebuild-timer))
    (setq gfm-code-fences--rebuild-timer
          (run-with-idle-timer
           0.2 nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when gfm-code-fences-mode
                   (gfm-code-fences--rebuild)))))
           (current-buffer)))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-code-fences-mode
  "Adorn fenced code blocks with curved dashed borders and language icons."
  :lighter " gfm-cf"
  (if gfm-code-fences-mode
      (progn
        (cursor-intangible-mode 1)
        (gfm-code-fences--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-code-fences--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-code-fences--schedule-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-code-fences--reveal nil t))
    (remove-hook 'after-change-functions
                 #'gfm-code-fences--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-code-fences--schedule-rebuild t)
    (remove-hook 'post-command-hook #'gfm-code-fences--reveal t)
    (when (timerp gfm-code-fences--rebuild-timer)
      (cancel-timer gfm-code-fences--rebuild-timer))
    (gfm-code-fences--remove-overlays)
    (cursor-intangible-mode -1)))

(provide '+gfm-code-fences)

;;; +gfm-code-fences.el ends here
