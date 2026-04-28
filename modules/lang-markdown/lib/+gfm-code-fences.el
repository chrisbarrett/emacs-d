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

(defconst gfm-code-fences--border-face 'font-lock-doc-face
  "Face for the box border around pre blocks.")

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

(defun gfm-code-fences--top-string (width face &optional icon)
  "Build a top border WIDTH cols wide.
ICON, if non-nil, is right-aligned like `┌──── <icon> ┐'."
  (let* ((l (propertize "┌" 'face face))
         (r (propertize "┐" 'face face)))
    (cond
     (icon
      (let* ((icon-w (string-width icon))
             (fill-w (max 1 (- width 4 icon-w)))
             (fill (propertize (make-string fill-w ?─) 'face face))
             (gap (propertize " " 'face face)))
        (concat l fill gap icon gap r)))
     (t
      (let ((fill (propertize (make-string (max 1 (- width 2)) ?─) 'face face)))
        (concat l fill r))))))

(defun gfm-code-fences--bottom-string (width face)
  "Build a bottom border WIDTH cols wide."
  (let ((fill (propertize (make-string (max 1 (- width 2)) ?─) 'face face)))
    (concat (propertize "└" 'face face)
            fill
            (propertize "┘" 'face face))))

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
right-aligned in the top border."
  (let* ((body-beg (save-excursion
                     (goto-char open-line-end) (forward-line 1) (point)))
         (body-end (max body-beg (1- close-line-beg)))
         (max-content (gfm-code-fences--max-line-width body-beg body-end))
         (box-width (max 80 (+ max-content 4)))
         (top-str (gfm-code-fences--top-string box-width face label))
         (bot-str (gfm-code-fences--bottom-string box-width face))
         (lhs (propertize "│ " 'face face)))
    (let ((ov (make-overlay open-line-beg open-line-end)))
      (overlay-put ov 'gfm-code-fences-kind 'top)
      (overlay-put ov 'gfm-code-fences-revealable t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display top-str)
      (gfm-code-fences--register ov))
    (save-excursion
      (goto-char body-beg)
      (while (< (line-beginning-position) close-line-beg)
        (let* ((lbeg (line-beginning-position))
               (lend (line-end-position))
               (lhs-ov (make-overlay lbeg lbeg))
               (rhs-ov (make-overlay lend lend nil nil t)))
          (overlay-put lhs-ov 'gfm-code-fences-kind 'body-lhs)
          (overlay-put lhs-ov 'before-string lhs)
          (gfm-code-fences--register lhs-ov)
          (overlay-put rhs-ov 'gfm-code-fences-kind 'body-rhs)
          (overlay-put rhs-ov 'after-string
                       (gfm-code-fences--right-after box-width face))
          (gfm-code-fences--register rhs-ov))
        (forward-line 1)))
    (let ((ov (make-overlay close-line-beg close-line-end)))
      (overlay-put ov 'gfm-code-fences-kind 'bottom)
      (overlay-put ov 'gfm-code-fences-revealable t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display bot-str)
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
           (box-width (max 80 (+ max-content 4)))
           (top-str (gfm-code-fences--top-string box-width face nil))
           (bot-str (gfm-code-fences--bottom-string box-width face))
           (lhs (propertize "│ " 'face face))
           (first t))
      (while (and (not (eobp)) (<= (line-beginning-position) end))
        (let* ((lbeg (line-beginning-position))
               (lend (line-end-position))
               (cover-end (min (+ lbeg indent-width) lend))
               (last-line (>= lend end))
               (body-ov (make-overlay lbeg cover-end))
               (rhs-ov (make-overlay lend lend nil nil t))
               (after (gfm-code-fences--right-after box-width face)))
          (overlay-put body-ov 'gfm-code-fences-kind 'indent-body)
          (overlay-put body-ov 'evaporate t)
          (overlay-put body-ov 'display lhs)
          (when first
            (overlay-put body-ov 'before-string (concat top-str "\n")))
          (gfm-code-fences--register body-ov)
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
        (gfm-code-fences--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-code-fences--schedule-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-code-fences--reveal nil t))
    (remove-hook 'after-change-functions
                 #'gfm-code-fences--schedule-rebuild t)
    (remove-hook 'post-command-hook #'gfm-code-fences--reveal t)
    (when (timerp gfm-code-fences--rebuild-timer)
      (cancel-timer gfm-code-fences--rebuild-timer))
    (gfm-code-fences--remove-overlays)))

(provide '+gfm-code-fences)

;;; +gfm-code-fences.el ends here
