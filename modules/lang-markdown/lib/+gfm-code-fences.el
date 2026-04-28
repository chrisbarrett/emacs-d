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

;;; Border face

(defconst gfm-code-fences--border-face 'markdown-markup-face
  "Face for the curved fence border.")

(defun gfm-code-fences--display-string (kind face icon has-lang)
  "Build the display string for KIND with FACE.
KIND is `top', `body' or `bottom'. ICON and HAS-LANG only apply to `top'."
  (let ((box (propertize (pcase kind
                           ('top    "╭┄")
                           ('body   "│ ")
                           ('bottom "╰┄"))
                         'face face)))
    (pcase kind
      ('top (cond (icon     (concat box " " icon " "))
                  (has-lang (concat box " "))
                  (t        box)))
      (_ box))))

(defun gfm-code-fences--target-prop (kind)
  "Overlay property used to render KIND."
  (if (eq kind 'body) 'before-string 'display))

;;; Overlay application

(defvar-local gfm-code-fences--overlays nil
  "All fence overlays currently in this buffer.")

(defun gfm-code-fences--make-overlay (beg end kind block-beg block-end
                                          &optional icon has-lang revealable)
  "Create a fence overlay between BEG and END with KIND.
BLOCK-BEG/BLOCK-END mark the enclosing block. ICON and HAS-LANG
configure the `top' kind. REVEALABLE marks the overlay for cursor reveal."
  (let* ((str (gfm-code-fences--display-string
               kind gfm-code-fences--border-face icon has-lang))
         (ov (make-overlay beg end)))
    (overlay-put ov 'gfm-code-fences t)
    (overlay-put ov 'gfm-code-fences-kind kind)
    (overlay-put ov 'gfm-code-fences-icon icon)
    (overlay-put ov 'gfm-code-fences-has-lang has-lang)
    (overlay-put ov 'gfm-code-fences-block-beg block-beg)
    (overlay-put ov 'gfm-code-fences-block-end block-end)
    (when revealable
      (overlay-put ov 'gfm-code-fences-revealable t)
      (overlay-put ov 'evaporate t))
    (overlay-put ov (gfm-code-fences--target-prop kind) str)
    (push ov gfm-code-fences--overlays)
    ov))

(defun gfm-code-fences--apply-overlays ()
  "Create overlays for fenced code blocks in the current buffer."
  (save-excursion
    (dolist (block (gfm-code-fences--find-blocks))
      (cl-destructuring-bind
          (open-beg _open-end close-beg close-end lang lang-beg _lang-end) block
        (let* ((block-beg (save-excursion
                            (goto-char open-beg) (line-beginning-position)))
               (block-end close-end)
               (icon (and lang (gfm-code-fences--icon-for-lang lang))))
          ;; Top: single evaporative overlay over backticks (+ leading
          ;; whitespace before the language tag).
          (let ((cover-end (or lang-beg
                               (save-excursion
                                 (goto-char open-beg)
                                 (line-end-position)))))
            (gfm-code-fences--make-overlay open-beg cover-end 'top
                                           block-beg block-end
                                           icon (and lang t) t))
          ;; Body lines: prepend `│ '.
          (save-excursion
            (goto-char open-beg)
            (forward-line 1)
            (while (< (line-beginning-position) close-beg)
              (let ((lbeg (line-beginning-position)))
                (gfm-code-fences--make-overlay lbeg lbeg 'body
                                               block-beg block-end))
              (forward-line 1)))
          ;; Bottom: replace closing backticks with curve + dash.
          (gfm-code-fences--make-overlay close-beg close-end 'bottom
                                         block-beg block-end nil nil t))))))

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
