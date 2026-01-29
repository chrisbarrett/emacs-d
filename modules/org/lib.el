;;; lib.el --- Org-mode library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for org module.

;;; Code:

;;;###autoload
(defface +org-id-link
  '((t (:weight semi-bold :inherit font-lock-variable-name-face)))
  "Face for ID links; these would typically be org-roam links."
  :group 'org-link)

;;;###autoload
(defun +org-cut-subtree-or-cancel-note ()
  "Cut subtree, cancel note, or kill capture depending on context."
  (interactive)
  (cond ((bound-and-true-p org-finish-function)
         (let ((org-note-abort t)) (funcall org-finish-function)))
        ((bound-and-true-p org-capture-mode)
         (org-capture-kill))
        (t
         (org-cut-subtree))))

;;;###autoload
(defun +org-metareturn-insert-heading-insert-state-h ()
  "Insert heading with M-RET in list items and enter insert state."
  (when (org-in-item-p)
    (org-insert-heading current-prefix-arg)
    (when (bound-and-true-p evil-mode)
      (evil-append-line 1))
    t))

;;;###autoload
(defun +ad-org-enter-evil-insert-state (&rest _)
  "Enter evil insert state after heading creation."
  (when (and (bound-and-true-p evil-mode)
             (called-interactively-p nil))
    (evil-insert-state)))


;;; Clocktable functions

(defun +clocktable--realign-table (ipos)
  "Realign org table at IPOS."
  (save-excursion
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when (bound-and-true-p org-hide-emphasis-markers)
      (org-table-align))))

;;;###autoload
(defun +clocktable-fmt-daily-log (ipos tables params)
  "Custom clocktable formatter for daily logs.
IPOS is insertion position, TABLES is clock data, PARAMS is user params."
  (require 'org-clock)
  (require 'org-duration)
  ;; See: `org-clocktable-write-default'
  (let* ((block (plist-get params :block))
         (total-time (or (apply #'+ (mapcar #'cadr tables))
                         0)))
    (insert-before-markers
     (or (plist-get params :header)
         (format "#+CAPTION: %s %s%s\n"
	         "Clock summary at"
	         (format-time-string (org-time-stamp-format t t))
	         (if block
	             (let ((range-text
		            (nth 2 (org-clock-special-range
			            block nil t
			            (plist-get params :wstart)
			            (plist-get params :mstart)))))
		       (format ", for %s." range-text))
	           ""))))

    ;; Table header
    (insert-before-markers
     "| HEADLINE | H:MM |\n"
     "|---\n")

    ;; Insert table rows
    (unless (zerop total-time)
      (pcase-dolist (`(,_file-name ,_file-time ,entries) tables)
        (pcase-dolist (`(,_level ,headline ,_tags ,_ts ,time ,_props) entries)
          (insert-before-markers
           "| " headline " | " (org-duration-from-minutes time) "|\n"))))

    ;; Summary
    (insert-before-markers
     "|---\n"
     "| *Total* | " (org-duration-from-minutes total-time) "|\n")

    (+clocktable--realign-table ipos)
    total-time))


