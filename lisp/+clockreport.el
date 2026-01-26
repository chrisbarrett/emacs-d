;;; +clockreport.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-clock)

(defun +clocktable--realign-table (ipos)
  (save-excursion
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when org-hide-emphasis-markers
      (org-table-align))))

(defun +clocktable-fmt-daily-log (ipos tables params)
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
     "| *Total* | " (org-duration-from-minutes total-time) "|\n"
     )

    (+clocktable--realign-table ipos)
    total-time))

(provide '+clockreport)

;;; +clockreport.el ends here
