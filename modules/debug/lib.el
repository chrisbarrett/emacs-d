;;; init.el --- Debug module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'debug)

;;;###autoload
(defun +debugger-toggle-on-exit-frame ()
  "Toggle whether to reactivate on exit frame."
  (interactive)
  (let ((enabled-for-line (save-excursion
                            (goto-char (line-beginning-position))
                            (looking-at (rx (* space) "*" (+ space))))))
    (cond
     (enabled-for-line
      (debugger-frame-clear)
      (message "debug on exit for frame disabled"))
     (t
      (debugger-frame)
      (message "debug on exit for frame enabled")))))

;;; lib.el ends here
