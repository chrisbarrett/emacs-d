;;; lib.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defvar +after-make-tty-frame-functions nil)

;;;###autoload
(defun +tty-run-frame-hook-h (frame)
  (unless (display-graphic-p frame)
    (run-hook-with-args '+after-make-tty-frame-functions frame)))
;;;###autoload
(add-hook 'after-make-frame-functions #'+tty-run-frame-hook-h)


;;;###autoload
(defun +tty-frame-use-box-characters (frame)
  "On TTY FRAME, use Unicode box-drawing for window separators."
  (with-selected-frame frame
    (let ((dt (or (frame-parameter frame '+vborder-dtable)
                  (let ((dt (make-display-table)))
                    (set-display-table-slot dt 'vertical-border (make-glyph-code ?│))
                    (set-frame-parameter frame '+vborder-dtable dt)
                    dt))))
      (set-display-table-slot dt 'truncation (make-glyph-code ?… 'warning))

      (dolist (window (window-list frame 'no-minibuf))
        (set-window-display-table window dt))

      (let ((update-display-table (lambda ()
                                    (when (eq (selected-frame) frame)
                                      (dolist (window (window-list frame 'no-minibuf))
                                        (unless (eq (window-display-table window) dt)
                                          (set-window-display-table window dt)))))))
        (with-current-buffer (window-buffer (frame-selected-window frame))
          (add-hook 'window-configuration-change-hook update-display-table nil t))))))


;;; lib.el ends here
