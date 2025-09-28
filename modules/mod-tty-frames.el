;;; mod-tty-frames.el --- Configuration specific to TTY frames -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +tty-frame-setup (frame)
  "On TTY FRAME, use Unicode box-drawing for window separators."
  (unless (display-graphic-p frame)
    (with-selected-frame frame
      (let ((dt (or (frame-parameter frame '+vborder-dtable)
                    (let ((dt (make-display-table)))
                      (set-display-table-slot dt 'vertical-border (make-glyph-code ?│))
                      (set-frame-parameter frame '+vborder-dtable dt)
                      dt))))

        (dolist (window (window-list frame 'no-minibuf))
          (set-window-display-table window dt))

        (let ((update-display-table (lambda ()
                                      (when (eq (selected-frame) frame)
                                        (dolist (window (window-list frame 'no-minibuf))
                                          (unless (eq (window-display-table window) dt)
                                            (set-window-display-table window dt)))))))
          (with-current-buffer (window-buffer (frame-selected-window frame))
            (add-hook 'window-configuration-change-hook update-display-table nil t)))))))


(add-hook 'after-make-frame-functions #'+tty-frame-setup)

(+tty-frame-setup (selected-frame))

(provide 'mod-tty-frames)

;;; mod-tty-frames.el ends here
