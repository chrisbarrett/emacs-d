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
(defun +tty-window-display-table (window)
  "Return WINDOW's `window-display-table', creating a box-glyph one if absent.
The table carries the `vertical-border' and `truncation' glyphs used for
window separators on terminals.  Because a `window-display-table' shadows
`buffer-display-table' outright (Emacs uses a single display table per
window, no merging), we only ever install one on TTY windows -- graphical
frames render separators and truncation natively and must keep
`buffer-display-table' visible so `page-break-lines-mode' works there."
  (or (window-display-table window)
      (let ((dt (make-display-table)))
        (set-display-table-slot dt 'vertical-border (make-glyph-code ?│))
        (set-display-table-slot dt 'truncation (make-glyph-code ?… 'warning))
        (set-window-display-table window dt)
        dt)))

;;;###autoload
(defun +tty-frame-use-box-characters (&optional frame)
  "Give every window on a TTY FRAME a box-drawing `window-display-table'.
FRAME defaults to the selected frame.  Graphical frames are left untouched
so their native separators, fringe truncation indicators, and
`page-break-lines-mode' rules (drawn via `buffer-display-table') keep
working."
  (setq frame (or frame (selected-frame)))
  (unless (display-graphic-p frame)
    (dolist (window (window-list frame 'no-minibuf))
      (+tty-window-display-table window))))


;;; lib.el ends here
