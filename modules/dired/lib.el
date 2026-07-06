;;; lib.el --- Dired library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for the dired module, notably the eldoc
;; integration that surfaces the ls detail columns concealed by
;; `dired-hide-details-mode'.

;;; Code:

(require 'dired)

;;;###autoload
(defun +dired-details-at-point ()
  "Return the ls detail columns for the dired file line at point.
These are the permission, link-count, owner, group, size and
timestamp fields that precede the file name — the same text
`dired-hide-details-mode' conceals.  Font-lock faces (e.g. those
from diredfl) are preserved so the echo area shows them, but the
`invisible' property is stripped so the text is not hidden there.
Return nil when point is not on a file line."
  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))
      (when (dired-move-to-filename)
        (let ((details (string-trim (buffer-substring bol (point)))))
          (unless (string-empty-p details)
            (remove-text-properties 0 (length details) '(invisible nil) details)
            details))))))

;;;###autoload
(defun +dired-hide-details-eldoc-function (callback &rest _)
  "Report the concealed file details for the dired line at point.
Intended for `eldoc-documentation-functions'.  Only reports while
`dired-hide-details-mode' is active, so the hidden ls columns
surface in the echo area.  CALLBACK is the eldoc documentation
callback.  Return nil off a file line so other providers run."
  (ignore callback)
  (when (and (derived-mode-p 'dired-mode)
             (bound-and-true-p dired-hide-details-mode))
    (+dired-details-at-point)))

;;;###autoload
(defun +dired-enable-details-eldoc ()
  "Enable eldoc display of concealed dired details in this buffer.
Registers `+dired-hide-details-eldoc-function' on
`eldoc-documentation-functions' and turns on `eldoc-mode'.

Dired's own point-movement commands are not eldoc-triggering by
default, so also register them; otherwise eldoc never refreshes as
point moves between files."
  (eldoc-add-command 'dired-next-line
                     'dired-previous-line
                     'dired-next-dirline
                     'dired-prev-dirline
                     'dired-goto-file
                     'dired-jump)
  (add-hook 'eldoc-documentation-functions
            #'+dired-hide-details-eldoc-function nil t)
  (eldoc-mode 1))

;;; lib.el ends here
