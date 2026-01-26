;;; lib.el --- Eglot module library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for eglot module.

;;; Code:

;;;###autoload
(defun +eglot-open-link ()
  "Open URL at point in eldoc buffer.
Looks for `help-echo' text property to find URL."
  (interactive)
  (if-let* ((url (get-text-property (point) 'help-echo)))
      (browse-url url)
    (user-error "No URL at point")))

;;;###autoload
(defun +eglot-inlay-hints-off ()
  "Disable inlay hints mode.
Intended for `eglot-managed-mode-hook'."
  (eglot-inlay-hints-mode -1))

(provide 'eglot-lib)

;;; lib.el ends here
