;;; lib.el --- Eglot module library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for eglot module.

;;; Code:

;;;###autoload
(defun +eglot-decode-html-entities (string)
  "Decode common HTML entities in STRING."
  (when string
    (thread-last string
      (string-replace "&amp;" "&")
      (string-replace "&lt;" "<")
      (string-replace "&gt;" ">")
      (string-replace "&quot;" "\"")
      (string-replace "&#39;" "'")
      (string-replace "&apos;" "'"))))

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



;;; lib.el ends here
