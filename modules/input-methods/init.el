;;; init.el --- Input methods initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; French postfix input method with smart punctuation handling for French
;; spacing conventions.

;;; Code:

(require 'quail)

(cl-eval-when (compile)
  (require 'org))

;; Default input method settings
(setq default-input-method "french-postfix")
(setq default-transient-input-method default-input-method)

;; Smart punctuation for french-postfix
(with-eval-after-load "quail/latin-post"
  ;; Smart semicolon - French spacing convention
  (+quail-defun "french-postfix" ";"
    (delete-horizontal-space)
    (insert " ; "))

  ;; Smart colon - context-aware French spacing
  (+quail-defun "french-postfix" ":"
    (delete-horizontal-space)
    (let ((left-pad (cond
                     ;; After existing colon - no left padding
                     ((equal (char-before) ?:)
                      "")
                     ;; Org mode list items get spacing
                     ((and (derived-mode-p 'org-mode)
                           (fboundp 'org-at-item-p)
                           (org-at-item-p)
                           (not (org-at-item-description-p)))
                      " ")
                     ;; Default - full spacing
                     (t
                      " "))))
      (insert left-pad ": "))))

(provide 'input-methods-init)

;;; init.el ends here
