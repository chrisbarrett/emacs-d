;;; tests.el --- Tests for shells module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for shell and terminal emulator configuration.
;; Tests P1-P9 from spec.md.

;;; Code:

(require 'ert)

;; Capture module directory at load-time
(defvar shells--test-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "init.el" shells--test-dir))
(load (expand-file-name "lib.el" shells--test-dir))
;; Load evil module's lib for `+evil-collection-disabled-list'.
(load (expand-file-name "modules/evil/lib.el" user-emacs-directory))
(require 'eat)
(require 'evil)


;;; P6: eshell/cd advice updates zoxide database

(ert-deftest shells/p6-eshell-cd-advice ()
  "P6: eshell/cd has zoxide update advice."
  ;; Load em-dirs to ensure advice is applied
  (require 'em-dirs)
  (should (advice-member-p 'eshell/cd@update-zoxide 'eshell/cd)))


;;; P8: Evil is disabled in eat buffers

(ert-deftest shells/p8-evil-buffer-regexps ()
  "P8: Evil buffer regexps include eat pattern."
  (should (cl-some (lambda (entry)
                     (string-match-p "\\*eat" (car entry)))
                   evil-buffer-regexps)))


;;; P9: +evil-collection-disabled-list includes eat

(ert-deftest shells/p9-evil-collection-disabled ()
  "P9: +evil-collection-disabled-list includes eat."
  (should (memq 'eat +evil-collection-disabled-list)))


(provide 'shells-tests)

;;; tests.el ends here
