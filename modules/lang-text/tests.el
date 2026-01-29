;;; tests.el --- lang-text module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for lang-text module based on spec testable properties.

;;; Code:

(require 'ert)

;; Load module init at top level
(condition-case nil
    (load (expand-file-name "init.el" (file-name-directory
                                       (or load-file-name buffer-file-name))))
  (error nil))

;;; P1: /LICENSE files open in text-mode

(ert-deftest lang-text-test-license-auto-mode ()
  "LICENSE files should open in text-mode."
  (let ((entry (assoc "/LICENSE\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'text-mode))))

(provide 'lang-text-tests)

;;; tests.el ends here
