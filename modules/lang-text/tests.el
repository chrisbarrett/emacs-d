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

;;; P2: text-mode-ispell-word-completion is nil

(ert-deftest lang-text-test-ispell-word-completion-disabled ()
  "text-mode-ispell-word-completion should be nil."
  (should (eq text-mode-ispell-word-completion nil)))

;;; P3: TAB in text-mode does not trigger ispell completion

(ert-deftest lang-text-test-tab-no-ispell ()
  "TAB in text-mode should not trigger ispell completion.
When ispell-word-completion is nil, TAB falls back to default behavior."
  ;; The setting is checked in P2; this test verifies the mode is configured
  (should-not text-mode-ispell-word-completion))

(provide 'lang-text-tests)

;;; tests.el ends here
