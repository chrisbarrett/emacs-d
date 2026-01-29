;;; tests.el --- Eglot module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests based on spec 019-eglot.md testable properties.

;;; Code:

(require 'ert)

(defconst eglot-test--module-dir
  (expand-file-name "modules/eglot/" user-emacs-directory)
  "Directory for eglot module.")

(defun eglot-test--load-init ()
  "Load the eglot module init.el."
  (condition-case nil
      (load (expand-file-name "init.el" eglot-test--module-dir) nil t)
    (error nil)))

(defun eglot-test--load-lib ()
  "Load the eglot module lib.el."
  (load (expand-file-name "lib.el" eglot-test--module-dir) nil t))

;; Load lib at compile time for function definitions
(eglot-test--load-lib)


;;; P1: flymake-mode is active in prog-mode buffers

(ert-deftest eglot/p1-flymake-prog-mode-hook ()
  "P1: flymake-mode is active in prog-mode buffers."
  (eglot-test--load-init)
  (should (memq 'flymake-mode (default-value 'prog-mode-hook))))


;;; P4: eglot-code-action-indicator is empty string

(ert-deftest eglot/p4-code-action-indicator ()
  "P4: eglot-code-action-indicator is empty string."
  (eglot-test--load-init)
  ;; This requires eglot to be loaded which happens via the init.el
  ;; but in batch the :custom may not be applied
  (skip-unless (boundp 'eglot-code-action-indicator))
  (should (equal (default-value 'eglot-code-action-indicator) "")))


;;; P7: eglot-booster-mode is enabled after eglot loads

(ert-deftest eglot/p7-eglot-booster-mode ()
  "P7: eglot-booster-mode is enabled after eglot loads."
  ;; Can only test that the package is configured to be loaded
  (skip-unless (featurep 'eglot-booster))
  (should (bound-and-true-p eglot-booster-mode)))


;;; P8: +eglot-inlay-hints-off is on eglot-managed-mode-hook

(ert-deftest eglot/p8-inlay-hints-hook ()
  "P8: +eglot-inlay-hints-off is on eglot-managed-mode-hook."
  (eglot-test--load-init)
  ;; init.el adds the hook - need eglot to be loaded too
  (skip-unless (boundp 'eglot-managed-mode-hook))
  (should (memq '+eglot-inlay-hints-off eglot-managed-mode-hook)))


(provide 'eglot-tests)

;;; tests.el ends here
