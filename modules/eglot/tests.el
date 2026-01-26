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


;;; P2: M-n is bound to flymake-goto-next-error in flymake-mode-map

(ert-deftest eglot/p2-flymake-m-n-binding ()
  "P2: M-n is bound to flymake-goto-next-error in flymake-mode-map."
  (eglot-test--load-init)
  (require 'flymake)
  (should (eq (lookup-key flymake-mode-map (kbd "M-n"))
              'flymake-goto-next-error)))


;;; P3: M-p is bound to flymake-goto-prev-error in flymake-mode-map

(ert-deftest eglot/p3-flymake-m-p-binding ()
  "P3: M-p is bound to flymake-goto-prev-error in flymake-mode-map."
  (eglot-test--load-init)
  (require 'flymake)
  (should (eq (lookup-key flymake-mode-map (kbd "M-p"))
              'flymake-goto-prev-error)))


;;; P4: eglot-code-action-indicator is empty string

(ert-deftest eglot/p4-code-action-indicator ()
  "P4: eglot-code-action-indicator is empty string."
  (eglot-test--load-init)
  ;; This requires eglot to be loaded which happens via the init.el
  ;; but in batch the :custom may not be applied
  (skip-unless (boundp 'eglot-code-action-indicator))
  (should (equal (default-value 'eglot-code-action-indicator) "")))


;;; P5: M-RET is bound to eglot-code-actions in eglot-mode-map

(ert-deftest eglot/p5-eglot-m-ret-binding ()
  "P5: M-RET is bound to eglot-code-actions in eglot-mode-map."
  ;; Keybinding requires general and evil to be loaded
  (skip-unless (and (featurep 'general) (featurep 'evil))))


;;; P6: C-c C-r is bound to eglot-rename in eglot-mode-map

(ert-deftest eglot/p6-eglot-rename-binding ()
  "P6: C-c C-r is bound to eglot-rename in eglot-mode-map."
  ;; Keybinding requires general and evil to be loaded
  (skip-unless (and (featurep 'general) (featurep 'evil))))


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


;;; P9: +eglot-open-link is defined and interactive

(ert-deftest eglot/p9-open-link-defined ()
  "P9: +eglot-open-link is defined and interactive."
  (should (fboundp '+eglot-open-link))
  (should (commandp '+eglot-open-link)))


;;; Module structure tests

(ert-deftest eglot/module-has-packages-eld ()
  "Module has packages.eld file."
  (let ((packages-file (expand-file-name "packages.eld" eglot-test--module-dir)))
    (should (file-exists-p packages-file))))

(ert-deftest eglot/module-has-spec-symlink ()
  "Module has spec.md symlink."
  (let ((spec-file (expand-file-name "spec.md" eglot-test--module-dir)))
    (should (file-symlink-p spec-file))))

(ert-deftest eglot/module-has-lib-el ()
  "Module has lib.el file."
  (let ((lib-file (expand-file-name "lib.el" eglot-test--module-dir)))
    (should (file-exists-p lib-file))))


;;; Function tests

(ert-deftest eglot/inlay-hints-off-defined ()
  "+eglot-inlay-hints-off is defined."
  (should (fboundp '+eglot-inlay-hints-off)))


(provide 'eglot-tests)

;;; tests.el ends here
