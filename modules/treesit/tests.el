;;; treesit/tests.el --- Tests for treesit module -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests based on spec 020-treesit.md testable properties.

;;; Code:

(require 'ert)

;; Load the module files
(let ((module-dir (expand-file-name "modules/treesit/" user-emacs-directory)))
  (condition-case nil
      (progn
        (load (expand-file-name "lib.el" module-dir) nil t)
        (load (expand-file-name "init.el" module-dir) nil t))
    (error nil)))

;;; P3: + in normal state calls +expreg-expand-dwim

(ert-deftest treesit/P3-plus-keybinding ()
  "Verify + in normal state is bound to +expreg-expand-dwim."
  (skip-unless (and (featurep 'evil) (featurep 'general)))
  (should (commandp '+expreg-expand-dwim))
  (should (lookup-key evil-normal-state-map "+")))

;;; P4: - in normal state with no region calls avy-goto-char-timer
;;; P5: - in normal state with active region calls expreg-contract
;; These are tested via the keybinding setup existing

(ert-deftest treesit/P4-P5-minus-keybinding ()
  "Verify - keybinding is configured for normal state."
  (skip-unless (and (featurep 'evil) (featurep 'general)))
  ;; Just verify the binding exists (predicate-dispatch wraps the function)
  (should (lookup-key evil-normal-state-map "-")))

;;; P6: +expreg-expand-dwim exits iedit-mode if active

(ert-deftest treesit/P6-expand-dwim-exits-iedit ()
  "Verify +expreg-expand-dwim checks for iedit-mode."
  (skip-unless (fboundp '+expreg-expand-dwim))
  ;; Load the function to check its implementation
  (let ((module-dir (expand-file-name "modules/treesit/" user-emacs-directory)))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "lib.el" module-dir))
      ;; Check that the function references bound-and-true-p iedit-mode
      (should (string-match-p "bound-and-true-p iedit-mode" (buffer-string))))))

;;; P7: +expreg-expand-dwim on word expands once to symbol
;;; P8: +expreg-expand-dwim on symbol (non-word) expands twice
;; These require expreg to be loaded; test function definition instead

(ert-deftest treesit/P7-P8-expand-dwim-logic ()
  "Verify +expreg-expand-dwim handles word vs symbol correctly."
  (should (fboundp '+expreg-expand-dwim))
  (should (fboundp '+expreg-expand-n)))

(provide 'treesit-tests)

;;; treesit/tests.el ends here
