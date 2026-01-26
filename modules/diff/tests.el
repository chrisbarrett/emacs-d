;;; tests.el --- Tests for diff module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the diff module, covering the spec's testable properties.

;;; Code:

(require 'ert)

(defun diff-test--load-init ()
  "Load the diff module init.el."
  (load (expand-file-name "modules/diff/init.el" user-emacs-directory) nil t))

;; P1: diff-default-read-only is t
(ert-deftest diff-module-test-p1-diff-default-read-only ()
  "P1: diff-default-read-only should be t."
  (diff-test--load-init)
  (require 'diff-mode)
  (should (eq diff-default-read-only t)))

;; P2: diff-font-lock-syntax is hunk-also
(ert-deftest diff-module-test-p2-diff-font-lock-syntax ()
  "P2: diff-font-lock-syntax should be `hunk-also'."
  (diff-test--load-init)
  (require 'diff-mode)
  (should (eq diff-font-lock-syntax 'hunk-also)))

;; P3: ediff-diff-options contains -w
(ert-deftest diff-module-test-p3-ediff-diff-options ()
  "P3: ediff-diff-options should contain -w."
  (diff-test--load-init)
  (require 'ediff)
  (should (string-match-p "-w" ediff-diff-options)))

;; P4: ediff-split-window-function is split-window-horizontally
(ert-deftest diff-module-test-p4-ediff-split-window-function ()
  "P4: ediff-split-window-function should be `split-window-horizontally'."
  (diff-test--load-init)
  (require 'ediff)
  (should (eq ediff-split-window-function #'split-window-horizontally)))

;; P5: ediff-window-setup-function is ediff-setup-windows-plain
(ert-deftest diff-module-test-p5-ediff-window-setup-function ()
  "P5: ediff-window-setup-function should be `ediff-setup-windows-plain'."
  (diff-test--load-init)
  (require 'ediff)
  (should (eq ediff-window-setup-function #'ediff-setup-windows-plain)))

;; P6: ediff-show-clashes-only is t
(ert-deftest diff-module-test-p6-ediff-show-clashes-only ()
  "P6: ediff-show-clashes-only should be t."
  (diff-test--load-init)
  (require 'ediff)
  (should (eq ediff-show-clashes-only t)))

;; P7: ediff-next-difference has advice
(ert-deftest diff-module-test-p7-ediff-next-difference-advice ()
  "P7: ediff-next-difference should have org reveal advice."
  (diff-test--load-init)
  (require 'ediff)
  (should (advice-member-p #'+ad-ediff-reveal-org-content-around-hunk 'ediff-next-difference)))

;; Additional tests

(ert-deftest diff-module-test-diff-advance-after-apply-hunk ()
  "diff-advance-after-apply-hunk should be t."
  (diff-test--load-init)
  (require 'diff-mode)
  (should (eq diff-advance-after-apply-hunk t)))

(ert-deftest diff-module-test-diff-font-lock-prettify ()
  "diff-font-lock-prettify should be t."
  (diff-test--load-init)
  (require 'diff-mode)
  (should (eq diff-font-lock-prettify t)))

(ert-deftest diff-module-test-ediff-previous-difference-advice ()
  "ediff-previous-difference should have org reveal advice."
  (diff-test--load-init)
  (require 'ediff)
  (should (advice-member-p #'+ad-ediff-reveal-org-content-around-hunk 'ediff-previous-difference)))

(ert-deftest diff-module-test-advice-function-defined ()
  "+ad-ediff-reveal-org-content-around-hunk should be defined."
  (diff-test--load-init)
  (should (fboundp '+ad-ediff-reveal-org-content-around-hunk)))

(provide 'diff-tests)

;;; tests.el ends here
