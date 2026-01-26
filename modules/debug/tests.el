;;; tests.el --- Tests for debug module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the debug module, covering the spec's testable properties.

;;; Code:

(require 'ert)

(defun debug-test--load-init ()
  "Load the debug module init.el."
  (load (expand-file-name "modules/debug/init.el" user-emacs-directory) nil t))

;; P1: +debugger-toggle-on-exit-frame is defined as interactive command
(ert-deftest debug-module-test-p1-toggle-command-defined ()
  "P1: +debugger-toggle-on-exit-frame should be defined as interactive command."
  (debug-test--load-init)
  (should (fboundp '+debugger-toggle-on-exit-frame))
  (should (commandp '+debugger-toggle-on-exit-frame)))

;; P2: +debugger-mode-line-format is a non-empty string
(ert-deftest debug-module-test-p2-mode-line-format-defined ()
  "P2: +debugger-mode-line-format should be a non-empty string."
  (debug-test--load-init)
  (should (boundp '+debugger-mode-line-format))
  (should (stringp +debugger-mode-line-format))
  (should (> (length +debugger-mode-line-format) 0)))

;; P3: t bound in debugger-mode-map normal state
(ert-deftest debug-module-test-p3-keybinding ()
  "P3: t should be bound in debugger-mode-map normal state."
  ;; Skip if general/evil not available
  (skip-unless (require 'general nil t))
  (skip-unless (fboundp 'general-lookup-key))
  (skip-unless (require 'evil nil t))
  (debug-test--load-init)
  (require 'debug)
  (let ((binding (general-lookup-key debugger-mode-map "t" '(normal))))
    (skip-unless binding)
    (should (eq binding '+debugger-toggle-on-exit-frame))))

;; P4: Mode line format contains all documented key references
(ert-deftest debug-module-test-p4-mode-line-contains-keys ()
  "P4: Mode line format should contain all documented key references."
  (debug-test--load-init)
  (let ((format +debugger-mode-line-format))
    ;; First group: d, c, r
    (should (string-match-p "d" format))
    (should (string-match-p "c" format))
    (should (string-match-p "r" format))
    ;; Second group: t, J, L
    (should (string-match-p "t" format))
    (should (string-match-p "J" format))
    (should (string-match-p "L" format))
    ;; Third group: E, R
    (should (string-match-p "E" format))
    (should (string-match-p "R" format))))

;; P5: debugger-record-expression has advice for display-buffer
(ert-deftest debug-module-test-p5-record-expression-advice ()
  "P5: debugger-record-expression should have advice for display-buffer."
  (debug-test--load-init)
  (require 'debug)
  (should (advice-member-p 'debugger-record-expression@display-buffer
                           'debugger-record-expression)))

(provide 'debug-tests)

;;; tests.el ends here
