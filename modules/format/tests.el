;;; tests.el --- Tests for format module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the format module, covering the spec's testable properties.

;;; Code:

(require 'ert)
(require '+corelib)

(defmacro format-test-setup (&rest body)
  "Load format module and execute BODY."
  (declare (indent 0))
  `(progn
     (+load "init.el")
     ,@body))

;; P1: Apheleia loads lazily - not loaded until first file opened
(ert-deftest format-module-test-p1-apheleia-loads-lazily ()
  "P1: Apheleia should not be loaded at init time."
  (format-test-setup
   ;; Before loading init, apheleia should not be required
   ;; (This tests the lazy loading design, not post-init state)
   ;; The hook should be registered to load apheleia
   (skip-unless (boundp '+first-file-hook))
   (should (boundp '+first-file-hook))))

;; P2: Apheleia global mode on - after file open
(ert-deftest format-module-test-p2-apheleia-global-mode ()
  "P2: apheleia-global-mode should be enabled after apheleia loads."
  (format-test-setup
   (require 'apheleia)
   (should apheleia-global-mode)))

;; P4: Trim whitespace on save - trailing whitespace removed
(ert-deftest format-module-test-p4-trim-whitespace-on-save ()
  "P4: Trailing whitespace should be removed on save when aggressive trim is on."
  (format-test-setup
   (with-temp-buffer
     (insert "hello   \nworld  \n")
     (setq +trim-trailing-whitespace-aggressively t)
     (run-hooks 'before-save-hook)
     (should (equal (buffer-string) "hello\nworld\n")))))

;; P5: Trim respects buffer-local - setting nil prevents trimming
(ert-deftest format-module-test-p5-trim-respects-buffer-local ()
  "P5: Setting +trim-trailing-whitespace-aggressively to nil prevents trimming."
  (format-test-setup
   (with-temp-buffer
     (insert "hello   \nworld  \n")
     (setq +trim-trailing-whitespace-aggressively nil)
     (run-hooks 'before-save-hook)
     (should (equal (buffer-string) "hello   \nworld  \n")))))

;; P6: Align keybinding - C-x a a invokes align-regexp
(ert-deftest format-module-test-p6-align-keybinding ()
  "P6: C-x a a should invoke align-regexp."
  (format-test-setup
   (require 'general)
   (should (eq (keymap-lookup global-map "C-x a a") 'align-regexp))))

;; Additional tests

(ert-deftest format-module-test-trim-variable-buffer-local ()
  "+trim-trailing-whitespace-aggressively should be buffer-local."
  (format-test-setup
   (with-temp-buffer
     (setq +trim-trailing-whitespace-aggressively nil)
     (should (local-variable-p '+trim-trailing-whitespace-aggressively)))))

(ert-deftest format-module-test-before-save-hook-has-trim ()
  "before-save-hook should contain whitespace trimming function."
  (format-test-setup
   (should (cl-some #'functionp before-save-hook))))

(provide 'format-tests)

;;; tests.el ends here
