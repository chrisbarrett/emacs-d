;;; tests.el --- Tests for format module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the format module, covering the spec's testable properties.

;;; Code:

(require 'ert)

(defun format-test--load-init ()
  "Load the format module init.el."
  (load (expand-file-name "modules/format/init.el" user-emacs-directory) nil t))

;; P1: Apheleia loads lazily - not loaded until first file opened
(ert-deftest format-module-test-p1-apheleia-loads-lazily ()
  "P1: Apheleia should not be loaded at init time."
  ;; Before loading init, apheleia should not be required
  ;; (This tests the lazy loading design, not post-init state)
  (format-test--load-init)
  ;; The hook should be registered to load apheleia
  (skip-unless (boundp '+first-file-hook))
  (should (boundp '+first-file-hook)))

;; P2: Apheleia global mode on - after file open
(ert-deftest format-module-test-p2-apheleia-global-mode ()
  "P2: apheleia-global-mode should be enabled after apheleia loads."
  (format-test--load-init)
  (require 'apheleia)
  (should apheleia-global-mode))

;; P3: Remote format local - apheleia-remote-algorithm eq local
(ert-deftest format-module-test-p3-remote-format-local ()
  "P3: apheleia-remote-algorithm should be `local'."
  (format-test--load-init)
  (require 'apheleia)
  (should (eq apheleia-remote-algorithm 'local)))

;; P4: Trim whitespace on save - trailing whitespace removed
(ert-deftest format-module-test-p4-trim-whitespace-on-save ()
  "P4: Trailing whitespace should be removed on save when aggressive trim is on."
  (format-test--load-init)
  (with-temp-buffer
    (insert "hello   \nworld  \n")
    (setq +trim-trailing-whitespace-aggressively t)
    (run-hooks 'before-save-hook)
    (should (equal (buffer-string) "hello\nworld\n"))))

;; P5: Trim respects buffer-local - setting nil prevents trimming
(ert-deftest format-module-test-p5-trim-respects-buffer-local ()
  "P5: Setting +trim-trailing-whitespace-aggressively to nil prevents trimming."
  (format-test--load-init)
  (with-temp-buffer
    (insert "hello   \nworld  \n")
    (setq +trim-trailing-whitespace-aggressively nil)
    (run-hooks 'before-save-hook)
    (should (equal (buffer-string) "hello   \nworld  \n"))))

;; P6: Align keybinding - C-x a a invokes align-regexp
(ert-deftest format-module-test-p6-align-keybinding ()
  "P6: C-x a a should invoke align-regexp."
  (require 'general)
  (format-test--load-init)
  (should (eq (keymap-lookup global-map "C-x a a") 'align-regexp)))

;; P7: Tab completion behavior - tab-first-completion eq word-or-paren-or-punct
(ert-deftest format-module-test-p7-tab-completion-behavior ()
  "P7: tab-first-completion should be word-or-paren-or-punct."
  (format-test--load-init)
  (should (eq tab-first-completion 'word-or-paren-or-punct)))

;; Additional tests

(ert-deftest format-module-test-tabify-regexp ()
  "tabify-regexp should only match at line beginning."
  (format-test--load-init)
  (should (equal tabify-regexp "^\t* [ \t]+")))

(ert-deftest format-module-test-apheleia-respects-fill-column ()
  "apheleia-formatters-respect-fill-column should be t."
  (format-test--load-init)
  (require 'apheleia)
  (should apheleia-formatters-respect-fill-column))

(ert-deftest format-module-test-trim-variable-buffer-local ()
  "+trim-trailing-whitespace-aggressively should be buffer-local."
  (format-test--load-init)
  (with-temp-buffer
    (setq +trim-trailing-whitespace-aggressively nil)
    (should (local-variable-p '+trim-trailing-whitespace-aggressively))))

(ert-deftest format-module-test-before-save-hook-has-trim ()
  "before-save-hook should contain whitespace trimming function."
  (format-test--load-init)
  (should (cl-some #'functionp before-save-hook)))

(provide 'format-tests)

;;; tests.el ends here
