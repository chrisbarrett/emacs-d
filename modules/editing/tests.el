;;; tests.el --- Tests for editing module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the editing module, covering the spec's testable properties.

;;; Code:

(require 'ert)
(require '+corelib)

(defun editing-test--load-module ()
  "Load the editing module files."
  (+load "lib.el")
  (+load "init.el"))

;; P1: erase-buffer is not disabled
(ert-deftest editing-module-test-p1-erase-buffer-enabled ()
  "P1: erase-buffer should not be disabled."
  (editing-test--load-module)
  (should-not (get 'erase-buffer 'disabled)))

;; P2: narrow-to-region is not disabled
(ert-deftest editing-module-test-p2-narrow-to-region-enabled ()
  "P2: narrow-to-region should not be disabled."
  (editing-test--load-module)
  (should-not (get 'narrow-to-region 'disabled)))

;; P3: downcase-region is not disabled
(ert-deftest editing-module-test-p3-downcase-region-enabled ()
  "P3: downcase-region should not be disabled."
  (editing-test--load-module)
  (should-not (get 'downcase-region 'disabled)))

;; Additional tests

;; Auto-revert functions are defined
(ert-deftest editing-module-test-auto-revert-functions-defined ()
  "Auto-revert helper functions should be defined."
  (editing-test--load-module)
  (should (fboundp '+auto-revert-current-buffer-h))
  (should (fboundp '+auto-revert-visible-buffers-h)))

;; minibuffer keybindings
(ert-deftest editing-module-test-minibuffer-c-p ()
  "C-p in minibuffer should be bound to history navigation."
  (editing-test--load-module)
  (should (eq (keymap-lookup minibuffer-local-map "C-p")
              #'previous-line-or-history-element)))

(ert-deftest editing-module-test-minibuffer-c-n ()
  "C-n in minibuffer should be bound to history navigation."
  (editing-test--load-module)
  (should (eq (keymap-lookup minibuffer-local-map "C-n")
              #'next-line-or-history-element)))

;; cursor-intangible-mode hook
(ert-deftest editing-module-test-cursor-intangible-hook ()
  "cursor-intangible-mode should be in minibuffer-setup-hook."
  (editing-test--load-module)
  (should (memq #'cursor-intangible-mode minibuffer-setup-hook)))

;; after-find-file advice
(ert-deftest editing-module-test-after-find-file-advice ()
  "after-find-file should have advice for autosave prompt suppression."
  (editing-test--load-module)
  (should (advice-member-p 'after-find-file@dont-block-on-autosave-exists
                           'after-find-file)))

(provide 'editing-tests)

;;; tests.el ends here
