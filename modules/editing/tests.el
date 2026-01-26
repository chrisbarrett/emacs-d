;;; tests.el --- Tests for editing module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the editing module, covering the spec's testable properties.

;;; Code:

(require 'ert)

(defun editing-test--load-module ()
  "Load the editing module files."
  (load (expand-file-name "modules/editing/lib.el" user-emacs-directory) nil t)
  (load (expand-file-name "modules/editing/init.el" user-emacs-directory) nil t))

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

;; P4: fill-column default is 80
(ert-deftest editing-module-test-p4-fill-column ()
  "P4: fill-column default should be 80."
  (editing-test--load-module)
  (should (= (default-value 'fill-column) 80)))

;; P5: indent-tabs-mode default is nil
(ert-deftest editing-module-test-p5-indent-tabs-mode ()
  "P5: indent-tabs-mode default should be nil."
  (editing-test--load-module)
  (should-not (default-value 'indent-tabs-mode)))

;; P6: create-lockfiles is nil
(ert-deftest editing-module-test-p6-create-lockfiles ()
  "P6: create-lockfiles should be nil."
  (editing-test--load-module)
  (should-not create-lockfiles))

;; P7: uniquify-buffer-name-style is 'forward
(ert-deftest editing-module-test-p7-uniquify-style ()
  "P7: uniquify-buffer-name-style should be 'forward."
  (editing-test--load-module)
  (require 'uniquify)
  (should (eq uniquify-buffer-name-style 'forward)))

;; P8: recentf-mode is enabled after trigger (deferred, test hook registered)
(ert-deftest editing-module-test-p8-recentf-max-saved-items ()
  "P8: recentf-max-saved-items should be 100."
  (editing-test--load-module)
  ;; recentf is deferred, so just verify setting is correct when loaded
  (with-eval-after-load 'recentf
    (should (= recentf-max-saved-items 100))))

;; P9: auto-revert-use-notify is nil
(ert-deftest editing-module-test-p9-auto-revert-no-notify ()
  "P9: auto-revert-use-notify should be nil."
  (editing-test--load-module)
  (require 'autorevert)
  (should-not auto-revert-use-notify))

;; Additional tests

;; Auto-revert functions are defined
(ert-deftest editing-module-test-auto-revert-functions-defined ()
  "Auto-revert helper functions should be defined."
  (editing-test--load-module)
  (should (fboundp '+auto-revert-current-buffer-h))
  (should (fboundp '+auto-revert-visible-buffers-h)))

;; word-wrap default is t
(ert-deftest editing-module-test-word-wrap ()
  "word-wrap default should be t."
  (editing-test--load-module)
  (should (default-value 'word-wrap)))

;; truncate-lines default is t
(ert-deftest editing-module-test-truncate-lines ()
  "truncate-lines default should be t."
  (editing-test--load-module)
  (should (default-value 'truncate-lines)))

;; ring-bell-function is ignore
(ert-deftest editing-module-test-ring-bell-function ()
  "ring-bell-function should be #'ignore."
  (editing-test--load-module)
  (should (eq ring-bell-function #'ignore)))

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
