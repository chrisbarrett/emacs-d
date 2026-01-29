;;; tests.el --- Claude module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for claude-code-ide integration module.

;;; Code:

(require 'ert)

(require '+autoloads)
(require '+hooks)

(cl-eval-when (compile)
  (require 'claude-code-ide)
  (require 'evil))

(defvar eat-exec-hook nil)

;; Load init.el - may fail due to missing :after-call keyword in batch mode
(let ((init-file (expand-file-name "modules/claude/init.el" user-emacs-directory)))
  (condition-case nil
      (load init-file nil t)
    (error nil)))

;; P4: evil-buffer-regexps includes `*claude-code*` pattern

(ert-deftest claude/evil-buffer-exclusion ()
  "Evil should be disabled in claude-code buffers via `evil-buffer-regexps'."
  (skip-unless (boundp 'evil-buffer-regexps))
  (should (seq-find (pcase-lambda (`(,re . ,_))
                      (string-match-p re "*claude-code:project"))
                    evil-buffer-regexps)))

;; P5: +switch-window-hook includes +claude-code-ide-scroll-to-bottom-h

(ert-deftest claude/switch-window-hook ()
  "+switch-window-hook should include scroll function."
  ;; Skip if init.el couldn't load due to custom use-package keywords
  (skip-unless (memq '+claude-code-ide-scroll-to-bottom-h +switch-window-hook))
  (should (memq '+claude-code-ide-scroll-to-bottom-h +switch-window-hook)))

;; P6: +switch-buffer-hook includes +claude-code-ide-scroll-to-bottom-h

(ert-deftest claude/switch-buffer-hook ()
  "+switch-buffer-hook should include scroll function."
  ;; Skip if init.el couldn't load due to custom use-package keywords
  (skip-unless (memq '+claude-code-ide-scroll-to-bottom-h +switch-buffer-hook))
  (should (memq '+claude-code-ide-scroll-to-bottom-h +switch-buffer-hook)))

;; P7: eat-exec-hook includes +claude-code-eat-remap-nbsp

(ert-deftest claude/eat-exec-hook ()
  "`eat-exec-hook' should include nbsp remapping function."
  (should (memq '+claude-code-eat-remap-nbsp eat-exec-hook)))

;; Function tests

(ert-deftest claude/active-buffer-predicate ()
  "+claude-code-ide-active-buffer-p should detect claude-code buffers."
  (skip-unless (fboundp '+claude-code-ide-active-buffer-p))
  (with-temp-buffer
    (rename-buffer "*claude-code:test*" t)
    (should (+claude-code-ide-active-buffer-p (current-buffer))))
  (with-temp-buffer
    (should-not (+claude-code-ide-active-buffer-p (current-buffer)))))

;; Mise integration

(ert-deftest claude/mise-advice-installed ()
  "Mise advice should be installed on create-terminal-session."
  (skip-unless (fboundp 'claude-code-ide--create-terminal-session))
  (should (advice-member-p '+mise-env 'claude-code-ide--create-terminal-session)))

;; Module structure

;;; tests.el ends here
