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

;; Function tests

(ert-deftest claude/active-buffer-predicate ()
  "+claude-code-ide-active-buffer-p should detect claude-code buffers."
  (skip-unless (fboundp '+claude-code-ide-active-buffer-p))
  (with-temp-buffer
    (rename-buffer "*claude-code:test*" t)
    (should (+claude-code-ide-active-buffer-p (current-buffer))))
  (with-temp-buffer
    (should-not (+claude-code-ide-active-buffer-p (current-buffer)))))

;;; tests.el ends here
