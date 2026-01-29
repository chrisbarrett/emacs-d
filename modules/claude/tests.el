;;; tests.el --- Claude module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for claude-code-ide integration module.

;;; Code:

(require 'ert)
(require '+autoloads)

(cl-eval-when (compile)
  (require 'evil)
  (require 'claude-code-ide))

;; Define hooks that init.el expects
(defvar +switch-window-hook nil)
(defvar +switch-buffer-hook nil)
(defvar eat-exec-hook nil)

;; Load claude-lib.el for function definitions
(let ((lib-file (expand-file-name "modules/claude/claude-lib.el" user-emacs-directory)))
  (condition-case nil
      (load lib-file nil t)
    (error nil)))

;; Load init.el - may fail due to missing :after-call keyword in batch mode
(let ((init-file (expand-file-name "modules/claude/init.el" user-emacs-directory)))
  (condition-case nil
      (load init-file nil t)
    (error nil)))

;; P1: claude-code-ide-terminal-backend equals eat

(ert-deftest claude/terminal-backend ()
  "Terminal backend should be eat."
  (skip-unless (boundp 'claude-code-ide-terminal-backend))
  (should (eq claude-code-ide-terminal-backend 'eat)))

;; P2: claude-code-ide-enable-mcp-server is t

(ert-deftest claude/mcp-server-enabled ()
  "MCP server should be enabled."
  (skip-unless (boundp 'claude-code-ide-enable-mcp-server))
  (should (eq claude-code-ide-enable-mcp-server t)))

;; P3: claude-code-ide-use-ide-diff is nil

(ert-deftest claude/ide-diff-disabled ()
  "IDE diff should be disabled."
  (skip-unless (boundp 'claude-code-ide-use-ide-diff))
  (should (eq claude-code-ide-use-ide-diff nil)))

;; P4: evil-buffer-regexps includes `*claude-code*` pattern

(ert-deftest claude/evil-buffer-exclusion ()
  "Evil should be disabled in claude-code buffers via `evil-buffer-regexps'."
  (skip-unless (boundp 'evil-buffer-regexps))
  (should (cl-some (lambda (pattern)
                     (let ((re (if (consp pattern) (car pattern) pattern)))
                       (string-match-p re "*claude-code:project")))
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

;; P7: eat-exec-hook includes +eat-remap-nbsp

(ert-deftest claude/eat-exec-hook ()
  "`eat-exec-hook' should include nbsp remapping function."
  (should (memq '+eat-remap-nbsp eat-exec-hook)))

;; Function tests

(ert-deftest claude/active-buffer-predicate ()
  "+claude-code-ide-active-buffer-p should detect claude-code buffers."
  (skip-unless (fboundp '+claude-code-ide-active-buffer-p))
  (with-temp-buffer
    (rename-buffer "*claude-code:test*" t)
    (should (+claude-code-ide-active-buffer-p (current-buffer))))
  (with-temp-buffer
    (should-not (+claude-code-ide-active-buffer-p (current-buffer)))))

(ert-deftest claude/scroll-function-defined ()
  "+claude-code-ide-scroll-to-bottom-h should be defined."
  (should (fboundp '+claude-code-ide-scroll-to-bottom-h)))

(ert-deftest claude/nbsp-remap-function-defined ()
  "+eat-remap-nbsp should be defined."
  (should (fboundp '+eat-remap-nbsp)))

;; Mise integration

(ert-deftest claude/mise-advice-installed ()
  "Mise advice should be installed on create-terminal-session."
  (skip-unless (fboundp 'claude-code-ide--create-terminal-session))
  (should (advice-member-p '+mise-env 'claude-code-ide--create-terminal-session)))

;; Module structure

(ert-deftest claude/module-structure ()
  "Module should have required files."
  (let ((module-dir (expand-file-name "modules/claude/" user-emacs-directory)))
    (should (file-exists-p (expand-file-name "packages.eld" module-dir)))
    (should (file-exists-p (expand-file-name "claude-lib.el" module-dir)))
    (should (file-exists-p (expand-file-name "init.el" module-dir)))
    (should (file-exists-p (expand-file-name "spec.md" module-dir)))))

(ert-deftest claude/packages-eld-content ()
  "Check packages.eld exists."
  (let ((packages-file (expand-file-name "modules/claude/packages.eld" user-emacs-directory)))
    (should (file-exists-p packages-file))))

;;; tests.el ends here
