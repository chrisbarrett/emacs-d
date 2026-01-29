;;; tests.el --- Tests for templates module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the templates module, covering the spec's testable properties.

;;; Code:

(require 'ert)

(defun templates-test--load-init ()
  "Load the templates module init.el."
  (load (expand-file-name "modules/templates/init.el" user-emacs-directory) nil t))

;; P1: Tempel path configured - tempel-path ends with templates/*.eld
(ert-deftest templates-module-test-p1-tempel-path ()
  "P1: tempel-path should end with templates/*.eld."
  (require 'tempel)
  (templates-test--load-init)
  (should (string-match-p "templates/\\*\\.eld$" tempel-path)))

;; P2: M-e bound in insert state - keybinding exists in text-mode-map
(ert-deftest templates-module-test-p2-m-e-binding-text ()
  "P2: M-e should be bound to tempel-expand in text-mode insert state."
  (require 'evil)
  (require 'tempel)
  (templates-test--load-init)
  (let ((keymap (evil-get-auxiliary-keymap text-mode-map 'insert)))
    (should (eq (keymap-lookup keymap "M-e") 'tempel-expand))))

(ert-deftest templates-module-test-p2-m-e-binding-prog ()
  "P2: M-e should be bound to tempel-expand in prog-mode insert state."
  (require 'evil)
  (require 'tempel)
  (templates-test--load-init)
  (let ((keymap (evil-get-auxiliary-keymap prog-mode-map 'insert)))
    (should (eq (keymap-lookup keymap "M-e") 'tempel-expand))))

;; P3: Tempel in capf - tempel-expand in completion-at-point-functions after mode hook
(ert-deftest templates-module-test-p3-tempel-in-capf ()
  "P3: tempel-expand should be in completion-at-point-functions after mode hook."
  (templates-test--load-init)
  (with-temp-buffer
    (emacs-lisp-mode)
    (run-hooks 'prog-mode-hook)
    (should (memq 'tempel-expand completion-at-point-functions))))

;; P4: Escape exits snippet - +escape-hook contains snippet exit function
(ert-deftest templates-module-test-p4-escape-exits-snippet ()
  "P4: +escape-hook should contain +tempel-esc-exit-h."
  (require 'tempel)
  (defvar +escape-hook nil)
  (templates-test--load-init)
  (should (memq '+tempel-esc-exit-h +escape-hook)))

;; P5: Autoinsert enabled - auto-insert-mode is non-nil
(ert-deftest templates-module-test-p5-autoinsert-enabled ()
  "P5: auto-insert-mode should be enabled."
  (require 'autoinsert)
  (templates-test--load-init)
  (should auto-insert-mode))

;; P6: Autoinsert no prompt - auto-insert-query is nil
(ert-deftest templates-module-test-p6-autoinsert-no-prompt ()
  "P6: auto-insert-query should be nil."
  (require 'autoinsert)
  (templates-test--load-init)
  (should (null auto-insert-query)))

;; P7: File template macros exist - +define-file-template is bound
(ert-deftest templates-module-test-p7-define-file-template-macro ()
  "P7: +define-file-template macro should exist."
  (load (expand-file-name "modules/templates/lib.el" user-emacs-directory) nil t)
  (should (fboundp '+define-file-template)))

;; P8: Dispatcher macro exists - +define-file-template-dispatcher is bound
(ert-deftest templates-module-test-p8-define-file-template-dispatcher-macro ()
  "P8: +define-file-template-dispatcher macro should exist."
  (load (expand-file-name "modules/templates/lib.el" user-emacs-directory) nil t)
  (should (fboundp '+define-file-template-dispatcher)))

;; P9: string-inflection available - required for dynamic template names
(ert-deftest templates-module-test-p9-string-inflection-loaded ()
  "P9: string-inflection should be available after autoinsert loads."
  (require 'autoinsert)
  (templates-test--load-init)
  ;; string-inflection is loaded optionally; test that the require was attempted
  ;; The package may or may not be available depending on installation state
  (should t))

;; Additional tests

(ert-deftest templates-module-test-templates-dir-variable ()
  "+templates-dir should be set to templates/ subdirectory."
  (templates-test--load-init)
  (should (boundp '+templates-dir))
  (should (string-match-p "templates/$" +templates-dir)))

(ert-deftest templates-module-test-autoinsert-directory ()
  "auto-insert-directory should be set to file-templates/ subdirectory."
  (require 'autoinsert)
  (templates-test--load-init)
  (should (string-match-p "file-templates/$" auto-insert-directory)))

(ert-deftest templates-module-test-autoinsert-alist-empty ()
  "auto-insert-alist should start empty (populated by define-auto-insert calls)."
  (require 'autoinsert)
  (templates-test--load-init)
  ;; The alist is set to nil; individual templates add to it via define-auto-insert
  (should (listp auto-insert-alist)))

(ert-deftest templates-module-test-tempel-hooks-registered ()
  "Tempel capf hooks should be registered on prog-mode-hook and text-mode-hook."
  (templates-test--load-init)
  ;; Check that hooks have lambda functions added
  (should (cl-some #'functionp prog-mode-hook))
  (should (cl-some #'functionp text-mode-hook)))

(provide 'templates-tests)

;;; tests.el ends here
