;;; tests.el --- Tests for ui module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the ui module based on spec 009-ui.md testable properties.

;;; Code:

(require 'ert)

;; Get module directory
(defvar ui-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the ui module.")

;; Load lib.el to get autoloaded functions
(let ((lib-file (expand-file-name "lib.el" ui-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil 'nomessage)))

;; Load init.el for settings
(let ((init-file (expand-file-name "init.el" ui-test--module-dir)))
  (condition-case nil
      (load init-file nil t)
    (error nil)))


;;; P1: Tab bar mode is active after init

(ert-deftest ui-tab-bar-mode-active ()
  "P1: Tab bar mode should be active."
  ;; Skip in batch mode - tab-bar-mode is only enabled in interactive sessions
  (skip-unless (not noninteractive))
  (skip-unless (boundp 'tab-bar-mode))
  (should tab-bar-mode))


;;; hl-todo keywords

(ert-deftest ui-hl-todo-keywords-defined ()
  "P9: hl-todo-keyword-faces should include TODO and FIXME."
  (skip-unless (boundp 'hl-todo-keyword-faces))
  (should (assoc "TODO" hl-todo-keyword-faces))
  (should (assoc "FIXME" hl-todo-keyword-faces)))


(provide 'ui-tests)
;;; tests.el ends here
