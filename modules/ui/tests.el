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


;;; P2: Tab switching keybindings

(ert-deftest ui-tab-keybinding-next ()
  "P2: M-> should be bound to tab-bar-switch-to-next-tab."
  (skip-unless (and (featurep 'general)
                    (boundp 'override-global-map)))
  (should (eq (lookup-key (symbol-value 'override-global-map) (kbd "M->"))
              'tab-bar-switch-to-next-tab)))

(ert-deftest ui-tab-keybinding-prev ()
  "P2: M-< should be bound to tab-bar-switch-to-prev-tab."
  (skip-unless (and (featurep 'general)
                    (boundp 'override-global-map)))
  (should (eq (lookup-key (symbol-value 'override-global-map) (kbd "M-<"))
              'tab-bar-switch-to-prev-tab)))


;;; hl-todo keywords

(ert-deftest ui-hl-todo-keywords-defined ()
  "P9: hl-todo-keyword-faces should include TODO and FIXME."
  (skip-unless (boundp 'hl-todo-keyword-faces))
  (should (assoc "TODO" hl-todo-keyword-faces))
  (should (assoc "FIXME" hl-todo-keyword-faces)))


;;; Display buffer rules

;;; Functions from lib.el

(ert-deftest ui-display-buffer-fallback-defined ()
  "Lib: +display-buffer-fallback should be defined."
  (should (fboundp '+display-buffer-fallback)))

(ert-deftest ui-display-buffer-reuse-non-dedicated-defined ()
  "Lib: +display-buffer-reuse-non-dedicated-window should be defined."
  (should (fboundp '+display-buffer-reuse-non-dedicated-window)))

(ert-deftest ui-tty-frame-setup-defined ()
  "Lib: +tty-frame-setup should be defined."
  (should (fboundp '+tty-frame-setup)))

(ert-deftest ui-goto-address-maybe-defined ()
  "Lib: +goto-address-maybe-h should be defined."
  (should (fboundp '+goto-address-maybe-h)))

(ert-deftest ui-pulsar-with-eval-pulse-defined ()
  "Lib: +pulsar--with-eval-pulse should be a macro."
  (should (macrop '+pulsar--with-eval-pulse)))


;;; Tab bar functions

(ert-deftest ui-tab-bar-set-alert-defined ()
  "Tab: +tab-bar-set-alert should be defined."
  (should (fboundp '+tab-bar-set-alert)))

(ert-deftest ui-tab-bar-clear-alert-defined ()
  "Tab: +tab-bar-clear-alert should be defined."
  (should (fboundp '+tab-bar-clear-alert)))

(ert-deftest ui-tab-bar-set-transient-alert-defined ()
  "Tab: +tab-bar-set-transient-alert should be defined."
  (should (fboundp '+tab-bar-set-transient-alert)))

(ert-deftest ui-tabs-menu-defined ()
  "Tab: +tabs-menu should be defined."
  (should (fboundp '+tabs-menu)))

(ert-deftest ui-update-tab-bar-themes-defined ()
  "Tab: +update-tab-bar-themes should be defined."
  (should (fboundp '+update-tab-bar-themes)))


;;; Faces

(ert-deftest ui-tab-bar-tab-alert-face ()
  "Faces: tab-bar-tab-alert should be defined."
  (skip-unless (featurep 'ui-tabs))
  (should (facep 'tab-bar-tab-alert)))

(ert-deftest ui-tab-bar-tab-inactive-alert-face ()
  "Faces: tab-bar-tab-inactive-alert should be defined."
  (skip-unless (featurep 'ui-tabs))
  (should (facep 'tab-bar-tab-inactive-alert)))


(provide 'ui-tests)
;;; tests.el ends here
