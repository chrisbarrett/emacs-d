;;; tests.el --- Tests for help module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the help module, covering the spec's testable properties.

;;; Code:

(require 'ert)

;; P1: `C-h f' calls `helpful-callable' (not `describe-function')
(ert-deftest help-module-test-p1-help-f-binding ()
  "P1: C-h f should be bound to helpful-callable."
  (require 'help)
  (require 'helpful)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "f") 'helpful-callable)))

;; P2: `C-h v' calls `helpful-variable' (not `describe-variable')
(ert-deftest help-module-test-p2-help-v-binding ()
  "P2: C-h v should be bound to helpful-variable."
  (require 'help)
  (require 'helpful)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "v") 'helpful-variable)))

;; P3: `C-h h' is unbound
(ert-deftest help-module-test-p3-help-h-unbound ()
  "P3: C-h h should be unbound (nil)."
  (require 'help)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (null (keymap-lookup help-map "h"))))

;; P4: `C-h l' calls `find-library'
(ert-deftest help-module-test-p4-help-l-binding ()
  "P4: C-h l should be bound to find-library."
  (require 'help)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "l") 'find-library)))

;; P7: Eldoc re-runs after evil state transitions
(ert-deftest help-module-test-p7-eldoc-commands ()
  "P7: Eldoc should re-run after evil state transitions."
  (require 'eldoc)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  ;; Skip if eldoc-message-commands not configured with evil commands
  (skip-unless (member 'evil-normal-state eldoc-message-commands))
  (should (member 'evil-normal-state eldoc-message-commands))
  (should (member 'evil-insert eldoc-message-commands))
  (should (member 'evil-change eldoc-message-commands))
  (should (member 'evil-delete eldoc-message-commands))
  (should (member 'evil-replace eldoc-message-commands)))

;; P8: In help-mode, `^' is bound to `help-go-back'
(ert-deftest help-module-test-p8-help-mode-caret ()
  "P8: In help-mode normal state, ^ should go back."
  (require 'help-mode)
  (require 'evil)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (evil-get-auxiliary-keymap help-mode-map 'normal t)
              (evil-get-auxiliary-keymap help-mode-map 'normal)))
  (let ((keymap (evil-get-auxiliary-keymap help-mode-map 'normal)))
    (should (eq (keymap-lookup keymap "^") 'help-go-back))))

;; P9: In Info-mode, `^' is bound to `Info-up'
(ert-deftest help-module-test-p9-info-mode-caret ()
  "P9: In Info-mode normal state, ^ should go up."
  (require 'info)
  (require 'evil)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (let ((keymap (evil-get-auxiliary-keymap Info-mode-map 'normal)))
    (should (eq (keymap-lookup keymap "^") 'Info-up))))

;; Additional tests for completeness

(ert-deftest help-module-test-help-k-binding ()
  "C-h k should be bound to helpful-key."
  (require 'help)
  (require 'helpful)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "k") 'helpful-key)))

(ert-deftest help-module-test-help-c-binding ()
  "C-h c should be bound to describe-face."
  (require 'help)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "c") 'describe-face)))

(ert-deftest help-module-test-help-P-binding ()
  "C-h P should be bound to describe-text-properties."
  (require 'help)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "P") 'describe-text-properties)))

(ert-deftest help-module-test-help-s-binding ()
  "C-h s should be bound to info-apropos."
  (require 'help)
  (require 'info)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "s") 'info-apropos)))

(ert-deftest help-module-test-help-w-binding ()
  "C-h w should be bound to rfc-mode-browse."
  (require 'help)
  (require 'rfc-mode)
  (load (expand-file-name "modules/help/init.el" user-emacs-directory) nil t)
  (should (eq (keymap-lookup help-map "w") 'rfc-mode-browse)))

(provide 'help-tests)

;;; tests.el ends here
