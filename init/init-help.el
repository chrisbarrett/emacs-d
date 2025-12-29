;;; init-help.el --- Help & documentation systems -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; Defines the main help functionality for Emacs & Emacs-Lisp.
(use-package help
  :hook (help-mode-hook . turn-on-visual-line-mode)
  :custom
  (help-window-select t)
  :general
  (:keymaps 'help-map
            "h" nil ; view-hello-file: never intended, always annoying
            "l" #'find-library
            "c" #'describe-face
            "P" #'describe-text-properties))


;; Major-mode for help buffers.
(use-package help-mode
  :general-config
  (:keymaps 'help-mode-map :states 'normal
            "^" #'help-go-back
            "M-n" #'forward-button
            "M-p" #'backward-button
            "C-n" #'forward-button
            "C-p" #'backward-button))


;; Extended help system, showing definitions etc. in the help buffer.
(use-package helpful :ensure t
  :general (:keymaps 'help-map
                     "f" #'helpful-callable
                     "v" #'helpful-variable
                     "k" #'helpful-key))


;; Display help hints in the echo area as you move around.
(use-package eldoc
  :config
  ;; Teach eldoc to re-run after these commands.
  (eldoc-add-command 'evil-normal-state
                     'evil-insert
                     'evil-change
                     'evil-delete
                     'evil-replace))


;; Emacs' built-in system for reading texinfo manuals.
(use-package info
  :general
  (:keymaps 'help-map "s" #'info-apropos)
  :general-config
  (:keymaps 'Info-mode-map :states 'normal "^" #'Info-up
            "C-n" #'Info-forward-node
            "C-p" #'Info-backward-node))


;; Built-in manpage reader.
(use-package man
  :custom
  ;; Tell man to use pop-to-buffer under the hood, which uses display-buffer and
  ;; selects the window.
  (Man-notify-method 'aggressive))


;; Emacs' built-in navigator for points of interest in a buffer.
(use-package imenu
  :general-config
  (:keymaps 'Info-mode-map [remap consult-imenu] #'Info-menu))


;; Interface for browsing RFC documents.
(use-package rfc-mode :ensure t
  :general (:keymaps 'help-map "w" #'rfc-mode-browse)
  :custom
  (rfc-mode-directory "~/.cache/emacs/rfc-mode/rfcs/")
  :config
  ;; Fix the default face, which doesn't allow highlights (e.g. in vertico).
  (custom-theme-set-faces 'user
                          '(rfc-mode-browser-title-face ((t (:inherit bold)))))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'rfc-mode 'motion)))


(provide 'init-help)

;;; init-help.el ends here
