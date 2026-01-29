;;; init.el --- Help module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures help, helpful, eldoc, info, man, and rfc-mode.
;; Provides vim-style keybindings and enhanced help buffers.

;;; Code:

(require '+autoloads)

;; Help: main help functionality for Emacs & Emacs-Lisp.
(with-eval-after-load 'help
  (setq help-window-select t)
  (add-hook 'help-mode-hook #'turn-on-visual-line-mode)
  (keymap-set help-map "h" nil)  ; remove view-hello-file
  (keymap-set help-map "l" #'find-library)
  (keymap-set help-map "c" #'describe-face)
  (keymap-set help-map "P" #'describe-text-properties))

;; help-mode: keybindings for help buffers
(with-eval-after-load 'help-mode
  (with-eval-after-load 'evil
    (evil-define-key* 'normal help-mode-map
      "^" #'help-go-back
      (kbd "M-n") #'forward-button
      (kbd "M-p") #'backward-button
      (kbd "C-n") #'forward-button
      (kbd "C-p") #'backward-button)))

;; helpful: enhanced help buffers
(with-eval-after-load 'helpful
  (keymap-set help-map "f" #'helpful-callable)
  (keymap-set help-map "v" #'helpful-variable)
  (keymap-set help-map "k" #'helpful-key))

;; eldoc: teach eldoc to re-run after evil state changes
(with-eval-after-load 'eldoc
  (eldoc-add-command 'evil-normal-state
                     'evil-insert
                     'evil-change
                     'evil-delete
                     'evil-replace))

;; info: built-in texinfo manual reader
(with-eval-after-load 'info
  (keymap-set help-map "s" #'info-apropos)
  (with-eval-after-load 'evil
    (evil-define-key* 'normal Info-mode-map
      "^" #'Info-up
      (kbd "C-n") #'Info-forward-node
      (kbd "C-p") #'Info-backward-node)))

;; imenu: remap consult-imenu in Info-mode
(with-eval-after-load 'imenu
  (with-eval-after-load 'info
    (keymap-set Info-mode-map "<remap> <consult-imenu>" #'Info-menu)))

;; man: built-in manpage reader
(with-eval-after-load 'man
  (setq Man-notify-method 'aggressive))

;; rfc-mode: RFC document browser
(with-eval-after-load 'rfc-mode
  (setq rfc-mode-directory "~/.cache/emacs/rfc-mode/rfcs/")
  (keymap-set help-map "w" #'rfc-mode-browse)
  ;; Fix browser title face to allow highlights
  (custom-theme-set-faces 'user
                          '(rfc-mode-browser-title-face ((t (:inherit bold)))))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'rfc-mode 'motion)))


