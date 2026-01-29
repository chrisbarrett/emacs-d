;;; nav-init.el --- Navigation module init -*- lexical-binding: t; -*-

;;; Commentary:

;; Navigation, window management, jumping, and position history.

;;; Code:

(require '+autoloads)

(require 'cl-lib)

;;; Windmove - directional window navigation

(use-package windmove
  :general (:keymaps 'override-global-map
                     "M-C" #'windmove-up
                     "M-H" #'windmove-left
                     "M-N" #'windmove-right
                     "M-T" #'windmove-down))

;;; Window management commands

(use-package nav-lib
  :general (:keymaps 'override-global-map
                     "M-f" #'+toggle-window-fullframe
                     "M-r" #'+toggle-side-window-raised
                     "M-S-<return>" #'+toggle-window-fullframe
                     "C-M-c" #'+win-swap-up
                     "C-M-h" #'+win-swap-left
                     "C-M-n" #'+win-swap-right
                     "C-M-t" #'+win-swap-down))

;;; Frame settings

(use-package frame
  :custom
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :init
  (window-divider-mode +1))

;;; Winner mode - window layout undo/redo

(use-package winner
  :general-config (:keymaps 'winner-mode-map
                            "M-<left>" #'winner-undo
                            "M-<right>" #'winner-redo)
  :after-call +first-file-hook +first-buffer-hook
  :init
  (winner-mode +1)
  :custom
  (winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                           "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                           "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
  :config
  (with-eval-after-load 'evil
    (keymap-set evil-normal-state-map "C-." #'winner-redo)))

;;; Save place - restore cursor position

(use-package saveplace
  :init (save-place-mode +1)

  :config
  (eval-and-compile
    (define-advice save-place-find-file-hook (:after-while (&rest _) recenter)
      "Recenter on cursor when loading a saved place."
      (when buffer-file-name (ignore-errors (recenter))))

    (define-advice save-place-alist-to-file (:around (fn &rest args) use-prin1-not-pp)
      "Use the faster prin1 for saving history."
      (cl-letf (((symbol-function #'pp) #'prin1))
        (apply fn args)))))

;;; Rotate - window layout rotation

(use-package rotate
  :commands (rotate-layout)
  :defines rotate-functions
  :config
  (setq rotate-functions '(rotate:even-horizontal rotate:even-vertical)))

;;; Better-jumper - unified jump list

(use-package better-jumper
  :after-call +first-file-hook +first-buffer-hook
  :init
  (better-jumper-mode +1)

  :config
  (add-hook 'kill-buffer-hook #'+set-jump-point)
  (advice-add #'consult-imenu :before #'+set-jump-point)
  (advice-add #'org-mark-ring-push :before #'+set-jump-point)
  (add-hook 'org-open-at-point-functions #'+set-jump-point)

  :general
  (:states 'normal
           "C-." #'better-jumper-jump-forward
           "C-," #'better-jumper-jump-backward)

  :general-config
  ([remap evil-jump-forward]  #'better-jumper-jump-forward
   [remap evil-jump-backward] #'better-jumper-jump-backward
   [remap xref-pop-marker-stack] #'better-jumper-jump-backward
   [remap xref-go-back] #'better-jumper-jump-backward
   [remap pop-tag-mark] #'better-jumper-jump-backward
   [remap xref-go-forward] #'better-jumper-jump-forward))

;;; Avy - jump to visible text

(use-package avy
  :general ("M-g" #'avy-goto-char-timer)

  :custom
  ;; Customise the action keys to make actions a bit more vimmy.
  (avy-dispatch-alist '((?x . avy-action-kill-stay)
                        (?d . avy-action-kill-move)
                        (?c . +avy-action-change-move)
                        (?t . avy-action-teleport)
                        (?v . avy-action-mark)
                        (?y . avy-action-copy)
                        (?p . avy-action-yank)
                        (?P . avy-action-yank-line)
                        (?i . avy-action-ispell)
                        (?K . +avy-action-evil-lookup)
                        (? . avy-action-zap-to-char))))



;;; init.el ends here
