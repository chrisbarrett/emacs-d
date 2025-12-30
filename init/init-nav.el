;;; init-nav.el --- Navigation, buffers, windows -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; Tiling window navigation--I configure bindings here that only work in GUI
;; frames. In terminals I use Zellij.
;;
;; NB terminals can't interpret M-S-* or M-C-* bindings, making these bindings
;; GUI-frame-only.

(use-package windmove
  :general (:keymaps 'override-global-map
                     "M-C" #'windmove-up
                     "M-H" #'windmove-left
                     "M-N" #'windmove-right
                     "M-T" #'windmove-down))


(use-package mod-tty-frames :demand t)


(use-package +window
  :general (:keymaps 'override-global-map
                     "M-f" #'+toggle-window-fullframe
                     "M-r" #'+toggle-side-window-raised
                     "M-S-<return>" #'+toggle-window-fullframe
                     "C-M-c" #'+win-swap-up
                     "C-M-h" #'+win-swap-left
                     "C-M-n" #'+win-swap-right
                     "C-M-t" #'+win-swap-down))


;; Frame management settings
(use-package frame
  :custom
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :init
  (window-divider-mode +1))


;; Provides undo/redo for buffer & window layout changes.
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


;; Save buffer position when re-visiting files, even across Emacs sessions.
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


;; Provides a few commands for arranging windows in pre-configured
;; layouts--very handy.
;;
;; TODO: Define my own version that ignores side-windows when re-arranging.
(use-package rotate :ensure t
  :commands (rotate-layout)
  :defines rotate-functions
  :config
  (setq rotate-functions '(rotate:even-horizontal rotate:even-vertical)))


;; Maintains a jump list so you can more easily get back to where you were if
;; a command takes you somewhere else.
(use-package better-jumper :ensure t
  :after-call +first-file-hook +first-buffer-hook
  :preface
  (defun +set-jump-point ()
    (when (get-buffer-window)
      (better-jumper-set-jump))
    nil)
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


;; Jump to things or execute other actions by typing a few letters.
(use-package avy :ensure t
  :general ("M-g" #'avy-goto-char-timer)

  :config
  (defun +avy-action-change-move (pt)
    "Delete the thing at PT and enter insert state."
    (goto-char pt)
    (avy-forward-item)
    (kill-region pt (point))
    (evil-insert-state)
    (point))

  (defun +avy-action-evil-lookup (pt)
    "Look up the definition of thing at PT with evil."
    (save-excursion
      (goto-char pt)
      (avy-forward-item)
      (evil-lookup))
    t)

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
                        (? . avy-action-zap-to-char))))


(provide 'init-nav)

;;; init-nav.el ends here
