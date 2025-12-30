;;; init-avy.el --- in-buffer jumping interface -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


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
                        (? . avy-action-zap-to-char)))

  :init
  ;; Use +/- to mark syntactic elements with tree-sitter. However, if I don't have
  ;; a selection, make - call avy.
  (general-define-key :states '(normal motion)
                      "-" (general-predicate-dispatch #'avy-goto-char-timer
                            (region-active-p) #'expreg-contract)
                      "+" #'+expreg-expand-dwim))


(provide 'init-avy)

;;; init-avy.el ends here
