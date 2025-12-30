;;; init-c.el --- C language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package c-ts-mode
  :general-config
  (:keymaps 'c-ts-mode-map :states 'insert
            "<" #'+c-electric-left-angle-bracket)

  (:keymaps 'c-ts-mode-map :states '(normal insert)
            "S-RET" #'+c-auto-insert-semi-newline)
  :init
  (defun +c-auto-insert-semi-newline ()
    (interactive)
    (goto-char (line-end-position))
    (unless (thing-at-point-looking-at (rx (any "{:;") (* space) eol))
      (insert ";"))
    (evil-insert-state)
    (newline-and-indent))

  (defun +c-electric-left-angle-bracket (&optional arg)
    (interactive "P")
    (let* ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
           (include-line-p (string-match-p (rx bol (* space) "#" (* space) "include" symbol-end)
                                           current-line)))
      (cond (include-line-p
             (just-one-space)
             (insert "<")
             (save-excursion
               (insert ">")))
            (t
             (call-interactively #'self-insert-command))))))


(alist-set! major-mode-remap-alist #'c-mode #'c-ts-mode)

(provide 'init-c)

;;; init-c.el ends here
