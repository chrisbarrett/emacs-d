;;; mod-magit.el --- Magit configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'magit)

(autoload 'evil-insert-state "evil-states")

;; Set initial evil state depending on whether the line is empty or not. Empty
;; line = new commit message, whereas non-empty means we're editing an
;; existing one.
(add-hook 'git-commit-mode-hook
          (defun +git-commit-set-message ()
            (when (and (bolp) (eolp))
              (evil-insert-state))))

(provide 'mod-magit)

;;; mod-magit.el ends here
