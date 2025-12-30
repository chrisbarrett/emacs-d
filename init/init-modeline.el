;;; init-modeline.el --- Mode-line configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; The modeline from doom, packaged independently.
(use-package doom-modeline :ensure t
  :hook elpaca-after-init-hook
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-check-simple-format t)
  (doom-modeline-modal nil)


  ;; Make modeline more minimal in magit-status buffer.

  :functions doom-modeline-set-modeline
  :preface
  (autoload 'hide-mode-line-mode "hide-mode-line")
  :config
  (add-hook 'magit-mode-hook
            (defun +modeline-hide-in-non-status-buffer-h ()
              "Show minimal modeline in magit-status buffer, no modeline elsewhere."
              (if (eq major-mode 'magit-status-mode)
                  (doom-modeline-set-modeline 'magit)
                (hide-mode-line-mode)))))


;; Display number of isearch results in the modeline
(use-package anzu :ensure t
  :after-call isearch-mode)


;; Extends anzu to evil-mode commands.
(use-package evil-anzu
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))

(provide 'init-modeline)

;;; init-modeline.el ends here
