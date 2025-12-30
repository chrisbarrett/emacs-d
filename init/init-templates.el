;;; init-templates.el --- Templating & snippets -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(defvar +templates-dir (file-name-concat user-emacs-directory "templates/"))

;; Text snippets.
(use-package tempel :ensure t
  ;; NB. Field navigation uses M-{ and M-}.
  :custom
  (tempel-path (file-name-concat +templates-dir "*.eld"))
  :functions (tempel-expand tempel--active-p tempel-done)
  :init
  (add-hook! '(prog-mode-hook text-mode-hook config-mode-hook)
    (add-hook 'completion-at-point-functions #'tempel-expand -90 t))

  :config
  (add-hook '+escape-hook
            (defun +tempel-esc-exit-h ()
              (when (tempel--active-p nil (current-buffer))
                (tempel-done)
                t))))


(use-package autoinsert
  :after-call +first-buffer-hook +first-file-hook
  :custom
  (auto-insert-directory (file-name-concat user-emacs-directory "file-templates/"))
  (auto-insert-alist nil)
  (auto-insert-query nil)
  :config
  (auto-insert-mode +1))


(use-package +file-templates
  :after autoinsert
  :demand t
  :config
  (require 'string-inflection nil t) ;; Used by some templates.
  )


(provide 'init-templates)

;;; init-templates.el ends here
