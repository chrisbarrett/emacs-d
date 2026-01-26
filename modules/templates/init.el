;;; init.el --- Templates module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures Tempel snippets and skeleton-based file templates via autoinsert.
;; Provides M-e for snippet expansion and automatic file template insertion.

;;; Code:

(defvar +templates-dir (file-name-concat user-emacs-directory "templates/"))

;; Tempel: text snippets with field navigation.
(with-eval-after-load 'tempel
  (setq tempel-path (file-name-concat +templates-dir "*.eld"))

  ;; Exit active snippet on ESC via +escape-hook.
  (when (boundp '+escape-hook)
    (add-hook '+escape-hook
              (defun +tempel-esc-exit-h ()
                (when (tempel--active-p nil (current-buffer))
                  (tempel-done)
                  t)))))

;; Add tempel-expand to completion-at-point-functions for prog/text/config modes.
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'tempel-expand -90 t))))

;; Keybindings for tempel expansion (M-e in insert state).
(with-eval-after-load 'evil
  (with-eval-after-load 'tempel
    (evil-define-key 'insert text-mode-map (kbd "M-e") #'tempel-expand)
    (evil-define-key 'insert prog-mode-map (kbd "M-e") #'tempel-expand)))

;; Autoinsert: file templates for new files.
(with-eval-after-load 'autoinsert
  (setq auto-insert-directory (file-name-concat user-emacs-directory "file-templates/"))
  (setq auto-insert-alist nil)
  (setq auto-insert-query nil)
  (auto-insert-mode +1))

;; Load string-inflection for dynamic template names.
(with-eval-after-load 'autoinsert
  (require 'string-inflection nil t))

(provide 'templates-init)

;;; init.el ends here
