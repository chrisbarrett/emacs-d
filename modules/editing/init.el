;;; init.el --- Editing module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Core editing settings and behaviors: autorevert, recentf, uniquify,
;; file IO settings.  Uses built-in packages only.

;;; Code:

(require '+corelib)

;;; Disabled commands re-enabled

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; macOS keybindings removed

(when (equal system-type 'darwin)
  (keymap-global-unset "s-o")
  (keymap-global-unset "s-f")
  (keymap-global-unset "s-q")
  (keymap-global-unset "s-t")
  (keymap-global-unset "s-n"))

;;; Fill and wrap settings

(setq-default fill-column 80)
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;;; Sound and lockfiles

(setq ring-bell-function #'ignore)
(setq create-lockfiles nil)
(setq auto-save-include-big-deletions t)

;;; simple.el settings

(use-package simple
  :custom
  (kill-do-not-save-duplicates t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq-default indent-tabs-mode nil))

;;; Minibuffer behavior

(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(keymap-set minibuffer-local-map "C-p" #'previous-line-or-history-element)
(keymap-set minibuffer-local-map "C-n" #'next-line-or-history-element)

;;; Global keybinding

(keymap-global-set "C-c e e" #'toggle-debug-on-error)

;;; files.el settings

(use-package files
  :custom
  (backup-inhibited t)
  (require-final-newline t)
  (find-file-visit-truename t)
  (make-backup-files nil)
  (confirm-nonexistent-file-or-buffer nil)
  (auto-mode-case-fold nil)
  (version-control nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 5)
  (kept-new-versions 5))

;;; Autosave exists prompt suppressed

(define-advice after-find-file (:around (fn &rest args) dont-block-on-autosave-exists)
  "Prevent the editor blocking to inform you when an autosave file exists."
  (cl-letf (((symbol-function #'sit-for) #'ignore))
    (apply fn args)))

;;; Uniquify settings

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

;;; Autorevert configuration

(use-package autorevert
  :functions (auto-revert-handler)
  :hook
  (after-save-hook . +auto-revert-visible-buffers-h)
  (+switch-buffer-hook . +auto-revert-current-buffer-h)
  (+switch-window-hook . +auto-revert-current-buffer-h)
  :config
  (add-function :after after-focus-change-function #'+auto-revert-visible-buffers-h)
  :custom
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  (revert-without-query (list ".")))

;;; Recentf configuration

(use-package recentf
  :after-call recentf consult-buffer
  :defer-incrementally t
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

;;; Readonly protection for vendor directories

(add-hook! 'find-file-hook
  (when (+file-should-be-opened-read-only-p (buffer-file-name))
    (read-only-mode +1)))

(provide 'editing-init)

;;; init.el ends here
