;; -*- lexical-binding: t; -*-

(load-file (expand-file-name "./elpaca-bootstrap.el" user-emacs-directory))

(setq inhibit-startup-screen t)
(setq use-package-always-defer t)

;; Disable unneeded UI clutter.

(tool-bar-mode -1)
(menu-bar-mode -1)

;;; General editing

(electric-pair-mode +1)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)

;;; evil-mode

(use-package evil :ensure (:wait t)
  :demand t
  :config
  (evil-mode +1))

(use-package evil-org :ensure t
  :hook (org-mode . evil-org-mode)

  :init
  (use-package evil-org-agenda
    :after org-agenda
    :config
    (evil-org-agenda-set-keys)))

;;; magit

(use-package transient :ensure t)

(use-package magit :ensure t
  :bind (("C-x g" . magit-status)))
