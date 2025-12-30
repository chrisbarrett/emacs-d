;;; init.el --- main Emacs init file -*- lexical-binding: t; no-byte-compile: t; no-update-autoloads: t; no-native-compile: t; -*-

;;; Commentary:

;; This is the primary Emacs init file, loaded by the editor on startup. It is
;; loaded after early-init.el.

;;; Code:

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

(defvar org-directory "~/org/")
(defvar org-roam-directory "~/org/roam/")
(defvar org-default-notes-file "~/org/notes.org")

(defvar +site-files-directory (file-name-concat user-emacs-directory "site/"))

(add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
(add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
(add-to-list 'trusted-content +init-dir)
(add-to-list 'trusted-content +lisp-dir)
(add-to-list 'trusted-content +modules-dir)


;;; Bootstrap Elpaca & critical packages

;; Suppress warning when loading Elpaca with latest Emacs.
(add-to-list 'warning-suppress-types '(elpaca core \30.2))
(add-to-list 'warning-suppress-types '(elpaca core \31.0.50))

(unless (featurep 'elpaca)
  (load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el")))

(with-no-warnings
  (elpaca elpaca-use-package
    (elpaca-use-package-mode)))

;; General provides a featureful key binding system. It makes defining leader
;; key bindings much easier, and must be loaded immediately for its use-package
;; integration.
(use-package general :ensure t :demand t)

;; Configure Emacs features & packages to follow a structured approach to
;; writing cache files, temp data, etc.
(use-package no-littering :ensure t :demand t
  :config
  (no-littering-theme-backups))

;; Ensure we never attempt to load outdated ELC files.
(use-package auto-compile :ensure t :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Block until these packages are activated.
(elpaca-wait)


;; Key init files that must be loaded early in the sequence.

(use-package init-elpaca :demand t)

(use-package init-hooks :demand t)

(use-package init-leader :demand t)

(use-package init-input :demand t)

;; Load init/**.el

(dolist (file (directory-files-recursively +init-dir (rx ".el" eos)))
  (let ((basename (file-name-base file)))
    (unless (string-match-p (rx bol (any ".~#_")) basename)
      (eval `(use-package ,(intern basename)
               :demand t)))))

;;; Load site/**.el

(when (file-directory-p +site-files-directory)
  (dolist (file (directory-files-recursively +site-files-directory (rx ".el" eos)))
    (let ((basename (file-name-base file)))
      (unless (string-match-p (rx bol (any ".~#_")) basename)
        (load file t nil nil t)))))
