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

;; Key init files that must be loaded early in the sequence.

(use-package init-elpaca
  :demand t
  :config
  (defconst +chrisbarrett-elpaca-repos (seq-map (lambda (repo)
                                                  (file-name-concat elpaca-repos-directory (concat repo "/")))
                                                '("emacs-beads" "nursery"))
    "List of my repos managed via elpaca."))

(use-package init-hooks :demand t)
(use-package init-nolitter :demand t)
(use-package init-autocompile :demand t)
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
