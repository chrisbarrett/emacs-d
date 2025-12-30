;;; init-security.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'init-elpaca)

(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
  (add-to-list 'trusted-content +init-dir)
  (add-to-list 'trusted-content +lisp-dir)
  (add-to-list 'trusted-content +modules-dir)
  (dolist (repo +chrisbarrett-elpaca-repos)
    (add-to-list 'trusted-content (abbreviate-file-name repo))))

(provide 'init-security)

;;; init-security.el ends here
