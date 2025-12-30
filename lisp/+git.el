;;; +git.el --- Git utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides git-related utility functions.

;;; Code:

(autoload 'magit-primary-remote "magit-git")
(autoload 'magit-git-string "magit-git")

(defun +git-repo-display-name ()
  "Get a display name for the current repository.
For GitHub repos, returns owner/name format. Returns nil otherwise."
  (when-let* ((remote (or (magit-primary-remote) "origin"))
              (url (magit-git-string "remote" "get-url" remote)))
    (when (string-match (rx (or "git@github.com:" "https://github.com/")
                            ;; owner / repo
                            (group (+? nonl)) "/" (group (+? nonl))
                            (? ".git")
                            eos)
                        url)
      (format "%s/%s" (match-string 1 url) (match-string 2 url)))))

(provide '+git)

;;; +git.el ends here
