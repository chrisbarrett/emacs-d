;;; project-lib.el --- Project library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Library functions for project management with beframe integration.

;;; Code:

(autoload 'magit-primary-remote "magit-git")
(autoload 'magit-git-string "magit-git")

;;;###autoload
(defvar +project-scan-dirs-alist nil
  "Alist of directories to rescan, stored as (DIR . RECURSIVE).")

;;;###autoload
(defvar +beframe-strict-project-isolation-p nil
  "When non-nil, switch frames when reading files from other projects.")

;;;###autoload
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

;;;###autoload
(defun +projects-rescan ()
  "Re-scan all tracked project directories."
  (interactive)
  (pcase-dolist (`(,dir . ,recursive) +project-scan-dirs-alist)
    (project-remember-projects-under dir recursive))
  (message "Re-scanned %s project dir(s)" (length +project-scan-dirs-alist)))

;;;###autoload
(defun +project-switch-magit-status ()
  "Show `magit-status' buffer when switching projects.
Falls back to `dired' if not a git repository."
  (interactive)
  (let* ((proj (project-current t))
         (root (project-root proj)))
    (if (file-directory-p (file-name-concat root ".git"))
        (magit-status-setup-buffer root)
      (dired root))))

(autoload 'magit-status-setup-buffer "magit-status")

(provide 'project-lib)

;;; project-lib.el ends here
