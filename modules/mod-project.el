;;; mod-project.el --- Configuration for project.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'project)

(autoload 'magit-status-setup-buffer "magit-status")

(defun +project-switch-magit-status ()
  "Show `magit-status' buffer on switch."
  (interactive)
  (let* ((proj (project-current t))
         (root (project-root proj)))
    (if (file-directory-p (file-name-concat root ".git"))
        (magit-status-setup-buffer root)
      (dired root))))

(setq project-switch-commands '+project-switch-magit-status)


;;; Add a command to re-scan project directories.

(defvar +project-scan-dirs-alist nil)

(define-advice project-remember-projects-under (:before (dir &optional recursive) save-for-rescan)
  (alist-set! +project-scan-dirs-alist dir recursive))

(defun +projects-rescan ()
  "Re-scan all projects."
  (interactive)
  (pcase-dolist (`(,dir . ,recursive) +project-scan-dirs-alist)
    (project-remember-projects-under dir recursive))
  (message "Re-scanned %s project dir(s)" (length +project-scan-dirs-alist)))

(provide 'mod-project)

;;; mod-project.el ends here
