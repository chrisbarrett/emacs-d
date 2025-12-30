;;; init-project.el --- Projects & workspaces -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar org-directory)

;; Emacs' built-in project lib
(use-package project
  :custom
  (project-vc-ignores '(".cache/"))
  (project-list-exclude (list
                         (rx bol "/nix/store/")
                         (defun +project-exclude-hidden-dirs (project)
                           "Exclude projects in any hidden directory, except for the ~/.config dir."
                           (let ((root (project-root project)))
                             (and (string-match-p  (rx "/.") root)
                                  (not (string-prefix-p "~/.config/" root)))))))


  ;; Remember a couple of key projects

  :functions project-try-vc
  :config
  (project-remember-project (project-try-vc user-emacs-directory))
  (project-remember-project (project-try-vc org-directory))


  ;; Add a command to rescan for projects.

  :config
  (defvar +project-scan-dirs-alist nil)

  (define-advice project-remember-projects-under (:before (dir &optional recursive) save-for-rescan)
    (alist-set! +project-scan-dirs-alist dir recursive))

  (defun +projects-rescan ()
    "Re-scan all projects."
    (interactive)
    (pcase-dolist (`(,dir . ,recursive) +project-scan-dirs-alist)
      (project-remember-projects-under dir recursive))
    (message "Re-scanned %s project dir(s)" (length +project-scan-dirs-alist)))

  :general-config
  (:keymaps 'project-prefix-map "R" #'+projects-rescan)


  ;; Use magit for `project-switch-command'.

  :preface
  (autoload 'magit-status-setup-buffer "magit-status")

  :config
  (defun +project-switch-magit-status ()
    "Show `magit-status' buffer on switch."
    (interactive)
    (let* ((proj (project-current t))
           (root (project-root proj)))
      (if (file-directory-p (file-name-concat root ".git"))
          (magit-status-setup-buffer root)
        (dired root))))

  :custom
  (project-switch-commands '+project-switch-magit-status))

;; Associate frames with lists of buffers; useful for workflows where you have
;; separate frames per project.
(use-package beframe :ensure t
  :demand t
  :init
  (use-package mod-beframe
    :general (:keymaps 'override-global-map
                       "s-t" 'project-switch-beframed
                       "M-W" 'project-switch-beframed)
    :general (:keymaps 'project-prefix-map
                       "p" #'project-switch-beframed)
    :demand t
    :after beframe))


(provide 'init-project)

;;; init-project.el ends here
