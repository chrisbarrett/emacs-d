;;; init-project.el --- Projects & workspaces -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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
  :config
  (use-package mod-project
    :demand t
    :general
    (:keymaps 'project-prefix-map "R" #'+projects-rescan))

  (project-remember-project (project-try-vc user-emacs-directory))
  (project-remember-project (project-try-vc org-directory)))


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
