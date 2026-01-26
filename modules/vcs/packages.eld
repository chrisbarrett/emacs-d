;; -*- lisp-data -*-
;; VCS packages

;; Pop-up command menus with switches (transient ships with Emacs but magit needs newer)
(transient)

;; Git porcelain
(magit)

;; Issue tracker integration
((emacs-beads :host github :repo "chrisbarrett/emacs-beads" :main "beads.el"))

;; Browse file history interactively
(git-timemachine)

;; Open file/region in GitHub/GitLab
(browse-at-remote)

;; GitHub/GitLab PR and issue integration
(forge)

;; Auto-commit files on save
(git-auto-commit-mode)
