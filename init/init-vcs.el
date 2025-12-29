;;; init-vcs.el --- Git & VCS configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; Lib for showing pop-up menus of key commands, often with switches to modify
;; behaviour.
;;
;; Magit depends on a more recent version of transient than the one that ships
;; with Emacs.
(use-package transient :ensure t
  :general-config
  (:keymaps 'transient-map [escape] #'transient-quit-one))


;; Magit is the definitive UX for working with git.
(use-package magit :ensure t
  :config
  (use-package mod-magit :demand t)
  :custom
  (magit-display-buffer-function #'+magit-display-buffer-same-frame)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-diff-visit-prefer-worktree t)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-insert-related-refs nil)
  (magit-format-file-function #'magit-format-file-nerd-icons))


;; Emacs integration for beads issue tracker
(use-package beads :ensure (emacs-beads :host github :repo "chrisbarrett/emacs-beads" :main "beads.el")
  :commands (beads-issue-create
             beads-process-call
             beads-process-show-log)
  :custom
  (beads-worktree-root-function #'+worktrees-path-for-selected-tab))


;; Integrate git worktrees with tabs
(use-package mod-worktrees
  :commands (+worktrees-menu +worktrees-adopt-initial-tab)
  :general (:keymaps 'override-global-map
                     "M-G" #'+worktrees-menu
                     "M-O" #'+worktrees-create-switch))

(use-package git-timemachine :ensure t
  :general-config
  (:states 'normal
   :keymaps 'git-timemachine-mode-map
   "C-p" #'git-timemachine-show-previous-revision
   "C-n" #'git-timemachine-show-next-revision
   "gb"  #'git-timemachine-blame
   "gtc" #'git-timemachine-show-commit)

  :config
  ;; git-timemachine uses `delay-mode-hooks', which can suppress font-lock.
  (add-hook 'git-timemachine-mode-hook #'font-lock-mode)
  ;; Ensure evil keymaps are applied
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)

  ;; Show information in header-line for better visibility.
  :custom
  (git-timemachine-show-minibuffer-details t)
  :config
  (define-advice git-timemachine--show-minibuffer-details (:override (revision) use-header-line)
    "Show revision details in the header-line, instead of the minibuffer."
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize author 'face 'git-timemachine-minibuffer-author-face)
                    (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                    date-full date-relative)))))


(use-package browse-at-remote :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :config
  (use-package mod-browse-at-remote :demand t))


;; Teach magit how to work with pull requests on GitHub and other git hosting
;; services.
(use-package forge :ensure t
  :after-call magit-status ; avoids compilation until first use
  :general
  (:keymaps 'magit-mode-map [remap magit-browse-thing] #'forge-browse)
  (:keymaps 'magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote)
  (:keymaps 'magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch)
  (:keymaps 'forge-topic-list-mode-map :states 'normal "q" #'kill-current-buffer))


(use-package vc
  :custom
  ;; Don't prompt when following links to files that are under version control.
  (vc-follow-symlinks t)
  ;; I literally only ever use Git these days.
  (vc-handled-backends '(Git))
  :functions (vc-git-root)
  :config
  (pushnew! vc-directory-exclusion-list
            "node_modules"
            "cdk.out"
            "target"
            ".direnv"))


;; Provides a minor mode that automatically commits files as you edit--this is
;; useful mainly for my org files, where I don't care about commit messages
;; and just want a git history.
(use-package git-auto-commit-mode :ensure t
  :custom
  (gac-silent-message-p t)
  :init
  (put 'gac-debounce-interval 'safe-local-variable 'integerp)
  (put 'gac-automatically-add-new-files-p 'safe-local-variable 'booleanp))


(provide 'init-vcs)

;;; init-vcs.el ends here
