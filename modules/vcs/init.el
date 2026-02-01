;;; vcs-init.el --- VCS initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Git porcelain, worktree management, and forge integration.

;;; Code:

(require '+autoloads)
(require '+corelib)

(require 'general) ; safety: init.el
(require 'transient)

;;; Transient - pop-up command menus

(use-package transient
  :general-config
  (:keymaps 'transient-map [escape] #'transient-quit-one))

(transient-define-prefix +worktrees-menu ()
  "Transient menu for git worktree operations."
  [["Change"
    :if +worktrees--repo-root-for-selected-frame
    ("o" "Switch or new..." +worktrees-create-switch)]

   ["Update"
    :if +worktrees-tab-dedicated-to-child-p
    ("u" "Rebase on main" +worktrees-rebase-on-local-main)
    ("U" "Rebase on origin/main" +worktrees-rebase-on-origin-main)]

   ["Complete"
    :if +worktrees-tab-dedicated-to-child-p
    ("m" "Merge to main" +worktrees-absorb-into-main)
    ("x" "Destroy" +worktrees-destroy-current)]]

  ["Show"
   :inapt-if-not +worktrees--repo-root-for-selected-frame
   ("g" "git status" +worktrees-magit-status)
   ("c" "claude-code" +worktrees-claude-code)])


;;; Magit - git porcelain

(use-package magit
  :custom
  (magit-display-buffer-function #'+magit-display-buffer-same-frame)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-diff-visit-prefer-worktree t)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-insert-related-refs nil)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-read-worktree-directory-function #'+magit-read-worktree-directory)
  :config
  (+load "config/+magit.el"))


;; Pulsar integration

(use-package pulsar
  :config
  (pushnew! pulsar-pulse-functions '+magit-diff-visit-file-unselected))


;;; Git Timemachine - browse file history

(use-package git-timemachine
  :general-config
  (:states 'normal
   :keymaps 'git-timemachine-mode-map
   "C-p" 'git-timemachine-show-previous-revision
   "C-n" 'git-timemachine-show-next-revision
   "gb"  'git-timemachine-blame
   "gtc" 'git-timemachine-show-commit)
  :custom
  (git-timemachine-show-minibuffer-details t)
  :config
  (+load "config/+git-timemachine.el"))


;;; Browse at Remote

(use-package browse-at-remote
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :config
  (+load "config/browse-at-remote.el"))


;;; Forge - GitHub/GitLab PR integration

(use-package forge
  :after-call magit-status
  :custom
  (forge-checkout-worktree-read-directory-function #'+forge-read-worktree-directory)
  :general
  (:keymaps 'magit-mode-map [remap magit-browse-thing] #'forge-browse)
  (:keymaps 'magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote)
  (:keymaps 'magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch)
  (:keymaps 'forge-topic-list-mode-map :states 'normal "q" #'kill-current-buffer)
  :config
  (advice-add 'forge-checkout-worktree :after #'+forge-checkout-worktree-open-tab))


;;; VC settings

(use-package vc
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))
  :functions (vc-git-root)
  :config
  (pushnew! vc-directory-exclusion-list
            "node_modules"
            "cdk.out"
            "target"
            ".direnv"))

;;; Git Auto Commit Mode

(use-package git-auto-commit-mode
  :custom
  (gac-silent-message-p t)
  :init
  (put 'gac-debounce-interval 'safe-local-variable 'integerp)
  (put 'gac-automatically-add-new-files-p 'safe-local-variable 'booleanp))

;;; Worktrees - tab-per-worktree workflow

(general-def
  "M-G" #'+worktrees-menu
  "M-O" #'+worktrees-create-switch)

(defvar project-find-functions) ; project.el

(defun +worktrees--cleanup-worktree-tab (tab _sole-tab)
  "Clean up resources when a worktree TAB is closed."
  (when-let* ((worktree-path (alist-get 'worktree-path tab)))
    (when (fboundp 'claude-code-ide-stop)
      (let ((default-directory worktree-path)
            (project-find-functions nil)
            (kill-buffer-query-functions nil))
        (ignore-errors
          (claude-code-ide-stop))))
    (+worktree-kill-buffers worktree-path)))

(add-hook 'tab-bar-tab-pre-close-functions #'+worktrees--cleanup-worktree-tab)

;; Ensure above key sequences are respected in eat.

(use-package eat
  :general-config
  (:keymaps 'eat-semi-char-mode-map
            "M-G" nil
            "M-O" nil))



;;; init.el ends here
