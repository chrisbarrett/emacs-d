;;; mod-projects.el --- Git worktree management with tabs -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements a zellij-inspired workflow for managing git worktrees
;; using Emacs tabs. Each tab represents a git worktree, allowing for isolated
;; contexts when working on different branches.
;;
;; For frame-per-project workflow, see mod-beframe.el.

;;; Code:

(require '+corelib)
(require 'tab-bar)
(require 'magit)
(require 'general)
(require 'transient)
(require 'project)

(autoload 'dired-jump "dired-x")
(autoload 'claude-code-ide "claude-code-ide")

(transient-define-prefix +projects-worktree-menu ()
  "Transient menu for git worktree operations."
  ["Worktree Operations"
   ("X" "Delete tab, worktree, and branch" +projects-delete-worktree)
   ("M" "Merge to main and cleanup" +projects-merge-and-cleanup)
   ("r" "Rebase on main" +projects-rebase-on-main)])

;;; Configuration

(defvar +projects-worktree-base-dir ".worktrees"
  "Base directory name for creating new worktrees.")


;;; Helper functions

(defun +projects--default-branch ()
  "Get the default branch name for the current repository."
  (let* ((remote (magit-primary-remote))
         (branch (and remote
                      (magit-git-string "symbolic-ref" "--short"
                                        (format "refs/remotes/%s/HEAD" remote)))))
    (or (and branch (cdr (magit-split-branch-name branch)))
        "main")))

(defun +projects--project-root ()
  (when-let* ((project (project-current)))
    (project-root project)))

(defun +projects--worktree-branch (worktree-path)
  (or
   (seq-find (pcase-lambda (`(,path ,_commit ,branch . ,_))
               (when (equal path worktree-path)
                 branch))
             (magit-list-worktrees))
   ;; Guess based on target directory
   (file-name-nondirectory (directory-file-name worktree-path))))

(defun +projects--tab-for-worktree (worktree-path)
  "Find the tab associated with WORKTREE-PATH, if any."
  (when tab-bar-mode
    (seq-find (lambda (tab)
                (equal worktree-path (alist-get 'worktree-path tab)))
              (funcall tab-bar-tabs-function))))

(defun +projects--detect-worktree-path ()
  "Detect the worktree path for the current directory, if in a worktree."
  (when-let* ((project-root (+projects--project-root)))
    (let* ((toplevel (magit-gitdir))
           (worktrees (magit-list-worktrees))
           (current-dir (expand-file-name default-directory)))
      ;; Find a worktree that contains the current directory
      (seq-find (lambda (worktree-info)
                  (let ((worktree-path (car worktree-info)))
                    (and (not (equal (directory-file-name worktree-path)
                                     (directory-file-name toplevel)))
                         (string-prefix-p (file-name-as-directory worktree-path)
                                          current-dir))))
                worktrees))))

(defun +projects--worktree-for-selected-tab ()
  "Get the worktree path for the current tab, if set.
If not set but we're in a worktree, detect and set it."
  (when tab-bar-mode
    (let ((current-tab (tab-bar--current-tab)))
      (or (alist-get 'worktree-path current-tab)
          ;; Try to detect and set it
          (when-let* ((detected (car (+projects--detect-worktree-path))))
            (let ((tab (tab-bar--current-tab-find)))
              (setf (alist-get 'worktree-path (cdr tab)) detected))
            detected)))))

(defun +projects--worktree-clean-p (worktree-path)
  (let ((default-directory worktree-path))
    (not (magit-anything-modified-p))))


;;; Worktree operations

(defun +projects--open-worktree-tab (worktree-path)
  (if-let* ((existing-tab (+projects--tab-for-worktree worktree-path)))
      (tab-bar-select-tab (1+ (tab-bar--tab-index existing-tab)))
    ;; Create new tab
    (let ((tab-name (+projects--worktree-branch worktree-path)))
      (tab-bar-new-tab)
      ;; Set worktree-path in tab parameters
      (set-frame-parameter nil
                           'buffer-list
                           (frame-parameter nil 'buffer-list))
      (tab-bar-rename-tab tab-name)
      ;; Store worktree path in the current tab
      (let ((current-tab (tab-bar--current-tab-find)))
        (setf (alist-get 'worktree-path (cdr current-tab)) worktree-path))))

  (magit-status-setup-buffer worktree-path)

  ;; Start claude-code-ide for this worktree.
  (when (fboundp 'claude-code-ide)
    (let ((default-directory worktree-path)
          (project-find-functions nil))
      (ignore-errors
        (claude-code-ide)))))

(defun +projects-switch-worktree (&optional initial-rev)
  "Switch to a worktree tab, creating a branch and worktree if needed.

If called interactively with a prefix arg, prompt INITIAL-REV."
  (interactive (list (when current-prefix-arg
                       (magit-read-branch-or-commit "Create worktree from"))))
  (unless (+projects--project-root)
    (user-error "Not in a git project"))
  (let* ((worktrees (magit-list-worktrees))
         (dir-or-new-branch (completing-read "Worktree: " worktrees nil nil)))

    (if (alist-get dir-or-new-branch worktrees nil nil #'equal)
        (+projects--open-worktree-tab dir-or-new-branch)
      (let ((default-directory (magit-gitdir))
            (worktree-path (file-name-concat (magit-gitdir) +projects-worktree-base-dir dir-or-new-branch)))
        ;; Ensure intervening directories exist
        (make-directory (file-name-directory worktree-path) t)
        (magit-run-git "worktree" "add" (magit--expand-worktree worktree-path) (or initial-rev "HEAD"))
        (+projects--open-worktree-tab worktree-path)))))

;;; Worktree management commands

(defun +project--kill-worktree-buffers (worktree-path)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (string-prefix-p worktree-path default-directory)
        (unless (get-buffer-process buf)
          (kill-buffer buf))))))

(defun +projects-delete-worktree ()
  "Delete the current tab, worktree, and associated branch.
Requires a clean working tree (no uncommitted changes)."
  (interactive)
  (let ((worktree-path (+projects--worktree-for-selected-tab)))
    (when (equal worktree-path (magit-gitdir))
      (user-error "Refusing to act on repo root worktree"))

    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))

    (unless (+projects--worktree-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes"))

    (let ((branch-name (+projects--worktree-branch worktree-path)))
      (unless (yes-or-no-p (format "Delete worktree and branch '%s'?" branch-name))
        (user-error "Aborted without changes"))

      (when tab-bar-mode
        (tab-bar-close-tab))

      (+project--kill-worktree-buffers worktree-path)
      (let ((magit-no-confirm '(trash)))
        (magit-worktree-delete worktree-path))
      (message "Deleted worktree and branch: %s" branch-name))))

(defun +projects-merge-and-cleanup ()
  "Merge current worktree branch to main, then delete worktree and branch."
  (interactive)
  (let* ((default-directory (magit-gitdir))
         (worktree-path (+projects--worktree-for-selected-tab))
         (worktree-branch (+projects--worktree-branch worktree-path))
         (default-branch (+projects--default-branch)))

    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))
    (when (equal worktree-path (magit-gitdir))
      (user-error "Refusing to act on repo root worktree"))

    (unless (+projects--worktree-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes"))

    (unless (yes-or-no-p (format "Merge branch `%s' into `%s' and delete worktree? "
                                 worktree-branch
                                 default-branch))
      (user-error "Aborted without changes"))

    (+project--kill-worktree-buffers worktree-path)
    (magit-worktree-delete worktree-path) ; if this fails, the branch is still around for recovery.
    (magit-run-git "switch" default-branch)
    (magit-merge-absorb worktree-branch) ; NB. async

    (when tab-bar-mode
      (tab-bar-close-tab))))

(defun +projects-rebase-on-main ()
  "Rebase the current worktree branch on main."
  (interactive)
  (let ((worktree-path (+projects--worktree-for-selected-tab)))
    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))
    (when (equal worktree-path (magit-gitdir))
      (user-error "Refusing to act on root worktree"))

    (unless (+projects--worktree-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes. Please commit or stash changes before rebase"))

    (let* ((branch-name (+projects--worktree-branch worktree-path))
           (default-directory worktree-path)
           (default-branch (+projects--default-branch)))

      (unless (yes-or-no-p (format "Rebase branch '%s' on %s? "
                                   branch-name
                                   default-branch))
        (user-error "Rebase cancelled"))

      (let ((remote (magit-primary-remote)))
        (unless remote
          (user-error "Cannot determine primary remote"))

        (message "Fetching latest changes from %s..." remote)
        ;; Use synchronous fetch
        (unless (zerop (magit-call-git "fetch" remote))
          (user-error "Fetch failed"))

        (message "Rebasing %s on %s/%s..." branch-name remote default-branch)
        ;; Use synchronous rebase
        (unless (zerop (magit-call-git "rebase" (format "%s/%s" remote default-branch)))
          (user-error "Rebase failed - resolve conflicts and run 'git rebase --continue'"))

        (magit-refresh)
        (message "Rebase successful: %s rebased on %s/%s"
                 branch-name remote default-branch)))))

;;; Tab cleanup

(defun +projects--cleanup-worktree-tab (tab)
  "Clean up resources when a worktree TAB is closed."
  (when-let* ((worktree-path (alist-get 'worktree-path tab)))
    (when (fboundp 'claude-code-ide-stop)
      (let ((default-directory worktree-path)
            (project-find-functions nil))
        (ignore-errors
          (claude-code-ide-stop))))

    (+project--kill-worktree-buffers worktree-path)))

;; Always add the hook - it will only fire when tab-bar-mode is active
(add-hook 'tab-bar-tab-post-close-functions #'+projects--cleanup-worktree-tab)

;;; Magit integration

(defun +projects-magit-status ()
  "Display magit status buffer, for the current worktree if appropriate."
  (interactive)
  (if-let* ((worktree-path (+projects--worktree-for-selected-tab)))
      (magit-status-setup-buffer worktree-path)
    (call-interactively #'magit-status)))

(provide 'mod-projects)

;;; mod-projects.el ends here
