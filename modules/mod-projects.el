;;; mod-projects.el --- Project and worktree management with tabs -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements a zellij-inspired workflow for managing git worktrees
;; using Emacs tabs, and a frame-per-project workflow for general projects.
;; Each tab represents a git worktree, and each frame can represent a project,
;; allowing for isolated contexts.

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
  "Get the current project root, or nil if not in a project."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun +projects--worktree-branch (worktree-path)
  "Get the branch name for WORKTREE-PATH."
  (let ((default-directory worktree-path))
    (magit-get-current-branch)))

(defun +projects--worktree-name (worktree-path)
  "Get a human-readable name for WORKTREE-PATH."
  (file-name-nondirectory (directory-file-name worktree-path)))

(defun +projects--tab-for-worktree (worktree-path)
  "Find the tab associated with WORKTREE-PATH, if any."
  (when tab-bar-mode
    (seq-find (lambda (tab)
                (equal worktree-path (alist-get 'worktree-path tab)))
              (funcall tab-bar-tabs-function))))

(defun +projects--detect-worktree-path ()
  "Detect the worktree path for the current directory, if in a worktree."
  (when-let* ((project-root (+projects--project-root)))
    (let* ((toplevel (magit-toplevel))
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

(defun +projects--current-worktree-path ()
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

(defun +projects--validate-branch-name (name)
  "Validate NAME as a legal git branch name.
Returns t if valid, nil otherwise."
  (and (stringp name)
       (not (string-empty-p name))
       (not (string-match-p "[\000-\037\177 ~^:?*\[]" name))
       (not (string-match-p "\\.\\." name))
       (not (string-prefix-p "/" name))
       (not (string-suffix-p "/" name))
       (not (string-suffix-p ".lock" name))
       (not (string= name "@"))
       t))

(defun +projects--git-clean-p (&optional worktree-path)
  "Check if WORKTREE-PATH (or current directory) has no uncommitted changes."
  (let ((default-directory (or worktree-path default-directory)))
    (not (magit-anything-modified-p))))

;;; Worktree operations

(defun +projects--create-worktree (branch-name &optional start-point)
  "Create a new worktree for BRANCH-NAME under the worktree base directory.
START-POINT is the branch or ref to use as the starting point (defaults to HEAD)."
  (let* ((root (+projects--project-root))
         (worktree-path (expand-file-name
                         (concat +projects-worktree-base-dir "/" branch-name)
                         root))
         (default-directory root))
    (unless (file-directory-p (expand-file-name +projects-worktree-base-dir root))
      (make-directory (expand-file-name +projects-worktree-base-dir root) t))
    (message "Creating worktree for branch '%s'..." branch-name)
    (magit-worktree-branch worktree-path branch-name (or start-point "HEAD"))
    worktree-path))

(defun +projects--open-worktree-tab (worktree-path)
  "Open or switch to a tab for WORKTREE-PATH."
  (if tab-bar-mode
      (if-let* ((existing-tab (+projects--tab-for-worktree worktree-path)))
          (tab-bar-select-tab (1+ (tab-bar--tab-index existing-tab)))

        ;; Create new tab
        (let* ((tab-name (+projects--worktree-name worktree-path)))
          (tab-bar-new-tab)
          ;; Set worktree-path in tab parameters
          (set-frame-parameter nil
                               'buffer-list
                               (frame-parameter nil 'buffer-list))
          (tab-bar-rename-tab tab-name)
          ;; Store worktree path in the current tab
          (let ((current-tab (tab-bar--current-tab-find)))
            (setf (alist-get 'worktree-path (cdr current-tab)) worktree-path))
          ;; Open dired at worktree root
          (dired worktree-path)
          ;; Start claude-code-ide for this worktree
          (when (fboundp 'claude-code-ide)
            (let ((default-directory worktree-path))
              (claude-code-ide)))
          (message "Opened tab for worktree: %s" tab-name)))

    ;; When tab-bar-mode is disabled, just open dired
    (dired worktree-path)
    (when (fboundp 'claude-code-ide)
      (let ((default-directory worktree-path))
        (claude-code-ide)))))

(defun +projects-switch-worktree (&optional start-point)
  "Switch to an existing worktree or create a new one.
Shows a completing-read prompt with existing worktrees. If the input
doesn't match an existing worktree, creates a new one with that name.

With prefix argument, prompt for a branch or ref to use as the starting
point for the new worktree."
  (interactive (list (when current-prefix-arg
                       (magit-read-branch-or-commit "Create worktree from"))))
  (unless (+projects--project-root)
    (user-error "Not in a git project"))
  (let* ((toplevel (magit-toplevel))
         (worktrees (cl-delete (directory-file-name toplevel)
                               (mapcar #'car (magit-list-worktrees))
                               :test #'equal))
         (worktree-names (mapcar #'+projects--worktree-name worktrees))
         (worktree-alist (cl-mapcar #'cons worktree-names worktrees))
         (input (completing-read "Worktree: " worktree-names nil nil)))
    (if-let* ((existing-path (alist-get input worktree-alist nil nil #'equal)))
        (+projects--open-worktree-tab existing-path)

      (unless (+projects--validate-branch-name input)
        (user-error "Invalid branch name: %s" input))

      (let ((new-path (+projects--create-worktree input start-point)))
        (+projects--open-worktree-tab new-path)))))

;;; Worktree management commands

(defun +projects-delete-worktree ()
  "Delete the current tab, worktree, and associated branch.
Requires a clean working tree (no uncommitted changes)."
  (interactive)
  (let ((worktree-path (+projects--current-worktree-path)))
    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))

    (unless (+projects--git-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes. Please commit or stash changes before deletion"))

    (let ((branch-name (+projects--worktree-branch worktree-path)))

      ;; Confirm deletion
      (unless (yes-or-no-p (format "Delete worktree '%s' and branch '%s'? "
                                   (+projects--worktree-name worktree-path)
                                   branch-name))
        (user-error "Deletion cancelled"))

      (when tab-bar-mode
        (tab-bar-close-tab))
      (magit-worktree-delete worktree-path)
      (magit-branch-delete branch-name)

      (message "Deleted worktree and branch: %s" branch-name))))

(defun +projects-merge-and-cleanup ()
  "Merge current worktree branch to main, then delete worktree and branch."
  (interactive)
  (let ((worktree-path (+projects--current-worktree-path)))
    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))

    (unless (+projects--git-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes. Please commit or stash changes before merge"))

    (let* ((branch-name (+projects--worktree-branch worktree-path))
           ;; Use toplevel to get the main repository, not the worktree
           (toplevel (magit-toplevel))
           (default-directory toplevel))

      (let ((default-branch (+projects--default-branch)))
        (unless (yes-or-no-p (format "Merge branch '%s' to %s and cleanup? "
                                     branch-name
                                     default-branch))
          (user-error "Merge cancelled"))

        (message "Switching to %s in main repository..." default-branch)

        (unless (zerop (magit-call-git "checkout" default-branch))
          (user-error "Failed to checkout %s" default-branch))

        (message "Merging %s into %s..." branch-name default-branch)

        (unless (zerop (magit-call-git "merge" "--no-edit" branch-name))
          (user-error "Merge failed - please resolve conflicts manually"))

        (magit-refresh)

        (when tab-bar-mode
          (tab-bar-close-tab))

        (magit-worktree-delete worktree-path)
        (magit-branch-delete branch-name)

        (message "Merged %s to %s and cleaned up" branch-name default-branch)))))

(defun +projects-rebase-on-main ()
  "Rebase the current worktree branch on main."
  (interactive)
  (let ((worktree-path (+projects--current-worktree-path)))
    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))

    (unless (+projects--git-clean-p worktree-path)
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

;;; Frame-per-project operations

(defun +projects--frame-for-project (project-root)
  "Find the frame associated with PROJECT-ROOT, if any."
  (seq-find (lambda (frame)
              (equal project-root (frame-parameter frame 'project-root)))
            (frame-list)))

(defun +projects--project-frame-root (&optional frame)
  "Get the project root for FRAME (defaults to current frame)."
  (frame-parameter frame 'project-root))

(defun +projects--open-project-frame (project-root)
  "Open or switch to a frame for PROJECT-ROOT."
  (if-let* ((existing-frame (+projects--frame-for-project project-root)))
      ;; Switch to existing frame
      (select-frame-set-input-focus existing-frame)

    ;; Create new frame
    (let* ((project-name (file-name-nondirectory (directory-file-name project-root)))
           (new-frame (make-frame `((name . ,project-name)
                                    (project-root . ,project-root)))))
      (select-frame-set-input-focus new-frame)
      ;; Open project root in dired
      (dired project-root)
      ;; Start claude-code-ide for this project
      (when (fboundp 'claude-code-ide)
        (let ((default-directory project-root))
          (claude-code-ide)))
      (message "Opened frame for project: %s" project-name))))

(defun +projects-switch-project-frame ()
  "Switch to a project in a dedicated frame.
Shows a list of known projects. Selecting a project will either
switch to an existing frame for that project or create a new one."
  (interactive)
  (let* ((projects (project-known-project-roots))
         (project-names (mapcar (lambda (p)
                                  (file-name-nondirectory (directory-file-name p)))
                                projects))
         (project-alist (cl-mapcar #'cons project-names projects))
         (input (completing-read "Project: " project-names nil t)))
    (when-let* ((project-root (alist-get input project-alist nil nil #'equal)))
      (+projects--open-project-frame project-root))))

(provide 'mod-projects)

;;; mod-projects.el ends here
