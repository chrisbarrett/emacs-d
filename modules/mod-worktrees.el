;;; mod-worktrees.el --- Git worktree management with tabs -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements a zellij-inspired workflow for managing git worktrees
;; using Emacs tabs. Each tab represents a git worktree, allowing for isolated
;; contexts when working on different branches.
;;
;; For frame-per-project workflow, see mod-beframe.el.

;;; Code:

(require 'beads)
(require '+corelib)
(require 'f)
(require 'general)
(require 'magit)
(require 'project)
(require 'tab-bar)
(require 'transient)

(autoload 'dired-jump "dired-x")
(autoload 'claude-code-ide "claude-code-ide")

(transient-define-prefix +worktrees-menu ()
  "Transient menu for git worktree operations."
  [["Change"
    :if +worktrees--repo-root
    ("r" "Ready (work on issue)" +worktrees-work-on-issue)
    ("o" "Switch or new..." +worktrees-create-switch)]

   ["Update"
    :if +worktrees-tab-dedicated-to-child-p
    ("u" "Rebase on main" +worktrees-rebase-on-local-main)
    ("U" "Rebase on origin/main" +worktrees-rebase-on-origin-main)]

   ["Complete"
    :if +worktrees-tab-dedicated-to-child-p
    ("m" "Merge to main" +worktrees-absorb-into-main)
    ("x" "Destroy" +worktrees-destroy-current)]]

  ["Beads Issues"
   :if +worktrees--repo-root
   ("n" "Create..." beads-issue-create)
   ("`" "Show log..." beads-process-show-log)]

  ["Show"
   :if +worktrees--repo-root
   ("g" "git status" +worktrees-magit-status)
   ("c" "claude-code" +worktrees-claude-code)])

;;; Configuration

(defvar +worktrees-worktree-base-dir ".worktrees"
  "Base directory name for creating new worktrees.")


;;; Helper functions

(defun +worktrees-in-repo-root-p (&optional dir)
  (when-let* ((repo-root (+worktrees--repo-root)))
    (f-same-p (or dir default-directory) repo-root)))

(defun +worktrees--repo-root ()
  "Get the repository root directory (not the .git directory)."
  (when-let* ((gitdir (magit-gitdir)))
    (file-name-directory (directory-file-name gitdir))))

(defun +worktrees--default-branch ()
  "Get the default branch name for the current repository."
  (let* ((remote (magit-primary-remote))
         (branch (and remote
                      (magit-git-string "symbolic-ref" "--short"
                                        (format "refs/remotes/%s/HEAD" remote)))))
    (or (and branch (cdr (magit-split-branch-name branch)))
        "main")))

(defun +worktrees--project-root ()
  (when-let* ((project (project-current)))
    (project-root project)))

(defun +worktrees--worktree-branch (worktree-path)
  (or
   (car (seq-keep (pcase-lambda (`(,path ,_commit ,branch . ,_))
                    (when (f-same-p path worktree-path)
                      branch))
                  (magit-list-worktrees)))
   ;; Guess based on target directory
   (file-name-nondirectory (directory-file-name worktree-path))))

(defun +worktrees--tab-for-worktree (worktree-path)
  "Find the tab associated with WORKTREE-PATH, if any.
Normalizes paths to handle trailing slashes correctly."
  (when tab-bar-mode
    (seq-find (lambda (tab)
                (when-let* ((tab-path (alist-get 'worktree-path tab)))
                  (f-same-p worktree-path tab-path)))
              (funcall tab-bar-tabs-function))))

(defun +worktrees--detect-child-worktree-path ()
  "Detect the worktree path for the current directory, if in a worktree.

Return nil if the current worktree is the root worktree for the repo."
  (when-let* ((project-root (+worktrees--project-root))
              (repo-root (+worktrees--repo-root))
              (worktrees
               ;; NOTE: Puts root worktree at the end, ensure that's matched
               ;; last.
               (nreverse (magit-list-worktrees)))
              (current-dir (expand-file-name default-directory)))
    ;; Find a worktree that contains the current directory
    (car (seq-keep (lambda (worktree-info)
                     (when-let* ((worktree-path (car worktree-info)))
                       (unless (+worktrees-in-repo-root-p worktree-path)
                         (when (f-descendant-of-p current-dir worktree-path)
                           worktree-path))))
                   worktrees))))

(defun +worktrees-tab-dedicated-to-child-p ()
  (+worktrees-path-for-selected-tab t))

(defun +worktrees-path-for-selected-tab (&optional exclude-root)
  "Get the worktree path for the current tab, if set.
If not set but we're in a worktree, detect and set it.
If EXCLUDE-ROOT is non-nil, return nil if the worktree is the repo root."
  (when tab-bar-mode
    (let ((current-tab (tab-bar--current-tab)))
      (when-let* ((worktree-path
                   (or (alist-get 'worktree-path current-tab)
                       ;; First time checking this tab--try to assign it to a worktree.
                       (when-let* ((detected (+worktrees--detect-child-worktree-path)))
                         (let ((tab (tab-bar--current-tab-find)))
                           (setf (alist-get 'worktree-path (cdr tab)) detected))
                         detected))))
        (if exclude-root
            (unless (+worktrees-in-repo-root-p worktree-path)
              worktree-path)
          worktree-path)))))

(defun +worktrees--worktree-clean-p (worktree-path)
  (let ((default-directory worktree-path))
    (not (magit-anything-modified-p))))

(defun +worktrees--ensure-claude-trust (worktree-path)
  "Ensure WORKTREE-PATH exists in ~/.claude.json projects."
  (require 'json)
  (let ((claude-config (expand-file-name "~/.claude.json")))
    (when (file-exists-p claude-config)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents claude-config)
            (let* ((json-object-type 'alist)
                   (json-key-type 'string)
                   (config (json-read))
                   (projects (or (alist-get "projects" config nil nil #'equal)
                                 (make-hash-table :test 'equal))))
              ;; Ensure projects is a hash table
              (unless (hash-table-p projects)
                (let ((new-projects (make-hash-table :test 'equal)))
                  (dolist (pair projects)
                    (puthash (car pair) (cdr pair) new-projects))
                  (setq projects new-projects)))
              ;; Add worktree-path if not present
              (unless (gethash worktree-path projects)
                (puthash worktree-path (make-hash-table :test 'equal) projects))
              ;; Update config
              (setf (alist-get "projects" config nil nil #'equal) projects)
              ;; Write back
              (erase-buffer)
              (insert (json-encode config))
              (write-region nil nil claude-config nil 'silent)))
        (error
         (message "Warning: Failed to update .claude.json: %s"
                  (error-message-string err)))))))


;;; Worktree operations

(defun +worktrees-create-switch ()
  "Read a worktree from the user and create a tab for it.

If no such worktree exists, create it."
  (interactive)
  (let* ((worktrees (magit-list-worktrees))
         (worktree-paths (seq-map #'car worktrees))
         (input (completing-read "Existing worktree, or name for new branch: " (remove (+worktrees-path-for-selected-tab) worktree-paths) nil
                                 (lambda (input)
                                   (or (member input worktree-paths)
                                       (not (string-match-p (rx space) input))))) ))
    (if (member input worktree-paths)
        (+worktrees-open-tab input)

      ;; User input is a new branch to create.
      (let* ((branch input)
             (start-point (magit-read-branch-or-commit "Start worktree at"))
             (path (file-name-concat (+worktrees--repo-root) +worktrees-worktree-base-dir branch)))
        (make-directory (file-name-directory path) t)
        (magit-worktree-branch path branch start-point)
        (+worktrees-open-tab path)))))

(defun +worktrees-claude-code (worktree-path)
  "Run claude-code for the worktree at WORKTREE-PATH."
  (interactive (list (or (+worktrees-path-for-selected-tab)
                         (user-error "Selected tab not associated with a worktree"))))
  (+worktrees--ensure-claude-trust worktree-path)
  (let ((default-directory worktree-path)
        (project-find-functions nil))
    (ignore-errors
      (claude-code-ide))))

(defun +worktrees-adopt-initial-tab ()
  "Set up the initial tab to represent the repo root."
  (when tab-bar-mode
    (let* ((repo-root (+worktrees--repo-root))
           (project-name (or (and (frame-parameter nil 'project-root)
                                  (file-name-nondirectory
                                   (directory-file-name (frame-parameter nil 'project-root))))
                             (file-name-nondirectory
                              (directory-file-name repo-root))))
           (current-tab (tab-bar--current-tab-find)))
      ;; Associate current tab with repo root
      (setf (alist-get 'worktree-path (cdr current-tab)) repo-root)
      (setf (alist-get 'worktree-type (cdr current-tab)) 'root)
      ;; Rename to project name
      (tab-bar-rename-tab project-name)
      t)))

(defun +worktrees-open-tab (worktree-path &optional issue)
  "Open a tab for WORKTREE-PATH, creating it if needed.

If ISSUE is provided, write a markdown description of the issue into the
new worktree to hand over the context to a dedicated claude-code
instance."
  (interactive (list (completing-read "Worktree: " (magit-list-worktrees) nil t)))
  (if-let* ((existing-tab (+worktrees--tab-for-worktree worktree-path)))
      (tab-bar-select-tab (1+ (tab-bar--tab-index existing-tab)))
    ;; Initialise new tab
    (let ((tab-name (+worktrees--worktree-branch worktree-path)))
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      ;; Store worktree path in the current tab
      (let ((current-tab (tab-bar--current-tab-find)))
        (setf (alist-get 'worktree-type (cdr current-tab)) 'child)
        (setf (alist-get 'worktree-path (cdr current-tab)) worktree-path)))

    (magit-status-setup-buffer worktree-path)
    (+worktrees-claude-code worktree-path))


  ;; If an issue was provided, save issue context for Claude
  (when issue
    (let* ((id (alist-get 'id issue))
           (title (alist-get 'title issue))
           (description (alist-get 'description issue))
           (claude-dir (expand-file-name ".claude" worktree-path))
           (context-file (expand-file-name "issue-context.md" claude-dir))
           (prompt (format "# Working on: %s\n\n## %s\n\n%s"
                           id title (or description ""))))
      ;; Ensure .claude directory exists
      (make-directory claude-dir t)
      (with-current-buffer (get-buffer-create "*claude-issue-context*")
        (erase-buffer)
        (insert prompt)
        (write-region (point-min) (point-max) context-file nil 'silent))
      (message "Issue context saved for Claude: %s" id))))

(defun +worktrees-open-epic-tab (worktree-path)
  "Open epic tab for WORKTREE-PATH.

Shows magit status on left and logs directory on right with refresh-on-focus."
  (if-let* ((existing-tab (+worktrees--tab-for-worktree worktree-path)))
      (tab-bar-select-tab (1+ (tab-bar--tab-index existing-tab)))
    ;; Initialise new tab
    (let ((tab-name (+worktrees--worktree-branch worktree-path)))
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      ;; Store worktree path in the current tab
      (let ((current-tab (tab-bar--current-tab-find)))
        (setf (alist-get 'worktree-type (cdr current-tab)) 'epic)
        (setf (alist-get 'worktree-path (cdr current-tab)) worktree-path)))
    (magit-status-setup-buffer worktree-path)
    (+worktrees-claude-code worktree-path)))

(defun +worktrees-open-subagent-tab (worktree-path)
  "Open orchestrator monitoring tab for WORKTREE-PATH.

Shows magit status on left and logs directory on right with refresh-on-focus."
  (if-let* ((existing-tab (+worktrees--tab-for-worktree worktree-path)))
      (tab-bar-select-tab (1+ (tab-bar--tab-index existing-tab)))
    ;; Initialise new tab
    (let ((tab-name (+worktrees--worktree-branch worktree-path)))
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      ;; Store worktree path in the current tab
      (let ((current-tab (tab-bar--current-tab-find)))
        (setf (alist-get 'worktree-type (cdr current-tab)) 'subagent)
        (setf (alist-get 'worktree-path (cdr current-tab)) worktree-path)))
    (magit-status-setup-buffer worktree-path)))

(defun +worktrees--refresh-logs-buffer-on-focus (window)
  "Refresh logs buffer when WINDOW is selected, if marked for refresh-on-focus."
  (when (and (eq window (selected-window))
             (buffer-local-value '+worktrees--refresh-on-focus (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'dired-mode)
        (revert-buffer nil t)))))  ; NOCONFIRM PRESERVE-MODES

(defun +worktrees--refresh-orchestrator-tab (&rest _)
  "Refresh logs buffer in orchestrator tabs when switching to them.
Hook function for `tab-bar-post-select-functions'."
  (when tab-bar-mode
    (let ((current-tab (tab-bar--current-tab)))
      (when (eq (alist-get 'worktree-type current-tab) 'orchestrator)
        ;; Find the logs dired buffer in this tab and refresh it
        (dolist (window (window-list))
          (with-current-buffer (window-buffer window)
            (when (and (derived-mode-p 'dired-mode)
                       (buffer-local-value '+worktrees--refresh-on-focus (current-buffer)))
              (revert-buffer nil t))))))))

;; Refresh orchestrator logs when switching tabs
(add-hook 'tab-bar-post-select-functions #'+worktrees--refresh-orchestrator-tab)

(defun +worktrees--branch-name-from-issue (issue)
  "Generate a git branch name from ISSUE."
  (let* ((id (alist-get 'id issue))
         (title (alist-get 'title issue))
         ;; Sanitize title: lowercase, replace spaces/special chars with hyphens
         (sanitized (downcase
                     (replace-regexp-in-string
                      "[^a-z0-9-]+" "-"
                      (replace-regexp-in-string "^[^a-z0-9]+\\|[^a-z0-9]+$" "" title)))))
    (format "%s-%s" id sanitized)))

(defun +worktrees-work-on-issue ()
  "Pick an issue from beads and create/switch to a worktree for it.
If the worktree already exists, switch to it. Otherwise, create a new
worktree with a branch name derived from the issue ID and title."
  (interactive)
  (unless (+worktrees--project-root)
    (user-error "Not in a git project"))

  (when-let* ((issue (+beads-pick-issue))
              (branch-name (+worktrees--branch-name-from-issue issue))
              (worktrees (magit-list-worktrees))
              (default-directory (+worktrees--repo-root)))

    ;; Check if worktree already exists for this branch
    (if-let* ((existing-worktree
               (seq-find (pcase-lambda (`(,_path ,_commit ,branch . ,_))
                           (equal branch branch-name))
                         worktrees)))
        (progn
          (message "Switching to existing worktree for %s" branch-name)
          (+worktrees-open-tab (car existing-worktree) issue))

      ;; Create new worktree
      (let* ((worktree-path (file-name-concat (+worktrees--repo-root)
                                              +worktrees-worktree-base-dir
                                              branch-name)))
        (message "Creating worktree for issue %s..." (alist-get 'id issue))
        (make-directory (file-name-directory worktree-path) t)
        (magit-run-git "worktree" "add" "-b" branch-name
                       (magit--expand-worktree worktree-path)
                       "HEAD")
        (+worktrees-open-tab worktree-path issue)

        ;; Update beads to mark issue as in_progress
        (let ((issue-id (alist-get 'id issue)))
          (unless (zerop (call-process "bd" nil nil nil
                                       "update" issue-id
                                       "--status" "in_progress"
                                       "--no-daemon"))
            (message "Warning: Failed to update issue %s status" issue-id))
          (message "Issue %s marked as in_progress" issue-id))))))

;;; Worktree management commands

(defun +worktrees-destroy-current ()
  "Delete the current tab, worktree, and associated branch.
Requires a clean working tree (no uncommitted changes)."
  (interactive)
  (let ((worktree-path (+worktrees-path-for-selected-tab)))
    (when (+worktrees-in-repo-root-p worktree-path)
      (user-error "Refusing to act on repo root worktree"))

    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))

    (unless (+worktrees--worktree-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes"))

    (let ((branch-name (+worktrees--worktree-branch worktree-path)))
      (unless (yes-or-no-p (format "Delete worktree and branch '%s'?" branch-name))
        (user-error "Aborted without changes"))

      (+worktrees-close-tabs worktree-path)

      (let ((magit-no-confirm '(trash))
            (default-directory (+worktrees--repo-root)))
        (magit-worktree-delete worktree-path)
        (magit-run-git "branch" "-D" branch-name))
      (message "Deleted worktree and branch: %s" branch-name))))

(defun +worktrees-absorb-into-main ()
  "Merge current worktree branch to main, then delete worktree and branch."
  (interactive)
  (let* ((default-directory (+worktrees--repo-root))
         (worktree-path (+worktrees-path-for-selected-tab))
         (worktree-branch (+worktrees--worktree-branch worktree-path))
         (default-branch (+worktrees--default-branch)))

    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))
    (when (+worktrees-in-repo-root-p worktree-path)
      (user-error "Refusing to act on repo root worktree"))

    (unless (+worktrees--worktree-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes"))

    (unless (yes-or-no-p (format "Merge branch `%s' into `%s' and delete worktree? "
                                 worktree-branch
                                 default-branch))
      (user-error "Aborted without changes"))

    (+worktree--kill-worktree-buffers worktree-path)
    (magit-worktree-delete worktree-path) ; if this fails, the branch is still around for recovery.
    (magit-run-git "switch" default-branch)
    (magit-merge-absorb worktree-branch) ; NB. async

    (when tab-bar-mode
      (tab-bar-close-tab))))

(defun +worktrees-rebase-on-local-main ()
  "Rebase the current worktree branch on local main branch."
  (interactive)
  (let ((worktree-path (+worktrees-path-for-selected-tab)))
    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))
    (when (+worktrees-in-repo-root-p worktree-path)
      (user-error "Refusing to act on root worktree"))

    (unless (+worktrees--worktree-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes. Please commit or stash changes before rebase"))

    (let* ((branch-name (+worktrees--worktree-branch worktree-path))
           (default-directory worktree-path)
           (default-branch (+worktrees--default-branch)))

      (message "Rebasing %s on local %s..." branch-name default-branch)
      ;; Use synchronous rebase on local branch
      (unless (zerop (magit-call-git "rebase" default-branch))
        (user-error "Rebase failed - resolve conflicts and run 'git rebase --continue'"))

      (magit-refresh)
      (message "Rebase successful: %s rebased on local %s"
               branch-name default-branch))))

(defun +worktrees-rebase-on-origin-main ()
  "Rebase the current worktree branch on main."
  (interactive)
  (let ((worktree-path (+worktrees-path-for-selected-tab)))
    (unless worktree-path
      (user-error "Current tab is not associated with a worktree"))
    (when (+worktrees-in-repo-root-p worktree-path)
      (user-error "Refusing to act on root worktree"))

    (unless (+worktrees--worktree-clean-p worktree-path)
      (user-error "Worktree has uncommitted changes. Please commit or stash changes before rebase"))

    (let* ((branch-name (+worktrees--worktree-branch worktree-path))
           (default-directory worktree-path)
           (default-branch (+worktrees--default-branch)))

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

;;; Tab & buffer cleanup

(defun +worktree--kill-worktree-buffers (worktree-path)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (string-prefix-p worktree-path default-directory)
        (unless (get-buffer-process buf)
          (kill-buffer buf))))))

(defun +worktrees-close-tabs (worktree-path)
  "Close any tabs in any frames that are dedicated to WORKTREE-PATH.
Returns a list of closed tab names."
  (let (closed-tabs)
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (when-let* ((tab (+worktrees--tab-for-worktree worktree-path)))
          (let ((tab-name (alist-get 'name tab)))
            (+worktree--kill-worktree-buffers worktree-path)
            ;; Runs `+worktrees--cleanup-worktree-tab'.
            (let* ((tabs (funcall tab-bar-tabs-function))
                   (tab-index (seq-position tabs tab)))
              (when tab-index
                ;; tab-bar-close-tab uses 1-based indexing
                (tab-bar-close-tab (1+ tab-index))))
            (push tab-name closed-tabs)))
        (force-mode-line-update t)))
    (when closed-tabs
      (message (concat "Closed worktree tab(s) for " (propertize (abbreviate-file-name worktree-path) 'face 'dired-directory)))
      closed-tabs)))

(defun +worktrees--cleanup-worktree-tab (tab _sole-tab)
  "Clean up resources when a worktree TAB is closed."
  (when-let* ((worktree-path (alist-get 'worktree-path tab)))
    (when (fboundp 'claude-code-ide-stop)
      (let ((default-directory worktree-path)
            (project-find-functions nil)
            (kill-buffer-query-functions nil))
        (ignore-errors
          (claude-code-ide-stop))))

    (+worktree--kill-worktree-buffers worktree-path)))

(add-hook 'tab-bar-tab-pre-close-functions #'+worktrees--cleanup-worktree-tab)

;;; Magit integration

(defun +worktrees-magit-status ()
  "Display magit status buffer, for the current worktree if appropriate."
  (interactive)
  (with-no-warnings
    (magit-status (+worktrees-path-for-selected-tab))))

(provide 'mod-worktrees)

;;; mod-worktrees.el ends here
