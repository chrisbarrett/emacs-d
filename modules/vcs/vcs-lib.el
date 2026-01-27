;;; vcs-lib.el --- VCS library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for VCS module.

;;; Code:

(require 'json)

(autoload 'magit-primary-remote "magit-git")
(autoload 'magit-git-string "magit-git")
(autoload 'magit-gitdir "magit-git")
(autoload 'magit-list-worktrees "magit-git")

(defvar no-littering-var-directory)

;;; Git repo display name

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

;;; Emoji support

(defvar +git-commit-emoji-data-url
  "https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json"
  "URL to GitHub's official emoji data (gemoji project).")

(defvar +git-commit-emoji-cache-file nil
  "Path to cached emoji data file.
Set during init after no-littering is loaded.")

(defvar +git-commit-emoji-table nil
  "Hash table mapping emoji shortcodes (e.g., \":smile:\") to Unicode emoji.")

;;;###autoload
(defun +git-commit-emoji-download-data ()
  "Download GitHub emoji data from gemoji and cache it locally."
  (interactive)
  (unless +git-commit-emoji-cache-file
    (user-error "Emoji cache file path not configured"))
  (let ((url-request-method "GET"))
    (url-retrieve
     +git-commit-emoji-data-url
     (lambda (status)
       (if (plist-get status :error)
           (message "Failed to download emoji data: %s" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "^$")
         (let ((json-data (buffer-substring-no-properties (point) (point-max)))
               (coding-system-for-write 'utf-8))
           (with-temp-file +git-commit-emoji-cache-file
             (insert json-data))
           (message "Emoji data downloaded and cached successfully")
           (+git-commit-emoji-load-data)))))))

(defun +git-commit-emoji-load-data ()
  "Load emoji data from cache and build hash table."
  (when (and +git-commit-emoji-cache-file
             (file-exists-p +git-commit-emoji-cache-file))
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'string)
           (coding-system-for-read 'utf-8)
           (data (json-read-file +git-commit-emoji-cache-file))
           (table (make-hash-table :test 'equal :size 2000)))
      (dolist (entry data)
        (let ((emoji (cdr (assoc "emoji" entry)))
              (aliases (cdr (assoc "aliases" entry))))
          (dolist (alias aliases)
            (puthash (concat ":" alias ":") emoji table))))
      (setq +git-commit-emoji-table table)
      (message "Loaded %d emoji mappings" (hash-table-count table)))))

(defun +git-commit-emoji-ensure-data ()
  "Ensure emoji data is loaded, downloading if necessary."
  (unless +git-commit-emoji-table
    (if (and +git-commit-emoji-cache-file
             (file-exists-p +git-commit-emoji-cache-file))
        (+git-commit-emoji-load-data)
      (message "Downloading GitHub emoji data...")
      (+git-commit-emoji-download-data))))

(defun +git-commit-fontify-emoji (limit)
  "Fontify emoji codes like :emoji: as actual emoji up to LIMIT."
  (+git-commit-emoji-ensure-data)
  (when +git-commit-emoji-table
    (catch 'found
      (while (re-search-forward ":\\([a-z0-9_]+\\):" limit t)
        (let* ((code (match-string 0))
               (emoji (gethash code +git-commit-emoji-table)))
          (when emoji
            (compose-region (match-beginning 0) (match-end 0) emoji)
            (throw 'found t))))
      nil)))

;;;###autoload
(defun +git-commit-enable-emoji-display ()
  "Enable emoji display in `git-commit-mode' buffers."
  (font-lock-add-keywords nil '((+git-commit-fontify-emoji)) t))

;;; Magit display buffer

;;;###autoload
(defun +magit-display-buffer-same-frame (buffer)
  "Display BUFFER in the same frame, preferring the selected window.
Similar to `magit-display-buffer-same-window-except-diff-v1' but
constrained to the current frame when using per-project frames."
  (display-buffer
   buffer
   (cond
    ;; If this is a diff buffer, display it in another window in the same frame
    ((with-current-buffer buffer
       (derived-mode-p 'magit-diff-mode))
     '((display-buffer-reuse-window
        display-buffer-below-selected
        display-buffer-in-previous-window)
       . ((inhibit-same-window . t)
          (reusable-frames . nil))))  ; nil means current frame only
    ;; For all other Magit buffers, use the same window
    (t
     '(display-buffer-same-window)))))

;;;###autoload
(defun +magit-diff-visit-file-unselected ()
  "Visit the file at point but keep the magit window selected."
  (interactive)
  (let ((orig-window (selected-window)))
    (magit-diff-visit-file t)
    (select-window orig-window)))

;;; Worktree prune

;;;###autoload
(defun +magit-worktree-prune ()
  "Prune stale worktree information."
  (interactive)
  (magit-run-git "worktree" "prune"))

;;; Worktrees workflow

(defvar +worktrees-worktree-base-dir ".worktrees"
  "Base directory name for creating new worktrees.")

(defun +worktrees--repo-root ()
  "Get the repository root directory (not the .git directory)."
  (when-let* ((gitdir (magit-gitdir)))
    (file-name-directory (directory-file-name gitdir))))

(defun +worktrees--repo-root-for-file (file-path)
  "Get the git repository root for FILE-PATH."
  (let ((default-directory (file-name-directory (expand-file-name file-path))))
    (+worktrees--repo-root)))

;;;###autoload
(defun +worktrees-path-for-selected-tab (&optional exclude-root)
  "Get the worktree path for the current tab, if set.
If not set but we're in a worktree, detect and set it.
If EXCLUDE-ROOT is non-nil, return nil if the worktree is the repo root."
  (when (bound-and-true-p tab-bar-mode)
    (let ((current-tab (tab-bar--current-tab)))
      (when-let* ((worktree-path (alist-get 'worktree-path current-tab)))
        (if exclude-root
            (let ((repo-root (+worktrees--repo-root)))
              (unless (and repo-root (file-equal-p worktree-path repo-root))
                worktree-path))
          worktree-path)))))

;;;###autoload
(defun +worktrees-tab-dedicated-to-child-p ()
  "Return non-nil if current tab is dedicated to a child worktree."
  (+worktrees-path-for-selected-tab t))

;;;###autoload
(defun +worktrees-set-alert (worktree-path)
  "Set a persistent alert on the tab associated with WORKTREE-PATH.
The alert will remain until the user dwells on the tab.
If no tab exists for the worktree, this function does nothing.
Returns t if alert was set, nil otherwise."
  (when-let* ((tab (+worktrees--tab-for-worktree worktree-path))
              (tab-name (alist-get 'name tab)))
    (when (fboundp '+tab-bar-set-alert)
      (+tab-bar-set-alert tab-name))))

;;;###autoload
(defun +worktrees-set-transient-alert (worktree-path &optional color cycles)
  "Set a transient alert on the tab associated with WORKTREE-PATH.
COLOR is the pulse color (default pulsar-magenta).
CYCLES is the number of pulses (default 3).
Transient alerts play their animation and automatically clear.
If no tab exists for the worktree, this function does nothing.
Returns t if alert was dispatched, nil otherwise."
  (when-let* ((tab (+worktrees--tab-for-worktree worktree-path))
              (tab-name (alist-get 'name tab)))
    (when (fboundp '+tab-bar-set-transient-alert)
      (+tab-bar-set-transient-alert tab-name color cycles))))

;;;###autoload
(defun +worktrees-clear-alert (worktree-path)
  "Clear the persistent alert on the tab associated with WORKTREE-PATH.
If no tab exists for the worktree, this function does nothing.
Returns t if alert was cleared, nil otherwise."
  (when-let* ((tab (+worktrees--tab-for-worktree worktree-path))
              (tab-name (alist-get 'name tab)))
    (when (fboundp '+tab-bar-clear-alert)
      (+tab-bar-clear-alert tab-name))))

(defun +worktrees--tab-for-worktree (worktree-path)
  "Find the tab associated with WORKTREE-PATH, if any.
Normalizes paths to handle trailing slashes correctly."
  (when (bound-and-true-p tab-bar-mode)
    (seq-find (lambda (tab)
                (when-let* ((tab-path (alist-get 'worktree-path tab)))
                  (file-equal-p worktree-path tab-path)))
              (funcall tab-bar-tabs-function))))

;;;###autoload
(defun +worktrees-refresh-magit (worktree-path)
  "Refresh magit buffers for WORKTREE-PATH.
Find the tab showing WORKTREE-PATH, and if it exists, look through
the `magit-status' buffers in that frame and call `magit-refresh' with
each buffer as current.

Returns t if any refresh took place, otherwise nil."
  (when-let* ((tab (+worktrees--tab-for-worktree worktree-path)))
    (let ((refreshed nil)
          (tab-index (seq-position (funcall tab-bar-tabs-function) tab)))
      (when tab-index
        (dolist (frame (frame-list))
          (with-selected-frame frame
            (when (seq-find (lambda (tab-in-frame)
                              (equal (alist-get 'name tab-in-frame)
                                     (alist-get 'name tab)))
                            (funcall tab-bar-tabs-function))
              (dolist (buf (buffer-list frame))
                (with-current-buffer buf
                  (when (and (derived-mode-p 'magit-status-mode)
                             (string-prefix-p worktree-path default-directory))
                    (magit-refresh)
                    (setq refreshed t))))))))
      refreshed)))

;;;###autoload
(defun +worktrees-close-tabs (worktree-path)
  "Close any tabs in any frames that are dedicated to WORKTREE-PATH.
Returns a list of closed tab names."
  (let (closed-tabs)
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (when-let* ((tab (+worktrees--tab-for-worktree worktree-path)))
          (let ((tab-name (alist-get 'name tab)))
            (+worktree--kill-worktree-buffers worktree-path)
            (let* ((tabs (funcall tab-bar-tabs-function))
                   (tab-index (seq-position tabs tab)))
              (when tab-index
                (tab-bar-close-tab (1+ tab-index))))
            (push tab-name closed-tabs)))
        (force-mode-line-update t)))
    (when closed-tabs
      (message (concat "Closed worktree tab(s) for "
                       (propertize (abbreviate-file-name worktree-path) 'face 'dired-directory)))
      closed-tabs)))

(defun +worktree--kill-worktree-buffers (worktree-path)
  "Kill buffers associated with WORKTREE-PATH."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (string-prefix-p worktree-path default-directory)
        (unless (get-buffer-process buf)
          (kill-buffer buf))))))

;;; Worktrees menu and commands

(autoload 'magit-anything-modified-p "magit-git")
(autoload 'magit-call-git "magit-process")
(autoload 'magit-run-git "magit-process")
(autoload 'magit-refresh "magit-mode")
(autoload 'magit-status-setup-buffer "magit-status")
(autoload 'magit-worktree-branch "magit-worktree")
(autoload 'magit-worktree-delete "magit-worktree")
(autoload 'magit-merge-absorb "magit-merge")
(autoload 'magit-read-branch-or-commit "magit-refs")
(autoload 'magit--expand-worktree "magit-worktree")
(autoload 'transient-define-prefix "transient")

(autoload 'beads-issue-create "beads")
(autoload 'beads-process-show-log "beads")
(autoload 'beads-workon-issue "beads")

(autoload 'claude-code-ide "claude-code-ide")
(defvar claude-code-ide-cli-extra-flags nil)

(defun +worktrees--repo-root-for-selected-frame ()
  "Get the repo root for the selected frame's project."
  (let ((default-directory (or (frame-parameter (selected-frame) 'project-root)
                               default-directory)))
    (+worktrees--repo-root)))

(defun +worktrees-in-repo-root-p (&optional dir)
  "Return non-nil if DIR (or `default-directory') is the repo root."
  (when-let* ((repo-root (+worktrees--repo-root)))
    (file-equal-p (or dir default-directory) repo-root)))

(defun +worktrees--default-branch ()
  "Get the default branch name for the current repository."
  (let* ((remote (magit-primary-remote))
         (branch (and remote
                      (magit-git-string "symbolic-ref" "--short"
                                        (format "refs/remotes/%s/HEAD" remote)))))
    (or (and branch (cdr (magit-split-branch-name branch)))
        "main")))

(autoload 'magit-split-branch-name "magit-git")

(defun +worktrees--project-root ()
  "Get the project root via `project-current'."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun +worktrees--worktree-branch (worktree-path)
  "Get the branch name for WORKTREE-PATH."
  (or
   (car (seq-keep (pcase-lambda (`(,path ,_commit ,branch . ,_))
                    (when (file-equal-p path worktree-path)
                      branch))
                  (magit-list-worktrees)))
   (file-name-nondirectory (directory-file-name worktree-path))))

(defun +worktrees--worktree-clean-p (worktree-path)
  "Return non-nil if WORKTREE-PATH has no uncommitted changes."
  (let ((default-directory worktree-path))
    (not (magit-anything-modified-p))))

(defun +worktrees--detect-child-worktree-path ()
  "Detect the worktree path for the current directory, if in a worktree.
Return nil if the current worktree is the root worktree for the repo."
  (when-let* ((project-root (+worktrees--project-root))
              (repo-root (+worktrees--repo-root))
              (worktrees (nreverse (magit-list-worktrees)))
              (current-dir (expand-file-name default-directory)))
    (car (seq-keep (lambda (worktree-info)
                     (when-let* ((worktree-path (car worktree-info)))
                       (unless (+worktrees-in-repo-root-p worktree-path)
                         (when (string-prefix-p worktree-path current-dir)
                           worktree-path))))
                   worktrees))))

;;;###autoload
(defun +worktrees-adopt-initial-tab ()
  "Set up the initial tab to represent the repo root."
  (when (bound-and-true-p tab-bar-mode)
    (let* ((repo-root (file-truename (or (+worktrees--repo-root)
                                         (funcall project-prompter))))
           (project-name (or (+git-repo-display-name)
                             (and (frame-parameter nil 'project-root)
                                  (file-name-nondirectory
                                   (directory-file-name (frame-parameter nil 'project-root))))
                             (file-name-nondirectory
                              (directory-file-name repo-root))))
           (current-tab (tab-bar--current-tab-find)))
      (setf (alist-get 'worktree-path (cdr current-tab)) repo-root)
      (setf (alist-get 'worktree-type (cdr current-tab)) 'root)
      (tab-bar-rename-tab project-name)
      repo-root)))

(defvar project-prompter)

;;;###autoload
(defun +worktrees-open-tab (worktree-path &optional type)
  "Open a tab for WORKTREE-PATH, creating it if needed.
If TYPE is set, this will be used to determine the type of tab that will
be created. See generic function `+worktrees-new-tab-layout'."
  (interactive (list (completing-read "Worktree: " (magit-list-worktrees) nil t)))
  (if-let* ((existing-tab (+worktrees--tab-for-worktree worktree-path)))
      (tab-bar-select-tab (1+ (tab-bar--tab-index existing-tab)))
    (let ((tab-name (+worktrees--worktree-branch worktree-path))
          (type (cond
                 ((null type) 'branch)
                 ((symbolp type) type)
                 ((stringp type) (intern type))
                 (t 'branch))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (let* ((tabs (frame-parameter nil 'tabs))
             (tab-index (tab-bar--current-tab-index tabs))
             (current-tab (nth tab-index tabs))
             (tab-type (car current-tab))
             (tab-rest (cdr current-tab)))
        (setf (alist-get 'worktree-type tab-rest) type)
        (setf (alist-get 'worktree-path tab-rest) worktree-path)
        (setf (nth tab-index tabs) (cons tab-type tab-rest))
        (set-frame-parameter nil 'tabs tabs)))
    (+worktrees-new-tab-layout type worktree-path)))

(cl-defgeneric +worktrees-new-tab-layout (_type worktree-path)
  "Set up the layout for a new worktree tab.
TYPE is a symbol indicating the worktree type.
WORKTREE-PATH is the path to the worktree."
  (magit-status-setup-buffer worktree-path))

;;;###autoload
(defun +worktrees-create-switch ()
  "Read a worktree from the user and create a tab for it.
The worktrees are for the repo associated with the selected tab. If no
such worktree exists, create it."
  (interactive)
  (let* ((default-directory (or (+worktrees-path-for-selected-tab)
                                (+worktrees--repo-root)
                                default-directory))
         (worktree-paths (seq-keep (pcase-lambda (`(,path ,_rev ,branch . ,_rest))
                                     (unless (equal branch "beads-sync")
                                       path))
                                   (magit-list-worktrees)))
         (input (completing-read "Existing worktree, or name for new branch: "
                                 (remove (+worktrees-path-for-selected-tab) worktree-paths) nil
                                 (lambda (input)
                                   (or (member input worktree-paths)
                                       (not (string-match-p (rx space) input)))))))
    (if (member input worktree-paths)
        (+worktrees-open-tab input)
      (let* ((branch input)
             (start-point (magit-read-branch-or-commit "Start worktree at"))
             (path (file-name-concat (+worktrees--repo-root) +worktrees-worktree-base-dir branch)))
        (make-directory (file-name-directory path) t)
        (save-window-excursion
          (magit-worktree-branch path branch start-point))
        (+worktrees-open-tab path)))))

;;;###autoload
(defun +worktrees-magit-status ()
  "Display magit status buffer for the current worktree."
  (interactive)
  (with-no-warnings
    (magit-status (+worktrees-path-for-selected-tab))))

;;;###autoload
(defun +worktrees-claude-code (worktree-path &optional initial-command)
  "Run claude-code for the worktree at WORKTREE-PATH.
When INITIAL-COMMAND is provided, run that."
  (interactive (list (or (+worktrees-path-for-selected-tab)
                         (user-error "Selected tab not associated with a worktree"))))
  (let ((default-directory worktree-path)
        (project-find-functions nil)
        (claude-code-ide-cli-extra-flags (concat claude-code-ide-cli-extra-flags
                                                 (if initial-command
                                                     (shell-quote-argument initial-command)
                                                   ""))))
    (ignore-errors
      (claude-code-ide))))

(defun +worktrees--branch-name-from-issue (issue)
  "Generate a git branch name from ISSUE."
  (let* ((id (alist-get 'id issue))
         (title (alist-get 'title issue))
         (sanitized (downcase
                     (replace-regexp-in-string
                      "[^a-z0-9-]+" "-"
                      (replace-regexp-in-string "^[^a-z0-9]+\\|[^a-z0-9]+$" "" title)))))
    (format "%s-%s" id sanitized)))

;;;###autoload
(defun +worktrees-work-on-issue ()
  "Pick an issue from beads and create/switch to a worktree for it."
  (interactive)
  (unless (+worktrees--project-root)
    (user-error "Not in a git project"))
  (when-let* ((issue (beads-workon-issue))
              (branch-name (+worktrees--branch-name-from-issue issue))
              (worktrees (magit-list-worktrees))
              (default-directory (+worktrees--repo-root)))
    (if-let* ((existing-worktree
               (seq-find (pcase-lambda (`(,_path ,_commit ,branch . ,_))
                           (equal branch branch-name))
                         worktrees)))
        (progn
          (message "Switching to existing worktree for %s" branch-name)
          (+worktrees-open-tab (car existing-worktree)))
      (let* ((worktree-path (file-name-concat (+worktrees--repo-root)
                                              +worktrees-worktree-base-dir
                                              branch-name)))
        (message "Creating worktree for issue %s..." (alist-get 'id issue))
        (make-directory (file-name-directory worktree-path) t)
        (magit-run-git "worktree" "add" "-b" branch-name
                       (magit--expand-worktree worktree-path)
                       "HEAD")
        (+worktrees-open-tab worktree-path)
        (let ((issue-id (alist-get 'id issue)))
          (unless (zerop (call-process "bd" nil nil nil
                                       "update" issue-id
                                       "--status" "in_progress"
                                       "--no-daemon"))
            (message "Warning: Failed to update issue %s status" issue-id))
          (message "Issue %s marked as in_progress" issue-id))))))

;;;###autoload
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
        (magit-run-git "worktree" "prune")
        (magit-run-git "branch" "-D" branch-name))
      (message "Deleted worktree and branch: %s" branch-name))))

(defvar magit-no-confirm)

;;;###autoload
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
    (magit-worktree-delete worktree-path)
    (magit-run-git "worktree" "prune")
    (magit-run-git "switch" default-branch)
    (magit-merge-absorb worktree-branch)
    (when (bound-and-true-p tab-bar-mode)
      (tab-bar-close-tab))))

;;;###autoload
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
      (unless (zerop (magit-call-git "rebase" default-branch))
        (user-error "Rebase failed - resolve conflicts and run 'git rebase --continue'"))
      (magit-refresh)
      (message "Rebase successful: %s rebased on local %s"
               branch-name default-branch))))

;;;###autoload
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
        (unless (zerop (magit-call-git "fetch" remote))
          (user-error "Fetch failed"))
        (message "Rebasing %s on %s/%s..." branch-name remote default-branch)
        (unless (zerop (magit-call-git "rebase" (format "%s/%s" remote default-branch)))
          (user-error "Rebase failed - resolve conflicts and run 'git rebase --continue'"))
        (magit-refresh)
        (message "Rebase successful: %s rebased on %s/%s"
                 branch-name remote default-branch)))))

;;; Tab cleanup hook

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

;;; Transient menu

(require 'transient)

;;;###autoload (autoload '+worktrees-menu "vcs-lib" nil t)
(transient-define-prefix +worktrees-menu ()
  "Transient menu for git worktree operations."
  [["Change"
    :if +worktrees--repo-root-for-selected-frame
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
   :if +worktrees--repo-root-for-selected-frame
   ("n" "Create..." beads-issue-create)
   ("`" "Show log..." beads-process-show-log)]

  ["Show"
   :inapt-if-not +worktrees--repo-root-for-selected-frame
   ("g" "git status" +worktrees-magit-status)
   ("c" "claude-code" +worktrees-claude-code)])

(provide 'vcs-lib)

;;; vcs-lib.el ends here
