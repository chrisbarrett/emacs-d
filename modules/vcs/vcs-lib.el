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

(provide 'vcs-lib)

;;; vcs-lib.el ends here
