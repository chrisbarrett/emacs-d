;;; mod-magit.el --- Magit configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'magit)
(require 'general)
(require 'no-littering)

(cl-eval-when (compile)
  (require 'pulsar nil t))

(autoload 'evil-insert-state "evil-states")

;;; Display buffer configuration

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

;;; Emoji rendering support

(require 'json)
(require 'url)

(defvar +git-commit-emoji-data-url
  "https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json"
  "URL to GitHub's official emoji data (gemoji project).")

(defvar +git-commit-emoji-cache-file
  (expand-file-name "github-emoji.json" no-littering-var-directory)
  "Path to cached emoji data file.")

(defvar +git-commit-emoji-table nil
  "Hash table mapping emoji shortcodes (e.g., \":smile:\") to Unicode emoji.")

(defun +git-commit-emoji-download-data ()
  "Download GitHub emoji data from gemoji and cache it locally."
  (interactive)
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
  (when (file-exists-p +git-commit-emoji-cache-file)
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
    (if (file-exists-p +git-commit-emoji-cache-file)
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

(defun +git-commit-enable-emoji-display ()
  "Enable emoji display in `git-commit-mode' buffers."
  (font-lock-add-keywords nil '((+git-commit-fontify-emoji)) t))

(add-hook 'git-commit-mode-hook #'+git-commit-enable-emoji-display)

;; Also enable in magit-revision-mode and magit-log-mode for viewing commits
(add-hook 'magit-revision-mode-hook #'+git-commit-enable-emoji-display)
;; (add-hook 'magit-log-mode-hook #'+git-commit-enable-emoji-display)

(defun +magit-diff-visit-file-unselected ()
  "Visit the file at point but keep the magit window selected."
  (interactive)
  (let ((orig-window (selected-window)))
    (magit-diff-visit-file t)
    (select-window orig-window)))

(general-def :keymaps 'magit-diff-section-map
  "S-<return>" #'+magit-diff-visit-file-unselected)

(with-eval-after-load 'pulsar
  (pushnew! pulsar-pulse-functions '+magit-diff-visit-file-unselected))

;; Automatically enter insert state on empty commit message.
(add-hook! 'git-commit-mode-hook
  (when (and (bolp) (eolp))
    (evil-insert-state)))



;;; Worktree prune command

(defun +magit-worktree-prune ()
  "Prune stale worktree information."
  (interactive)
  (magit-run-git "worktree" "prune"))

(transient-append-suffix 'magit-worktree "g"
  '("p" "Prune" +magit-worktree-prune))

;;; Log margin fix

(define-advice magit-log-format-author-margin (:override (author date) handle-error)
  "Fix issue with attempt to use function name as a number directly.

See: beads:emacs-jtd"
  (pcase-let ((`(,_ ,style ,width ,details ,details-width)
               (or magit--right-margin-config
                   (symbol-value (magit--right-margin-option))
                   (error "No margin format specified for %s" major-mode))))
    (magit-make-margin-overlay
     (concat (and details
                  (concat (magit--propertize-face
                           (truncate-string-to-width
                            (or author "")
                            details-width
                            nil ?\s
                            (magit--ellipsis 'margin))
                           'magit-log-author)
                          " "))
             (magit--propertize-face
              (if (stringp style)
                  (format-time-string
                   style
                   (seconds-to-time (string-to-number date)))
                (pcase-let* ((abbr (eq style 'age-abbreviated))
                             (`(,cnt ,unit) (magit--age date abbr)))
                  (format (format (if abbr "%%2d%%-%dc" "%%2d %%-%ds")
                                  (-
                                   ;; HACK
                                   ;; See arglist for `magit-log-margin-width'.
                                   (if (functionp width)
                                       (funcall width style details details-width)
                                     width)
                                   ;; END HACK

                                   (if details (1+ details-width) 0)))
                          cnt unit)))
              'magit-log-date)))))

(provide 'mod-magit)

;;; mod-magit.el ends here
