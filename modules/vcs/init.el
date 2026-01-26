;;; vcs-init.el --- VCS initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Git porcelain, worktree management, and forge integration.

;;; Code:

(require '+corelib)

;;; Transient - pop-up command menus

(use-package transient :ensure t
  :general-config
  (:keymaps 'transient-map [escape] #'transient-quit-one))

;;; Magit - git porcelain

(use-package magit :ensure t
  :custom
  (magit-display-buffer-function #'+magit-display-buffer-same-frame)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-diff-visit-prefer-worktree t)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-insert-related-refs nil)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :config
  ;; Set emoji cache file path after no-littering is available
  (when (boundp 'no-littering-var-directory)
    (setq +git-commit-emoji-cache-file
          (expand-file-name "github-emoji.json" no-littering-var-directory)))

  ;; Emoji display in commit and revision buffers
  (add-hook 'git-commit-mode-hook #'+git-commit-enable-emoji-display)
  (add-hook 'magit-revision-mode-hook #'+git-commit-enable-emoji-display)

  ;; S-RET to visit file but keep magit focused
  (general-def :keymaps 'magit-diff-section-map
    "S-<return>" #'+magit-diff-visit-file-unselected)

  ;; Automatically enter insert state on empty commit message
  (add-hook! 'git-commit-mode-hook
    (when (and (bolp) (eolp))
      (evil-insert-state)))

  ;; Add worktree prune command
  (transient-append-suffix 'magit-worktree "g"
    '("p" "Prune" +magit-worktree-prune))

  ;; Pulsar integration
  (with-eval-after-load 'pulsar
    (pushnew! pulsar-pulse-functions '+magit-diff-visit-file-unselected))

  ;; Log margin fix for issue with attempt to use function name as number
  (define-advice magit-log-format-author-margin (:override (author date) handle-error)
    "Fix issue with attempt to use function name as a number directly."
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
                                     (if (functionp width)
                                         (funcall width style details details-width)
                                       width)
                                     (if details (1+ details-width) 0)))
                            cnt unit)))
                'magit-log-date))))))

;;; Beads - issue tracker integration

(use-package beads :ensure (emacs-beads :host github :repo "chrisbarrett/emacs-beads" :main "beads.el")
  :commands (beads-issue-create
             beads-process-call
             beads-process-show-log)
  :custom
  (beads-worktree-root-function #'+worktrees-path-for-selected-tab))

;;; Git Timemachine - browse file history

(use-package git-timemachine :ensure t
  :general-config
  (:states 'normal
   :keymaps 'git-timemachine-mode-map
   "C-p" #'git-timemachine-show-previous-revision
   "C-n" #'git-timemachine-show-next-revision
   "gb"  #'git-timemachine-blame
   "gtc" #'git-timemachine-show-commit)

  :custom
  (git-timemachine-show-minibuffer-details t)

  :config
  ;; git-timemachine uses `delay-mode-hooks', which can suppress font-lock.
  (add-hook 'git-timemachine-mode-hook #'font-lock-mode)
  ;; Ensure evil keymaps are applied
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)

  ;; Show information in header-line for better visibility.
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

;;; Browse at Remote

(use-package browse-at-remote :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :config
  ;; Emacs 31 compatibility - override functions affected by vc-git--call signature change
  (define-advice browse-at-remote--get-local-branch (:override () emacs-31-compat)
    "Get the local branch name, returning `main' in detached state."
    (or (car (vc-git-branches)) "main"))

  (define-advice browse-at-remote--get-remote-url (:override (remote) emacs-31-compat)
    "Get URL of REMOTE from current repo."
    (string-trim-right (shell-command-to-string (format "git ls-remote --get-url %s" remote))))

  (define-advice browse-at-remote--get-remotes (:override () emacs-31-compat)
    "Get a list of known remotes."
    (let ((remotes (string-trim (shell-command-to-string "git remote"))))
      (unless (string-empty-p remotes)
        (split-string remotes "\n" t))))

  (define-advice browse-at-remote--get-from-config (:override (key) emacs-31-compat)
    "Get git config value for KEY."
    (string-trim (shell-command-to-string (format "git config --get %s" key))))

  ;; Git timemachine integration
  (define-advice browse-at-remote-get-url (:around (fn &rest args) git-timemachine-integration)
    "Teach `browse-at-remote' to show rev from `git-timemachine' session."
    (if (bound-and-true-p git-timemachine-mode)
        (let* ((start-line (line-number-at-pos (min (region-beginning) (region-end))))
               (end-line (line-number-at-pos (max (region-beginning) (region-end))))
               (remote-ref (browse-at-remote--remote-ref buffer-file-name))
               (remote (car remote-ref))
               (ref (car git-timemachine-revision))
               (relname
                (file-relative-name
                 buffer-file-name (expand-file-name (vc-git-root buffer-file-name))))
               (target-repo (browse-at-remote--get-url-from-remote remote))
               (remote-type (browse-at-remote--get-remote-type target-repo))
               (repo-url (cdr target-repo))
               (url-formatter (browse-at-remote--get-formatter 'region-url remote-type)))
          (unless url-formatter
            (error (format "Origin repo parsing failed: %s" repo-url)))
          (funcall url-formatter repo-url ref relname
                   (if start-line start-line)
                   (if (and end-line (not (equal start-line end-line))) end-line)))
      (apply fn args))))

;;; Forge - GitHub/GitLab PR integration

(use-package forge :ensure t
  :after-call magit-status
  :general
  (:keymaps 'magit-mode-map [remap magit-browse-thing] #'forge-browse)
  (:keymaps 'magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote)
  (:keymaps 'magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch)
  (:keymaps 'forge-topic-list-mode-map :states 'normal "q" #'kill-current-buffer))

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

(use-package git-auto-commit-mode :ensure t
  :custom
  (gac-silent-message-p t)
  :init
  (put 'gac-debounce-interval 'safe-local-variable 'integerp)
  (put 'gac-automatically-add-new-files-p 'safe-local-variable 'booleanp))

(provide 'vcs-init)

;;; vcs-init.el ends here
