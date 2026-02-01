;;; +magit.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+autoloads)
(require '+corelib)
(require 'general)
(require 'magit)

;; Add worktree prune command

(transient-append-suffix 'magit-worktree "g"
  '("p" "Prune" +magit-worktree-prune))

;; Clean up tabs/buffers when deleting worktrees via magit

(define-advice magit-worktree-delete (:before (worktree &rest _) cleanup)
  "Cleanup before deleting WORKTREE via `magit-worktree-delete'.
Aborts if WORKTREE is the root worktree. Otherwise closes any
associated tabs and buffers."
  (let ((worktree-path (magit-convert-filename-for-git (expand-file-name worktree))))
    (when (+worktrees-in-repo-root-p worktree-path)
      (user-error "Cannot delete the root worktree"))
    (+worktrees-close-tabs worktree-path)))

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
  (when (and (fboundp 'evil-insert-state) (bolp) (eolp))
    (evil-insert-state)))

;; Log margin fix for issue with attempt to use function name as number

;; TODO: Still needed?

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
              'magit-log-date)))))

;;; +magit.el ends here
