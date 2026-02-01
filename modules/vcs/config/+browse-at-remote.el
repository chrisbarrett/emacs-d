;;; +browse-at-remote ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'browse-at-remote)

;; git-timemachine integration

(defvar git-timemachine-revision)

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
    (apply fn args)))

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

;;; +browse-at-remote.el ends here
