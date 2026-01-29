;;; project-init.el --- Project configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Frame-isolated project management using built-in project.el and beframe.

;;; Code:

(require '+autoloads)

(require '+corelib)

;; project-lib functions/variables are autoloaded

(defvar org-directory)
(defvar eat-buffer-name)

(autoload 'eat "eat")
(autoload 'beframe-buffer-list "beframe")


;;; project.el configuration

(use-package project
  :custom
  (project-vc-ignores '(".cache/"))
  (project-list-exclude (list
                         (rx bol "/nix/store/")
                         (defun +project-exclude-hidden-dirs (project)
                           "Exclude projects in any hidden directory, except for ~/.config."
                           (let ((root (project-root project)))
                             (and (string-match-p (rx "/.") root)
                                  (not (string-prefix-p "~/.config/" root)))))))
  (project-switch-commands '+project-switch-magit-status)

  :functions project-try-vc

  :config
  (project-remember-project (project-try-vc user-emacs-directory))
  (project-remember-project (project-try-vc org-directory))

  (define-advice project-remember-projects-under (:before (dir &optional recursive) save-for-rescan)
    (alist-set! +project-scan-dirs-alist dir recursive))

  :general-config
  (:keymaps 'project-prefix-map "R" #'+projects-rescan))


;;; Beframe for frame isolation

(defun eat-beframed (&optional arg)
  "Run an `eat' instance for the current beframe context.
With prefix argument ARG, create a new session."
  (interactive "P")
  (let ((eat-buffer-name (format "*eat %s*" (frame-parameter (selected-frame) 'name))))
    (eat nil arg)))

(defun project-switch-beframed (dir &optional force-same-frame)
  "Switch to project DIR with dedicated frame isolation.

If prefix arg FORCE-SAME-FRAME is set, switch in current frame without
creating a new one."
  (interactive (list (funcall project-prompter)
                     current-prefix-arg))
  (if force-same-frame
      (project-switch-project dir)

    (pcase-exhaustive (seq-find (lambda (frame)
                                  (equal dir (frame-parameter frame 'project-root)))
                                (frame-list))
      (`()
       ;; No frame found. Dedicate the initial frame or create a new one.
       (pcase (frame-list)
         ((and `(,sole-frame)
               (guard (null (frame-parameter sole-frame 'project-root))))
          ;; First frame--re-use this one.
          )
         (_
          ;; Initial frame has been dedicated; prepare to create a new one.
          (let ((message-log-max nil)
                (inhibit-message t))
            (other-frame-prefix))))

       (project-switch-project dir)
       (set-frame-parameter nil 'project-root dir)
       (let ((frame-name (or (+git-repo-display-name)
                             (file-name-nondirectory (expand-file-name dir)))))
         (set-frame-parameter nil 'name frame-name))
       (when (fboundp '+worktrees-adopt-initial-tab)
         (+worktrees-adopt-initial-tab)))

      ((and existing (guard (equal existing (selected-frame))))
       (project-switch-project dir))

      (existing
       ;; Switch to other frame
       (raise-frame existing)
       (select-frame existing)))))

(use-package beframe
  :demand t
  :config
  (define-advice project--read-file-name (:around (fn project &rest args) switch-frame)
    "Switch to a project's frame when reading a file or dir."
    (let ((result (apply fn project args)))
      (when +beframe-strict-project-isolation-p
        (unless (string= "" result)
          (project-switch-beframed (project-root project))))
      result))

  (define-advice consult--buffer-query (:filter-return (bufs-alist) with-beframe-restriction)
    "Restrict consult buffers to only those in the beframe context."
    (if (bound-and-true-p beframe-mode)
        (let ((in-scope (beframe-buffer-list)))
          (seq-filter
           (pcase-lambda (`(,_name . ,buf))
             (seq-contains-p in-scope buf))
           bufs-alist))
      bufs-alist))

  (beframe-mode +1)

  :general
  (:keymaps 'override-global-map
            "s-t" #'project-switch-beframed
            "M-W" #'project-switch-beframed)
  (:keymaps 'project-prefix-map
            "p" #'project-switch-beframed))



;;; init.el ends here
