;;; mod-beframe.el --- Configuration for beframe -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'beframe)
(require 'general)
(require 'project)

(autoload 'eat "eat")

(defvar org-directory)

(defvar +beframe-strict-project-isolation-p nil)



(defvar eat-buffer-name)

(defun eat-beframed (&optional arg)
  "Run an `eat' instance for the current beframe context."
  (interactive "P")
  (let ((eat-buffer-name (format "*eat %s*" (frame-parameter (selected-frame) 'name))))
    (eat nil arg)))



(defun project-switch-beframed (dir &optional force-same-frame)
  "A wrapper for `project-switch-project' to create dedicated project frames.

DIR is the project root.

If prefix arg FORCE-SAME-FRAME is set, then switch to the project in the
current beframe context."
  (interactive (list (funcall project-prompter)
                     current-prefix-arg))
  (if force-same-frame
      ;; no special handling.
      (project-switch-project dir)

    (pcase-exhaustive  (seq-find (lambda (frame)
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
          (let ((message-log-max nil))
            (other-frame-prefix))))

       (project-switch-project dir)
       (set-frame-parameter nil 'name (file-name-nondirectory dir))
       (set-frame-parameter nil 'project-root dir)
       (when (fboundp '+worktrees-adopt-initial-tab)
         (+worktrees-adopt-initial-tab)))

      ((and existing (guard (equal existing (selected-frame))))
       (project-switch-project dir))

      (existing
       ;; Switch to other frame
       (raise-frame existing)
       (select-frame existing)))))

(define-advice project--read-file-name (:around (fn project &rest args))
  "Switch to a project's frame when reading a file or dir."
  (let ((result (apply fn project args)))
    (when +beframe-strict-project-isolation-p
      (unless (string= "" result)
        (project-switch-beframed (project-root project))))
    result))

(define-advice consult--buffer-query (:filter-return (bufs-alist) with-beframe-restriction)
  "Restrict the buffers shown in consult to only the ones in the beframe context."
  (if (bound-and-true-p beframe-mode)
      (let ((in-scope (beframe-buffer-list)))
        (seq-filter
         (pcase-lambda (`(,_name . ,buf))
           (seq-contains-p in-scope buf))
         bufs-alist))
    bufs-alist))



(beframe-mode +1)

(provide 'mod-beframe)

;;; mod-beframe.el ends here
