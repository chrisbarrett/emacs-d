;;; lib.el --- UI module library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for the ui module.

;;; Code:

(require '+autoloads)


;;; Pulsar macro

;;;###autoload
(defmacro +pulsar--with-eval-pulse (start end &rest body)
  "Pulse a region to indicate whether a command ran or signalled an error.

START and END are the buffer locations to pulse after evaluating BODY.

START & END are evaluated after BODY has completed, and thus after any
buffer modifications have happened."
  (declare (indent 2))
  (let ((gfailed (gensym "failed-"))
        (gerr (gensym "err-"))
        (gstart (gensym "start-"))
        (gend (gensym "end-")))
    `(let (,gfailed)
       (unwind-protect
           (condition-case ,gerr
               (progn ,@body)
             (t
              (setq ,gfailed t)
              (signal (car ,gerr) (cdr ,gerr))))
         (deactivate-mark)
         (when (and (bound-and-true-p pulsar-mode)
                    (require 'pulsar nil t))
           (let ((,gstart ,start)
                 (,gend ,end))
             (when (and ,gstart ,gend)
               (let ((pulse-flag t)
                     (pulse-delay pulsar-delay)
                     (pulse-iterations pulsar-iterations))
                 (pulsar--create-pulse (cons ,gstart ,gend)
                                       (if ,gfailed 'pulsar-red 'pulsar-green))))))))))


;;; Display buffer functions

;;;###autoload
(defun +display-buffer-reuse-non-dedicated-window (buffer alist)
  "Reuse a non-dedicated window for BUFFER when navigating.
Only works for specific navigation commands like `dired-find-file',
`next-error', etc.  ALIST is a `display-buffer' action alist."
  (when (member this-command '(dired-find-file
                               next-error
                               previous-error
                               compile-goto-error
                               push-button
                               magit-diff-visit-file
                               +magit-diff-visit-file-unselected))
    (let ((candidates (seq-remove (lambda (it)
                                    (or (+side-window-p it)
                                        (window-dedicated-p it)))
                                  (window-list))))
      (pcase candidates
        (`(,sole-window)
         (window--display-buffer buffer sole-window 'reuse alist))))))

;;;###autoload
(defun +display-buffer-fallback (buffer &rest _)
  "Sensible split fallback for displaying BUFFER."
  (when-let* ((win (split-window-sensibly)))
    (with-selected-window win
      (switch-to-buffer buffer)
      (help-window-setup (selected-window)))
    t))


;;; Goto address helper

;;;###autoload
(defun +goto-address-maybe-h ()
  "Enable `goto-address-mode' unless in `org-mode' (which handles URLs natively)."
  (unless (derived-mode-p 'org-mode 'org-agenda-mode)
    (goto-address)
    (goto-address-mode +1)))


;;; lib.el ends here
