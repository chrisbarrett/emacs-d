;;; mod-eshell.el --- Extra commands for eshell -*- lexical-binding: t; -*-

;;; Commentary:

;; Defines extra commands that should be available in Eshell sessions. I use
;; these to more closely align eshell with my regular shell.

;;; Code:

(require 'cl-lib)

(cl-eval-when (compile)
  (require 'eshell))

(autoload 'eshell/cd "em-dirs")
(autoload 'project-root "project")


;;; Teach eshell to use Zoxide for quickly jumping around; this aligns it with
;;; the behaviour my external shell.

(defun +zoxide-query (query)
  (with-temp-buffer
    (call-process "zoxide" nil t nil "query" "--exclude" default-directory "--" query)
    (when (string-match-p "no match found" (buffer-string))
      (user-error "%s" (string-trim (buffer-string))))
    (goto-char (point-min))
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun +zoxide-add (dir)
  (call-process "zoxide" nil 0 nil "add" dir))

(defvar +eshell-suppress-zoxide-updates-p nil
  "Let-bound to t when programmatically cd'ing around.")

(defun eshell/j (query &rest _)
  (let ((+eshell-suppress-zoxide-updates-p t))
    (eshell/cd (+zoxide-query query))))

(define-advice eshell/cd (:after (&rest args) update-zoxide)
  "Teach eshell to update Zoxide's index."
  (unless +eshell-suppress-zoxide-updates-p
    (when args
      (+zoxide-add default-directory))))



(defun eshell/g ()
  "Navigate to the Git root."
  (let (message-log-max)
    (if-let* ((dir (locate-dominating-file default-directory ".git")))
        (progn
          (message "Moving to git repository root")
          (eshell/cd dir))
      (if-let* ((dir (project-root)))
          (progn
            (message "Moving to project root")
            (eshell/cd dir))
        (user-error "Not in a project or git repo")))))

(provide 'mod-eshell)

;;; mod-eshell.el ends here
