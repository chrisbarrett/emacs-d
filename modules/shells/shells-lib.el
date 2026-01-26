;;; lib.el --- Shell utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Utility functions for shell integration, including zoxide directory jumping.

;;; Code:

(require 'cl-lib)

(cl-eval-when (compile)
  (require 'eshell))

(autoload 'eshell/cd "em-dirs")
(autoload 'project-root "project")


;;; Zoxide integration

;;;###autoload
(defvar +eshell-suppress-zoxide-updates-p nil
  "Let-bound to t when programmatically cd'ing around.")

;;;###autoload
(defun +zoxide-query (query)
  "Query zoxide for directory matching QUERY."
  (with-temp-buffer
    (call-process "zoxide" nil t nil "query" "--exclude" default-directory "--" query)
    (when (string-match-p "no match found" (buffer-string))
      (user-error "%s" (string-trim (buffer-string))))
    (goto-char (point-min))
    (buffer-substring (line-beginning-position) (line-end-position))))

;;;###autoload
(defun +zoxide-add (dir)
  "Add DIR to zoxide database."
  (call-process "zoxide" nil 0 nil "add" dir))

;;;###autoload
(defun eshell/j (query &rest _)
  "Jump to directory matching QUERY using zoxide."
  (let ((+eshell-suppress-zoxide-updates-p t))
    (eshell/cd (+zoxide-query query))))

;;;###autoload
(defun eshell/g ()
  "Navigate to the Git root or project root."
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

(provide 'shells-lib)

;;; lib.el ends here
