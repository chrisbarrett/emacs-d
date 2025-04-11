;;; +capture.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 's)

(cl-eval-when (compile)
  (require 'org-roam))

(defvar +capture-current-file-title-from-read nil
  "Contains the title entered by the user, for use in templates.")

(setq-hook! 'org-capture-after-finalize-hook
  +capture-current-file-title-from-read nil)

(defun +capture-litnote-function ()
  (let ((title (read-string "Title: ")))
    (setq +capture-current-file-title-from-read title)
    (find-file
     (file-name-concat org-roam-directory "litnotes" (format "%s.org" (s-snake-case title))))))

(provide '+capture)

;;; +capture.el ends here
