;;; init-readonly.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'init-elpaca)

;;; Open some files as read-only, e.g. vendored deps.


(defun +file-should-be-opened-read-only-p (file)
  (let ((file (file-truename file)))
    (and
     ;; matches truthy
     (string-match-p (rx (or
                          ;;; These files should be read-only...

                          "/vendor/"
                          "/elpaca/"
                          "/node_modules/"

                          ))
                     file)
     (not
      ;; matches falsey
      (string-match-p (rx-to-string `(or
                           ;;; ...except when they match these patterns.
                                      "/.git/" ; Ensure we can still use git.
                                      ,@+chrisbarrett-elpaca-repos))
                      file)))))

(add-hook! 'find-file-hook
  (when (+file-should-be-opened-read-only-p (buffer-file-name))
    (read-only-mode +1)))

(provide 'init-readonly)

;;; init-readonly.el ends here
