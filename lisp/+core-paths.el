;;; core-paths.el -- paths in this emacs configuration -*- lexical-binding: t; -*-

(defvar +config-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of this Emacs configuration.
Derived from the location of +core-paths.el (lisp/+core-paths.el).")

(defvar +lisp-dir (file-name-concat +config-root "lisp/"))
(defvar +init-dir (file-name-concat +config-root "init/"))
(defvar +config-dir (file-name-concat +config-root "config/"))
(defvar +modules-directory (file-name-concat +config-root "modules/"))

(provide '+core-paths)

;;; core-paths.el ends here
