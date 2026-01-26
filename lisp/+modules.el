;;; +modules.el --- Module system for self-contained config units.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides discovery and loading of self-contained modules from
;; `modules/` directory. Each module can contain:
;;
;; - spec.md        : module specification
;; - packages.eld   : elpaca specs (data only)
;; - init.el        : evaluated during init
;; - lib.el         : autoloaded functions
;; - lib/           : alternative: multiple lib files
;; - tests.el       : ERT tests

;;; Code:

(require 'cl-lib)

(defvar +modules-directory
  (expand-file-name "modules" user-emacs-directory)
  "Directory containing module directories.")

(defun +modules-discover ()
  "Discover all module directories under `+modules-directory'.

Returns a list of absolute paths to module directories. Each
module directory is expected to be a direct child of
`+modules-directory' containing at least one recognized module
file (init.el, lib.el, packages.eld, spec.md, or tests.el)."
  (when (file-directory-p +modules-directory)
    (let ((candidates (directory-files +modules-directory t "\\`[^.]" t)))
      (seq-filter #'+modules--valid-module-p candidates))))

(defun +modules--valid-module-p (dir)
  "Return non-nil if DIR is a valid module directory.

A valid module directory is a directory containing at least one
recognized module file."
  (and (file-directory-p dir)
       (let ((files (directory-files dir nil "\\`[^.]" t)))
         (seq-some (lambda (f)
                     (member f '("init.el" "lib.el" "packages.eld"
                                 "spec.md" "tests.el")))
                   files))))

(provide '+modules)

;;; +modules.el ends here
