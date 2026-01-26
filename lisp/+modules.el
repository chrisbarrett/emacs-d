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

(defun +modules-read-packages (module-dir)
  "Read packages.eld from MODULE-DIR and return package specs.

The file should contain a list of elpaca package specifications,
for example:

  ;; -*- lisp-data -*-
  ((evil :host github :repo \"emacs-evil/evil\")
   (evil-collection))

Returns the list of package specs, or nil if the file doesn't
exist or is empty."
  (let ((packages-file (expand-file-name "packages.eld" module-dir)))
    (when (file-exists-p packages-file)
      (with-temp-buffer
        (insert-file-contents packages-file)
        (goto-char (point-min))
        (condition-case nil
            (read (current-buffer))
          (end-of-file nil))))))

(defun +modules--package-name (spec)
  "Extract the package name from SPEC.
SPEC can be a symbol or a list with the package name as the first element."
  (if (consp spec) (car spec) spec))

(defun +modules-collect-packages ()
  "Collect all package specs from discovered modules.

Returns a flat list of all package specifications from all
modules' packages.eld files, de-duplicated by package name.
When duplicates are found, the first occurrence is kept."
  (let ((modules (+modules-discover))
        (seen (make-hash-table :test 'eq))
        (result nil))
    (dolist (module modules)
      (dolist (spec (+modules-read-packages module))
        (let ((name (+modules--package-name spec)))
          (unless (gethash name seen)
            (puthash name t seen)
            (push spec result)))))
    (nreverse result)))

(defun +modules-read-extra-packages (path)
  "Read extra package specs from PATH.
PATH should be a lisp-data file containing a list of package specs.
Returns nil if PATH doesn't exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (condition-case nil
          (read (current-buffer))
        (end-of-file nil)))))

(defun +modules-install-packages (package-specs)
  "Install PACKAGE-SPECS using elpaca.

PACKAGE-SPECS is a list of elpaca package specifications, each
being a list with the package name as the first element and
optional recipe keywords following.

Example specs:
  ((evil :host github :repo \"emacs-evil/evil\")
   (evil-collection))

This function requires elpaca to be available. Each spec is
passed to the `elpaca' macro for installation."
  (when (and package-specs (fboundp 'elpaca))
    (dolist (spec package-specs)
      (eval `(elpaca ,spec) t))))


;;; Autoload Registration

(defun +modules--discover-lib-files (module-dir)
  "Discover library files in MODULE-DIR.

Returns a list of absolute paths to lib files. Checks for:
- lib.el in the module root
- Any *-lib.el file in the module root
- All .el files in the lib/ subdirectory"
  (let ((lib-el (expand-file-name "lib.el" module-dir))
        (lib-dir (expand-file-name "lib" module-dir)))
    (append
     (when (file-exists-p lib-el) (list lib-el))
     ;; Find module-specific lib files (e.g., project-lib.el)
     (directory-files module-dir t "-lib\\.el\\'" t)
     (when (file-directory-p lib-dir)
       (directory-files lib-dir t "\\.el\\'" t)))))

(defun +modules--extract-autoloads (lib-file)
  "Extract autoload-annotated forms from LIB-FILE.

Returns an alist of (FORM . SOURCE-FILE) pairs for each form
preceded by ;;;###autoload."
  (when (file-exists-p lib-file)
    (let ((autoloads nil))
      (with-temp-buffer
        (insert-file-contents lib-file)
        (goto-char (point-min))
        (while (re-search-forward "^;;;###autoload" nil t)
          (forward-line 1)
          (let ((form (ignore-errors (read (current-buffer)))))
            (when form
              (push (cons form lib-file) autoloads)))))
      (nreverse autoloads))))

(defun +modules--autoload-form (form source-file)
  "Generate an autoload form for FORM defined in SOURCE-FILE.

FORM should be a definition form like defun, defmacro,
define-minor-mode, defvar, or defconst. Returns an autoload
form (for functions/macros) or the form itself (for variables),
or nil if FORM is not a recognized definition type."
  (ignore source-file) ;; Used by autoload but not variable definitions
  (pcase form
    ;; defvar/defconst - return the form directly (variables are evaluated immediately)
    (`(,(or 'defvar 'defconst) ,_name . ,_rest)
     form)
    ;; defun/cl-defun - check for optional docstring
    (`(,(and (or 'defun 'defun* 'cl-defun) _) ,name ,_args ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) t))
    (`(,(and (or 'defun 'defun* 'cl-defun) _) ,name ,_args . ,_)
     `(autoload ',name ,source-file nil t))
    ;; defmacro/cl-defmacro
    (`(,(and (or 'defmacro 'cl-defmacro) _) ,name ,_args ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) nil 'macro))
    (`(,(and (or 'defmacro 'cl-defmacro) _) ,name ,_args . ,_)
     `(autoload ',name ,source-file nil nil 'macro))
    ;; define-minor-mode
    (`(define-minor-mode ,name ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) t))
    ;; define-derived-mode
    (`(define-derived-mode ,name ,_parent ,_name ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) t))
    (`(define-derived-mode ,name ,_parent ,_name . ,_)
     `(autoload ',name ,source-file nil t))
    ;; Unrecognized form
    (_ nil)))

(defun +modules-collect-autoloads ()
  "Collect all autoload entries from discovered modules.

Returns an alist of (FORM . SOURCE-FILE) pairs for all
autoload-annotated definitions in module lib files."
  (let ((modules (+modules-discover)))
    (apply #'append
           (mapcar (lambda (module-dir)
                     (let ((lib-files (+modules--discover-lib-files module-dir)))
                       (apply #'append
                              (mapcar #'+modules--extract-autoloads lib-files))))
                   modules))))

(defun +modules-register-autoloads (autoload-entries)
  "Register AUTOLOAD-ENTRIES with Emacs.

AUTOLOAD-ENTRIES is an alist of (FORM . SOURCE-FILE) pairs.
Each FORM is converted to an autoload and evaluated, making
the symbol `fboundp' without loading its source file."
  (dolist (entry autoload-entries)
    (let* ((form (car entry))
           (source-file (cdr entry))
           (autoload-form (+modules--autoload-form form source-file)))
      (when autoload-form
        (eval autoload-form t)))))


;;; Init Loading

(defun +modules--find-init-file (module-dir)
  "Return the init.el path for MODULE-DIR if it exists, nil otherwise."
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (when (file-exists-p init-file)
      init-file)))

(defun +modules-collect-init-files ()
  "Collect all init.el files from discovered modules.

Returns a list of absolute paths to init.el files, in the order
modules are discovered."
  (let ((modules (+modules-discover)))
    (delq nil (mapcar #'+modules--find-init-file modules))))

(defun +modules-load-inits (init-files)
  "Load all INIT-FILES.

INIT-FILES is a list of absolute paths to init.el files.
Each file is loaded using `load', which evaluates its contents."
  (dolist (init-file init-files)
    (load init-file nil 'nomessage)))

(provide '+modules)

;;; +modules.el ends here
