;;; module-system.el --- Feature-based modular configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; A module system for Emacs configuration that organizes features as
;; self-contained units with specs, packages, init files, and tests.
;;
;; The system separates filesystem traversal (discovery) from evaluation
;; (loading) to enable independent testing of each phase.
;;
;; Feature directory structure:
;;   features/{slug}/
;;     spec.md        ; feature specification
;;     packages.eld   ; elpaca specs (data only)
;;     init.el        ; evaluated during init
;;     lib.el         ; autoloaded functions (or lib/*.el)
;;     tests.el       ; ERT tests
;;
;; Load order: packages.eld (all) -> autoloads (all) -> init.el (all)

;;; Code:

(require 'cl-lib)

(defgroup module-system nil
  "Feature-based modular configuration system."
  :group 'initialization
  :prefix "module-system-")

(defcustom module-system-features-directory
  (expand-file-name "features" user-emacs-directory)
  "Root directory containing feature modules."
  :type 'directory
  :group 'module-system)


;;; Data Structures

;; A module represents a discovered feature directory and its contents.
;; This is a pure data structure with no side effects.

(cl-defstruct (module-system-module (:constructor module-system--make-module)
                                    (:copier nil))
  "A discovered feature module."
  slug           ; string: feature name (directory basename)
  directory      ; string: absolute path to feature directory
  spec-file      ; string or nil: path to spec.md
  packages-file  ; string or nil: path to packages.eld
  init-file      ; string or nil: path to init.el
  lib-files      ; list of strings: paths to lib.el and/or lib/*.el files
  tests-file)    ; string or nil: path to tests.el


;;; Filesystem Traversal (Pure Discovery)

;; These functions discover and parse the filesystem structure without
;; evaluating or loading anything. They can be tested with mock directories.

(defun module-system--file-exists-p (path)
  "Return PATH if it exists as a file, nil otherwise."
  (when (file-exists-p path)
    path))

(defun module-system--list-el-files (directory)
  "Return list of .el files in DIRECTORY, or nil if directory doesn't exist."
  (when (file-directory-p directory)
    (directory-files directory t "\\.el\\'" t)))

(defun module-system--discover-lib-files (feature-dir)
  "Discover library files in FEATURE-DIR.
Returns a list of paths. Checks for lib.el and lib/*.el files."
  (let ((lib-el (expand-file-name "lib.el" feature-dir))
        (lib-dir (expand-file-name "lib" feature-dir)))
    (append
     (when (file-exists-p lib-el) (list lib-el))
     (module-system--list-el-files lib-dir))))

(defun module-system--discover-module (feature-dir)
  "Discover a module from FEATURE-DIR.
Returns a `module-system-module' struct describing the feature's contents.
Does not evaluate or load any files."
  (let ((slug (file-name-nondirectory (directory-file-name feature-dir))))
    (module-system--make-module
     :slug slug
     :directory feature-dir
     :spec-file (module-system--file-exists-p
                 (expand-file-name "spec.md" feature-dir))
     :packages-file (module-system--file-exists-p
                     (expand-file-name "packages.eld" feature-dir))
     :init-file (module-system--file-exists-p
                 (expand-file-name "init.el" feature-dir))
     :lib-files (module-system--discover-lib-files feature-dir)
     :tests-file (module-system--file-exists-p
                  (expand-file-name "tests.el" feature-dir)))))

(defun module-system--list-feature-directories (root-dir)
  "List all feature directories under ROOT-DIR.
Returns a list of absolute paths to directories that appear to be features
\(i.e., immediate subdirectories of ROOT-DIR)."
  (when (file-directory-p root-dir)
    (cl-remove-if-not
     #'file-directory-p
     (directory-files root-dir t "\\`[^.]" t))))

(defun module-system-discover-all (&optional features-dir)
  "Discover all modules under FEATURES-DIR.
FEATURES-DIR defaults to `module-system-features-directory'.
Returns a list of `module-system-module' structs.

This function performs pure filesystem traversal with no side effects."
  (let ((root (or features-dir module-system-features-directory)))
    (mapcar #'module-system--discover-module
            (module-system--list-feature-directories root))))


;;; Data Reading (Pure Parsing)

;; These functions read and parse data files without side effects.

(defun module-system--read-packages-file (packages-file)
  "Read PACKAGES-FILE and return its contents as a list of elpaca specs.
PACKAGES-FILE should be a lisp-data file containing a list of package specs.
Returns nil if PACKAGES-FILE is nil or doesn't exist."
  (when (and packages-file (file-exists-p packages-file))
    (with-temp-buffer
      (insert-file-contents-literally packages-file)
      (read (current-buffer)))))

(defun module-system--package-name (spec)
  "Extract the package name from SPEC.
SPEC can be a symbol or a list with the package name as the first element."
  (if (consp spec) (car spec) spec))

(defun module-system-collect-packages (modules)
  "Collect all package specs from MODULES.
MODULES is a list of `module-system-module' structs.
Returns a flat list of elpaca package specs, de-duplicated by package name.
When duplicate package names are encountered, the first occurrence is kept."
  (let ((seen (make-hash-table :test 'eq))
        (result nil))
    (dolist (mod modules)
      (dolist (spec (module-system--read-packages-file
                     (module-system-module-packages-file mod)))
        (let ((name (module-system--package-name spec)))
          (unless (gethash name seen)
            (puthash name t seen)
            (push spec result)))))
    (nreverse result)))

(defun module-system--read-extra-package-sources (sources)
  "Read package specs from SOURCES files.
SOURCES is a list of paths to packages.eld files.
Returns a flat list of package specs from all source files."
  (cl-mapcan #'module-system--read-packages-file sources))

(defun module-system--deduplicate-packages (packages)
  "Remove duplicate package specs from PACKAGES.
PACKAGES is a list of package specs.
Returns a de-duplicated list, keeping the first occurrence of each package."
  (let ((seen (make-hash-table :test 'eq))
        (result nil))
    (dolist (spec packages)
      (let ((name (module-system--package-name spec)))
        (unless (gethash name seen)
          (puthash name t seen)
          (push spec result))))
    (nreverse result)))


;;; Autoload Generation

;; Functions to extract and register autoloads from library files.

(defun module-system--extract-autoloads (lib-file)
  "Extract autoload forms from LIB-FILE.
Returns a list of autoload forms found in the file."
  (when (file-exists-p lib-file)
    (let ((autoloads nil))
      (with-temp-buffer
        (insert-file-contents-literally lib-file)
        (goto-char (point-min))
        (while (re-search-forward "^;;;###autoload" nil t)
          (forward-line 1)
          (let ((form (ignore-errors (read (current-buffer)))))
            (when form
              (push (cons form lib-file) autoloads)))))
      (nreverse autoloads))))

(defun module-system-collect-autoloads (modules)
  "Collect all autoload definitions from MODULES.
MODULES is a list of `module-system-module' structs.
Returns an alist of (FORM . SOURCE-FILE) for each autoload-annotated definition."
  (cl-mapcan
   (lambda (mod)
     (cl-mapcan #'module-system--extract-autoloads
                (module-system-module-lib-files mod)))
   modules))

(defun module-system--autoload-form-for-defun (form source-file)
  "Generate an autoload form for FORM defined in SOURCE-FILE.
FORM should be a `defun', `defmacro', `define-minor-mode', etc."
  (pcase form
    (`(,(and (or 'defun 'defun* 'cl-defun 'defsubst) _) ,name ,_args ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) t))
    (`(,(and (or 'defun 'defun* 'cl-defun 'defsubst) _) ,name ,_args . ,_)
     `(autoload ',name ,source-file nil t))
    (`(,(and (or 'defmacro 'cl-defmacro) _) ,name ,_args ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) nil 'macro))
    (`(,(and (or 'defmacro 'cl-defmacro) _) ,name ,_args . ,_)
     `(autoload ',name ,source-file nil nil 'macro))
    (`(define-minor-mode ,name ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) t))
    (`(define-derived-mode ,name ,_parent ,_name ,docstring . ,_)
     `(autoload ',name ,source-file ,(if (stringp docstring) docstring nil) t))
    (`(define-derived-mode ,name ,_parent ,_name . ,_)
     `(autoload ',name ,source-file nil t))
    ;; Fall back to skipping unrecognized forms
    (_ nil)))


;;; Evaluation (Side Effects)

;; These functions perform the actual loading and evaluation.
;; They should be called after discovery is complete.

(defvar module-system--loaded-modules nil
  "List of module slugs that have been loaded.")

(defun module-system--install-packages (package-specs)
  "Install PACKAGE-SPECS using elpaca.
PACKAGE-SPECS is a list of elpaca package specifications."
  (when (and package-specs (fboundp 'elpaca))
    (dolist (spec package-specs)
      (eval `(elpaca ,spec) t))))

(defun module-system--register-autoloads (autoload-entries)
  "Register autoloads from AUTOLOAD-ENTRIES.
AUTOLOAD-ENTRIES is an alist of (FORM . SOURCE-FILE) pairs."
  (dolist (entry autoload-entries)
    (let* ((form (car entry))
           (source-file (cdr entry))
           (autoload-form (module-system--autoload-form-for-defun form source-file)))
      (when autoload-form
        (eval autoload-form t)))))

(defun module-system--evaluate-init-file (init-file)
  "Evaluate INIT-FILE if it exists."
  (when (and init-file (file-exists-p init-file))
    (load init-file nil 'nomessage)))

(defun module-system--add-lib-to-load-path (modules)
  "Add library directories from MODULES to `load-path'."
  (dolist (mod modules)
    (let ((lib-dir (expand-file-name "lib"
                                     (module-system-module-directory mod))))
      (when (file-directory-p lib-dir)
        (add-to-list 'load-path lib-dir)))
    ;; Also add the feature directory itself if it has lib.el
    (when (member (expand-file-name "lib.el"
                                    (module-system-module-directory mod))
                  (module-system-module-lib-files mod))
      (add-to-list 'load-path (module-system-module-directory mod)))))

(cl-defun module-system-load (modules &key extra-package-sources)
  "Load MODULES in the correct order.
MODULES is a list of `module-system-module' structs.
EXTRA-PACKAGE-SOURCES is an optional list of paths to additional packages.eld
files to load (e.g., for bootstrap packages not associated with any module).

Load order:
1. Install all packages (packages.eld + extra sources)
2. Add lib directories to `load-path'
3. Register all autoloads (lib.el, lib/*.el)
4. Evaluate all init files (init.el)

Returns the list of loaded module slugs."
  ;; Phase 1: Collect and install all packages (modules + extra sources)
  (let* ((module-packages (module-system-collect-packages modules))
         (extra-packages (module-system--read-extra-package-sources
                          extra-package-sources))
         (all-packages (module-system--deduplicate-packages
                        (append extra-packages module-packages))))
    (module-system--install-packages all-packages))

  ;; Phase 2: Add lib directories to load-path
  (module-system--add-lib-to-load-path modules)

  ;; Phase 3: Register all autoloads
  (let ((all-autoloads (module-system-collect-autoloads modules)))
    (module-system--register-autoloads all-autoloads))

  ;; Phase 4: Evaluate all init files
  (dolist (mod modules)
    (module-system--evaluate-init-file
     (module-system-module-init-file mod))
    (push (module-system-module-slug mod) module-system--loaded-modules))

  (nreverse module-system--loaded-modules))


;;; Public API

;;;###autoload
(defun module-system-init (&optional features-dir)
  "Initialize the module system.
Discover and load all features from FEATURES-DIR.
FEATURES-DIR defaults to `module-system-features-directory'.

This is the main entry point for the module system."
  (interactive)
  (let ((modules (module-system-discover-all features-dir)))
    (module-system-load modules)))

;;;###autoload
(defun module-system-list-modules (&optional features-dir)
  "List all discovered modules.
FEATURES-DIR defaults to `module-system-features-directory'.
Returns a list of module slugs."
  (mapcar #'module-system-module-slug
          (module-system-discover-all features-dir)))

(provide 'module-system)
;;; module-system.el ends here
