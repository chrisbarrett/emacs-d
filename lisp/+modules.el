;;; +modules.el --- Module system for self-contained config units.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides discovery and loading of self-contained modules from
;; `modules/' directory. Each module can contain:
;;
;; - spec.md        : module specification
;; - packages.eld   : elpaca specs (data only)
;; - init.el        : evaluated during init
;; - lib.el         : autoloaded functions
;; - lib/           : alternative: multiple lib files
;; - tests.el       : ERT tests
;;
;; Public entry points:
;;
;;   `+install-packages'      Install all module packages (call before elpaca-wait).
;;   `+autoloads-rebuild'     Rebuild autoload registrations and `+autoloads.el'.
;;   `+modules-load-all'      Load module init.el files and register trusted dirs.
;;   `+modules-discover'      List module directories.

;;; Code:

(require 'cl-lib)
(require '+core-paths)

(defconst +modules-autoloads-file (file-name-concat +lisp-dir "+autoloads.el"))


;;; Discovery

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
	 (seq-intersection files '("init.el" "lib.el" "packages.eld" "spec.md" "tests.el")))))


;;; Package collection

(defun +modules--read-packages-file (file)
  "Read package specs from FILE.

FILE should contain a list of elpaca package specifications,
for example:

  ;; -*- lisp-data -*-
  ((evil :host github :repo \"emacs-evil/evil\")
   (evil-collection))

Returns the list of package specs, or nil if empty."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case nil
        (read (current-buffer))
      (end-of-file nil))))

(defun +modules--read-packages (module-dir)
  "Read packages.eld from MODULE-DIR and return package specs.
Returns nil if the file doesn't exist."
  (let ((packages-file (expand-file-name "packages.eld" module-dir)))
    (when (file-exists-p packages-file)
      (+modules--read-packages-file packages-file))))

(defun +modules--package-name (spec)
  "Extract the package name from SPEC.
SPEC can be a symbol or a list with the package name as the first element."
  (if (consp spec) (car spec) spec))

(defun +modules--collect-packages ()
  "Collect all package specs from `+modules-directory'.

Finds all packages.eld files at any depth under `+modules-directory'
and returns a flat list of package specifications, de-duplicated by
package name. When duplicates are found, the first occurrence is kept."
  (when (file-directory-p +modules-directory)
    (let ((package-files (directory-files-recursively +modules-directory
                                                      "\\`packages\\.eld\\'"
                                                      nil))
          (seen (make-hash-table :test 'equal))
          (result nil))
      (dolist (file package-files)
        (dolist (spec (+modules--read-packages-file file))
          (let ((name (+modules--package-name spec)))
            (unless (gethash name seen)
              (puthash name t seen)
              (push spec result)))))
      (nreverse result))))

(defun +modules--install-packages (package-specs)
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
    (dolist (spec (seq-uniq package-specs))
      (eval `(elpaca ,spec) t))))

;;;###autoload
(defun +install-packages ()
  "Install all packages declared in the module system.

Packages are sourced from the `packages.eld' files in each
module."
  (interactive)
  (+modules--install-packages (+modules--collect-packages)))


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

FORM should be a definition form like `defun', `defmacro',
`define-minor-mode', `defvar', or `defconst'. Returns an autoload
form (for functions/macros) or the form itself (for variables), or nil
if FORM is not a recognized definition type."
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
    ;; Side-effecting top-level form annotated with ;;;###autoload —
    ;; pass through verbatim so it runs when `+autoloads.el' is loaded
    ;; (e.g. `(add-to-list 'auto-mode-alist …)' for activation triggers).
    (_ form)))

(defun +modules--lisp-family-lib-files ()
  "Discover library files under `lisp/<family>/' subdirectories.

Returns a flat list of absolute paths to .el files (excluding
-tests.el) in each immediate subdirectory of `+lisp-dir'. Library
families live under `lisp/' as a sibling structure to `modules/'."
  (when (file-directory-p +lisp-dir)
    (let (files)
      (dolist (dir (directory-files +lisp-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t "\\.el\\'" t))
            (unless (string-suffix-p "-tests.el" file)
              (push file files)))))
      (nreverse files))))

(defun +modules--collect-autoloads ()
  "Collect all autoload entries from discovered modules and lisp libraries.

Returns an alist of (FORM . SOURCE-FILE) pairs for all
autoload-annotated definitions in module lib files and in
`lisp/<family>/' library files."
  (append
   (seq-mapcat (lambda (module-dir)
                 (seq-mapcat #'+modules--extract-autoloads (+modules--discover-lib-files module-dir)))
               (+modules-discover))
   (seq-mapcat #'+modules--extract-autoloads (+modules--lisp-family-lib-files))))

(defun +modules--register-autoloads (autoload-entries)
  "Register AUTOLOAD-ENTRIES with Emacs.

AUTOLOAD-ENTRIES is an alist of (FORM . SOURCE-FILE) pairs.
Each FORM is converted to an autoload and evaluated, making
the symbol `fboundp' without loading its source file."
  (pcase-dolist (`(,form . ,source-file) autoload-entries)
    (when-let* ((form (+modules--autoload-form form source-file)))
      (eval form t))))

(defun +modules--write-autoloads (autoload-entries)
  "Write AUTOLOAD-ENTRIES to `+modules-autoloads-file'."
  (let ((backup-inhibited t))
    (with-current-buffer (find-file-noselect +modules-autoloads-file)
      (erase-buffer)
      (insert ";; -*- lexical-binding: t; -*-\n")

      (pcase-dolist (`(,form . ,source-file) autoload-entries)
        (when-let* ((form (+modules--autoload-form form source-file)))
          (insert (format "%S\n" form))))

      (insert "(provide '+autoloads)\n")
      (save-buffer))))

;;;###autoload
(defun +autoloads-rebuild ()
  "Rebuild module autoload registrations and `+autoloads.el'.

Collects autoload-annotated forms from every module lib file,
registers them with Emacs (so symbols become `fboundp' without
loading their source), and writes them to disk so subsequent
sessions can skip the scan."
  (interactive)
  (let ((autoloads (+modules--collect-autoloads)))
    (+modules--register-autoloads autoloads)
    (+modules--write-autoloads autoloads))
  (when (called-interactively-p 'interactive)
    (message "Rebuilt %s" +modules-autoloads-file)))


;;; Trusted Content & Init Loading

(defun +modules--register-trusted-content (module-dir)
  "Add MODULE-DIR and its lib/ subdirectory to `trusted-content'.

`trusted-content' matches against the immediate parent directory of a
file, so each module's base directory and lib/ subdirectory must be
registered explicitly."
  (add-to-list 'trusted-content (file-name-as-directory module-dir))
  (add-to-list 'trusted-content
               (file-name-as-directory (expand-file-name "lib" module-dir))))

(defun +modules--find-init-file (module-dir)
  "Return the init.el path for MODULE-DIR if it exists, nil otherwise."
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (when (file-exists-p init-file)
      init-file)))

(defun +modules--collect-init-files ()
  "Collect all init.el files from discovered modules.

Returns a list of absolute paths to init.el files, in the order
modules are discovered."
  (let ((modules (+modules-discover)))
    (delq nil (mapcar #'+modules--find-init-file modules))))

(defun +modules--load-inits (init-files)
  "Load all INIT-FILES.

INIT-FILES is a list of absolute paths to init.el files.
Each file is loaded using `load', which evaluates its contents."
  (dolist (init-file init-files)
    (load init-file nil 'nomessage)))

(defun +modules-load-all ()
  "Load every module's init.el and register module dirs as trusted content.

Call after package install and autoload registration. Loads init
files in discovery order, then adds each module directory and its
`lib/' subdirectory to `trusted-content' so flymake byte-compile
treats them as safe."
  (+modules--load-inits (+modules--collect-init-files))
  (mapc #'+modules--register-trusted-content (+modules-discover)))


(provide '+modules)

;;; +modules.el ends here
