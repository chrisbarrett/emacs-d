;;; +modules-tests.el --- Tests for +modules.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require '+modules)

(ert-deftest modules--discover--returns-empty-when-no-modules-dir ()
  "Discovery returns nil when modules directory doesn't exist."
  (let ((+modules-directory (make-temp-name "/tmp/nonexistent-")))
    (should (null (+modules-discover)))))

(ert-deftest modules--discover--returns-empty-when-no-valid-modules ()
  "Discovery returns nil when no valid module directories exist."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (progn
          ;; Create empty subdirectory
          (make-directory (expand-file-name "empty-module" +modules-directory))
          (should (null (+modules-discover))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-init-el ()
  "Discovery finds module containing init.el."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "init.el" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-lib-el ()
  "Discovery finds module containing lib.el."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "lib.el" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-packages-eld ()
  "Discovery finds module containing packages.eld."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "packages.eld" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-spec-md ()
  "Discovery finds module containing spec.md."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "spec.md" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-tests-el ()
  "Discovery finds module containing tests.el."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "tests.el" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-multiple-modules ()
  "Discovery finds all valid module directories."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-a (expand-file-name "module-a" +modules-directory))
              (module-b (expand-file-name "module-b" +modules-directory)))
          (make-directory module-a)
          (make-directory module-b)
          (write-region "" nil (expand-file-name "init.el" module-a))
          (write-region "" nil (expand-file-name "lib.el" module-b))
          (let ((result (+modules-discover)))
            (should (= 2 (length result)))
            (should (member module-a result))
            (should (member module-b result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--ignores-hidden-directories ()
  "Discovery ignores directories starting with dot."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((hidden-dir (expand-file-name ".hidden-module" +modules-directory))
              (visible-dir (expand-file-name "visible-module" +modules-directory)))
          (make-directory hidden-dir)
          (make-directory visible-dir)
          (write-region "" nil (expand-file-name "init.el" hidden-dir))
          (write-region "" nil (expand-file-name "init.el" visible-dir))
          (let ((result (+modules-discover)))
            (should (equal (list visible-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--ignores-regular-files ()
  "Discovery ignores regular files in modules directory."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "real-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "init.el" module-dir))
          ;; Create a regular file that shouldn't be treated as a module
          (write-region "" nil (expand-file-name "not-a-module.el" +modules-directory))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--valid-module-p--returns-nil-for-file ()
  "Validation returns nil for regular files."
  (let ((temp-file (make-temp-file "not-a-module")))
    (unwind-protect
        (should-not (+modules--valid-module-p temp-file))
      (delete-file temp-file))))

(ert-deftest modules--valid-module-p--returns-nil-for-empty-dir ()
  "Validation returns nil for empty directories."
  (let ((temp-dir (make-temp-file "empty-module-" t)))
    (unwind-protect
        (should-not (+modules--valid-module-p temp-dir))
      (delete-directory temp-dir t))))

(ert-deftest modules--valid-module-p--returns-t-for-valid-module ()
  "Validation returns t for directories with recognized files."
  (let ((temp-dir (make-temp-file "valid-module-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "init.el" temp-dir))
          (should (+modules--valid-module-p temp-dir)))
      (delete-directory temp-dir t))))

;;; Tests for +modules-read-packages

(ert-deftest modules--read-packages--returns-nil-when-no-file ()
  "Reading packages returns nil when packages.eld doesn't exist."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (should (null (+modules-read-packages temp-dir)))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--reads-single-package ()
  "Reading packages returns spec for single package."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region "((evil-collection))" nil packages-file)
          (let ((result (+modules-read-packages temp-dir)))
            (should (equal '((evil-collection)) result))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--reads-multiple-packages ()
  "Reading packages returns list of multiple package specs."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region "((evil :host github :repo \"emacs-evil/evil\")\n (evil-collection))"
                        nil packages-file)
          (let ((result (+modules-read-packages temp-dir)))
            (should (= 2 (length result)))
            (should (equal '(evil :host github :repo "emacs-evil/evil") (car result)))
            (should (equal '(evil-collection) (cadr result)))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--handles-lisp-data-comment ()
  "Reading packages ignores mode line comment."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region ";; -*- lisp-data -*-\n((consult))" nil packages-file)
          (let ((result (+modules-read-packages temp-dir)))
            (should (equal '((consult)) result))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--returns-nil-for-empty-file ()
  "Reading packages returns nil for empty file."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region "" nil packages-file)
          (should (null (+modules-read-packages temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--returns-nil-for-comment-only ()
  "Reading packages returns nil when file contains only comments."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region ";; -*- lisp-data -*-\n;; No packages yet" nil packages-file)
          (should (null (+modules-read-packages temp-dir))))
      (delete-directory temp-dir t))))

;;; Tests for +modules-collect-packages

(ert-deftest modules--collect-packages--returns-empty-when-no-modules ()
  "Collecting packages returns nil when no modules exist."
  (let ((+modules-directory (make-temp-name "/tmp/nonexistent-")))
    (should (null (+modules-collect-packages)))))

(ert-deftest modules--collect-packages--aggregates-from-multiple-modules ()
  "Collecting packages aggregates specs from all modules."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-a (expand-file-name "module-a" +modules-directory))
              (module-b (expand-file-name "module-b" +modules-directory)))
          (make-directory module-a)
          (make-directory module-b)
          (write-region "((evil))" nil (expand-file-name "packages.eld" module-a))
          (write-region "((consult) (vertico))" nil (expand-file-name "packages.eld" module-b))
          (let ((result (+modules-collect-packages)))
            (should (= 3 (length result)))
            (should (member '(evil) result))
            (should (member '(consult) result))
            (should (member '(vertico) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--collect-packages--skips-modules-without-packages ()
  "Collecting packages skips modules without packages.eld."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((with-packages (expand-file-name "with-packages" +modules-directory))
              (without-packages (expand-file-name "without-packages" +modules-directory)))
          (make-directory with-packages)
          (make-directory without-packages)
          (write-region "((evil))" nil (expand-file-name "packages.eld" with-packages))
          (write-region "" nil (expand-file-name "init.el" without-packages))
          (let ((result (+modules-collect-packages)))
            (should (equal '((evil)) result))))
      (delete-directory +modules-directory t))))

;;; Tests for +modules-install-packages

(ert-deftest modules--install-packages--no-op-when-elpaca-unavailable ()
  "Installing packages is a no-op when elpaca is not available."
  ;; Temporarily unbind elpaca to simulate it not being available
  (let ((elpaca-was-bound (fboundp 'elpaca)))
    (when elpaca-was-bound
      (fmakunbound 'elpaca))
    (unwind-protect
        ;; Should not error, just return nil
        (should-not (+modules-install-packages '((some-package))))
      ;; Restore elpaca if it was bound
      (when elpaca-was-bound
        ;; Note: We can't easily restore the original function, so we skip
        ;; restoring for this test. The test framework isolates tests.
        nil))))

(ert-deftest modules--install-packages--calls-elpaca-for-each-spec ()
  "Installing packages evaluates elpaca form for each spec."
  ;; This test verifies that +modules-install-packages calls eval with
  ;; the correct elpaca forms. We intercept eval to record the forms
  ;; and skip actual evaluation of elpaca forms to avoid side effects.
  (let ((eval-calls '())
        (original-eval (symbol-function 'eval)))
    (cl-letf (((symbol-function 'eval)
               (lambda (form &optional lexical)
                 ;; Record and skip elpaca calls to avoid actual package install
                 (if (and (consp form) (eq (car form) 'elpaca))
                     (push form eval-calls)
                   ;; Delegate non-elpaca forms to real eval
                   (funcall original-eval form lexical)))))
      ;; Ensure elpaca appears bound for the fboundp check
      (unless (fboundp 'elpaca)
        (fset 'elpaca (lambda (&rest _) nil)))
      (+modules-install-packages '((pkg-a) (pkg-b :host github :repo "user/pkg-b")))
      ;; Should have called eval with elpaca forms for each spec
      (should (= 2 (length eval-calls)))
      (should (member '(elpaca (pkg-a)) eval-calls))
      (should (member '(elpaca (pkg-b :host github :repo "user/pkg-b")) eval-calls)))))

(ert-deftest modules--install-packages--handles-empty-list ()
  "Installing empty package list does nothing."
  (let ((elpaca-called nil))
    (cl-letf (((symbol-function 'elpaca)
               (lambda (_spec) (setq elpaca-called t))))
      (+modules-install-packages nil)
      (should-not elpaca-called)
      (+modules-install-packages '())
      (should-not elpaca-called))))


;;; Tests for +modules--discover-lib-files

(ert-deftest modules--discover-lib-files--finds-lib-el ()
  "Discovers lib.el in module root."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "lib.el" temp-dir))
          (let ((result (+modules--discover-lib-files temp-dir)))
            (should (= 1 (length result)))
            (should (string-suffix-p "lib.el" (car result)))))
      (delete-directory temp-dir t))))

(ert-deftest modules--discover-lib-files--finds-lib-directory ()
  "Discovers .el files in lib/ subdirectory."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((lib-dir (expand-file-name "lib" temp-dir)))
          (make-directory lib-dir)
          (write-region "" nil (expand-file-name "utils.el" lib-dir))
          (write-region "" nil (expand-file-name "helpers.el" lib-dir))
          (let ((result (+modules--discover-lib-files temp-dir)))
            (should (= 2 (length result)))))
      (delete-directory temp-dir t))))

(ert-deftest modules--discover-lib-files--finds-both ()
  "Discovers lib.el and lib/*.el files."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((lib-dir (expand-file-name "lib" temp-dir)))
          (make-directory lib-dir)
          (write-region "" nil (expand-file-name "lib.el" temp-dir))
          (write-region "" nil (expand-file-name "extra.el" lib-dir))
          (let ((result (+modules--discover-lib-files temp-dir)))
            (should (= 2 (length result)))))
      (delete-directory temp-dir t))))

(ert-deftest modules--discover-lib-files--returns-nil-when-no-libs ()
  "Returns nil when no lib files exist."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "init.el" temp-dir))
          (should (null (+modules--discover-lib-files temp-dir))))
      (delete-directory temp-dir t))))


;;; Tests for +modules--extract-autoloads

(ert-deftest modules--extract-autoloads--extracts-defun ()
  "Extracts autoload-annotated defun."
  (let ((temp-file (make-temp-file "lib" nil ".el")))
    (unwind-protect
        (progn
          (write-region ";;;###autoload\n(defun my-func () nil)" nil temp-file)
          (let ((result (+modules--extract-autoloads temp-file)))
            (should (= 1 (length result)))
            (should (eq 'defun (caar (car result))))
            (should (eq 'my-func (cadr (car (car result)))))))
      (delete-file temp-file))))

(ert-deftest modules--extract-autoloads--extracts-multiple ()
  "Extracts multiple autoload-annotated forms."
  (let ((temp-file (make-temp-file "lib" nil ".el")))
    (unwind-protect
        (progn
          (write-region ";;;###autoload\n(defun func-1 () nil)\n\n;;;###autoload\n(defun func-2 () nil)"
                        nil temp-file)
          (let ((result (+modules--extract-autoloads temp-file)))
            (should (= 2 (length result)))))
      (delete-file temp-file))))

(ert-deftest modules--extract-autoloads--ignores-non-autoload ()
  "Ignores forms not preceded by ;;;###autoload."
  (let ((temp-file (make-temp-file "lib" nil ".el")))
    (unwind-protect
        (progn
          (write-region "(defun not-autoloaded () nil)\n\n;;;###autoload\n(defun is-autoloaded () nil)"
                        nil temp-file)
          (let ((result (+modules--extract-autoloads temp-file)))
            (should (= 1 (length result)))
            (should (eq 'is-autoloaded (cadr (car (car result)))))))
      (delete-file temp-file))))

(ert-deftest modules--extract-autoloads--returns-nil-for-missing-file ()
  "Returns nil when file doesn't exist."
  (should (null (+modules--extract-autoloads "/nonexistent/file.el"))))


;;; Tests for +modules--autoload-form

(ert-deftest modules--autoload-form--defun-with-docstring ()
  "Generates autoload for defun with docstring."
  (let ((form '(defun my-cmd () "Do thing." (interactive) nil)))
    (should (equal '(autoload 'my-cmd "/path/lib.el" "Do thing." t)
                   (+modules--autoload-form form "/path/lib.el")))))

(ert-deftest modules--autoload-form--defun-without-docstring ()
  "Generates autoload for defun without docstring."
  (let ((form '(defun my-cmd () (interactive) nil)))
    (should (equal '(autoload 'my-cmd "/path/lib.el" nil t)
                   (+modules--autoload-form form "/path/lib.el")))))

(ert-deftest modules--autoload-form--cl-defun ()
  "Generates autoload for cl-defun."
  (let ((form '(cl-defun my-cmd (&key arg) "Doc." arg)))
    (should (equal '(autoload 'my-cmd "/path/lib.el" "Doc." t)
                   (+modules--autoload-form form "/path/lib.el")))))

(ert-deftest modules--autoload-form--defmacro ()
  "Generates autoload for defmacro."
  (let ((form '(defmacro my-macro (x) "Doc." `(foo ,x))))
    (should (equal '(autoload 'my-macro "/path/lib.el" "Doc." nil 'macro)
                   (+modules--autoload-form form "/path/lib.el")))))

(ert-deftest modules--autoload-form--define-minor-mode ()
  "Generates autoload for define-minor-mode."
  (let ((form '(define-minor-mode my-mode "Toggle mode." :lighter " My")))
    (should (equal '(autoload 'my-mode "/path/lib.el" "Toggle mode." t)
                   (+modules--autoload-form form "/path/lib.el")))))

(ert-deftest modules--autoload-form--define-derived-mode ()
  "Generates autoload for define-derived-mode."
  (let ((form '(define-derived-mode my-mode prog-mode "MyMode" "Doc.")))
    (should (equal '(autoload 'my-mode "/path/lib.el" "Doc." t)
                   (+modules--autoload-form form "/path/lib.el")))))

(ert-deftest modules--autoload-form--returns-nil-for-unknown ()
  "Returns nil for unrecognized forms."
  (let ((form '(setq foo 'bar)))
    (should (null (+modules--autoload-form form "/path/lib.el")))))


;;; Tests for +modules-collect-autoloads

(ert-deftest modules--collect-autoloads--collects-from-modules ()
  "Collects autoloads from all discovered modules."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region ";;;###autoload\n(defun my-func () nil)"
                        nil (expand-file-name "lib.el" module-dir))
          (let ((result (+modules-collect-autoloads)))
            (should (= 1 (length result)))
            (should (eq 'my-func (cadr (car (car result)))))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--collect-autoloads--empty-when-no-autoloads ()
  "Returns empty list when no autoloads found."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "(defun not-autoloaded () nil)"
                        nil (expand-file-name "lib.el" module-dir))
          (should (null (+modules-collect-autoloads))))
      (delete-directory +modules-directory t))))


;;; Tests for +modules-register-autoloads

(ert-deftest modules--register-autoloads--makes-symbol-fboundp ()
  "Registering autoloads makes symbol fboundp."
  (let ((temp-file (make-temp-file "lib" nil ".el"))
        (sym (make-symbol "test-autoloaded-func")))
    (unwind-protect
        (progn
          ;; Intern symbol so we can check fboundp
          (setq sym (intern (symbol-name sym)))
          ;; Ensure symbol is not bound
          (fmakunbound sym)
          (should-not (fboundp sym))
          ;; Create autoload entry
          (let ((entries `(((defun ,sym () "Test." nil) . ,temp-file))))
            (+modules-register-autoloads entries))
          ;; Symbol should now be fboundp (as autoload)
          (should (fboundp sym))
          (should (autoloadp (symbol-function sym))))
      (when (fboundp sym)
        (fmakunbound sym))
      (delete-file temp-file))))

(ert-deftest modules--register-autoloads--handles-empty-list ()
  "Handles empty autoload list without error."
  (+modules-register-autoloads nil)
  (+modules-register-autoloads '()))

(ert-deftest modules--register-autoloads--skips-unknown-forms ()
  "Skips forms that can't be converted to autoloads."
  (let ((entries '(((setq foo 'bar) . "/path/lib.el"))))
    ;; Should not error
    (+modules-register-autoloads entries)))

(provide '+modules-tests)

;;; +modules-tests.el ends here
