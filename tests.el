;;; tests.el --- Test runner with Elpaca load-path setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch-mode test runner for the Emacs configuration.
;;
;; Usage:
;;   emacs -Q --batch -l tests.el                    # run all tests
;;   emacs -Q --batch -l tests.el lisp/+compile      # run tests for file
;;   emacs -Q --batch -l tests.el "prefix-*"         # run tests matching pattern
;;
;; See specs/0002-test-runner.md for specification.

;;; Code:

(require 'ert)
(require 'subr-x)

(defvar +test-runner-root-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the Emacs configuration.")

;;; Load Path Setup

(defun +test-runner-load-paths ()
  "Set up load paths for test execution.
Adds lisp/, modules/*/, and all elpaca/builds/*/ directories to `load-path'."
  (let ((lisp-dir (expand-file-name "lisp" +test-runner-root-dir))
        (lib-dir (expand-file-name "lib" +test-runner-root-dir))
        (modules-dir (expand-file-name "modules" +test-runner-root-dir))
        (elpaca-builds-dir (expand-file-name "elpaca/builds" +test-runner-root-dir)))
    ;; Add lisp/ directory
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path lisp-dir))
    ;; Add lib/ subdirectories (each package in lib/ is its own directory)
    (when (file-directory-p lib-dir)
      (dolist (dir (directory-files lib-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    ;; Add modules/ subdirectories (each module is its own directory)
    (when (file-directory-p modules-dir)
      (dolist (dir (directory-files modules-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    ;; Add all elpaca/builds/*/ directories
    (when (file-directory-p elpaca-builds-dir)
      (dolist (dir (directory-files elpaca-builds-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))))

;;; Test Discovery

(defun +test-runner--discover-all-tests ()
  "Find all test files matching **/*-tests.el or **/tests.el.
Returns list of absolute file paths."
  (let ((test-files '()))
    ;; Search in lisp/
    (let ((lisp-dir (expand-file-name "lisp" +test-runner-root-dir)))
      (when (file-directory-p lisp-dir)
        (setq test-files
              (append test-files
                      (directory-files-recursively lisp-dir "-tests\\.el\\'")))))
    ;; Search in lib/
    (let ((lib-dir (expand-file-name "lib" +test-runner-root-dir)))
      (when (file-directory-p lib-dir)
        (setq test-files
              (append test-files
                      (directory-files-recursively lib-dir "-tests\\.el\\'")))))
    ;; Search in features/
    (let ((features-dir (expand-file-name "features" +test-runner-root-dir)))
      (when (file-directory-p features-dir)
        (setq test-files
              (append test-files
                      (directory-files-recursively features-dir "-tests\\.el\\'")))))
    ;; Search in modules/ (both tests.el and *-tests.el)
    (let ((modules-dir (expand-file-name "modules" +test-runner-root-dir)))
      (when (file-directory-p modules-dir)
        (setq test-files
              (append test-files
                      (directory-files-recursively
                       modules-dir
                       "\\(?:^tests\\.el\\|-tests\\.el\\)\\'")))))
    test-files))

(defun +test-runner--find-test-file (source-path)
  "Find test file for SOURCE-PATH.
SOURCE-PATH is a path like \"lisp/+compile\" (without -tests.el suffix).
Returns absolute path to the test file or nil if not found."
  (let* ((expanded (expand-file-name source-path +test-runner-root-dir))
         ;; Try with -tests.el suffix
         (test-file (concat expanded "-tests.el")))
    (if (file-exists-p test-file)
        test-file
      ;; Maybe they included the .el suffix
      (let ((without-el (string-remove-suffix ".el" expanded)))
        (setq test-file (concat without-el "-tests.el"))
        (when (file-exists-p test-file)
          test-file)))))

(defun +test-runner--pattern-p (arg)
  "Return non-nil if ARG looks like an ERT pattern (quoted or contains special chars)."
  ;; ERT patterns can be: symbol, string, or quoted patterns like "prefix-*"
  ;; Arguments starting with " or containing * are likely patterns
  (or (string-prefix-p "\"" arg)
      (string-match-p "[*]" arg)
      ;; Also treat things that look like ERT selectors
      (string-prefix-p "(" arg)
      (string-prefix-p ":" arg)))

;;; Main Entry Point

(defun +test-runner-run ()
  "Main entry point for the test runner.
Parses command-line arguments and runs appropriate tests."
  (+test-runner-load-paths)
  (let* ((args command-line-args-left)
         (arg (car args)))
    ;; Clear args to prevent Emacs from trying to process them
    (setq command-line-args-left nil)
    (cond
     ;; No arguments: run all tests
     ((null arg)
      (+test-runner--run-all-tests))
     ;; Pattern argument
     ((+test-runner--pattern-p arg)
      (+test-runner--run-pattern arg))
     ;; File path argument
     (t
      (+test-runner--run-file arg)))))

(defun +test-runner--run-all-tests ()
  "Discover and run all test files."
  (let ((test-files (+test-runner--discover-all-tests)))
    (if (null test-files)
        (progn
          (message "No test files found")
          (kill-emacs 0))
      (message "Discovered %d test file(s):" (length test-files))
      (dolist (file test-files)
        (message "  %s" (file-relative-name file +test-runner-root-dir)))
      (dolist (file test-files)
        (load file nil t))
      (ert-run-tests-batch-and-exit t))))

(defun +test-runner--run-file (source-path)
  "Run tests for SOURCE-PATH."
  (let ((test-file (+test-runner--find-test-file source-path)))
    (if test-file
        (progn
          (message "Running tests from: %s"
                   (file-relative-name test-file +test-runner-root-dir))
          (load test-file nil t)
          (ert-run-tests-batch-and-exit t))
      (message "No test file found for: %s" source-path)
      (message "Expected: %s-tests.el" source-path)
      (kill-emacs 1))))

(defun +test-runner--run-pattern (pattern)
  "Run tests matching PATTERN."
  ;; Remove surrounding quotes if present
  (let ((selector (string-trim pattern "\"")))
    (message "Running tests matching: %s" selector)
    ;; Load all test files first
    (dolist (file (+test-runner--discover-all-tests))
      (load file nil t))
    ;; Run with pattern
    (ert-run-tests-batch-and-exit selector)))

;; Run when loaded in batch mode
(when noninteractive
  (+test-runner-run))

;;; tests.el ends here
