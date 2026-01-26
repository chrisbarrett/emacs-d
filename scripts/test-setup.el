;;; test-setup.el --- Load path setup for running specific test files -*- lexical-binding: t; -*-

;;; Commentary:

;; Sets up load paths for test execution without running the test discovery.
;; Used by run-tests.sh when running specific test files.

;;; Code:

(require 'ert)

(defvar +test-setup-root-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Root directory of the Emacs configuration.")

(defun +test-setup-load-paths ()
  "Set up load paths for test execution."
  (setq user-emacs-directory (file-name-as-directory +test-setup-root-dir))
  (let ((lisp-dir (expand-file-name "lisp" +test-setup-root-dir))
        (lib-dir (expand-file-name "lib" +test-setup-root-dir))
        (modules-dir (expand-file-name "modules" +test-setup-root-dir))
        (elpaca-builds-dir (expand-file-name "elpaca/builds" +test-setup-root-dir)))
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path lisp-dir))
    (when (file-directory-p lib-dir)
      (dolist (dir (directory-files lib-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    (when (file-directory-p modules-dir)
      (dolist (dir (directory-files modules-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    (when (file-directory-p elpaca-builds-dir)
      (dolist (dir (directory-files elpaca-builds-dir t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))))

(+test-setup-load-paths)

(provide 'test-setup)
;;; test-setup.el ends here
