;;; test-setup.el --- Load path setup for running specific test files -*- lexical-binding: t; -*-

;;; Commentary:

;; Sets up load paths for test execution without running the test discovery.
;; Used by run-tests.sh when running specific test files.

;;; Code:

(require 'ert)

(defvar +test-setup-root-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Root directory of the Emacs configuration.
This is the parent of the scripts/ directory where test-setup.el lives.")

(defun +test-setup-configure-paths ()
  "Configure data/cache paths to match no-littering defaults.
This prevents tests from creating files in `user-emacs-directory'."
  (let ((var-dir (expand-file-name "var/" +test-setup-root-dir)))
    ;; Prevent tests from polluting the emacs config directory.
    ;; These mirror the settings from no-littering.
    (setq savehist-file (expand-file-name "savehist.el" var-dir))
    (setq project-list-file (expand-file-name "project-list.el" var-dir))
    (setq spell-fu-directory (expand-file-name "spell-fu/" var-dir))))

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

(+test-setup-configure-paths)
(+test-setup-load-paths)

(provide 'test-setup)
;;; test-setup.el ends here
