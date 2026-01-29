;;; project-tests.el --- Tests for project module -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the project module.

;;; Code:

(require 'ert)

(defconst project-module--dir
  (expand-file-name "modules/project/" user-emacs-directory)
  "Directory containing the project module.")

(defun project-module--load-lib ()
  "Load project module lib.el."
  (load (expand-file-name "lib.el" project-module--dir) nil t))

(defun project-module--load-init ()
  "Load project module init.el."
  (require '+corelib)
  (project-module--load-lib)
  (condition-case nil
      (load (expand-file-name "init.el" project-module--dir) nil t)
    (error nil)))


;;; P1: project-vc-ignores contains ".cache/"

(ert-deftest project-test-vc-ignores-cache ()
  "project-vc-ignores includes .cache/."
  (require 'project)
  (project-module--load-init)
  (should (member ".cache/" project-vc-ignores)))


;;; P2: project-list-exclude filters /nix/store/ paths

(ert-deftest project-test-list-exclude-nix-store ()
  "project-list-exclude includes nix-store filter."
  (require 'project)
  (project-module--load-init)
  (should (consp project-list-exclude))
  (should (cl-some #'stringp project-list-exclude)))


;;; P3: Hidden directories (except ~/.config/) are excluded

(ert-deftest project-test-exclude-hidden-dirs-function-defined ()
  "Function for excluding hidden dirs is defined in project-list-exclude."
  (require 'project)
  (project-module--load-init)
  (should (cl-some #'functionp project-list-exclude)))


;;; P4: project-switch-commands is +project-switch-magit-status

(ert-deftest project-test-switch-commands ()
  "project-switch-commands is set to +project-switch-magit-status."
  (require 'project)
  (project-module--load-init)
  (should (eq project-switch-commands '+project-switch-magit-status)))


;;; P6: beframe-mode is enabled by default

(ert-deftest project-test-beframe-mode-enabled ()
  "beframe-mode is enabled."
  (skip-unless (featurep 'beframe))
  (project-module--load-init)
  (should (bound-and-true-p beframe-mode)))


;;; P9: consult--buffer-query advice filters by beframe context

(ert-deftest project-test-consult-buffer-query-advice ()
  "consult--buffer-query has beframe filter advice."
  (skip-unless (fboundp 'consult--buffer-query))
  (skip-unless (featurep 'beframe))
  (project-module--load-init)
  (let ((advices (advice--symbol-function 'consult--buffer-query)))
    (should advices)
    (should (advice-function-member-p 'consult--buffer-query@with-beframe-restriction advices))))


(provide 'project-tests)

;;; project-tests.el ends here
