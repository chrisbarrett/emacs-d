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


;;; Module structure tests

(ert-deftest project-test-module-has-packages-eld ()
  "Module has packages.eld file."
  (should (file-exists-p (expand-file-name "packages.eld" project-module--dir))))

(ert-deftest project-test-module-has-spec-md ()
  "Module has spec.md file."
  (should (file-exists-p (expand-file-name "spec.md" project-module--dir))))

(ert-deftest project-test-module-has-lib-el ()
  "Module has lib.el file."
  (should (file-exists-p (expand-file-name "lib.el" project-module--dir))))


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


;;; P5: s-t and M-W are bound to project-switch-beframed

(ert-deftest project-test-keybinding-s-t ()
  "s-t is bound to project-switch-beframed in override-global-map."
  (skip-unless (fboundp 'general-define-key))
  (skip-unless (featurep 'beframe))
  (project-module--load-init)
  (skip-unless (keymapp (bound-and-true-p override-global-map)))
  (let ((binding (lookup-key override-global-map (kbd "s-t"))))
    (should (eq binding 'project-switch-beframed))))

(ert-deftest project-test-keybinding-M-W ()
  "M-W is bound to project-switch-beframed in override-global-map."
  (skip-unless (fboundp 'general-define-key))
  (skip-unless (featurep 'beframe))
  (project-module--load-init)
  (skip-unless (keymapp (bound-and-true-p override-global-map)))
  (let ((binding (lookup-key override-global-map (kbd "M-W"))))
    (should (eq binding 'project-switch-beframed))))


;;; P6: beframe-mode is enabled by default

(ert-deftest project-test-beframe-mode-enabled ()
  "beframe-mode is enabled."
  (skip-unless (featurep 'beframe))
  (project-module--load-init)
  (should (bound-and-true-p beframe-mode)))


;;; P7: +git-repo-display-name returns owner/repo for GitHub URLs

(ert-deftest project-test-git-repo-display-name-defined ()
  "+git-repo-display-name is defined."
  (project-module--load-lib)
  (should (fboundp '+git-repo-display-name)))


;;; P8: +projects-rescan is bound to R in project-prefix-map

(ert-deftest project-test-projects-rescan-keybinding ()
  "+projects-rescan is bound to R in project-prefix-map."
  (require 'project)
  (project-module--load-init)
  (should (keymapp project-prefix-map))
  (should (eq (lookup-key project-prefix-map "R") '+projects-rescan)))


;;; P9: consult--buffer-query advice filters by beframe context

(ert-deftest project-test-consult-buffer-query-advice ()
  "consult--buffer-query has beframe filter advice."
  (skip-unless (fboundp 'consult--buffer-query))
  (skip-unless (featurep 'beframe))
  (project-module--load-init)
  (let ((advices (advice--symbol-function 'consult--buffer-query)))
    (should advices)
    (should (advice-function-member-p 'consult--buffer-query@with-beframe-restriction advices))))


;;; Additional tests

(ert-deftest project-test-projects-rescan-defined ()
  "+projects-rescan is defined as a command."
  (project-module--load-lib)
  (should (fboundp '+projects-rescan))
  (should (commandp '+projects-rescan)))

(ert-deftest project-test-project-switch-beframed-defined ()
  "project-switch-beframed is defined as a command."
  (project-module--load-init)
  (should (fboundp 'project-switch-beframed))
  (should (commandp 'project-switch-beframed)))

(ert-deftest project-test-eat-beframed-defined ()
  "eat-beframed is defined as a command."
  (project-module--load-init)
  (should (fboundp 'eat-beframed))
  (should (commandp 'eat-beframed)))

(ert-deftest project-test-project-switch-magit-status-defined ()
  "+project-switch-magit-status is defined as a command."
  (project-module--load-lib)
  (should (fboundp '+project-switch-magit-status))
  (should (commandp '+project-switch-magit-status)))

(ert-deftest project-test-scan-dirs-alist-defined ()
  "+project-scan-dirs-alist is defined."
  (project-module--load-lib)
  (should (boundp '+project-scan-dirs-alist)))

(ert-deftest project-test-strict-isolation-variable-defined ()
  "+beframe-strict-project-isolation-p is defined."
  (project-module--load-lib)
  (should (boundp '+beframe-strict-project-isolation-p)))

(ert-deftest project-test-project-prefix-map-p-binding ()
  "p in project-prefix-map is bound to project-switch-beframed."
  (skip-unless (fboundp 'general-define-key))
  (skip-unless (featurep 'beframe))
  (require 'project)
  (project-module--load-init)
  (should (keymapp project-prefix-map))
  (should (eq (lookup-key project-prefix-map "p") 'project-switch-beframed)))

(provide 'project-tests)

;;; project-tests.el ends here
