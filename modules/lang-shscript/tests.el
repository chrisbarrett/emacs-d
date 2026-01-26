;;; tests.el --- Tests for lang-shscript module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for shell script editing configuration.

;;; Code:

(require 'ert)

(condition-case nil
    (load (expand-file-name "init.el" (file-name-directory (or load-file-name buffer-file-name))))
  (error nil))

;;; P1: Files with nix-shell shebang open in bash-ts-mode

(ert-deftest lang-shscript-test-nix-shell-magic-mode ()
  "Nix-shell shebang should trigger bash-ts-mode."
  (should (assoc-default "#!/usr/bin/env nix-shell\n" magic-mode-alist #'string-match-p)))

;;; P2: sh-set-shell does not emit messages

(ert-deftest lang-shscript-test-sh-set-shell-silenced ()
  "sh-set-shell should have silence advice."
  (should (advice-member-p 'sh-set-shell@silence-messages 'sh-set-shell)))

;;; P3: New .sh files get bash shebang template

(ert-deftest lang-shscript-test-sh-file-template ()
  "Shell script file template should match .sh files."
  (skip-unless (featurep '+file-templates))
  (require 'autoinsert)
  (should (cl-some (lambda (entry)
                     (let ((pattern (if (consp (car entry)) (caar entry) (car entry))))
                       (and (stringp pattern)
                            (string-match-p pattern "test.sh"))))
                   auto-insert-alist)))

;;; P4: New .zsh files get zsh shebang template

(ert-deftest lang-shscript-test-zsh-file-template ()
  "Shell script template should match .zsh files."
  (skip-unless (featurep '+file-templates))
  (require 'autoinsert)
  (should (cl-some (lambda (entry)
                     (let ((pattern (if (consp (car entry)) (caar entry) (car entry))))
                       (and (stringp pattern)
                            (string-match-p pattern "test.zsh"))))
                   auto-insert-alist)))

;;; P5: Script files made executable on save

(ert-deftest lang-shscript-test-executable-on-save ()
  "after-save-hook should include executable-make-buffer-file-executable-if-script-p."
  (should (memq 'executable-make-buffer-file-executable-if-script-p after-save-hook)))

;;; P6: Tempel snippet dir available

(ert-deftest lang-shscript-test-tempel-dir-snippet ()
  "Tempel snippet 'dir' should exist in sh-base.eld."
  (let ((template-file (expand-file-name "templates/sh-base.eld" user-emacs-directory)))
    (skip-unless (file-exists-p template-file))
    (with-temp-buffer
      (insert-file-contents template-file)
      (should (search-forward "(dir " nil t)))))

;;; P7: Tempel snippet err available

(ert-deftest lang-shscript-test-tempel-err-snippet ()
  "Tempel snippet 'err' should exist in sh-base.eld."
  (let ((template-file (expand-file-name "templates/sh-base.eld" user-emacs-directory)))
    (skip-unless (file-exists-p template-file))
    (with-temp-buffer
      (insert-file-contents template-file)
      (should (search-forward "(err " nil t)))))

(provide 'lang-shscript-tests)

;;; tests.el ends here
