;;; tests.el --- Tests for lang-shscript module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for shell script editing configuration.

;;; Code:

(require 'ert)
(require 'cl-lib)

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
  (skip-unless (fboundp '+define-file-template))
  (require 'autoinsert)
  (should (cl-some (lambda (entry)
                     (let ((pattern (if (consp (car entry)) (caar entry) (car entry))))
                       (and (stringp pattern)
                            (string-match-p pattern "test.sh"))))
                   auto-insert-alist)))

;;; P4: New .zsh files get zsh shebang template

(ert-deftest lang-shscript-test-zsh-file-template ()
  "Shell script template should match .zsh files."
  (skip-unless (fboundp '+define-file-template))
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

;;; argc-mode activation gate tests

(require 'argc-mode)

;;; P33: +argc-maybe-enable skips if already active

(ert-deftest argc-test-maybe-enable-no-double ()
  "Calling +argc-maybe-enable twice should not double overlays."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (+argc-maybe-enable)
    (let ((count-1 (length (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'argc))
                            (overlays-in (point-min) (point-max))))))
      (+argc-maybe-enable)
      (let ((count-2 (length (cl-remove-if-not
                              (lambda (ov) (overlay-get ov 'argc))
                              (overlays-in (point-min) (point-max))))))
        (should (= count-1 count-2))))))

;;; P34: +argc-maybe-enable skips buffers without directives

(ert-deftest argc-test-maybe-enable-no-directives ()
  "Should not enable argc-mode without argc directives."
  (with-temp-buffer
    (insert "#!/bin/bash\necho hello\n")
    (+argc-maybe-enable)
    (should-not (bound-and-true-p argc-mode))))

;;; P35: +argc-maybe-enable only checks first 50 lines

(ert-deftest argc-test-maybe-enable-beyond-50-lines ()
  "Directives after line 50 should not trigger argc-mode."
  (with-temp-buffer
    (dotimes (_ 55) (insert "echo hello\n"))
    (insert "# @cmd Foo\n")
    (+argc-maybe-enable)
    (should-not (bound-and-true-p argc-mode))))

(ert-deftest argc-test-maybe-enable-within-50-lines ()
  "Directives within first 50 lines should trigger argc-mode."
  (with-temp-buffer
    (dotimes (_ 10) (insert "echo hello\n"))
    (insert "# @cmd Foo\n")
    (+argc-maybe-enable)
    (should (bound-and-true-p argc-mode))))

;;; P35b: +argc-maybe-enable skips indirect buffers

(ert-deftest argc-test-maybe-enable-skip-indirect ()
  "Should not enable argc-mode in indirect buffers."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (let ((indirect (make-indirect-buffer (current-buffer) " *argc-test-indirect*" t)))
      (unwind-protect
          (with-current-buffer indirect
            (+argc-maybe-enable)
            (should-not (bound-and-true-p argc-mode)))
        (kill-buffer indirect)))))

(provide 'lang-shscript-tests)

;;; tests.el ends here
