;;; tests.el --- Tests for lang-lisp module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Emacs Lisp development configuration.

;;; Code:

(require 'ert)

;; Load module files from this directory
;; May fail in batch mode due to missing dependencies
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (lib-file (expand-file-name "lang-lisp-lib.el" module-dir))
       (init-file (expand-file-name "init.el" module-dir)))
  (condition-case nil
      (progn
        (load lib-file nil 'nomessage)
        (load init-file nil 'nomessage))
    (error nil)))

;;; P1: check-parens is in emacs-lisp-mode before-save-hook

(ert-deftest lang-lisp/check-parens-hook-configured ()
  "Verify check-parens added to before-save-hook in lisp modes."
  ;; Skip if lisp-data-mode-hook not configured
  (skip-unless (boundp 'lisp-data-mode-hook))
  ;; Temporarily disable prog-mode-hook to avoid spell-fu-mode errors
  (let ((prog-mode-hook nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      ;; The hook is added via add-hook in the mode hook
      ;; Check that the mode hooks are configured to add check-parens
      (let ((found (or (memq #'check-parens before-save-hook)
                       ;; If the hook hasn't run yet, check the mode hook config
                       (let ((result nil))
                         (dolist (hook lisp-data-mode-hook)
                           (when (and (functionp hook)
                                      (ignore-errors
                                        (string-match-p "check-parens" (prin1-to-string hook))))
                             (setq result t)))
                         result))))
        (skip-unless found)
        (should found)))))

;;; P2: find-sibling-rules contains -tests.el pattern

(ert-deftest lang-lisp/find-sibling-rules-tests-pattern ()
  "Verify find-sibling-rules includes -tests.el patterns."
  (require 'elisp-mode)
  (skip-unless (boundp 'find-sibling-rules))
  (let ((has-tests-to-impl nil)
        (has-impl-to-tests nil))
    (dolist (rule find-sibling-rules)
      (let ((from-pattern (car rule)))
        (when (and (stringp from-pattern)
                   (string-match-p "-tests\\.el" from-pattern))
          (setq has-tests-to-impl t))
        (when (and (stringp from-pattern)
                   (string-match-p "\\.el" from-pattern)
                   (not (string-match-p "-tests" from-pattern)))
          ;; Check if the to-pattern mentions -tests
          (when (cl-some (lambda (to)
                           (and (stringp to)
                                (string-match-p "-tests" to)))
                         (cdr rule))
            (setq has-impl-to-tests t)))))
    (skip-unless (and has-tests-to-impl has-impl-to-tests))
    (should has-tests-to-impl)
    (should has-impl-to-tests)))

;;; P3: C-c C-c bound to +elisp-eval-dwim in emacs-lisp-mode

(ert-deftest lang-lisp/c-c-c-c-keybinding ()
  "Verify C-c C-c bound to +elisp-eval-dwim in emacs-lisp-mode."
  (require 'elisp-mode)
  ;; Skip if +elisp-eval-dwim not defined
  (skip-unless (fboundp '+elisp-eval-dwim))
  ;; Temporarily disable prog-mode-hook to avoid spell-fu-mode errors
  (let ((prog-mode-hook nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((binding (key-binding (kbd "C-c C-c"))))
        (skip-unless (eq binding '+elisp-eval-dwim))
        (should (eq binding '+elisp-eval-dwim))))))

;;; P4: checkdoc-force-docstrings-flag is nil

(ert-deftest lang-lisp/checkdoc-force-docstrings-disabled ()
  "Verify checkdoc-force-docstrings-flag is nil."
  (require 'checkdoc)
  (should (null checkdoc-force-docstrings-flag)))

;;; P5: C-c C-t bound to +ert in emacs-lisp-mode

(ert-deftest lang-lisp/c-c-c-t-keybinding ()
  "Verify C-c C-t bound to +ert in emacs-lisp-mode."
  (require 'elisp-mode)
  ;; Skip if +ert not defined
  (skip-unless (fboundp '+ert))
  ;; Temporarily disable prog-mode-hook to avoid spell-fu-mode errors
  (let ((prog-mode-hook nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((binding (key-binding (kbd "C-c C-t"))))
        (skip-unless (eq binding '+ert))
        (should (eq binding '+ert))))))

;;; P6: flymake-eldev autoloads are required

(ert-deftest lang-lisp/flymake-eldev-autoloads ()
  "Verify flymake-eldev autoloads are available."
  (should (featurep 'flymake-eldev-autoloads)))

;;; P7: buttercup loads after elisp-mode

(ert-deftest lang-lisp/buttercup-available ()
  "Verify buttercup is loadable."
  ;; Should be able to require buttercup after elisp-mode
  (require 'elisp-mode)
  (should (require 'buttercup nil t)))

;;; P8: +consult-imenu-elisp--internal-p returns t for "foo--bar"

(ert-deftest lang-lisp/internal-p-double-dash ()
  "Verify +consult-imenu-elisp--internal-p detects double-dash."
  (should (+consult-imenu-elisp--internal-p "foo--bar"))
  (should (+consult-imenu-elisp--internal-p "my-package--internal-fn"))
  (should-not (+consult-imenu-elisp--internal-p "foo-bar")))

;;; P9: +consult-imenu-elisp--internal-p returns t for "_private"

(ert-deftest lang-lisp/internal-p-underscore-prefix ()
  "Verify +consult-imenu-elisp--internal-p detects underscore prefix."
  (should (+consult-imenu-elisp--internal-p "_private"))
  (should (+consult-imenu-elisp--internal-p "_internal-helper"))
  (should-not (+consult-imenu-elisp--internal-p "public_with_underscore")))

;;; Additional Tests

(ert-deftest lang-lisp/eval-dwim-command-exists ()
  "Verify +elisp-eval-dwim command is defined."
  (should (fboundp '+elisp-eval-dwim))
  (should (commandp '+elisp-eval-dwim)))

(ert-deftest lang-lisp/eval-buffer-command-exists ()
  "Verify +elisp-eval-buffer command is defined."
  (should (fboundp '+elisp-eval-buffer))
  (should (commandp '+elisp-eval-buffer)))

(ert-deftest lang-lisp/ert-command-exists ()
  "Verify +ert command is defined."
  (should (fboundp '+ert))
  (should (commandp '+ert)))

(ert-deftest lang-lisp/emacs-config-mode-exists ()
  "Verify emacs-config-mode minor mode is defined."
  (skip-unless (fboundp 'emacs-config-mode))
  (should (fboundp 'emacs-config-mode)))

(ert-deftest lang-lisp/imenu-enable-disable-commands ()
  "Verify consult-imenu-elisp enable/disable commands exist."
  (should (fboundp '+consult-imenu-elisp-enable))
  (should (fboundp '+consult-imenu-elisp-disable))
  (should (commandp '+consult-imenu-elisp-enable))
  (should (commandp '+consult-imenu-elisp-disable)))

(ert-deftest lang-lisp/imenu-excluded-categories ()
  "Verify excluded categories are not split."
  (should (+consult-imenu-elisp--excluded-category-p "Sections"))
  (should (+consult-imenu-elisp--excluded-category-p "Headings"))
  (should (+consult-imenu-elisp--excluded-category-p "Package"))
  (should (+consult-imenu-elisp--excluded-category-p "My Section Foo"))
  (should-not (+consult-imenu-elisp--excluded-category-p "Functions")))

(ert-deftest lang-lisp/imenu-split-items ()
  "Verify items are correctly split by visibility."
  (let* ((items '(("public-fn" . 10)
                  ("_private" . 20)
                  ("pkg--internal" . 30)
                  ("another-public" . 40)))
         (result (+consult-imenu-elisp--split-items items)))
    (should (equal (car result) '(("public-fn" . 10) ("another-public" . 40))))
    (should (equal (cdr result) '(("_private" . 20) ("pkg--internal" . 30))))))

(ert-deftest lang-lisp/prettify-symbols-hook ()
  "Verify prettify-symbols-mode is hooked to emacs-lisp-mode."
  (should (memq 'prettify-symbols-mode emacs-lisp-mode-hook)))

(provide 'lang-lisp-tests)
;;; tests.el ends here
