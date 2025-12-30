;;; init-lisp.el --- Configuration for Elisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)


;; Emacs' built-in profiler.
(use-package profiler
  :general-config
  (:keymaps 'profiler-report-mode-map
   :states 'normal
   "A" #'profiler-report-ascending-sort
   "D" #'profiler-report-descending-sort
   "K" #'profiler-report-describe-entry))


;; General configuration for all derived lisp modes.
(use-package lisp-mode
  :config
  (add-hook! '(lisp-data-mode-hook emacs-lisp-mode-hook)
    (add-hook 'before-save-hook #'check-parens nil t)))

(use-package +file-templates
  :config
  (+define-file-template (rx ".el" eos) "emacs-lisp.eld"))

;; Emacs Lisp dialect-specific language.
(use-package elisp-mode
  :config
  (use-package mod-emacs-lisp :demand t))

;; Emacs' built-in test framework.
(use-package ert
  :general
  (:keymaps '(ert-results-mode-map emacs-lisp-mode-map)
            "C-c C-t" #'+ert)
  (:states 'motion :keymaps 'ert-results-mode-map
           "L" 'ert-results-toggle-printer-limits-for-test-at-point
           "T" 'ert-results-pop-to-timings
           "B" 'ert-results-pop-to-backtrace-for-test-at-point
           "H" 'ert-results-describe-test-at-point
           "M-n" 'ert-results-next-test
           "M-p" 'ert-results-previous-test)
  :init
  (defun +ert (arg)
    (interactive "p")
    (if arg
        (ert t)
      (call-interactively #'ert))))


;; Teach flymake to support eldev projects.
;;
;; See: https://emacs-eldev.github.io/eldev
(use-package flymake-eldev :ensure t
  :init
  (require 'flymake-eldev-autoloads))


;; A BDD-style testing framework for Elisp.
;;
;; Ordinarily it will be installed in relevant packages by eldev; however,
;; it's nice to have here so I can load it and get macro indentation right.
(use-package buttercup :ensure t
  :demand t
  :after elisp-mode)


(provide 'init-lisp)

;;; init-lisp.el ends here
