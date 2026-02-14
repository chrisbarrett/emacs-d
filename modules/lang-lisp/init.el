;;; init.el --- Emacs Lisp development -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Lisp development configuration with testing, linting, and enhanced indentation.

;;; Code:

(require '+autoloads)
(require '+corelib)

(pushnew! find-sibling-rules
          ;; Tests -> impl
          (list (rx (group (+? any)) "-tests.el" eos)
                (rx (backref 1) ".el"))
          ;; Impl -> tests
          (list (rx (group (+? any)) ".el" eos)
                (rx (backref 1) "-tests.el")))

(+dirlocals-set (list (file-name-concat user-emacs-directory "lisp")
                      (file-name-concat user-emacs-directory "modules"))
  `((emacs-lisp-mode . ((mode . emacs-config)))))

(+define-file-template (rx ".el" eos) "emacs-lisp.eld")

(use-package lisp-mode
  :hook (lisp-data-mode-hook . +check-parens-before-save-h))

(use-package elisp-mode
  :hook (emacs-lisp-mode-hook . +check-parens-before-save-h)
  :general
  (:keymaps 'emacs-lisp-mode-map
            "C-c RET" #'pp-macroexpand-last-sexp
            "C-c C-c" #'+elisp-eval-dwim)

  :preface
  ;; Correct value is project-dependent; avoid using global value and set via
  ;; hooks instead.
  (make-variable-buffer-local 'elisp-flymake-byte-compile-load-path)

  :config
  (+load "config/+indent.el")
  (+load "config/+loads.el")

  (+local-leader-set-key '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e" '(nil :which-key "eval")
    "eb" #'+elisp-eval-buffer))

(use-package tart-mode
  :disabled t
  :load-path "~/src/chrisbarrett/emacs-tart/lisp"
  :hook (emacs-lisp-mode-hook . tart-eglot-ensure)
  :custom
  (tart-executable "~/src/chrisbarrett/emacs-tart/tart")
  :config
  (+load "config/+tart.el"))

(use-package elpaca
  :after elisp-mode
  :config
  (+load "config/+elpaca.el"))

(use-package prog-mode
  :hook (emacs-lisp-mode-hook . prettify-symbols-mode))

(use-package evil
  :after elisp-mode
  :functions (evil-normal-state)
  :config
  (+load "config/+evil.el"))

(use-package checkdoc
  :custom
  (checkdoc-force-docstrings-flag nil))

(use-package profiler
  :general-config
  (:keymaps 'profiler-report-mode-map
   :states 'normal
   "A" #'profiler-report-ascending-sort
   "D" #'profiler-report-descending-sort
   "K" #'profiler-report-describe-entry))

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

  :config
  (+local-leader-set-key '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "t" '(nil :which-key "test")
    "tt" '+ert
    "td" 'ert-delete-test
    "tD" 'ert-delete-all-tests))

;;; init.el ends here
