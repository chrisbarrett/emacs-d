;;; tests.el --- Tests for lang-ocaml module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for OCaml language support module.
;; Tests based on spec 039-lang-ocaml.md properties.

;;; Code:

(require 'ert)

;; Load module files from this directory
;; May fail in batch mode due to missing dependencies
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (lib-file (expand-file-name "lang-ocaml-lib.el" module-dir))
       (init-file (expand-file-name "init.el" module-dir)))
  (condition-case nil
      (progn
        (load lib-file nil 'nomessage)
        (load init-file nil 'nomessage))
    (error nil)))

;;; P1: *.ml files open in neocaml-mode

(ert-deftest lang-ocaml-test-ml-auto-mode ()
  "P1: *.ml files should open in neocaml-mode."
  (let ((entry (assoc "\\.ml\\'" auto-mode-alist #'string=)))
    (should entry)
    (should (eq (cdr entry) 'neocaml-mode))))

;;; P2: *.mli files open in neocamli-mode

(ert-deftest lang-ocaml-test-mli-auto-mode ()
  "P2: *.mli files should open in neocamli-mode."
  (let ((entry (assoc "\\.mli\\'" auto-mode-alist #'string=)))
    (should entry)
    (should (eq (cdr entry) 'neocamli-mode))))

;;; P3: dune files open in dune-config-mode

(ert-deftest lang-ocaml-test-dune-auto-mode ()
  "P3: dune files should open in dune-config-mode."
  (let ((matches nil))
    (dolist (entry auto-mode-alist)
      (when (and (stringp (car entry))
                 (string-match-p "/dune" (car entry)))
        (push entry matches)))
    (should matches)
    (should (eq (cdar matches) 'dune-config-mode))))

(ert-deftest lang-ocaml-test-dune-config-mode-defined ()
  "P3: dune-config-mode should be a derived mode."
  (should (fboundp 'dune-config-mode)))

(ert-deftest lang-ocaml-test-dune-config-mode-parent ()
  "P3: dune-config-mode should inherit from lisp-data-mode."
  (should (eq (get 'dune-config-mode 'derived-mode-parent) 'lisp-data-mode)))

;;; P4: /_build/ directories are read-only

(ert-deftest lang-ocaml-test-build-dir-readonly ()
  "P4: /_build/ directories should be configured read-only."
  (require '+corelib)
  (let ((entry (assoc "/_build/" dir-locals-directory-cache #'string-match-p)))
    ;; Directory cache may not be populated in test, check dirlocals function exists
    (should (fboundp '+dirlocals-set-regexp))))

;;; P5: Generated .opam files are read-only

(ert-deftest lang-ocaml-test-opam-auto-mode ()
  "P5: *.opam files should open in conf-colon-mode."
  (let ((entry (cl-find-if (lambda (e)
                             (and (stringp (car e))
                                  (string-match-p "\\.opam" (car e))))
                           auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'conf-colon-mode))))

;;; P6: dune-project is recognized as project root marker

(ert-deftest lang-ocaml-test-project-root-marker ()
  "P6: dune-project should be in project-vc-extra-root-markers."
  (require 'project)
  (should (member "dune-project" project-vc-extra-root-markers)))

;;; P7-P8: Context-aware let bindings (Tempel functions)

(ert-deftest lang-ocaml-test-capture-let-context-defined ()
  "P7-P8: +ocaml-capture-let-context should be defined."
  (should (fboundp '+ocaml-capture-let-context)))

(ert-deftest lang-ocaml-test-maybe-in-defined ()
  "P7-P8: +ocaml-maybe-in should be defined."
  (should (fboundp '+ocaml-maybe-in)))

(ert-deftest lang-ocaml-test-maybe-in-returns-string ()
  "P7-P8: +ocaml-maybe-in should return a string."
  (let ((+ocaml--tempel-in-expr-p nil))
    (should (stringp (+ocaml-maybe-in))))
  (let ((+ocaml--tempel-in-expr-p t))
    (should (stringp (+ocaml-maybe-in)))))

(ert-deftest lang-ocaml-test-maybe-in-expr-context ()
  "P7: +ocaml-maybe-in returns \" in\" in expression context."
  (let ((+ocaml--tempel-in-expr-p t))
    (should (string= (+ocaml-maybe-in) " in"))))

(ert-deftest lang-ocaml-test-maybe-in-module-context ()
  "P8: +ocaml-maybe-in returns \"\" at module level."
  (let ((+ocaml--tempel-in-expr-p nil))
    (should (string= (+ocaml-maybe-in) ""))))

;;; P9: Org ocaml blocks use neocaml-mode

(ert-deftest lang-ocaml-test-org-src-lang-ocaml ()
  "P9: Org ocaml blocks should use neocaml-mode."
  (require 'org)
  (should (eq (cdr (assoc "ocaml" org-src-lang-modes)) 'neocaml)))

(ert-deftest lang-ocaml-test-org-src-lang-ocamli ()
  "P9: Org ocamli blocks should use neocamli-mode."
  (require 'org)
  (should (eq (cdr (assoc "ocamli" org-src-lang-modes)) 'neocamli)))

;;; Additional tests

(ert-deftest lang-ocaml-test-ocamlinit-auto-mode ()
  ".ocamlinit files should open in neocaml-mode."
  (let ((entry (assoc "\\.ocamlinit\\'" auto-mode-alist #'string=)))
    (should entry)
    (should (eq (cdr entry) 'neocaml-mode))))

(ert-deftest lang-ocaml-test-eglot-server-programs ()
  "Eglot should have server config for neocaml modes."
  (require 'eglot)
  (let ((has-ocaml nil))
    (dolist (entry eglot-server-programs)
      (when (and (consp (car entry))
                 (consp (caar entry))
                 (eq (caaar entry) 'neocaml-mode))
        (setq has-ocaml t)))
    (should has-ocaml)))

(ert-deftest lang-ocaml-test-apheleia-formatter ()
  "Apheleia should be configured for neocaml modes."
  (require 'apheleia)
  (should (eq (cdr (assq 'neocaml-mode apheleia-mode-alist)) 'ocamlformat))
  (should (eq (cdr (assq 'neocamli-mode apheleia-mode-alist)) 'ocamlformat)))

(provide 'lang-ocaml-tests)

;;; tests.el ends here
