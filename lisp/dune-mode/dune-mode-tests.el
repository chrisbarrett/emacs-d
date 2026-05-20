;;; dune-mode-tests.el --- Tests for dune-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for `dune-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dune-mode)


;;; Feature provisioning

(ert-deftest dune-mode/feature-provided ()
  "Loading `dune-mode.el' provides the `dune-mode' feature."
  (should (featurep 'dune-mode))
  (should (fboundp 'dune-mode)))


;;; Mode shape

(ert-deftest dune-mode/parent-is-lisp-data-mode ()
  "`dune-mode' derives from `lisp-data-mode'."
  (should (eq (get 'dune-mode 'derived-mode-parent) 'lisp-data-mode)))

(ert-deftest dune-mode/comment-add-is-zero ()
  "Activating `dune-mode' sets `comment-add' to 0 buffer-locally."
  (with-temp-buffer
    (dune-mode)
    (should (local-variable-p 'comment-add))
    (should (eq comment-add 0))))


;;; File association

(defun dune-mode-tests--dune-regex ()
  "Return the regex registered for `dune-mode' in `auto-mode-alist'."
  (car (cl-find-if (lambda (entry)
                     (and (stringp (car entry))
                          (eq (cdr entry) #'dune-mode)))
                   auto-mode-alist)))

(ert-deftest dune-mode/dune-files-open-in-dune-mode ()
  "Paths ending in dune/dune-workspace/dune-project match the regex."
  (let ((re (dune-mode-tests--dune-regex)))
    (should re)
    (should (string-match-p re "/tmp/proj/dune"))
    (should (string-match-p re "/tmp/proj/dune-workspace"))
    (should (string-match-p re "/tmp/proj/dune-project"))))

(ert-deftest dune-mode/dune-txt-does-not-match ()
  "A `dune.txt' file does not match the `dune-mode' regex."
  (let ((re (dune-mode-tests--dune-regex)))
    (should re)
    (should-not (string-match-p re "/tmp/proj/dune.txt"))))

(provide 'dune-mode-tests)

;;; dune-mode-tests.el ends here
