;;; opam-mode-tests.el --- Tests for opam-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for `opam-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'opam-mode)


;;; Feature provisioning

(ert-deftest opam-mode/feature-provided ()
  "Loading `opam-mode.el' provides the `opam-mode' feature."
  (should (featurep 'opam-mode))
  (should (fboundp 'opam-mode)))

(ert-deftest opam-mode/hook-variable-exists ()
  "`opam-mode-hook' is bound after loading the library."
  (should (boundp 'opam-mode-hook)))


;;; Mode shape

(ert-deftest opam-mode/parent-is-conf-colon-mode ()
  "`opam-mode' derives from `conf-colon-mode'."
  (should (eq (get 'opam-mode 'derived-mode-parent) 'conf-colon-mode)))


;;; File association

(defun opam-mode-tests--opam-regex ()
  "Return the regex registered for `opam-mode' in `auto-mode-alist'."
  (car (cl-find-if (lambda (entry)
                     (and (stringp (car entry))
                          (eq (cdr entry) 'opam-mode)))
                   auto-mode-alist)))

(ert-deftest opam-mode/opam-files-open-in-opam-mode ()
  "A `foo.opam' path matches the regex; a `foo.opamignore' does not."
  (let ((re (opam-mode-tests--opam-regex)))
    (should re)
    (should (string-match-p re "/tmp/pkgs/foo.opam"))
    (should-not (string-match-p re "/tmp/pkgs/foo.opamignore"))))

(provide 'opam-mode-tests)

;;; opam-mode-tests.el ends here
