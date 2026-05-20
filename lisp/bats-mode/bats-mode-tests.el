;;; bats-mode-tests.el --- Tests for bats-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for `bats-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bats-mode)


;;; Helpers

(defmacro bats-mode-tests--with-buffer (contents &rest body)
  "Insert CONTENTS into a `bats-mode' buffer and evaluate BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     (bats-mode)
     (font-lock-ensure)
     ,@body))

(defun bats-mode-tests--face-at (search-string)
  "Move past first occurrence of SEARCH-STRING; return face at its start."
  (goto-char (point-min))
  (search-forward search-string)
  (get-text-property (match-beginning 0) 'face))

(defun bats-mode-tests--faces-on (search-string)
  "Return list of faces covering first occurrence of SEARCH-STRING."
  (goto-char (point-min))
  (search-forward search-string)
  (let ((face (get-text-property (match-beginning 0) 'face)))
    (cond
     ((null face) nil)
     ((listp face) face)
     (t (list face)))))

(defun bats-mode-tests--has-face-p (search-string face)
  "Return non-nil if FACE applies anywhere on first match of SEARCH-STRING."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward search-string nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (found nil))
        (while (and (not found) (< beg end))
          (let* ((prop (get-text-property beg 'face))
                 (faces (cond ((null prop) nil)
                              ((listp prop) prop)
                              (t (list prop)))))
            (when (memq face faces) (setq found t)))
          (setq beg (1+ beg)))
        found))))


;;; Activation

(ert-deftest bats-mode/activates-on-bats-extension ()
  "Visiting a `.bats' file selects `bats-mode' via `set-auto-mode'."
  (let ((tmp (make-temp-file "bats-mode-test-" nil ".bats")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmp)
          (unwind-protect
              (progn
                (should (derived-mode-p 'bats-mode))
                (should (derived-mode-p 'bash-ts-mode)))
            (kill-buffer (current-buffer))))
      (delete-file tmp))))

(ert-deftest bats-mode/activates-on-bats-interpreter ()
  "A `#!/usr/bin/env bats' shebang selects `bats-mode' via interpreter alist."
  (let ((tmp (make-temp-file "bats-mode-test-shebang-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "#!/usr/bin/env bats\n"))
          (with-current-buffer (find-file-noselect tmp)
            (unwind-protect
                (should (derived-mode-p 'bats-mode))
              (kill-buffer (current-buffer)))))
      (delete-file tmp))))

(ert-deftest bats-mode/preserves-bash-ts-mode-fontification ()
  "Bash keywords still carry the faces bash-ts-mode would apply.

The mode derives from `bash-ts-mode'; bats keyword regexes layer
on top but must not displace inherited TS fontification."
  (bats-mode-tests--with-buffer "if true; then\n  echo foo\nfi\n"
    (should (eq (bats-mode-tests--face-at "if") 'font-lock-keyword-face))
    (should (eq (bats-mode-tests--face-at "then") 'font-lock-keyword-face))))


;;; Directive font-lock

(ert-deftest bats-mode/fontifies-test-directive ()
  "`@test' carries `bats-directive-face'."
  (bats-mode-tests--with-buffer "@test \"adds\" {\n  run true\n}\n"
    (should (bats-mode-tests--has-face-p "@test" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-fixture-setup ()
  (bats-mode-tests--with-buffer "setup() { :; }\n"
    (should (bats-mode-tests--has-face-p "setup" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-fixture-teardown ()
  (bats-mode-tests--with-buffer "teardown() { :; }\n"
    (should (bats-mode-tests--has-face-p "teardown" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-fixture-setup-file ()
  (bats-mode-tests--with-buffer "setup_file() { :; }\n"
    (should (bats-mode-tests--has-face-p "setup_file" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-fixture-teardown-file ()
  (bats-mode-tests--with-buffer "teardown_file() { :; }\n"
    (should (bats-mode-tests--has-face-p "teardown_file" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-fixture-setup-suite ()
  (bats-mode-tests--with-buffer "setup_suite() { :; }\n"
    (should (bats-mode-tests--has-face-p "setup_suite" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-fixture-teardown-suite ()
  (bats-mode-tests--with-buffer "teardown_suite() { :; }\n"
    (should (bats-mode-tests--has-face-p "teardown_suite" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-load-directive ()
  (bats-mode-tests--with-buffer "load 'test_helper'\n"
    (should (bats-mode-tests--has-face-p "load" 'bats-directive-face))))

(ert-deftest bats-mode/fontifies-bats-load-library ()
  (bats-mode-tests--with-buffer "bats_load_library 'bats-assert'\n"
    (should (bats-mode-tests--has-face-p "bats_load_library"
                                         'bats-directive-face))))

(ert-deftest bats-mode/fontifies-run-skip-on-failure ()
  (bats-mode-tests--with-buffer
      "@test \"x\" { run true; skip; bats::on_failure; }\n"
    (should (bats-mode-tests--has-face-p "run" 'bats-directive-face))
    (should (bats-mode-tests--has-face-p "skip" 'bats-directive-face))
    (should (bats-mode-tests--has-face-p "bats::on_failure"
                                         'bats-directive-face))))


;;; Profile detection + assertion font-lock

(ert-deftest bats-mode/assertion-face-when-bats-assert-loaded ()
  "Direct `bats_load_library' makes assertions carry `bats-assertion-face'."
  (bats-mode-tests--with-buffer
      "bats_load_library 'bats-assert'\n@test \"x\" { assert_equal 1 1; }\n"
    (should (bats-mode-tests--has-face-p "assert_equal"
                                         'bats-assertion-face))))

(ert-deftest bats-mode/assertion-face-via-test-helper-followthrough ()
  "`load test_helper' + sibling helper loading `bats-assert' fontifies asserts."
  (let* ((dir (make-temp-file "bats-mode-helper-" t))
         (helper (expand-file-name "test_helper.bash" dir))
         (test-file (expand-file-name "foo.bats" dir)))
    (unwind-protect
        (progn
          (with-temp-file helper
            (insert "bats_load_library 'bats-assert'\n"))
          (with-temp-file test-file
            (insert "load 'test_helper'\n")
            (insert "@test \"x\" { assert_equal 1 1; }\n"))
          (with-current-buffer (find-file-noselect test-file)
            (unwind-protect
                (progn
                  (font-lock-ensure)
                  (should (bats-mode-tests--has-face-p
                           "assert_equal" 'bats-assertion-face)))
              (kill-buffer (current-buffer)))))
      (delete-directory dir t))))

(ert-deftest bats-mode/no-assertion-face-when-not-loaded ()
  "Without a `bats-assert' load, assertion names are not faced."
  (bats-mode-tests--with-buffer "@test \"x\" { assert_equal 1 1; }\n"
    (should-not (bats-mode-tests--has-face-p
                 "assert_equal" 'bats-assertion-face))))

(ert-deftest bats-mode/profile-cache-refreshes-after-save ()
  "Saving the buffer recomputes active profiles."
  (let ((tmp (make-temp-file "bats-mode-save-" nil ".bats")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmp)
          (unwind-protect
              (progn
                (erase-buffer)
                (insert "@test \"x\" { assert_equal 1 1; }\n")
                (save-buffer)
                (font-lock-ensure)
                (should-not (bats-mode-tests--has-face-p
                             "assert_equal" 'bats-assertion-face))
                (goto-char (point-min))
                (insert "bats_load_library 'bats-assert'\n")
                (save-buffer)
                (font-lock-ensure)
                (should (bats-mode-tests--has-face-p
                         "assert_equal" 'bats-assertion-face)))
            (kill-buffer (current-buffer))))
      (delete-file tmp))))


;;; `$BATS_*' variable font-lock

(ert-deftest bats-mode/fontifies-documented-bats-variable ()
  (bats-mode-tests--with-buffer "echo \"$BATS_TEST_NAME\"\n"
    (should (bats-mode-tests--has-face-p
             "BATS_TEST_NAME" 'bats-variable-face))))

(ert-deftest bats-mode/does-not-fontify-user-coined-bats-prefix ()
  "A user-defined `BATS_*' variable does not carry `bats-variable-face'."
  (bats-mode-tests--with-buffer "echo \"$BATS_USER_THING\"\n"
    (should-not (bats-mode-tests--has-face-p
                 "BATS_USER_THING" 'bats-variable-face))))


;;; Imenu

(ert-deftest bats-mode/imenu-collects-tests ()
  (bats-mode-tests--with-buffer
      "@test \"adds\" { run true; }\n@test \"subtracts\" { run false; }\n"
    (let* ((index (bats-mode--imenu-create-index))
           (tests (cdr (assoc "Tests" index))))
      (should tests)
      (should (assoc "adds" tests))
      (should (assoc "subtracts" tests)))))

(ert-deftest bats-mode/imenu-collects-fixtures ()
  (bats-mode-tests--with-buffer
      "setup() { :; }\nteardown_file() { :; }\n"
    (let* ((index (bats-mode--imenu-create-index))
           (fixtures (cdr (assoc "Fixtures" index))))
      (should fixtures)
      (should (assoc "setup" fixtures))
      (should (assoc "teardown_file" fixtures)))))

(ert-deftest bats-mode/imenu-collects-bash-functions ()
  (bats-mode-tests--with-buffer
      "helper_fn() { echo hi; }\n@test \"x\" { run true; }\n"
    (let* ((index (bats-mode--imenu-create-index))
           (functions (cdr (assoc "Functions" index))))
      (should functions)
      (should (cl-some (lambda (entry)
                         (and (stringp (car entry))
                              (string-match-p "helper_fn" (car entry))))
                       functions)))))


(provide 'bats-mode-tests)
;;; bats-mode-tests.el ends here
