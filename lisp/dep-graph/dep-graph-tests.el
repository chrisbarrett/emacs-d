;;; dep-graph-tests.el --- Tests for dep-graph -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for the `dep-graph' library.  Covers the pure core
;; (build / dependents / affected), the IO shell (scan-file /
;; scan-root), and the CLI entry point.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dep-graph)

(ert-deftest dep-graph/harness-loads ()
  "Sanity check so the test runner picks up the file."
  (should (featurep 'dep-graph)))


;;; Pure core — `dep-graph-build'

(ert-deftest dep-graph/build/empty-input-yields-empty-graph ()
  "Empty file-forms list SHALL produce an empty graph."
  (let ((graph (dep-graph-build '())))
    (should (hash-table-p graph))
    (should (zerop (hash-table-count graph)))))

(ert-deftest dep-graph/build/direct-dependent-mapped ()
  "A file that requires a feature SHALL appear under that feature."
  (let ((graph (dep-graph-build
                '(("a.el" :provides (a) :requires (b))
                  ("b.el" :provides (b) :requires ())))))
    (should (equal (gethash 'b graph) '("a.el")))
    (should (null (gethash 'a graph)))))

(ert-deftest dep-graph/build/duplicate-provider-rejected ()
  "Two files that provide the same feature SHALL signal user-error
mentioning the feature and both paths."
  (let* ((err (should-error
               (dep-graph-build
                '(("a.el" :provides (a) :requires ())
                  ("a-other.el" :provides (a) :requires ())))
               :type 'user-error))
         (message (error-message-string err)))
    (should (string-match-p "\\<a\\>" message))
    (should (string-match-p "a\\.el" message))
    (should (string-match-p "a-other\\.el" message))))


;;; Pure core — `dep-graph-dependents'

(ert-deftest dep-graph/dependents/single-file-no-dependents ()
  "A file no one requires SHALL return a list containing just itself."
  (let ((graph (dep-graph-build
                '(("c.el" :provides (c) :requires ())))))
    (should (equal (sort (dep-graph-dependents graph "c.el")
                         #'string<)
                   '("c.el")))))

(ert-deftest dep-graph/dependents/two-hop-chain ()
  "Two-hop chain a → b → c SHALL yield all three files."
  (let ((graph (dep-graph-build
                '(("a.el" :provides (a) :requires (b))
                  ("b.el" :provides (b) :requires (c))
                  ("c.el" :provides (c) :requires ())))))
    (should (equal (sort (dep-graph-dependents graph "c.el")
                         #'string<)
                   '("a.el" "b.el" "c.el")))))

(ert-deftest dep-graph/dependents/cycle-terminates ()
  "A cycle SHALL terminate via a visited set rather than loop."
  (let ((graph (dep-graph-build
                '(("a.el" :provides (a) :requires (b))
                  ("b.el" :provides (b) :requires (a))))))
    (should (equal (sort (dep-graph-dependents graph "a.el")
                         #'string<)
                   '("a.el" "b.el")))))


;;; Pure core — `dep-graph-affected'

(ert-deftest dep-graph/affected/union-across-starting-files ()
  "Union of dependents from each starting file SHALL collapse duplicates."
  (let ((graph (dep-graph-build
                '(("a.el" :provides (a) :requires (b c))
                  ("b.el" :provides (b) :requires ())
                  ("c.el" :provides (c) :requires ())))))
    (should (equal (sort (dep-graph-affected graph '("b.el" "c.el"))
                         #'string<)
                   '("a.el" "b.el" "c.el")))))

(ert-deftest dep-graph/affected/filter-selects-matching-paths ()
  "Filter predicate SHALL drop paths that fail it."
  (let* ((graph (dep-graph-build
                 '(("a.el" :provides (a) :requires (b))
                   ("a-tests.el" :provides (a-tests) :requires (a))
                   ("b.el" :provides (b) :requires ())))))
    (should (equal (sort (dep-graph-affected
                          graph '("b.el")
                          (lambda (p) (string-suffix-p "-tests.el" p)))
                         #'string<)
                   '("a-tests.el")))))


;;; IO shell — `dep-graph-scan-file'

(defmacro dep-graph-tests--with-fixture (var contents &rest body)
  "Bind VAR to a temporary .el file containing CONTENTS, run BODY.

CONTENTS is a string written verbatim.  The file is deleted on
exit regardless of how BODY returns."
  (declare (indent 2))
  `(let ((,var (make-temp-file "dep-graph-fixture-" nil ".el")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,contents))
           ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(ert-deftest dep-graph/scan-file/basic-provide-and-require ()
  "A file with one `provide` and one `require` SHALL be summarised."
  (dep-graph-tests--with-fixture file
      ";;; foo.el -*- lexical-binding: t; -*-\n\
(require 'bar)\n\
(provide 'foo)\n"
    (let ((summary (dep-graph-scan-file file)))
      (should (equal (plist-get (cdr summary) :provides) '(foo)))
      (should (equal (plist-get (cdr summary) :requires) '(bar)))
      (should (equal (car summary) file)))))

(ert-deftest dep-graph/scan-file/docstring-require-not-extracted ()
  "`require` text inside a docstring SHALL NOT enter `:requires`."
  (dep-graph-tests--with-fixture file
      ";;; foo.el -*- lexical-binding: t; -*-\n\
(defun foo-fn ()\n\
  \"Like (require 'bar) but does not actually require anything.\"\n\
  nil)\n\
(provide 'foo)\n"
    (let ((summary (dep-graph-scan-file file)))
      (should (equal (plist-get (cdr summary) :requires) nil)))))

(ert-deftest dep-graph/scan-file/use-package-after-list ()
  "`(use-package foo :after (a b))` SHALL list a and b as requires."
  (dep-graph-tests--with-fixture file
      ";;; foo.el -*- lexical-binding: t; -*-\n\
(use-package foo\n\
  :after (a b))\n"
    (let* ((summary (dep-graph-scan-file file))
           (requires (plist-get (cdr summary) :requires)))
      (should (memq 'a requires))
      (should (memq 'b requires)))))

(ert-deftest dep-graph/scan-file/optional-require-counts ()
  "`(require 'foo nil t)` SHALL still appear in :requires."
  (dep-graph-tests--with-fixture file
      ";;; foo.el -*- lexical-binding: t; -*-\n\
(require 'foo nil t)\n"
    (let ((summary (dep-graph-scan-file file)))
      (should (equal (plist-get (cdr summary) :requires) '(foo))))))

(ert-deftest dep-graph/scan-file/load-records-basename ()
  "`(load \"path/to/foo\")` SHALL record `foo` as a dependency."
  (dep-graph-tests--with-fixture file
      ";;; foo.el -*- lexical-binding: t; -*-\n\
(load \"path/to/foo\")\n"
    (let ((summary (dep-graph-scan-file file)))
      (should (equal (plist-get (cdr summary) :requires) '(foo))))))


;;; IO shell — `dep-graph-scan-root'

(defmacro dep-graph-tests--with-tree (var spec &rest body)
  "Bind VAR to a temp directory populated from SPEC, run BODY.

SPEC is a list of `(RELATIVE-PATH . CONTENTS)' cons cells.
Parent directories are created as needed.  The whole tree is
deleted on exit."
  (declare (indent 2))
  `(let ((,var (make-temp-file "dep-graph-tree-" t)))
     (unwind-protect
         (progn
           (dolist (entry ,spec)
             (let* ((path (expand-file-name (car entry) ,var))
                    (dir (file-name-directory path)))
               (make-directory dir t)
               (with-temp-file path (insert (cdr entry)))))
           ,@body)
       (delete-directory ,var t))))

(ert-deftest dep-graph/scan-root/ignores-elpaca ()
  "Generated paths under `elpaca/' SHALL be excluded from the scan."
  (dep-graph-tests--with-tree root
      '(("lisp/foo.el" . "(provide 'foo)\n")
        ("lib/bar/baz.el" . "(provide 'baz)\n")
        ("elpaca/builds/excluded/excluded.el" . "(provide 'excluded)\n"))
    (let* ((entries (dep-graph-scan-root root))
           (paths (mapcar #'car entries)))
      (should (cl-some (lambda (p) (string-suffix-p "lisp/foo.el" p))
                       paths))
      (should (cl-some (lambda (p) (string-suffix-p "lib/bar/baz.el" p))
                       paths))
      (should-not (cl-some (lambda (p) (string-match-p "elpaca/" p))
                           paths)))))


;;; CLI — `dep-graph-main'

(ert-deftest dep-graph/main/affected-positional ()
  "`affected' with positional paths SHALL emit the starting file
plus every transitive dependent."
  (dep-graph-tests--with-tree root
      '(("lisp/foo.el" . "(require 'bar)\n(provide 'foo)\n")
        ("lisp/bar.el" . "(provide 'bar)\n"))
    (let* ((default-directory root)
           (command-line-args-left (list "affected" "lisp/bar.el"))
           (output (with-output-to-string (dep-graph-main)))
           (lines (split-string (string-trim output) "\n" t)))
      (should (member "lisp/bar.el" lines))
      (should (member "lisp/foo.el" lines)))))

(ert-deftest dep-graph/main/affected-tests-positional ()
  "`affected-tests' SHALL emit only test files for affected sources."
  (dep-graph-tests--with-tree root
      '(("lisp/foo.el" . "(require 'bar)\n(provide 'foo)\n")
        ("lisp/foo-tests.el" . "(require 'foo)\n(provide 'foo-tests)\n")
        ("lisp/bar.el" . "(provide 'bar)\n")
        ("lisp/bar-tests.el" . "(require 'bar)\n(provide 'bar-tests)\n"))
    (let* ((default-directory root)
           (command-line-args-left (list "affected-tests" "lisp/bar.el"))
           (output (with-output-to-string (dep-graph-main)))
           (lines (split-string (string-trim output) "\n" t)))
      (should (member "lisp/bar-tests.el" lines))
      (should (member "lisp/foo-tests.el" lines))
      (should-not (member "lisp/foo.el" lines))
      (should-not (member "lisp/bar.el" lines)))))

(provide 'dep-graph-tests)
;;; dep-graph-tests.el ends here
