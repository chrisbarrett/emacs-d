;;; dep-graph.el --- Elisp file-dependency graph -*- lexical-binding: t; -*-

;;; Commentary:

;; Build and query a reverse-adjacency graph of Elisp file
;; dependencies for the surrounding Emacs configuration repository.
;;
;; The library splits into a pure core and an IO shell:
;;
;;   - Pure core: `dep-graph-build', `dep-graph-dependents',
;;     `dep-graph-affected'.  These operate on hand-built
;;     file-form summaries and a graph hash-table; no filesystem
;;     access.
;;
;;   - IO shell: `dep-graph-scan-file', `dep-graph-scan-root'.
;;     These walk source files with the Emacs reader (not regex)
;;     to produce the file-form summaries the pure core consumes.
;;
;; The `dep-graph-main' entry point and the `scripts/dep-graph'
;; shell wrapper expose `affected', `affected-tests', and `print'
;; subcommands so prek hooks, the Makefile, and humans can all
;; consume the same graph through one interface.

;;; Code:

(require 'cl-lib)
(require 'subr-x)


;;; Pure core

(defun dep-graph-build (file-forms)
  "Build a reverse-adjacency graph from FILE-FORMS.

FILE-FORMS is a list of entries shaped like
  (PATH :provides FEATURES :requires FEATURES)
where PATH is a string, FEATURES are symbol lists, and the
property list may carry additional keys (ignored here).

Return a hash-table keyed by feature symbol whose value is the
list of file paths that require that feature.

Signal `user-error' if two distinct paths provide the same
feature symbol; the message names the feature and both paths."
  (let ((graph (make-hash-table :test #'eq))
        (providers (make-hash-table :test #'eq)))
    (dolist (entry file-forms)
      (let ((path (car entry))
            (plist (cdr entry)))
        (dolist (feature (plist-get plist :provides))
          (let ((prior (gethash feature providers)))
            (when (and prior (not (equal prior path)))
              (user-error
               "Feature `%s' provided by both `%s' and `%s'"
               feature prior path))
            (puthash feature path providers)))
        (dolist (feature (plist-get plist :requires))
          (push path (gethash feature graph)))))
    graph))

(defun dep-graph--file-feature (path)
  "Return the feature symbol PATH provides by basename convention."
  (intern (file-name-base path)))

(defun dep-graph-dependents (graph file)
  "Return every path that transitively depends on FILE in GRAPH.

The result includes FILE itself.  Cycles terminate via a visited
set; the function never raises an error on them."
  (let ((visited (make-hash-table :test #'equal))
        (queue (list file))
        result)
    (while queue
      (let ((current (pop queue)))
        (unless (gethash current visited)
          (puthash current t visited)
          (push current result)
          (dolist (dependent (gethash (dep-graph--file-feature current)
                                      graph))
            (unless (gethash dependent visited)
              (push dependent queue))))))
    (nreverse result)))

(defun dep-graph-affected (graph files &optional filter)
  "Return the union of dependents of every path in FILES via GRAPH.

When FILTER is non-nil it is called with each path; only paths
for which it returns non-nil appear in the result.  Duplicate
paths across the union are collapsed."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (file files)
      (dolist (dependent (dep-graph-dependents graph file))
        (unless (gethash dependent seen)
          (puthash dependent t seen)
          (when (or (null filter) (funcall filter dependent))
            (push dependent result)))))
    (nreverse result)))


;;; IO shell

(defun dep-graph--quoted-symbol (form)
  "Return the symbol inside FORM if it is `(quote SYM)', else nil."
  (and (consp form)
       (eq (car form) 'quote)
       (symbolp (cadr form))
       (cadr form)))

(defun dep-graph--use-package-after-features (rest)
  "Collect symbols from a `:after' clause's value REST.

Accepts a bare symbol or a list of symbols.  Returns a list of
symbols (possibly empty)."
  (cond
   ((symbolp rest) (when rest (list rest)))
   ((and (consp rest)
         (not (memq (car rest) '(quote function))))
    (cl-remove-if-not #'symbolp rest))
   (t nil)))

(defun dep-graph--scan-form (form provides requires)
  "Inspect top-level FORM and update PROVIDES / REQUIRES boxes.

PROVIDES and REQUIRES are single-cell lists used as mutable
accumulators; the function nconcs new symbols onto the head of
each."
  (when (consp form)
    (pcase form
      (`(provide ,quoted . ,_)
       (when-let* ((sym (dep-graph--quoted-symbol quoted)))
         (setcar provides (cons sym (car provides)))))
      (`(require ,quoted . ,_)
       (when-let* ((sym (dep-graph--quoted-symbol quoted)))
         (setcar requires (cons sym (car requires)))))
      (`(load ,path . ,_)
       (when (stringp path)
         (setcar requires
                 (cons (intern (file-name-base path))
                       (car requires)))))
      (`(use-package ,name . ,rest)
       (when (symbolp name)
         (setcar provides (cons name (car provides))))
       (let ((tail rest))
         (while tail
           (when (and (keywordp (car tail))
                      (eq (car tail) :after))
             (dolist (sym (dep-graph--use-package-after-features
                           (cadr tail)))
               (setcar requires (cons sym (car requires)))))
           (setq tail (cdr tail))))))))

(defun dep-graph-scan-file (file)
  "Parse FILE and return its file-form summary.

The summary is `(FILE :provides FEATURES :requires FEATURES)'
suitable for inclusion in the list `dep-graph-build' accepts.
The function reads top-level forms with `read', so content
inside comments and string literals is ignored automatically."
  (let ((provides (list nil))
        (requires (list nil)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (dep-graph--scan-form form provides requires)))
        (end-of-file nil)))
    (list file
          :provides (nreverse (car provides))
          :requires (nreverse (car requires)))))


(defvar dep-graph-scan-subdirs '("lisp" "lib")
  "Root-relative directories `dep-graph-scan-root' walks for sources.")

(defun dep-graph-scan-root (root)
  "Return a file-form alist for every `.el' file under ROOT.

The walk descends into the subdirectories named by
`dep-graph-scan-subdirs', recursing through their children.
Paths outside those subdirectories — `elpaca/', `eln-cache/',
`var/', etc. — are not visited."
  (let (entries)
    (dolist (sub dep-graph-scan-subdirs)
      (let ((dir (expand-file-name sub root)))
        (when (file-directory-p dir)
          (dolist (file (directory-files-recursively dir "\\.el\\'"))
            (push (dep-graph-scan-file file) entries)))))
    (nreverse entries)))


;;; CLI

(defvar dep-graph-source-filter-re
  (rx bos (or "lisp" "lib" "config" "init") "/" (* anything) ".el" eos)
  "Regexp matching source paths that pass the affected filter.")

(defun dep-graph--test-path-p (path)
  "Return non-nil if PATH names a test file."
  (string-match-p "-tests\\.el\\'" path))

(defun dep-graph--source-path-p (path)
  "Return non-nil if PATH is a tracked source path (non-test)."
  (and (string-match-p dep-graph-source-filter-re path)
       (not (dep-graph--test-path-p path))))

(defun dep-graph--git-changed-files (root)
  "Return repo-relative paths git considers staged or unstaged at ROOT.

Output mirrors the union of
  `git diff --cached --name-only --diff-filter=ACMR'
  `git diff --name-only --diff-filter=ACMR'
filtered to source files the graph cares about."
  (let ((default-directory root)
        (collect (lambda (args)
                   (with-temp-buffer
                     (when (zerop (apply #'process-file "git" nil t nil args))
                       (split-string (buffer-string) "\n" t))))))
    (cl-remove-if-not
     #'dep-graph--source-path-p
     (delete-dups
      (append
       (funcall collect '("diff" "--cached" "--name-only"
                          "--diff-filter=ACMR"))
       (funcall collect '("diff" "--name-only" "--diff-filter=ACMR")))))))

(defun dep-graph--build-graph (root)
  "Scan ROOT and return `(GRAPH . ENTRIES)' with tests excluded.

GRAPH is the reverse-adjacency hash-table.  ENTRIES is the full
scan-root output (including tests) so callers can resolve
sibling test files later."
  (let* ((entries (dep-graph-scan-root root))
         (sources (cl-remove-if (lambda (e) (dep-graph--test-path-p (car e)))
                                entries)))
    (cons (dep-graph-build sources) entries)))

(defun dep-graph--abs (root path)
  "Return the absolute form of PATH against ROOT."
  (expand-file-name path root))

(defun dep-graph--relative-paths (root paths)
  "Return PATHS rewritten relative to ROOT and sorted alphabetically."
  (sort (mapcar (lambda (p) (file-relative-name p root)) paths)
        #'string<))

(defun dep-graph--sibling-test (path)
  "Return the conventional `-tests.el' sibling of PATH."
  (concat (file-name-sans-extension path) "-tests.el"))

(defun dep-graph--print-lines (lines)
  "Print each entry of LINES followed by a newline."
  (dolist (line lines)
    (princ line)
    (princ "\n")))

(defun dep-graph--cmd-affected (root args)
  "Run the `affected' subcommand against ROOT with ARGS."
  (dep-graph--run-affected root args nil))

(defun dep-graph--cmd-affected-tests (root args)
  "Run the `affected-tests' subcommand against ROOT with ARGS."
  (dep-graph--run-affected root args t))

(defun dep-graph--run-affected (root args tests-only)
  "Compute the affected set under ROOT for ARGS, emitting paths.

When TESTS-ONLY is non-nil, output the sibling test file for
each affected source instead of the source path."
  (let* ((from-git (member "--from-git" args))
         (positional (cl-remove "--from-git" args :test #'equal))
         (raw-files (if from-git
                        (dep-graph--git-changed-files root)
                      positional))
         (relative-files
          (cl-remove-if-not
           #'dep-graph--source-path-p
           (mapcar (lambda (p)
                     (file-relative-name (dep-graph--abs root p) root))
                   raw-files)))
         (files (mapcar (lambda (p) (dep-graph--abs root p))
                        relative-files)))
    (cond
     ((and from-git (null files))
      (princ "all\n"))
     ((null files)
      (princ "none\n"))
     (t
      (let* ((built (dep-graph--build-graph root))
             (graph (car built))
             (entries (cdr built))
             (closure (dep-graph-affected graph files))
             (relative (cl-remove-if-not
                        #'dep-graph--source-path-p
                        (dep-graph--relative-paths root closure)))
             (output
              (if tests-only
                  (cl-loop
                   for src in relative
                   for test = (dep-graph--sibling-test src)
                   when (cl-some (lambda (e)
                                   (string= (file-relative-name
                                             (car e) root)
                                            test))
                                 entries)
                   collect test)
                relative)))
        (if (null output)
            (princ "none\n")
          (dep-graph--print-lines output)))))))

(defun dep-graph--cmd-print (root)
  "Run the `print' subcommand against ROOT."
  (let* ((built (dep-graph--build-graph root))
         (graph (car built))
         keys)
    (maphash (lambda (k _) (push k keys)) graph)
    (setq keys (sort keys (lambda (a b) (string< (symbol-name a)
                                                  (symbol-name b)))))
    (dolist (feature keys)
      (princ (format "%s:" feature))
      (dolist (path (sort (mapcar (lambda (p) (file-relative-name p root))
                                  (gethash feature graph))
                          #'string<))
        (princ (format " %s" path)))
      (princ "\n"))))

(defun dep-graph-main ()
  "Entry point: dispatch on the first `command-line-args-left' element.

A leading `--' marker (left over from the shell wrapper's
`emacs --batch ... -- ARGS') is stripped before dispatch."
  (let* ((root (expand-file-name default-directory))
         (args (if (equal (car command-line-args-left) "--")
                   (cdr command-line-args-left)
                 command-line-args-left)))
    (setq command-line-args-left nil)
    (pcase (car args)
      ("affected"       (dep-graph--cmd-affected root (cdr args)))
      ("affected-tests" (dep-graph--cmd-affected-tests root (cdr args)))
      ("print"          (dep-graph--cmd-print root))
      (cmd (error "Unknown dep-graph subcommand: %s" cmd)))))

(provide 'dep-graph)
;;; dep-graph.el ends here
