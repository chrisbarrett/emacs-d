;;; bats-mode.el --- Major mode for Bats test files -*- lexical-binding: t; -*-

;;; Commentary:

;; `bats-mode' is a major mode for [Bats][bats] (Bash Automated Testing
;; System) `.bats' files.  It derives from `bash-ts-mode' so the
;; inherited tree-sitter bash editing experience (indent, syntax
;; navigation, font-lock) stays intact, and layers bats-aware overlays
;; on top:
;;
;; - directive face for bats-specific surface tokens
;;   (`@test', fixture functions, `load', `bats_load_library',
;;   `bats_require_minimum_version', `run', `skip', `bats::on_failure');
;; - assertion face for helper-library assertion functions
;;   (`assert*' / `refute*' / `fail' / etc.), gated by which helper
;;   profiles the buffer actually loads;
;; - variable face for documented `$BATS_*' special variables;
;; - an imenu composing Tests, Fixtures and inherited bash functions.
;;
;; Activation is via `auto-mode-alist' (for `.bats') and
;; `interpreter-mode-alist' (for the `bats' shebang).
;;
;; [bats]: https://github.com/bats-core/bats-core

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'bash-ts-mode nil t)


;;; Customisation group

(defgroup bats-mode nil
  "Major mode for Bats (Bash Automated Testing System) files."
  :group 'languages
  :prefix "bats-mode-")


;;; Faces

(defface bats-directive-face
  '((t :inherit font-lock-keyword-face))
  "Face for bats directives (@test, fixtures, load, run, skip, …)."
  :group 'bats-mode)

(defface bats-assertion-face
  '((t :inherit font-lock-function-name-face))
  "Face for helper-library assertion functions in `bats-mode' buffers.

Only applied to assertion names whose helper profile has been
detected as active for the current buffer (see
`bats-mode-profile-keywords' and `bats-mode--detect-profiles')."
  :group 'bats-mode)

(defface bats-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for documented `$BATS_*' special variables."
  :group 'bats-mode)


;;; Directive keywords

(defconst bats-mode--directive-keywords
  '("setup" "teardown"
    "setup_file" "teardown_file"
    "setup_suite" "teardown_suite"
    "load" "bats_load_library" "bats_require_minimum_version"
    "run" "bats_pipe" "skip" "bats::on_failure")
  "Word-form bats surface tokens fontified with `bats-directive-face'.

The `@test' directive is matched separately; its leading `@' is
not a symbol character in bash-ts-mode's syntax table.")

(defconst bats-mode--directive-font-lock-keywords
  `(("@test\\>"
     (0 'bats-directive-face t))
    (,(rx-to-string
       `(seq symbol-start
             (or ,@bats-mode--directive-keywords)
             symbol-end)
       t)
     (0 'bats-directive-face t)))
  "Font-lock keyword form for bats directives.")


;;; `$BATS_*' variables

(defconst bats-mode--documented-variables
  '("BATS_TEST_NAME"
    "BATS_TEST_NAME_PREFIX"
    "BATS_TEST_DESCRIPTION"
    "BATS_TEST_NAMES"
    "BATS_TEST_NUMBER"
    "BATS_SUITE_TEST_NUMBER"
    "BATS_TEST_TAGS"
    "BATS_TEST_DIRNAME"
    "BATS_TEST_FILENAME"
    "BATS_TEST_RETRIES"
    "BATS_TEST_TIMEOUT"
    "BATS_TEST_TMPDIR"
    "BATS_TMPDIR"
    "BATS_FILE_TMPDIR"
    "BATS_SUITE_TMPDIR"
    "BATS_RUN_TMPDIR"
    "BATS_RUN_COMMAND"
    "BATS_VERSION"
    "BATS_FILE_EXTENSION")
  "Documented bats special-variable names (no `$' prefix).")

(defconst bats-mode--variable-font-lock-keywords
  `((,(rx-to-string
       `(seq "$"
             (? "{")
             (group (or ,@bats-mode--documented-variables))
             (or (not (any alnum "_")) eos))
       t)
     (1 'bats-variable-face t)))
  "Font-lock keyword form for documented bats `$BATS_*' variables.")


;;; Helper-library profiles

(defcustom bats-mode-profile-keywords
  '((:core
     "@test" "setup" "teardown"
     "setup_file" "teardown_file"
     "setup_suite" "teardown_suite"
     "load" "bats_load_library" "bats_require_minimum_version"
     "run" "bats_pipe" "skip" "bats::on_failure")
    (:bats-assert
     "assert" "refute"
     "assert_equal" "assert_not_equal"
     "assert_success" "assert_failure"
     "assert_output" "refute_output"
     "assert_output_contains" "refute_output_contains"
     "assert_line" "refute_line"
     "assert_regex" "refute_regex"
     "assert_not_match"
     "assert_stderr" "refute_stderr"
     "assert_stderr_line" "refute_stderr_line")
    (:bats-support
     "fail"
     "batslib_decorate"
     "batslib_is_caller"
     "batslib_print_kv_single"
     "batslib_print_kv_multi"
     "batslib_print_kv_single_or_multi"
     "batslib_err"
     "batslib_prefix"
     "batslib_mark"
     "batslib_count_lines")
    (:bats-file
     "assert_file_exists" "assert_file_not_exists"
     "assert_dir_exists" "assert_dir_not_exists"
     "assert_link_exists" "assert_not_exists"
     "assert_file_empty" "assert_file_not_empty"
     "assert_file_executable" "assert_file_not_executable"
     "assert_file_owner" "assert_file_group_owner"
     "assert_file_permission" "assert_file_size_zero"
     "assert_file_contains" "assert_file_not_contains"
     "assert_symlink_to"))
  "Alist mapping helper-library profile keys to their assertion names.

`:core' is always active.  Other profiles activate when the
buffer (or one level of `load'-resolved companion bash file) loads
the corresponding helper library."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'bats-mode)

(defvar-local bats-mode--active-profiles nil
  "Buffer-local list of helper-library profile keys active in this buffer.")

(defconst bats-mode--load-line-regexp
  (rx bol (* space)
      (or "load" "bats_load_library")
      (+ space)
      (? (any "\"'"))
      (group (+ (any alnum "/_.-")))
      (? (any "\"'")))
  "Regex matching a `load' or `bats_load_library' directive.
Group 1 captures the loaded argument (library name or file).")

(defconst bats-mode--known-library-profiles
  '(("bats-assert" . :bats-assert)
    ("bats-support" . :bats-support)
    ("bats-file" . :bats-file))
  "Mapping from helper-library names to profile keys.")

(defun bats-mode--scan-load-arguments (text)
  "Return list of arguments mentioned by `load'/`bats_load_library' in TEXT."
  (let (args)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward bats-mode--load-line-regexp nil t)
        (push (match-string 1) args)))
    (nreverse args)))

(defun bats-mode--profile-for-argument (arg)
  "Return profile key for ARG if it names a known helper library, else nil."
  (cdr (assoc arg bats-mode--known-library-profiles)))

(defun bats-mode--resolve-helper-file (arg)
  "Return absolute path to local helper file for load argument ARG, or nil."
  (let ((dir (and buffer-file-name
                  (file-name-directory buffer-file-name))))
    (when dir
      (cl-loop
       for candidate in (list (expand-file-name (concat arg ".bash") dir)
                              (expand-file-name arg dir))
       when (and candidate
                 (file-readable-p candidate)
                 (file-regular-p candidate))
       return candidate))))

(defun bats-mode--read-file (path)
  "Return contents of PATH as a string, or nil on failure."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error nil)))

(defun bats-mode--detect-profiles ()
  "Return the helper-library profile set active for the current buffer.

Always includes `:core'.  Scans the buffer for `load' /
`bats_load_library' lines; for each argument, either records the
matching helper profile or, if the argument resolves to a local
bash file, scans that file once (no deeper recursion)."
  (let ((profiles (list :core))
        (buffer-text (save-restriction
                       (widen)
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
    (dolist (arg (bats-mode--scan-load-arguments buffer-text))
      (let ((profile (bats-mode--profile-for-argument arg)))
        (cond
         (profile
          (cl-pushnew profile profiles))
         (t
          (when-let* ((path (bats-mode--resolve-helper-file arg))
                      (contents (bats-mode--read-file path)))
            (dolist (sub (bats-mode--scan-load-arguments contents))
              (when-let* ((sub-profile (bats-mode--profile-for-argument sub)))
                (cl-pushnew sub-profile profiles))))))))
    (nreverse profiles)))

(defun bats-mode--keywords-for-profile (profile)
  "Return the assertion-keyword list configured for PROFILE."
  (cdr (assq profile bats-mode-profile-keywords)))

(defun bats-mode--assertion-font-lock-keywords (profiles)
  "Return font-lock keyword forms for assertion names in PROFILES.

`:core' contributes nothing here — its keywords are covered by
`bats-mode--directive-font-lock-keywords'."
  (let ((names (cl-loop for profile in profiles
                        unless (eq profile :core)
                        append (bats-mode--keywords-for-profile profile))))
    (when names
      `((,(rx-to-string
           `(seq symbol-start
                 (or ,@names)
                 symbol-end)
           t)
         (0 'bats-assertion-face t))))))


;;; Font-lock installation

(defun bats-mode--install-directive-font-lock ()
  "Install directive and variable font-lock keywords (mode-wide)."
  (font-lock-add-keywords
   'bats-mode bats-mode--directive-font-lock-keywords 'append)
  (font-lock-add-keywords
   'bats-mode bats-mode--variable-font-lock-keywords 'append))

(defvar-local bats-mode--installed-assertion-keywords nil
  "Buffer-local cache of assertion font-lock keyword forms last installed.")

(defun bats-mode--refresh-assertion-font-lock ()
  "Recompute active profiles and refresh assertion font-lock for this buffer."
  (when bats-mode--installed-assertion-keywords
    (font-lock-remove-keywords nil bats-mode--installed-assertion-keywords)
    (setq bats-mode--installed-assertion-keywords nil))
  (setq bats-mode--active-profiles (bats-mode--detect-profiles))
  (when-let* ((keywords (bats-mode--assertion-font-lock-keywords
                         bats-mode--active-profiles)))
    (font-lock-add-keywords nil keywords 'append)
    (setq bats-mode--installed-assertion-keywords keywords))
  (when font-lock-mode
    (font-lock-flush)
    (font-lock-ensure)))

(defun bats-mode--after-save-refresh ()
  "Recompute helper profiles + assertion font-lock after the buffer saves."
  (when (derived-mode-p 'bats-mode)
    (bats-mode--refresh-assertion-font-lock)))


;;; Imenu

(defconst bats-mode--imenu-test-regexp
  (rx bol (* space) "@test" (+ space)
      "\"" (group (*? anything)) "\""
      (* space) "{")
  "Regex matching a `@test' declaration.  Group 1 captures the description.")

(defconst bats-mode--fixture-names
  '("setup" "teardown"
    "setup_file" "teardown_file"
    "setup_suite" "teardown_suite")
  "Fixture function names recognised by `bats-mode' imenu.")

(defconst bats-mode--imenu-fixture-regexp
  (rx-to-string
   `(seq bol (* space)
         (group (or ,@bats-mode--fixture-names))
         (* space) "(" (* space) ")")
   t)
  "Regex matching a fixture function definition.  Group 1 is the name.")

(defun bats-mode--imenu-collect-tests ()
  "Return alist of `@test' descriptions to markers."
  (let (entries)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward bats-mode--imenu-test-regexp nil t)
          (push (cons (match-string-no-properties 1)
                      (copy-marker (match-beginning 0)))
                entries))))
    (nreverse entries)))

(defun bats-mode--imenu-collect-fixtures ()
  "Return alist of fixture-function names to markers (only those present)."
  (let (entries)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward bats-mode--imenu-fixture-regexp nil t)
          (push (cons (match-string-no-properties 1)
                      (copy-marker (match-beginning 1)))
                entries))))
    (nreverse entries)))

(defun bats-mode--inherited-bash-imenu ()
  "Return the bash-ts-mode tree-sitter imenu entries for this buffer.

Falls back to nil if tree-sitter is unavailable for any reason."
  (condition-case nil
      (when (and (featurep 'treesit)
                 (fboundp 'treesit-simple-imenu)
                 (treesit-parser-list))
        (treesit-simple-imenu))
    (error nil)))

(defun bats-mode--imenu-create-index ()
  "Compose imenu index for a bats buffer: Tests, Fixtures, Functions."
  (let ((tests (bats-mode--imenu-collect-tests))
        (fixtures (bats-mode--imenu-collect-fixtures))
        (functions (bats-mode--inherited-bash-imenu))
        sections)
    (when tests
      (push (cons "Tests" tests) sections))
    (when fixtures
      (push (cons "Fixtures" fixtures) sections))
    (when functions
      (push (cons "Functions" functions) sections))
    (nreverse sections)))


;;; Mode definition

;;;###autoload
(define-derived-mode bats-mode bash-ts-mode "Bats"
  "Major mode for editing Bats (Bash Automated Testing System) files.

Derives from `bash-ts-mode' so tree-sitter-driven bash editing
(indent, navigation, font-lock) is preserved.  Adds bats-aware
font-lock for directives, helper-library assertions, and `$BATS_*'
special variables, plus an imenu composing Tests, Fixtures, and
inherited bash functions."
  (setq-local imenu-create-index-function #'bats-mode--imenu-create-index)
  (add-hook 'after-save-hook #'bats-mode--after-save-refresh nil t)
  (bats-mode--refresh-assertion-font-lock))

(bats-mode--install-directive-font-lock)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bats\\'" . bats-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("bats" . bats-mode))

(provide 'bats-mode)
;;; bats-mode.el ends here
