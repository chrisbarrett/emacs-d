;;; lib.el --- Leader key library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Editing commands and file navigation for leader keybindings.

;;; Code:

(require 'cl-lib)
(require 'marginalia)
(require 'project)

;; Silence byte-compiler for cl-letf locally-bound functions
(declare-function expand-backrefs nil)

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'puni-kill-line "puni")

;;; Editing commands

(defun +format-after-kill ()
  "Format point after killing a sexp."
  (cond
   ((thing-at-point-looking-at (rx bol (* space) eol))
    (kill-line))
   ((thing-at-point-looking-at (rx bol (* space))))
   (t
    (just-one-space)))
  (indent-for-tab-command))

;;;###autoload
(defun +forward-kill-sexp ()
  "Kill sexp forward and format whitespace."
  (interactive)
  (kill-sexp)
  (+format-after-kill))

;;;###autoload
(defun +backward-kill-sexp ()
  "Kill sexp backward and format whitespace."
  (interactive)
  (backward-kill-sexp)
  (+format-after-kill))

;;;###autoload
(defun +kill-line ()
  "Kill line using puni and format whitespace."
  (interactive)
  (puni-kill-line)
  (+format-after-kill))

;;;###autoload
(defun +insert-uuid ()
  "Insert a UUID at point."
  (interactive)
  (insert (string-trim (shell-command-to-string "uuidgen"))))

;;; File navigation

;;;###autoload
(defun +find-sibling-file-search-including-nonexisting (file)
  "Like `find-sibling-file-search', but don't require files to actually exist.
FILE is the current file to find siblings for."
  (cl-letf* (((symbol-function 'expand-backrefs)
              (lambda (match-data expansion)
                (let ((start 0))
                  (while (string-match (rx "\\" (group (+ (or ?& digit)))) expansion start)
                    (let ((index (string-to-number (match-string 1 expansion))))
                      (setq start (match-end 0))
                      (setq expansion
                            ;; KLUDGE: We don't use `file-expand-wildcards', so we need to
                            ;; remove regexp-quoted periods.
                            (string-replace "\\." "."
                                            (replace-match
                                             (substring file
                                                        (elt match-data (* index 2))
                                                        (elt match-data (1+ (* index 2))))
                                             t t expansion)))))
                  expansion)))

             ((symbol-function 'expansions-for-file)
              (pcase-lambda (`(,match . ,expansions))
                (when-let*  ((match-data (and (string-match match file)
                                              (match-data))))
                  (seq-keep (lambda (it)
                              (expand-backrefs match-data it))
                            expansions)))))

    (delete file (delete-dups (seq-mapcat 'expansions-for-file find-sibling-rules)))))

;;;###autoload
(defvar +find-sibling-functions nil
  "List of functions to call to generate extra sibling files.

Each function is called with one argument; the current file name. It
should return a (possibly empty) list of file paths.")

;;;###autoload
(defun +find-sibling-file--annotator (cand)
  "Marginalia annotator for sibling file candidates.
Shows \"Create\" for non-existing files.  CAND is the candidate string."
  (when (get-text-property 0 '+sibling-new-p cand)
    (marginalia--fields ("New" :face 'warning))))

;;;###autoload
(defun +find-sibling-file (file)
  "Like `find-sibling-file', but guess paths to files that don't exist yet.
FILE is the file to find siblings for, defaults to current buffer's file."
  (interactive (list (buffer-file-name)))
  (let* ((existing (find-sibling-file-search file ))
         (guesses (append (+find-sibling-file-search-including-nonexisting file)
                          (seq-mapcat (lambda (fn)
                                        (when (functionp fn) ; can be `t' for local hooks
                                          (seq-map #'expand-file-name (funcall fn file))))
                                      +find-sibling-functions)))
         (siblings (delete-dups (append existing guesses))))
    (find-file
     (cond
      ((null siblings)
       (user-error "No sibling file rules match this file"))

      ;; Go to existing file, ignoring any guesses.
      ((length= existing 1)
       (car existing))

      ;; No files exist, but we have precisely one guess, so we'll create that
      ;; file.
      ((length= siblings 1)
       (car siblings))

      ;; There are a mix of existing files and guesses to choose from. Prompt
      ;; the user.
      (t
       (let* ((relpath (if-let*  ((cwd (file-name-directory file))
                                  (project (project-current nil cwd)))
                           (project-root project)
                         cwd))
              (existing (seq-filter #'file-exists-p siblings))
              (relatives (seq-map (lambda (it)
                                    (let ((rel (file-relative-name it relpath)))
                                      (cons (if (seq-contains-p existing it)
                                                rel
                                              (propertize rel 'face '(:slant italic :inherit shadow)
                                                          '+sibling-new-p t))
                                            it)))
                                  siblings))
              (table (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata (category . sibling-file))
                         (complete-with-action action relatives string pred)))))

         (alist-get (completing-read "Find sibling: " table nil t)
                    relatives nil nil #'equal)))))))

;;; lib.el ends here
