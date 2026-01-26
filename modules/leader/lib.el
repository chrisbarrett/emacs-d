;;; leader-lib.el --- Leader key library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Editing commands and file navigation for leader keybindings.

;;; Code:

(require 'cl-lib)

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
(defun +find-sibling-file (file)
  "Like `find-sibling-file', but guess paths to files that don't exist yet.
FILE is the file to find siblings for, defaults to current buffer's file."
  (interactive (list (buffer-file-name)))
  (let* ((existing (find-sibling-file-search file ))
         (guesses (+find-sibling-file-search-including-nonexisting file))
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
       (let ((relatives (seq-map (lambda (it)
                                   (file-relative-name it (file-name-directory file)))
                                 siblings)))
         (completing-read (format-prompt "Find file" (car relatives))
                          relatives nil t nil nil (car relatives))))))))

(provide 'leader-lib)

;;; leader-lib.el ends here
