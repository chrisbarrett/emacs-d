;;; may-i-tests.el --- Tests for may-i -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for `may-i'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'may-i)


;;; Activation

(ert-deftest may-i/activates-on-may-i-dir ()
  "Visiting a file under `/may-i/' selects `may-i-config-mode'."
  (let* ((dir (make-temp-file "may-i-test-" t))
         (subdir (expand-file-name "may-i" dir))
         (file (expand-file-name "foo.lisp" subdir)))
    (unwind-protect
        (progn
          (make-directory subdir t)
          (with-temp-file file (insert ""))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (should (derived-mode-p 'may-i-config-mode))
              (kill-buffer (current-buffer)))))
      (delete-directory dir t))))

(ert-deftest may-i/activates-on-dotted-config ()
  "Visiting `.may-i.lisp' / `.may-i.local.lisp' selects `may-i-config-mode'."
  (dolist (basename '(".may-i.lisp" ".may-i.local.lisp"))
    (let* ((dir (make-temp-file "may-i-test-" t))
           (file (expand-file-name basename dir)))
      (unwind-protect
          (progn
            (with-temp-file file (insert ""))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (should (derived-mode-p 'may-i-config-mode))
                (kill-buffer (current-buffer)))))
        (delete-directory dir t)))))

(ert-deftest may-i/derived-from-lisp-data-mode ()
  "`may-i-config-mode' derives from `lisp-data-mode'."
  (with-temp-buffer
    (may-i-config-mode)
    (should (derived-mode-p 'lisp-data-mode))))


;;; Apheleia wiring

(defvar apheleia-formatters)
(defvar apheleia-mode-alist)

(ert-deftest may-i/apheleia-registers-formatter-after-load ()
  "After `apheleia' loads, may-i is registered in both apheleia alists.
If `apheleia' is already loaded the after-load form has already
fired against the global alists; otherwise providing it now fires
the form against the values bound here."
  (unless (featurep 'apheleia)
    (provide 'apheleia))
  (should (member '(may-i "may-i" "fmt" "-") apheleia-formatters))
  (should (eq (cdr (assq 'may-i-config-mode apheleia-mode-alist)) 'may-i)))


;;; Indent rules

(ert-deftest may-i/indent-rules-installed ()
  "Every may-i head has the documented `lisp-indent-function' value."
  (should (eq (get 'define 'lisp-indent-function) 1))
  (should (eq (get 'define-arg-style 'lisp-indent-function) 1))
  (should (eq (get 'parser 'lisp-indent-function) 1))
  (should (eq (get 'rule 'lisp-indent-function) 1))
  (should (eq (get 'when 'lisp-indent-function) 1))
  (should (eq (get 'unless 'lisp-indent-function) 1))
  (should (eq (get 'with-facts 'lisp-indent-function) 1))
  (should (eq (get 'cond 'lisp-indent-function) 0))
  (should (eq (get 'defcontext 'lisp-indent-function) 1)))

(ert-deftest may-i/lang-lisp-init-has-no-may-i-puts ()
  "`modules/lang-lisp/init.el' contains no `with-facts' or `defcontext'."
  (let* ((root (locate-dominating-file
                (or load-file-name buffer-file-name default-directory)
                "modules"))
         (init (expand-file-name "modules/lang-lisp/init.el" root)))
    (should (file-readable-p init))
    (with-temp-buffer
      (insert-file-contents init)
      (goto-char (point-min))
      (should-not (re-search-forward "with-facts" nil t))
      (goto-char (point-min))
      (should-not (re-search-forward "defcontext" nil t)))))


;;; Buffer/face helpers

(defmacro may-i-tests--with-buffer (contents &rest body)
  "Insert CONTENTS into a `may-i-config-mode' buffer and evaluate BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (may-i-config-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defmacro may-i-tests--with-fontified (contents &rest body)
  "Insert CONTENTS, enable mode, ensure font-lock, then evaluate BODY."
  (declare (indent 1))
  `(may-i-tests--with-buffer ,contents
     (font-lock-ensure)
     ,@body))

(defun may-i-tests--faces-on (search-string)
  "Return face property at start of first match of SEARCH-STRING."
  (save-excursion
    (goto-char (point-min))
    (search-forward search-string)
    (let ((face (get-text-property (match-beginning 0) 'face)))
      (cond ((null face) nil)
            ((listp face) face)
            (t (list face))))))

(defun may-i-tests--has-face-p (search-string face)
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
            (when (member face faces) (setq found t)))
          (setq beg (1+ beg)))
        found))))


;;; Font-lock

(ert-deftest may-i/font-lock-decision-verb-outside-check-is-bold ()
  "An `allow' head outside `(check …)' carries `may-i-allow-face',
and its reason string carries `may-i-reason-face'."
  (may-i-tests--with-fontified "(allow \"do thing\")\n"
    (should (may-i-tests--has-face-p "allow" 'may-i-allow-face))
    (should (may-i-tests--has-face-p "do thing" 'may-i-reason-face))))

(ert-deftest may-i/font-lock-decision-verb-inside-check-drops-bold ()
  "Inside `(check …)' the verb keeps its colour but the override
applies `:weight normal' and the string is not reason-faced."
  (may-i-tests--with-fontified "(check (allow \"do thing\"))\n"
    (should (may-i-tests--has-face-p "allow" 'may-i-allow-face))
    (should (member '(:weight normal)
                    (may-i-tests--faces-on "allow")))
    (should-not (may-i-tests--has-face-p "do thing" 'may-i-reason-face))))

(ert-deftest may-i/font-lock-rule-or-highlights-every-alternative ()
  "Each string element in `(rule (or \"foo\" \"bar\") …)' carries
`font-lock-function-name-face'."
  (may-i-tests--with-fontified "(rule (or \"foo\" \"bar\") (allow \"x\"))\n"
    (should (may-i-tests--has-face-p "foo" 'font-lock-function-name-face))
    (should (may-i-tests--has-face-p "bar" 'font-lock-function-name-face))))


;;; Imenu

(ert-deftest may-i/imenu-rules-named ()
  "Named `(rule \"foo\" …)' appears under Rules keyed by the string."
  (may-i-tests--with-buffer "(rule \"foo\" (allow \"x\"))\n"
    (let* ((index (may-i--imenu-index-function))
           (rules (cdr (assoc "Rules" index))))
      (should rules)
      (should (assoc "foo" rules)))))

(ert-deftest may-i/imenu-rules-or ()
  "`(rule (or \"a\" \"b\") …)' yields one entry per alternative.
Both entries point to the same `(rule' open paren."
  (may-i-tests--with-buffer "(rule (or \"a\" \"b\") (allow \"x\"))\n"
    (let* ((index (may-i--imenu-index-function))
           (rules (cdr (assoc "Rules" index)))
           (pos-a (cdr (assoc "a" rules)))
           (pos-b (cdr (assoc "b" rules))))
      (should rules)
      (should pos-a)
      (should pos-b)
      (should (= pos-a pos-b)))))

(ert-deftest may-i/imenu-parsers ()
  (may-i-tests--with-buffer "(parser p1 (style gnu))\n"
    (let* ((index (may-i--imenu-index-function))
           (parsers (cdr (assoc "Parsers" index))))
      (should parsers)
      (should (assoc "p1" parsers)))))

(ert-deftest may-i/imenu-arg-styles ()
  (may-i-tests--with-buffer "(define-arg-style s1 (style gnu))\n"
    (let* ((index (may-i--imenu-index-function))
           (styles (cdr (assoc "Arg styles" index))))
      (should styles)
      (should (assoc "s1" styles)))))

(ert-deftest may-i/imenu-definitions ()
  (may-i-tests--with-buffer "(define d1 \"value\")\n"
    (let* ((index (may-i--imenu-index-function))
           (defs (cdr (assoc "Definitions" index))))
      (should defs)
      (should (assoc "d1" defs)))))

(ert-deftest may-i/imenu-check-anonymous ()
  "Anonymous `(check …)' is labelled positionally."
  (may-i-tests--with-buffer "(check (allow \"x\"))\n"
    (let* ((index (may-i--imenu-index-function))
           (checks (cdr (assoc "Checks" index))))
      (should checks)
      (should (assoc "check (1)" checks)))))

(ert-deftest may-i/imenu-loads ()
  (may-i-tests--with-buffer "(load \"shared.lisp\")\n"
    (let* ((index (may-i--imenu-index-function))
           (loads (cdr (assoc "Loads" index))))
      (should loads)
      (should (assoc "shared.lisp" loads)))))

(ert-deftest may-i/imenu-safe-env-vars ()
  (may-i-tests--with-buffer "(safe-env-vars \"FOO\" \"BAR\")\n"
    (let* ((index (may-i--imenu-index-function))
           (envs (cdr (assoc "Safe env vars" index))))
      (should envs)
      (should (assoc "safe-env-vars" envs)))))

(ert-deftest may-i/imenu-skips-strings-and-comments ()
  "Form-shaped content inside strings or comments produces no entries."
  (may-i-tests--with-buffer
      ";; (parser fake-in-comment ())\n\"(rule \\\"fake-in-string\\\" (allow \\\"x\\\"))\"\n"
    (let ((index (may-i--imenu-index-function)))
      (should-not (assoc "Rules" index))
      (should-not (assoc "Parsers" index)))))

(ert-deftest may-i/imenu-section-order ()
  "Imenu sections appear in the documented fixed order."
  (may-i-tests--with-buffer
      (concat "(load \"a.lisp\")\n"
              "(parser p (style gnu))\n"
              "(rule \"r\" (allow \"x\"))\n"
              "(define d 1)\n"
              "(define-arg-style s (style gnu))\n"
              "(check (allow \"y\"))\n"
              "(safe-env-vars \"X\")\n")
    (let ((names (mapcar #'car (may-i--imenu-index-function))))
      (should (equal names
                     '("Rules" "Parsers" "Arg styles" "Definitions"
                       "Checks" "Loads" "Safe env vars"))))))


(provide 'may-i-tests)
;;; may-i-tests.el ends here
