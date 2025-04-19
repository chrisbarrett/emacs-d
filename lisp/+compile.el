;;; +compile.el --- Helpers for M-x compile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'cl-lib)
(require 'map)

(eval-and-compile
  (defconst +compile-metavars-alist '((file . (+? nonl))
                                      (line . (+ digit))
                                      (col . (+ digit))
                                      (message . (+? nonl)))
    "Meta-variables that are available for use in `rx' forms.

These are translated from symbols to match groups in the compiled
regular expression.")

  (defun +compile--subst-group-numbers (form group-numbers)
    (+tree-map (lambda (term)
                 (pcase term
                   ;; Resolve direct reference.
                   ((and (pred symbolp)
                         (let group-number (gethash term group-numbers))
                         (guard group-number))
                    group-number)

                   ;; Resolve named group (with colon suffix).
                   ((and (pred symbolp)
                         (let with-suffix (intern (format "%s:" term)))
                         (let group-number (gethash with-suffix group-numbers))
                         (guard group-number))
                    group-number)
                   (term
                    term)))
               form))

  (defun +compile--subst-from-env (form env group-numbers)
    (+tree-map (lambda (term)
                 (pcase term
                   ;; Rewrite standard metavar
                   ((and (pred symbolp)
                         (let rx-form (alist-get term +compile-metavars-alist))
                         (guard rx-form)
                         (let group-number (gethash term group-numbers))
                         (guard group-number)
                         )
                    `(group-n ,group-number ,rx-form))

                   ;; Rewrite named group to numbered group.
                   ((and `(,h . ,tl)
                         (guard (symbolp h))
                         (guard (string-suffix-p ":" (symbol-name h)))
                         (let group-number (gethash h group-numbers))
                         (guard group-number)
                         )
                    `(group-n ,group-number ,@tl))

                   ((and (pred symbolp)
                         (let expansion (gethash term env))
                         (guard (not (null expansion)))
                         )
                    `(and ,@expansion))

                   (term
                    term)))
               form))

  (defun +compile--optimize (form)
    (+tree-map (lambda (term)
                 (pcase term
                   (`(and ,x) x)
                   (`(or ,x) x)
                   (term
                    term)))
               form))

  (defun +compile--analyze (rx-forms)
    "Analyze RX-FORMS for information needed to build a matcher.

The result is a plist containing the following keys:

- `:referenced-metavars' - the values in `+compile-metavars-alist' that
  were used.

- `:highest-group-number' - the highest allocated match group number.

- `:named-groups' - a list of named capture groups."
    (let ((numbered-groups 0)
          (un-numbered-groups 0)
          (referenced-metavars '())
          (named-groups '())
          (metavars (seq-map #'car +compile-metavars-alist)))
      (+tree-map (lambda (it)
                   (pcase it
                     ((and (pred symbolp) (guard (member it metavars)))
                      (push it referenced-metavars))
                     (`(group . ,_rest)
                      (cl-incf un-numbered-groups))
                     (`(group-n ,n . ,_rest)
                      (setq numbered-groups (max numbered-groups n)))
                     ((and `(,h . ,_tl)
                           (guard (symbolp h))
                           (guard (string-suffix-p ":" (symbol-name h))))
                      (push h named-groups)))
                   it)
                 rx-forms)
      (list :highest-group-number (max  numbered-groups un-numbered-groups)
            :referenced-metavars (nreverse (delete-dups referenced-metavars))
            :named-groups (nreverse (delete-dups named-groups)))))


  (defun +compile--parse-keyword-args (keyword-args)

    ;; (+compile--parse-keyword-args '(:where x = (+? alnum) (* digit) :kw-y y :kw-z z))

    (let ((extra-keywords (make-hash-table))
          (where-bindings (make-hash-table))
          (groups (+chunk-by #'keywordp keyword-args)))
      (dolist (group groups)
        (pcase-exhaustive group
          ;; Both `:where foo = bar ...' and `:where foo bar ...' are
          ;; equivalent.
          ((or `(:where ,key = . ,values)
               `(:where ,key . ,values))
           (puthash key values where-bindings))
          (`(,key ,value)
           (puthash key value extra-keywords))))

      (list :where-bindings (+alist-from-hash-table where-bindings)
            :extra-keywords (+plist-from-hash-table extra-keywords))))


  (defun +compile--build-env (rx-forms keyword-args)

    ;; (+compile--build-env '(line-start "** (" module ") " message " on " file ":" line ":" col ":" (* nonl) line-end)
    ;;                      '(:where module = (+? alnum)
    ;;                        :type info))

    (pcase-let* ((env (make-hash-table))
                 (group-numbers (make-hash-table))
                 ((map :referenced-metavars :highest-group-number :named-groups)
                  (+compile--analyze rx-forms))
                 ((map :where-bindings :extra-keywords) (+compile--parse-keyword-args keyword-args)))

      (pcase-dolist (`(,key . ,value) where-bindings)
        (puthash key value env))

      (list
       :env env
       :extra-keywords extra-keywords
       :group-numbers
       (cl-loop for fresh-number from (1+ highest-group-number)
                for var in (append referenced-metavars
                                   named-groups)
                do (puthash var fresh-number group-numbers)
                finally return group-numbers))))

  (defun +compile-rx (rx-form keyword-args)
    (pcase-let* (((map :env :group-numbers) (+compile--build-env rx-form keyword-args)))
      (+compile--optimize (+compile--subst-from-env rx-form env group-numbers))))

  )



(defmacro +rx (&rest forms)
  "Like RX, but also allow auxiliary definitions using `:where' clauses."

  (cl-assert (cl-plusp (length forms)))
  (pcase-let* ((`(,rx-forms ,keyword-args) (+split-with (lambda (it) (not (keywordp it))) forms)))
    `(rx ,@(+compile-rx rx-forms keyword-args))))

(defun +compile-spec-for-compilation-error-alist (forms)
  (pcase-let* ((`(,rx-forms ,keyword-args)
                (+split-with (lambda (it) (not (keywordp it))) forms))
               ((map :group-numbers :extra-keywords) (+compile--build-env rx-forms keyword-args)))
    (list
     :rx-form (+compile-rx rx-forms keyword-args)

     :highlights (seq-map (lambda (it)
                            ;; The first element may be a symbol that resolves
                            ;; to a group number, but any other elements are
                            ;; part of a face spec.
                            (pcase-exhaustive it
                              (`(,match . ,rest)
                               (cons (+compile--subst-group-numbers match group-numbers)
                                     rest))))
                          (plist-get extra-keywords :highlights))

     :hyperlink (+compile--subst-group-numbers (plist-get extra-keywords :hyperlink) group-numbers)

     :type (pcase (plist-get extra-keywords :type)
             ('error 2)
             ('warning 1)
             ('warn 1)
             ('info 0)
             (value value))

     :file (or (plist-get extra-keywords :file) (gethash 'file group-numbers))
     :line (or (plist-get extra-keywords :line) (gethash 'line group-numbers))
     :col (or (plist-get extra-keywords :col) (gethash 'col group-numbers)))))

(cl-defmacro define-compilation-error-rx (name &rest forms)
  "Define a regexp parser for use in `compile'.

NAME, a symbol, is used as the key for the parsed regular expression in
`compilation-error-regexp-alist-alist'.

Subsequent arguments are interpreted as forms for the `rx' macro, until
a keyword is encountered. The remaining arguments are interpreted as
optional keyword arguments.

The rx forms may include special symbols which are expanded in the final
regexp, making the rx forms a more declarative. See
`+compile-metavars-alist' for the default list of these variables.

The optional keyword arguments are:

  - `:where NAME = RX-FORM' - additional symbols that will be expanded
    by their definitions in the final regexp. This could aid readability
    of the rx form.

  - `:type [error|warn|info|(N . M)]' - Determines the level of the
    line; the default interpretation is error-level. See
    `compilation-error-regexp-alist'.

  - `:file', `:line', `:col' - Default values are used by inspecting the
    rx-forms for corresponding variables; however, you can override this
    behaviour and supply functions to compute these values dynamically.
    See `compilation-error-regexp-alist'.

  - `:hyperlink' - Which expression to turn into a clickable hyperlink.
    Can be the number of a match group, or one of the symbols in
    `+compile-metavars-alist'.

\(fn NAME RX-FORMS... [:where SYMBOL = RX-FORM]* [:highlight number|var-symbol])"
  (declare (indent 1))
  (cl-assert (symbolp name))

  (pcase-let (((map :rx-form :file :line :col :type :hyperlink :highlights)
               (+compile-spec-for-compilation-error-alist forms)))
    `(progn
       (alist-set! compilation-error-regexp-alist-alist ',name
                   ;; See: `compilation-error-regexp-alist'
                   ;;
                   ;; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...])
                   ;;
                   '(,(rx-to-string (cons 'and rx-form) t)
                     ,file
                     ,line
                     ,col
                     ,type
                     ,hyperlink
                     ,@highlights))
       (add-to-list 'compilation-error-regexp-alist ',name)
       ',name)))

(provide '+compile)

;;; +compile.el ends here
