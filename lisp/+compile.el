;;; +compile.el --- Helpers for M-x compile -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'cl-lib)
(require 'map)

(eval-and-compile
  (defconst +compile-metavars-alist '((file . (and (any alnum "/~._") (*? (not (any ":")))))
                                      (line . (and (any "1-9") (* digit)))
                                      (col . (and (any "1-9") (* digit)))
                                      (message . (+? print)))
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

  (defun +compile--subst (form env group-numbers)
    (cl-letf* ((missing-var (gensym "missing-var-"))
               (missing-group (gensym "missing-group-"))

               ((symbol-function 'group-name-p)
                (lambda (term)
                  (and (symbolp term) (string-suffix-p ":" (symbol-name term)))))

               ((symbol-function 'rewrite-named-group-to-numbered-group)
                (lambda (form)
                  (pcase-exhaustive form
                    (`(,h . ,tl)
                     (let ((group-number (gethash h group-numbers )))
                       `(group-n ,group-number ,@tl))))))

               ((symbol-function 'subst)
                (lambda (term)
                  (pcase term
                    ;; Rewrite standard metavar
                    ((and (pred symbolp)
                          (let rx-form (alist-get term +compile-metavars-alist missing-var))
                          (guard (not (equal missing-var rx-form)))
                          (let group-number (gethash term group-numbers missing-group))
                          (guard (not (equal group-number missing-group)))
                          )
                     `(group-n ,group-number ,rx-form))

                    ;; Rewrite named group to numbered group.
                    (`(,(pred group-name-p) . ,_tl)
                     (rewrite-named-group-to-numbered-group term))

                    ((and (pred symbolp)
                          (let expansion (gethash term env missing-var))
                          (guard (not (equal missing-var expansion)))
                          )
                     (pcase expansion
                       ;; Further expand binding if it's a named group.
                       (`(,(pred group-name-p) . ,_tl)
                        (rewrite-named-group-to-numbered-group expansion))
                       (expansion
                        `(and ,@expansion))))

                    (term
                     term)))))

      (+tree-map 'subst form)))

  (defun +compile--optimize (form)
    ;; Rx does a good job, so just do some basic trivial hoists to make output
    ;; forms more readable.
    (+tree-map (lambda (term)
                 (pcase term
                   (`(and ,x) x)
                   (`(or ,x) x)
                   (term
                    term)))
               form))

  (defun +compile--analyze (rx-forms where-bindings)
    "Analyze RX-FORMS for information needed to build a matcher.

The result is a plist containing the following keys:

- `:referenced-metavars' - the values in `+compile-metavars-alist' that
  were used.

- `:highest-group-number' - the highest allocated match group number.

- `:named-groups' - a list of named capture groups."
    (let* ((numbered-groups 0)
           (un-numbered-groups 0)
           (referenced-metavars '())
           (named-groups '())
           (metavars (seq-map #'car +compile-metavars-alist))
           (analyze (lambda (it)
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
                      it)))

      (+tree-map analyze rx-forms)

      (pcase-dolist (`(,_name . ,value) where-bindings)
        (+tree-map analyze value))

      (let ((result
             (list :highest-group-number (max numbered-groups un-numbered-groups)
                   :referenced-metavars (nreverse (delete-dups referenced-metavars))
                   :named-groups (nreverse (delete-dups named-groups)))))
        result)))


  (define-error '+compile-duplicate-bindings "Duplicated where-bindings")

  (defun +compile--assert-no-duplicate-bindings (where-bindings-alist)
    (let* ((seen (make-hash-table))
           (duplicate-bindings))
      (pcase-dolist (`(,name . ,_) where-bindings-alist)
        (when (gethash name seen)
          (push name duplicate-bindings))
        (puthash name t seen))
      (when duplicate-bindings
        (throw '+compile-duplicate-bindings duplicate-bindings))))

  (defun +compile--parse-keyword-args (keyword-args)
    (let ((extra-keywords (make-hash-table))
          (where-bindings)
          (groups (+chunk-by #'keywordp keyword-args)))

      (dolist (group groups)
        (pcase-exhaustive group
          ;; Both `:where foo = bar ...' and `:where foo bar ...' are
          ;; accepted.
          ((or `(:where ,key = . ,values)
               `(:where ,key . ,values))
           (push (cons key values) where-bindings)
           )
          (`(,key ,value)
           (puthash key value extra-keywords))))

      (+compile--assert-no-duplicate-bindings where-bindings)

      (list :where-bindings (nreverse where-bindings)
            :extra-keywords (+plist-from-hash-table extra-keywords))))


  (defun +compile--build-env (rx-forms keyword-args)
    (pcase-let* ((env (make-hash-table))
                 (group-numbers (make-hash-table))
                 ((map :where-bindings :extra-keywords) (+compile--parse-keyword-args keyword-args))
                 ((map :referenced-metavars :highest-group-number :named-groups)
                  (+compile--analyze rx-forms where-bindings)))

      (cl-loop for fresh-number from (1+ highest-group-number)
               for var in (append referenced-metavars
                                  named-groups)
               do (puthash var fresh-number group-numbers))

      (pcase-dolist (`(,key . ,value) where-bindings)
        (puthash key value env))

      (list
       :env env
       :extra-keywords extra-keywords
       :group-numbers group-numbers)))

  (defun +compile-rx (rx-form keyword-args)
    (pcase-let* (((and analysis-result (map :env :group-numbers))
                  (+compile--build-env rx-form keyword-args))
                 )
      (append (list :rx-form (+compile--optimize (+compile--subst rx-form env group-numbers)))
              analysis-result))))



(define-error '+compile-unbound-function "No such function")

(defun +compile--no-unrecognised-plist-keys (extra-keywords)
  (let ((unknown-keywords (seq-difference (seq-map 'car (seq-partition extra-keywords 2))
                                          '(:where
                                            :highlights
                                            :type
                                            :hyperlink
                                            :file
                                            :line
                                            :col))))
    (cl-assert (null unknown-keywords) nil "Unknown keyword arguments: %S" unknown-keywords)))

(defun +compile-spec-for-compilation-error-alist (forms)
  (pcase-let* ((`(,rx-forms ,keyword-args)
                (+split-with (lambda (it) (not (keywordp it))) forms))
               ((map :group-numbers :extra-keywords :rx-form) (+compile-rx rx-forms keyword-args))
               )
    (+compile--no-unrecognised-plist-keys extra-keywords)

    (cl-labels ((assert-symbols-fboundp (it)
                  (when (and it (symbolp it) (not (fboundp it)))
                    (throw '+compile-unbound-function it))
                  it)

                (eval-group-numbers (form)
                  (assert-symbols-fboundp
                   (+compile--subst-group-numbers form group-numbers)))

                (compile-from-keyword-arg (keyword &optional default)
                  (assert-symbols-fboundp
                   (pcase (plist-get extra-keywords keyword)
                     ('()
                      default)
                     (it
                      (+compile--subst-group-numbers it group-numbers)))))
                )
      (list
       :rx-form rx-form

       :highlights
       (pcase-exhaustive (plist-get extra-keywords :highlights)
         ((and value (pred listp))
          (seq-map (lambda (it)
                     ;; The first element may be a symbol that resolves
                     ;; to a group number, but any remaining elements are
                     ;; part of a face spec and should not be
                     ;; substituted.
                     (pcase it
                       (`(,match . ,rest)
                        (pcase rest
                          ((or `(face ,_p1 ,_v1 . ,_)
                               `(',(and (pred symbolp) face-symbol)))
                           (unless (facep face-symbol)
                             (warn ":highlights[%S]: `%S' is not a known face" match it))
                           (cons (eval-group-numbers match) rest))
                          (`(face . ,_)
                           (error ":highlights[%S]: face properties must be key-value-pairs" match))
                          (_
                           (error ":highlights[%S]: must provide a 'face or (face props...) spec" match))))
                       (it
                        (error ":highlights must be an alist, but value was not a cons: %S" it))))
                   value))
         (_
          (error ":highlights must be an alist")))

       :type (assert-symbols-fboundp
              (pcase (plist-get extra-keywords :type)
                ('error 2)
                ('warning 1)
                ('warn 1)
                ('info 0)
                (`(,n . ,m)
                 (cons (eval-group-numbers n) (eval-group-numbers m)))
                (value (assert-symbols-fboundp value))))

       :hyperlink (compile-from-keyword-arg :hyperlink)
       :file (compile-from-keyword-arg :file (gethash 'file group-numbers))
       :line (compile-from-keyword-arg :line (gethash 'line group-numbers))
       :col (compile-from-keyword-arg :col (gethash 'col group-numbers))))))

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

  - `:highlights' - An alist of additional text properties to apply to
    the matched string. The car of each element is a match group (as a
    number or name) followed by a quoted face or a face spec. See
    `compilation-error-regexp-alist'.


\(fn NAME RX-FORMS... [:where SYMBOL = RX-FORM]* [:highlights number|var-symbol])"
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
