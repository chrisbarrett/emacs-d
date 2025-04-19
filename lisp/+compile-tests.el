;;; +compile-tests.el --- Tests for +compile.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require '+compile)
(require '+corelib)

(defun should-be-equiv-plists (plist1 plist2)
  (cl-labels ((sort-plist (plist)
                (seq-sort-by #'prin1-to-string
                             #'string<
                             (seq-partition plist 2))))
    (should (equal (sort-plist plist1)
                   (sort-plist plist2)))))


;;; Analysis

(ert-deftest compiling-specs--analysis--finds-named-groups ()
  (cl-labels ((named-groups (form &optional where-bindings)
                (plist-get (+compile--analyze form where-bindings)
                           :named-groups)))
    (should-not
     (named-groups '(and "hello" "world")))

    (should
     (equal '(foo: bar:)
            (named-groups '(and "hello" (foo: "world") (group (bar: "!"))))))))


(ert-deftest compiling-specs--analysis--finds-named-groups--in-where-bindings ()
  (cl-labels ((named-groups (form where-bindings)
                (plist-get (+compile--analyze form where-bindings)
                           :named-groups)))
    (should
     (equal '(foo: bar:)
            (named-groups '(and "hello")
                          '((a . (foo: "hello"))
                            (b . (and "world" (bar: "!")))))))))

(ert-deftest compiling-specs--analysis--finds-referenced-metavars ()
  (cl-labels ((metavars (form &optional where-bindings)
                (plist-get (+compile--analyze form where-bindings)
                           :referenced-metavars)))
    (should-not
     (metavars '(and "hello" "world!")))

    (should
     (equal '(file col)
            (metavars '(and "hello" (group file) (and col "world!")))))))

(ert-deftest compiling-specs--analysis--finds-referenced-metavars--in-where-bindings ()
  (cl-labels ((metavars (form where-bindings)
                (plist-get (+compile--analyze form where-bindings)
                           :referenced-metavars)))
    (should-not
     (metavars '(a b)
               '((a . "hello")
                 (b . "world!"))))

    (should
     (equal '(file col)
            (metavars '(a b)
                      '((a . file)
                        (b . col)))))))


(ert-deftest compiling-specs--analysis--computes-highest-group-number ()
  (cl-labels ((highest-group (form &optional where-bindings)
                (plist-get (+compile--analyze form where-bindings)
                           :highest-group-number)))
    (should
     (equal 0 (highest-group '(and "hello" "world!"))))

    (should
     (equal 1 (highest-group '(and "hello" (group "world!")))))

    (should
     (equal 5
            (highest-group '(and "hello"
                                 (group "w")
                                 (group "o")
                                 (group "r")
                                 (group "l")
                                 (group "d")
                                 (group-n 1 "!")))))
    (should
     (equal 3
            (highest-group '(and "hello"
                                 (group "world")
                                 (group-n 3 "!")))))))


;;; Binding Environment

(ert-deftest compiling-specs--env--computes-group-numbers ()
  (cl-labels ((build-env (form)
                (+plist-from-hash-table
                 (plist-get (+compile--build-env form nil)
                            :group-numbers))))

    (should-be-equiv-plists
     '(foo: 1)
     (build-env '(and (foo: 1) bar: 2)))

    (should-be-equiv-plists
     '(file 1)
     (build-env '(and "hello" file "world")))

    ;; Generates fresh numbers starting from 1+ highest explicit numbered group.
    (should-be-equiv-plists
     '(file 4 rest: 5)
     (build-env '(and "hello" file
                      (group-n 3 "w")
                      (rest: "orld!"))))))

(ert-deftest compiling-specs--env--bindings ()
  (cl-labels ((bindings (form &optional keyword-args)
                (+plist-from-hash-table
                 (plist-get (+compile--build-env form keyword-args)
                            :env))))

    (should-not
     (bindings '(and (foo: 1) bar: 2)))

    (should-be-equiv-plists
     '(b ((+ nonl)))

     (bindings '(and "hello" file b)
               '(:where b = (+ nonl))))))

(ert-deftest compiling-specs--env--extra-keywords ()
  (cl-labels ((extra-keywords (form &optional keyword-args)
                (plist-get (+compile--build-env form keyword-args)
                           :extra-keywords)))

    (should-not
     (extra-keywords '(and (foo: 1) bar: 2)))

    (should-be-equiv-plists
     '(:foo a)
     (extra-keywords '(and "hello" file b)
                     '(:foo a)))

    (should-not
     (extra-keywords '(and "hello" file b)
                     '(:where b = (+ nonl))))

    (should-be-equiv-plists
     '(:foo a :bar b)
     (extra-keywords '(and "hello" file b)
                     '(:where b = (+ nonl)
                       :foo a
                       :bar b)))))


;;; Top-level compilation output

(ert-deftest compiling-specs--no-metavars ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(bol "hello, world" eol))

   '(:rx-form (bol "hello, world" eol)
     :highlights nil
     :hyperlink nil
     :type nil
     :file nil
     :line nil
     :col nil)))

(ert-deftest compiling-specs--no-metavars--has-groups ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(bol (group "hello") (group ",") (group-n 3 ("world")) eol))

   '(:rx-form (bol (group "hello") (group ",") (group-n 3 ("world")) eol)
     :highlights nil
     :hyperlink nil
     :type nil
     :file nil
     :line nil
     :col nil)))

(ert-deftest compiling-specs--no-metavars--has-named-groups ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(bol (greeting: "hello, world") (punct: "!") eol
      :hyperlink greeting))

   '(:rx-form (bol (group-n 1 "hello, world") (group-n 2 "!") eol)
     :highlights nil
     :hyperlink 1
     :type nil
     :file nil
     :line nil
     :col nil)))

(ert-deftest compiling-specs--metavars ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(bol file ":" line ":" col " -- " message eol))

   '(:rx-form (bol
               (group-n 1 (+? nonl))
               ":" (group-n 2 (+ digit))
               ":" (group-n 3 (+ digit))
               " -- " (group-n 4 (+? nonl))
               eol)
     :file 1
     :line 2
     :col 3
     :highlights nil
     :hyperlink nil
     :type nil)))

(ert-deftest compiling-specs--where-bindings ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(bol file ":" custom-line ":" custom-col
      :where custom-line = (+? (any "1-9") (* digit))
      :where custom-col = (+? (any "1-9") (* digit))))

   '(:rx-form (bol
               (group-n 1 (+? nonl))
               ":" (+? (any "1-9") (* digit))
               ":" (+? (any "1-9") (* digit)))
     :highlights nil
     :hyperlink nil
     :type nil
     :file 1
     :line nil
     :col nil)))

(ert-deftest compiling-specs--where-bindings--named-group ()
  (cl-labels ((compiled-rx (form)
                (plist-get (+compile-spec-for-compilation-error-alist form)
                           :rx-form)))
    (should
     (equal (compiled-rx '(a b
                           :where a = foo: "hello"
                           :where b = bar: "world!"))
            '((group-n 1 "hello")
              (group-n 2 "world!"))))))

(ert-deftest compiling-specs--recursive-where-bindings ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(b c
      :where a = (* space)
      :where b = bol "hello" a
      :where c = b "world" eol))
   '(:rx-form ((and bol "hello" (* space))
               (and (and bol "hello" (* space)) "world" eol))
     :highlights nil
     :hyperlink nil
     :type nil
     :file nil
     :line nil
     :col nil)))

(ert-deftest compiling-specs--mutually-recursive-where-bindings ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(a
      :where a = b c d
      :where b = "hello"
      :where c = b d
      :where d = "world"))
   '(:rx-form ((and "hello" (and "hello" "world") "world"))
     :highlights nil
     :hyperlink nil
     :type nil
     :file nil
     :line nil
     :col nil)))

(ert-deftest compiling-specs--realistic-example ()
  (should-be-equiv-plists
   (+compile-spec-for-compilation-error-alist
    '(bol (+ space) level ":" (* space) message (? " Did you mean:") "\n"
      (+ (* space) (? (or hint source-context)) "\n")
      bol (* space) "└─ " file ":" line ":" col (? ":" (+ nonl))

      :where level = (or (warn: "warning") (info: "info") "error")

      :where hint = "hint: " (* space) (+ nonl)

      :where source-context = (* space) (or (and (? line-number) "│" (* nonl))
                                            (and "*" (+ space) ident)
                                            (and (* space) "..." (* nonl)))
      :where ident = (* nonl)
      :where line-number = (+ digit) (+ space)

      :type (warn . info)
      :highlight message))
   '(:rx-form
     (bol (+ space)
          (or (group-n 5 "warning") (group-n 6 "info") "error")
          ":"
          (* space) (group-n 1 (+? nonl)) (? " Did you mean:") "\n"
          (+ (* space)
             (? (or (and "hint: " (* space) (+ nonl))
                    (and (* space)
                         (or (and (? (and (+ digit) (+ space))) "│" (* nonl))
                             (and "*" (+ space) (* nonl))
                             (and (* space) "..." (* nonl))))))
             "\n")

          bol (* space) "└─ "
          (group-n 2 (+? nonl)) ":" (group-n 3 (+ digit)) ":" (group-n 4 (+ digit))
          (? ":" (+ nonl)))
     :highlights nil
     :hyperlink nil
     :type (5 . 6)
     :file 2
     :line 3
     :col 4)))

(ert-deftest compiling-specs--where-clauses--error-on-duplicates ()
  (should-error
   (+compile-spec-for-compilation-error-alist
    '(a
      :where a = "s"
      :where a = "t"))))

;;; +compile-tests.el ends here
