;;; +compile-tests.el --- Tests for +compile.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require '+compile)
(require '+corelib)

(defun +sort-plist (plist)
  (seq-sort-by #'prin1-to-string
               #'string<
               (seq-partition plist 2)))

(defun +equivalent-plists-p (plist1 plist2)
  (should (equal (+sort-plist plist1)
                 (+sort-plist plist2))))



(ert-deftest compiling-specs--no-metavars ()
  (should
   (+equivalent-plists-p
    (+compile-spec-for-compilation-error-alist
     '(bol "hello, world" eol))

    '(:rx-form (bol "hello, world" eol)
      :highlights nil
      :hyperlink nil
      :type nil
      :file nil
      :line nil
      :col nil))))

(ert-deftest compiling-specs--no-metavars--has-groups ()
  (should
   (+equivalent-plists-p
    (+compile-spec-for-compilation-error-alist
     '(bol (group "hello") (group ",") (group-n 3 ("world")) eol))

    '(:rx-form (bol (group "hello") (group ",") (group-n 3 ("world")) eol)
      :highlights nil
      :hyperlink nil
      :type nil
      :file nil
      :line nil
      :col nil))))

(ert-deftest compiling-specs--metavars ()
  (should
   (+equivalent-plists-p
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
      :type nil))))

(ert-deftest compiling-specs--where-bindings ()
  (should
   (+equivalent-plists-p
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
      :col nil))))

(ert-deftest compiling-specs--recursive-where-bindings ()
  (should
   (+equivalent-plists-p
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
      :col nil))))

(ert-deftest compiling-specs--mutually-recursive-where-bindings ()
  (should
   (+equivalent-plists-p
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
      :col nil))))

(ert-deftest compiling-specs--realistic-example ()
  (should
   (+equivalent-plists-p
    (+compile-spec-for-compilation-error-alist
     '(line-start (* space) level ":" (* space) message (? " Did you mean:") "\n"
       (? (* space) hint "\n")
       (+ (* space) (? source-context) "\n")
       line-start (* space) "└─ " file ":" line ":" col (? ":" (+ nonl))

       :where level = (or (group-n 1 "warning") (group-n 2 "info") "error")
       :where hint = "hint:" (* space) (+ nonl)

       :where suggested-ident = (* nonl)
       :where source-context = (* space) (or (and (? line-number) "│" (* nonl))
                                             (and "*" (+ space) suggested-ident)
                                             (and (* space) "..." (* nonl)))

       :where line-number = (+ digit) (+ space)

       :type (1 . 2)
       :highlight message))
    '(:rx-form
      (line-start (* space) (or (group-n 1 "warning") (group-n 2 "info") "error") ":"
                  (* space) (group-n 1 (+? nonl)) (? " Did you mean:") "\n"
                  (? (* space) (and "hint:" (* space) (+ nonl)) "\n")
                  (+ (* space)
                     (? (and (* space)
                             (or (and (? (and (+ digit) (+ space))) "│" (* nonl))
                                 (and "*" (+ space) (* nonl))
                                 (and (* space) "..." (* nonl)))))
                     "\n")
                  line-start (* space) "└─ " (group-n 2 (+? nonl)) ":"
                  (group-n 3 (+ digit)) ":" (group-n 4 (+ digit)) (? ":" (+ nonl)))
      :file 2
      :line 3
      :col 4
      :type (1 . 2)
      :highlights (1)
      :hyperlink nil))))

;;; +compile-tests.el ends here
