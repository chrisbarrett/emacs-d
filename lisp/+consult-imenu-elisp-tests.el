;;; +consult-imenu-elisp-tests.el --- Tests for +consult-imenu-elisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require '+consult-imenu-elisp)

;;; Classification Tests

(ert-deftest +consult-imenu-elisp--internal-p--double-dash ()
  "Symbols with double-dash are internal."
  (should (+consult-imenu-elisp--internal-p "my-pkg--helper"))
  (should (+consult-imenu-elisp--internal-p "foo--bar--baz"))
  (should (+consult-imenu-elisp--internal-p "--private")))

(ert-deftest +consult-imenu-elisp--internal-p--underscore-prefix ()
  "Symbols starting with underscore are internal."
  (should (+consult-imenu-elisp--internal-p "_temp-var"))
  (should (+consult-imenu-elisp--internal-p "_")))

(ert-deftest +consult-imenu-elisp--internal-p--public ()
  "Public symbols are not internal."
  (should-not (+consult-imenu-elisp--internal-p "my-pkg-init"))
  (should-not (+consult-imenu-elisp--internal-p "main"))
  (should-not (+consult-imenu-elisp--internal-p "foo-bar"))
  (should-not (+consult-imenu-elisp--internal-p "a-b-c")))

(ert-deftest +consult-imenu-elisp--internal-p--edge-cases ()
  "Edge cases for internal detection."
  ;; Single dash is public
  (should-not (+consult-imenu-elisp--internal-p "a-b"))
  ;; Dash at end is public
  (should-not (+consult-imenu-elisp--internal-p "test-"))
  ;; Triple dash contains double dash
  (should (+consult-imenu-elisp--internal-p "foo---bar")))

;;; Category Exclusion Tests

(ert-deftest +consult-imenu-elisp--excluded-category-p--explicit ()
  "Explicitly excluded categories are detected."
  (should (+consult-imenu-elisp--excluded-category-p "Sections"))
  (should (+consult-imenu-elisp--excluded-category-p "Headings"))
  (should (+consult-imenu-elisp--excluded-category-p "Package")))

(ert-deftest +consult-imenu-elisp--excluded-category-p--pattern ()
  "Categories matching Section/Heading patterns are detected."
  (should (+consult-imenu-elisp--excluded-category-p "Code Section"))
  (should (+consult-imenu-elisp--excluded-category-p "Heading Level 1"))
  (should (+consult-imenu-elisp--excluded-category-p "MySections")))

(ert-deftest +consult-imenu-elisp--excluded-category-p--normal ()
  "Normal categories are not excluded."
  (should-not (+consult-imenu-elisp--excluded-category-p "Functions"))
  (should-not (+consult-imenu-elisp--excluded-category-p "Variables"))
  (should-not (+consult-imenu-elisp--excluded-category-p "Macros")))

;;; Split Items Tests

(ert-deftest +consult-imenu-elisp--split-items--mixed ()
  "Items are correctly split into public and internal."
  (let* ((items '(("my-pkg-public" . 10)
                  ("my-pkg--private" . 20)
                  ("_temp" . 30)
                  ("other-public" . 40)))
         (result (+consult-imenu-elisp--split-items items)))
    (should (equal (car result) '(("my-pkg-public" . 10) ("other-public" . 40))))
    (should (equal (cdr result) '(("my-pkg--private" . 20) ("_temp" . 30))))))

(ert-deftest +consult-imenu-elisp--split-items--all-public ()
  "All public items returns empty internal list."
  (let* ((items '(("foo" . 1) ("bar" . 2)))
         (result (+consult-imenu-elisp--split-items items)))
    (should (equal (car result) items))
    (should (null (cdr result)))))

(ert-deftest +consult-imenu-elisp--split-items--all-internal ()
  "All internal items returns empty public list."
  (let* ((items '(("--foo" . 1) ("_bar" . 2)))
         (result (+consult-imenu-elisp--split-items items)))
    (should (null (car result)))
    (should (equal (cdr result) items))))

(ert-deftest +consult-imenu-elisp--split-items--empty ()
  "Empty input returns empty lists."
  (let ((result (+consult-imenu-elisp--split-items nil)))
    (should (null (car result)))
    (should (null (cdr result)))))

;;; Transform Category Tests

(ert-deftest +consult-imenu-elisp--transform-category--splits ()
  "Normal categories are split by visibility."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Functions"
                 '(("public-fn" . 10) ("--private-fn" . 20)))))
    (should (= (length result) 2))
    (should (equal (car result) '("Functions (Public)" ("public-fn" . 10))))
    (should (equal (cadr result) '("Functions (Internal)" ("--private-fn" . 20))))))

(ert-deftest +consult-imenu-elisp--transform-category--public-first ()
  "Public groups appear before internal groups."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Variables"
                 '(("--internal" . 1) ("public" . 2)))))
    (should (string-match-p "Public" (caar result)))
    (should (string-match-p "Internal" (caadr result)))))

(ert-deftest +consult-imenu-elisp--transform-category--only-public ()
  "When all items are public, only Public group is emitted."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Functions"
                 '(("foo" . 1) ("bar" . 2)))))
    (should (= (length result) 1))
    (should (equal (caar result) "Functions (Public)"))))

(ert-deftest +consult-imenu-elisp--transform-category--only-internal ()
  "When all items are internal, only Internal group is emitted."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Functions"
                 '(("--foo" . 1) ("_bar" . 2)))))
    (should (= (length result) 1))
    (should (equal (caar result) "Functions (Internal)"))))

(ert-deftest +consult-imenu-elisp--transform-category--empty ()
  "Empty category produces no groups."
  (let ((result (+consult-imenu-elisp--transform-category "Functions" nil)))
    (should (null result))))

(ert-deftest +consult-imenu-elisp--transform-category--excluded ()
  "Excluded categories pass through unchanged."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Sections"
                 '(("--internal" . 1) ("public" . 2)))))
    (should (= (length result) 1))
    (should (equal (car result) '("Sections" ("--internal" . 1) ("public" . 2))))))

;;; Transform Alist Tests

(ert-deftest +consult-imenu-elisp--transform-alist--full ()
  "Complete transformation of imenu alist."
  (let* ((alist '(("Functions" ("my-pkg-init" . 10) ("my-pkg--helper" . 20))
                  ("Variables" ("my-var" . 30) ("_temp" . 40))
                  ("Sections" ("Setup" . 50) ("--hidden" . 60))))
         (result (+consult-imenu-elisp--transform-alist alist)))
    ;; Should have: Functions (Public), Functions (Internal),
    ;;              Variables (Public), Variables (Internal),
    ;;              Sections (unchanged)
    (should (assoc "Functions (Public)" result))
    (should (assoc "Functions (Internal)" result))
    (should (assoc "Variables (Public)" result))
    (should (assoc "Variables (Internal)" result))
    (should (assoc "Sections" result))
    ;; Sections should have both items
    (let ((sections (cdr (assoc "Sections" result))))
      (should (= (length sections) 2)))))

(ert-deftest +consult-imenu-elisp--transform-alist--preserves-toplevel ()
  "Top-level items (not in categories) are preserved."
  (let* ((alist '(("top-level-fn" . 5)
                  ("Functions" ("foo" . 10))))
         (result (+consult-imenu-elisp--transform-alist alist)))
    ;; Top-level item should be preserved
    (should (assoc "top-level-fn" result))))

(ert-deftest +consult-imenu-elisp--transform-alist--empty ()
  "Empty alist produces empty result."
  (should (null (+consult-imenu-elisp--transform-alist nil))))

;;; Property Verification (from spec)

(ert-deftest spec-property-1--my-pkg--helper-internal ()
  "`my-pkg--helper' is classified as internal."
  (should (+consult-imenu-elisp--internal-p "my-pkg--helper")))

(ert-deftest spec-property-2--_temp-internal ()
  "`_temp' is classified as internal."
  (should (+consult-imenu-elisp--internal-p "_temp")))

(ert-deftest spec-property-3--my-pkg-init-public ()
  "`my-pkg-init' is classified as public."
  (should-not (+consult-imenu-elisp--internal-p "my-pkg-init")))

(ert-deftest spec-property-4--main-public ()
  "`main' is classified as public."
  (should-not (+consult-imenu-elisp--internal-p "main")))

(ert-deftest spec-property-5--functions-split ()
  "Functions category splits into Functions (Public) and Functions (Internal)."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Functions"
                 '(("public" . 1) ("--internal" . 2)))))
    (should (assoc "Functions (Public)" result))
    (should (assoc "Functions (Internal)" result))))

(ert-deftest spec-property-6--public-before-internal ()
  "Public groups appear before Internal groups in output."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Functions"
                 '(("--a" . 1) ("b" . 2) ("--c" . 3) ("d" . 4)))))
    (let ((public-pos (cl-position "Functions (Public)" result :key #'car :test #'equal))
          (internal-pos (cl-position "Functions (Internal)" result :key #'car :test #'equal)))
      (should (< public-pos internal-pos)))))

(ert-deftest spec-property-7--sections-unchanged ()
  "Sections category passes through unchanged."
  (let ((result (+consult-imenu-elisp--transform-category
                 "Sections"
                 '(("--internal" . 1) ("public" . 2)))))
    (should (= (length result) 1))
    (should (equal (caar result) "Sections"))))

(ert-deftest spec-property-8--empty-groups-omitted ()
  "Empty groups are omitted from output."
  ;; Only public items -> no Internal group
  (let ((result (+consult-imenu-elisp--transform-category
                 "Functions"
                 '(("public" . 1)))))
    (should-not (assoc "Functions (Internal)" result)))
  ;; Only internal items -> no Public group
  (let ((result (+consult-imenu-elisp--transform-category
                 "Functions"
                 '(("--internal" . 1)))))
    (should-not (assoc "Functions (Public)" result))))

(provide '+consult-imenu-elisp-tests)
;;; +consult-imenu-elisp-tests.el ends here
