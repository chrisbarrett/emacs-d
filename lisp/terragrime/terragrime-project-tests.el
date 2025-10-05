;;; terragrime-project-tests.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'terragrime-project)

(defvar +this-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defun +test-project-graph (relpath)
  "Build the project graph the project at ./test-projects/RELPATH."
  (terragrime-project--build-graph
   (file-name-concat +this-dir "test-projects" relpath)))

(defun +terragrime-unit-at (relpath)
  (terragrime-unit-create :path (file-name-concat +this-dir "test-projects" relpath)))



(ert-deftest terragrime-project-tests--empty-project ()
  (should (null (+test-project-graph "empty"))))

(ert-deftest terragrime-project-tests--single-unit ()
  (let ((graph (+test-project-graph "single-unit")))
    (should (equal (length graph) 1))
    (should (equal graph
                   (list (+terragrime-unit-at "single-unit/terragrunt.hcl"))))))

(ert-deftest terragrime-project-tests--nested-unit ()
  (let ((graph (+test-project-graph "nested-unit")))
    (should (equal (length graph) 1))
    (should (equal graph
                   (list (+terragrime-unit-at "nested-unit/nested/terragrunt.hcl"))))))

(ert-deftest terragrime-project-tests--several-units ()
  (let ((graph (+test-project-graph "several-units")))
    (should (equal (length graph) 2))
    (should (equal graph
                   (list
                    (+terragrime-unit-at "unit-1/terragrunt.hcl")
                    (+terragrime-unit-at "unit-2/terragrunt.hcl"))))))

;;; terragrime-project-tests.el ends here
