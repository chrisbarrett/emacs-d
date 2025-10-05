;;; terragrime-project.el --- Project graph types & functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(cl-defstruct (terragrime-unit
               (:constructor terragrime-unit-create)
               (:copier terragrime-unit-copy))
  "A unit in a terragrunt project graph.

It may or may not be managed via a stack."
  (path nil :documentation "The path to the unit")
  (entry nil :documentation "A reference to an entry object if managed by a stack, or nil"))

(cl-defstruct (terragrime-stack
               (:constructor terragrime-stack-create)
               (:copier terragrime-stack-copy))
  "A stack in a terragrunt project graph.

Its child units and stacks are wrapped stack entries."
  (path nil :documentation "The path to the stack file")
  (entry nil :documentation "A reference to an entry object if managed by a stack, or nil")
  (entries nil :documentation "Hash-table of ident->entry objects"))

(cl-defstruct (terragrime-stack-entry
               (:constructor terragrime-stack-entry-create)
               (:copier terragrime-stack-entry-copy))
  "A wrapper around a stack or unit."
  (ident nil :documentation "The identifier representing the entry in the declaring stack")
  (exists-p nil :documentation "Whether the associated resource exists on-disk"))

(defun terragrime-project--subgraph-from-stack (stack-file-path)
  nil
  )

(defun terragrime-project--build-graph (&optional dir)
  (seq-mapcat (lambda (path)
                (pcase (file-name-nondirectory path)
                  ;; Skip hidden files and . ..
                  ((and file (guard (string-prefix-p "." file))))

                  ("terragrunt.hcl"
                   (list (terragrime-unit-create :path path)))
                  ("terragrunt.stack.hcl"
                   (terragrime-project--subgraph-from-stack path))

                  ;; search children
                  ((guard (file-directory-p path))
                   (terragrime-project--build-graph path))))

              (directory-files (or dir default-directory) t)))

(provide 'terragrime-project)

;;; terragrime-project.el ends here
