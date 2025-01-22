;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(eval-when-compile
  (require 'quail))

(defmacro +quail-defun (package-name key &rest body)
  "Define a quail key to execute Lisp forms.

PACKAGE-NAME is the name of a quail package. When that quail package is
active and user enters KEY then BODY forms will be executed."
  ;; https://emacs.stackexchange.com/questions/76725/how-to-implement-a-function-in-quail-define-rules-for-set-input-method
  (declare (indent 2))
  (cl-assert (stringp key))
  (cl-assert (stringp package-name))
  (let ((gkey (gensym))
        (gpackage-name (gensym)))
    `(let* ((,gkey ,key)
            (,gpackage-name ,package-name)
            (fname (make-symbol (format "+quail-%s-key-%s" ,gpackage-name ,gkey))))
       (defalias fname (lambda (,(gensym "key") ,(gensym "idx"))
                         (quail-delete-region)
                         (setq quail-current-str nil
                               quail-converting nil
                               quail-conversion-str "")
                         (atomic-change-group ,@body)
                         (throw 'quail-tag nil)))

       (quail-defrule ,gkey fname ,gpackage-name t))))

(provide '+quail)
