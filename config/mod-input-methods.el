;;; mod-input-methods.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'quail)

(cl-eval-when (compile)
  (require 'org))

(setq default-input-method "french-postfix")
(setq default-transient-input-method default-input-method)



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

(with-eval-after-load "quail/latin-post"
  (message "Initializing custom keybindings for latin-post")

  (+quail-defun "french-postfix" ";"
    (delete-horizontal-space)
    (insert " ; "))

  (+quail-defun "french-postfix" ":"
    (delete-horizontal-space)
    (let ((left-pad (cond
                     ((equal (char-before) ?:)
                      "")
                     ((and (derived-mode-p 'org-mode) (org-at-item-p) (not (org-at-item-description-p)))
                      " ")
                     (t
                      " "))))
      (insert left-pad ": "))))

(provide 'mod-input-methods)

;;; mod-input-methods.el ends here
