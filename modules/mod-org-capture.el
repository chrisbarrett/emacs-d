;;; mod-org-capture.el --- Configuration for org-capture -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-capture)
(require '+capture)

(setq org-capture-templates
      (cl-flet ((notes-datetree (key desc template &rest kvps)
                  (append
                   (list key desc 'entry '(file+olp+datetree org-default-notes-file) template)
                   '(:tree-type (month day))
                   kvps))
                (template-file (name)
                  `(file ,(file-name-concat user-emacs-directory "capture-templates" name))))

        (list (notes-datetree "t" "Todo" "* TODO %?")
              (notes-datetree "n" "Note" "* %T %?")
              (notes-datetree "N" "Note (setting time)" "* %^T %?")

              '("w" "work")
              (notes-datetree "wt" "Todo" "* TODO %?               :%(timekeep-work-tag):work:")
              (notes-datetree "wn" "Note" "* %T %?                 :%(timekeep-work-tag):work:")
              (notes-datetree "wN" "Note (setting time)" "* %^T %? :%(timekeep-work-tag):work:")

              (notes-datetree "l" "Link" "* %T %(org-cliplink-capture)\n%?")

              `("L" "Litnote" plain
                (function +capture-litnote-function) ,(template-file "litnote.org")
                :immediate-finish t :jump-to-captured t)

              (notes-datetree "p" "Postmortem" (template-file "postmortem.org") :jump-to-captured t)
              (notes-datetree "j" "Journal" (template-file "journal.org"))
              (notes-datetree "r" "Language Learning Review"
                              (template-file "language-learning-review.org")
                              :immediate-finish t :jump-to-captured t))))

(org-capture-put :kill-buffer t)

(provide 'mod-org-capture)

;;; mod-org-capture.el ends here
