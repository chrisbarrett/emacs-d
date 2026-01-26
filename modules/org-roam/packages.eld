;; -*- lisp-data -*-
;; Package specifications for org-roam module

;; Core zettelkasten framework
(org-roam)

;; Extensions from nursery package
(nursery :host github
         :repo "chrisbarrett/nursery"
         :files (:defaults "lisp/*"))
