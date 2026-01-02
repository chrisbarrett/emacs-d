;;; +org-roam.el --- Supporting functions for org-roam -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-eval-when (compile)
  (require 'org-clock)
  (require 'org-roam))

(defvar +org-roam-sensitive-tags '("daily" "private"))

(defun +org-roam-node-find (&optional include-sensitive)
  "Find an org-roam node. See `org-roam-node-find'.

With optional prefix arg INCLUDE-SENSITIVE, include nodes with tags in
`+org-roam-sensitive-tags'."
  (interactive "P")
  (org-roam-node-find nil
                      nil
                      (unless include-sensitive
                        (lambda (node)
                          (let ((tags (org-roam-node-tags node)))
                            (null (seq-intersection tags +org-roam-sensitive-tags)))))))

(defun +org-roam-node-title-or-olp (node &optional full-olp)
  (funcall (if (or full-olp current-prefix-arg)
               #'+org-roam-node-formatted-olp
             #'org-roam-node-title)
           node))

(defun +org-roam--node-title-hierarchy (node)
  (thread-last
    (append (list (org-roam-node-file-title node))
            (org-roam-node-olp node)
            (list (org-roam-node-title node)))
    (seq-filter #'stringp)
    (seq-mapcat (lambda (it) (split-string it ":")))
    (seq-map #'string-trim)
    (seq-uniq)))

(defun +org-roam-node-formatted-olp (node)
  (pcase-let ((`(,title . ,rest) (nreverse (+org-roam--node-title-hierarchy node))))
    (let ((prefix (seq-map (lambda (it) (propertize it 'face 'org-property-value)) (nreverse rest)))
          (title (propertize title 'face 'org-roam-title)))

      (string-join (append prefix (list title))
                   (propertize ": " 'face 'org-property-value)))))

(provide '+org-roam)

;;; +org-roam.el ends here
