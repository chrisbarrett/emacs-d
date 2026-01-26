;;; org-roam-lib.el --- Autoloads for org-roam module -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for org-roam module.

;;; Code:

(require 'seq)

;;; Variables

;;;###autoload
(defconst +org-roam-sensitive-tags '("daily" "sensitive" "private")
  "Tags that indicate a node should not be displayed without explicit user action.")

;;; Node filtering

;;;###autoload
(defun +org-roam-node-sensitive-p (node)
  "Return non-nil if NODE should be shown in default completion.
Nodes with tags in `+org-roam-sensitive-tags' are filtered out."
  (null (seq-intersection (org-roam-node-tags node) +org-roam-sensitive-tags)))

;;;###autoload
(defun +org-roam-node-find (&optional include-sensitive)
  "Find an org-roam node. See `org-roam-node-find'.

With optional prefix arg INCLUDE-SENSITIVE, include nodes with tags in
`+org-roam-sensitive-tags'."
  (interactive "P")
  (org-roam-node-find nil
                      nil
                      (unless include-sensitive
                        #'+org-roam-node-sensitive-p)))

;;; Node display for completion

;;;###autoload
(defun +org-roam-node-formatted-olp (node)
  "Construct a title string for NODE that includes its outline path.

This is useful for distinguishing or narrowing nodes according to a
topic without using tags."
  (pcase-let ((`(,title . ,rest)
               (thread-last
                 (append (list (org-roam-node-file-title node))
                         (org-roam-node-olp node)
                         (list (org-roam-node-title node)))
                 (seq-filter #'stringp)
                 (seq-mapcat (lambda (it) (split-string it ":")))
                 (seq-map #'string-trim)
                 (seq-uniq)
                 (nreverse))))
    (let ((prefix (seq-map (lambda (it) (propertize it 'face 'org-property-value)) (nreverse rest)))
          (title (propertize title 'face 'org-roam-title)))

      (string-join (append prefix (list title))
                   (propertize ": " 'face 'org-property-value)))))

;;; Marginalia annotator

;;;###autoload
(defun +org-roam-node-tags-annotator (cand)
  "Marginalia annotator for org-roam node CAND.
Shows @slipbox and #tags."
  (when-let* ((node (get-text-property 0 'node cand)))
    (let* ((slipbox (when (fboundp 'org-roam-node-slipbox)
                      (org-roam-node-slipbox node)))
           (all-tags (seq-filter #'identity
                                 (seq-keep (lambda (tag)
                                             (unless (equal tag slipbox)
                                               (concat "#" tag)))
                                           (org-roam-node-tags node)))))
      (if (fboundp 'marginalia--fields)
          (marginalia--fields
           ((concat "@" slipbox) :face 'org-roam-dim)
           ((string-join all-tags " ") :face 'org-tag))
        (concat " @" slipbox " " (string-join all-tags " "))))))

(provide 'org-roam-lib)

;;; org-roam-lib.el ends here
