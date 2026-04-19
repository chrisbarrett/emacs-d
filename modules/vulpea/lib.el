;;; lib.el --- Autoloads for vulpea module -*- lexical-binding: t; -*-

;;; Code:

(require 'seq)

;;; Variables

;;;###autoload
(defvar +notes-directory "~/org/roam/"
  "Root directory for notes indexed by vulpea.")

;;;###autoload
(defconst +vulpea-sensitive-tags '("daily" "sensitive" "private")
  "Tags that indicate a note should not be displayed in default completion.")

;;; Note filtering

;;;###autoload
(defun +vulpea-note-visible-p (note)
  "Return non-nil if NOTE should appear in default completion.
Notes with tags in `+vulpea-sensitive-tags' are filtered out."
  (null (seq-intersection (vulpea-note-tags note) +vulpea-sensitive-tags)))

;;;###autoload
(defun +vulpea-find (&optional include-sensitive)
  "Find a vulpea note, filtering sensitive tags by default.

With optional prefix arg INCLUDE-SENSITIVE, include notes with
tags in `+vulpea-sensitive-tags'."
  (interactive "P")
  (vulpea-find :filter-fn (unless include-sensitive
                             #'+vulpea-note-visible-p)))

;;; Note display for completion

;;;###autoload
(defun +vulpea-note-describe-olp (note)
  "Format NOTE title with outline path prefix for completion display."
  (pcase-let ((`(,title . ,rest)
               (thread-last
                 (append (list (vulpea-note-file-title note))
                         (vulpea-note-outline-path note)
                         (list (vulpea-note-title note)))
                 (seq-filter #'stringp)
                 (seq-mapcat (lambda (it) (split-string it ":")))
                 (seq-map #'string-trim)
                 (seq-uniq)
                 (nreverse))))
    (let ((prefix (seq-map (lambda (it) (propertize it 'face 'org-property-value)) (nreverse rest)))
          (title (propertize title 'face 'bold)))
      (string-join (append prefix (list title))
                   (propertize ": " 'face 'org-property-value)))))




;;; lib.el ends here
