;;; mod-org-link.el --- Configuration for org-link (ol) -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom link types, etc.

;;; Code:

(require 'ol)

;; Highlight broken file links.
(org-link-set-parameters
 "file" :face (lambda (path)
                (if (or (file-remote-p path)
                        (file-exists-p path))
                    'org-link
                  '(warning org-link))))

(org-link-set-parameters
 "crate"
 :follow (lambda (name)
           (browse-url (format "https://docs.rs/%s/latest/%s" name name)))
 :export (lambda (path desc format)
           (let ((crate-name path)
                 (desc (or desc (concat "Crate " path))))
             (pcase format
               (`html (format "<a href=\"https://crates.io/crates/%s\">%s</a>" crate-name desc))
               (`latex (format "\\href{https://crates.io/crates/%s}{%s}" crate-name desc))
               (_ desc)))))

(org-link-set-parameters
 "github"
 :follow (lambda (path)
           (browse-url (format "https://github.com/%s" path)))
 :export (lambda (path desc format)
           (let ((desc (or desc (concat "GitHub: " path))))
             (pcase format
               (`html (format "<a href=\"https://github.com/%s\">%s</a>" path desc))
               (`latex (format "\\href{https://github.com/%s}{%s}" path desc))
               (_ desc)))))

(autoload 'rfc-mode--document-buffer "rfc-mode")

(org-link-set-parameters
 "RFC"
 :follow (lambda (number)
           (require 'rfc-mode)
           (pop-to-buffer (rfc-mode--document-buffer (string-to-number number))))
 :export (lambda (path desc format)
           (let ((rfc-num path)
                 (desc (or desc (concat "RFC " path))))
             (pcase format
               (`html (format "<a href=\"https://www.rfc-editor.org/rfc/rfc%s.html\">%s</a>" rfc-num desc))
               (`latex (format "\\href{https://www.rfc-editor.org/rfc/rfc%s.html}{%s}" rfc-num desc))
               (_ desc)))))

;; Use a distinct face for ID (org-roam) links

(defface +org-id-link
  '((t (:weight semi-bold :inherit font-lock-variable-name-face)))
  "Face for ID links; these would typically be org-roam links.")

(org-link-set-parameters "id" :face '+org-id-link)



;; Make RET follow ID links in the same window.

(define-advice org-id-open (:around (orig-fun id &optional argument) follow-links-same-window)
  (let ((org-link-frame-setup '((file . find-file))))
    (funcall orig-fun id argument)))


;;; Load extra link types provided by features

(require 'ol-man)

(provide 'mod-org-link)

;;; mod-org-link.el ends here
