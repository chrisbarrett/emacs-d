;;; lib.el --- Spellcheck utility functions.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides spellcheck configuration and dictionary management.
;; Uses aspell backend with en_AU dictionary and spell-fu for
;; lightweight on-the-fly checking.

;;; Code:

(defvar +spellcheck-dictionaries '("en_AU" "fr")
  "List of dictionaries to load for spell checking.")

(defvar +spellcheck-org-excluded-faces
  '(org-meta-line org-link org-code org-block
    org-block-begin-line org-block-end-line
    org-footnote org-tag org-modern-tag org-verbatim)
  "Faces to exclude from spell checking in org-mode.")

;;;###autoload
(defun +spellcheck-add-dictionaries ()
  "Add configured dictionaries to spell-fu.
Called from `spell-fu-mode-hook' to load dictionaries."
  (require 'spell-fu)
  (dolist (dict +spellcheck-dictionaries)
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary dict))))

;;;###autoload
(defun +spellcheck-setup-org ()
  "Configure spell-fu face exclusions for org-mode."
  (setq-local spell-fu-faces-exclude +spellcheck-org-excluded-faces))

(provide 'spellcheck-lib)

;;; lib.el ends here
