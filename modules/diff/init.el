;;; init.el --- Diff module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures diff viewing and interactive merge tools.
;; Uses built-in diff and ediff packages.

;;; Code:

(require '+autoloads)

;; Diff: Unix diff file support.

(use-package diff-mode
  :custom
  (diff-default-read-only t)
  (diff-advance-after-apply-hunk t)
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also))

;; Ediff: Interactive file diff & merge UI.

(use-package ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-show-clashes-only t))


;; Org integration: reveal org headings when navigating ediff hunks.

(use-package org
  :after ediff
  :autoload org-reveal
  :preface
  (defun +ad-ediff-reveal-org-content-around-hunk (&rest _)
    "Reveal org heading around current hunk in ediff buffers."
    (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (when (derived-mode-p 'org-mode)
            (org-reveal t))))))
  :config
  (advice-add 'ediff-next-difference :after #'+ad-ediff-reveal-org-content-around-hunk)
  (advice-add 'ediff-previous-difference :after #'+ad-ediff-reveal-org-content-around-hunk))

