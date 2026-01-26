;;; init.el --- Diff module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures diff viewing and interactive merge tools.
;; Uses built-in diff and ediff packages.

;;; Code:

;; Diff: Unix diff file support.
(with-eval-after-load 'diff-mode
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-font-lock-prettify t)
  (setq diff-font-lock-syntax 'hunk-also))

;; Ediff: Interactive file diff & merge UI.
(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-show-clashes-only t))

;; Org integration: reveal org headings when navigating ediff hunks.
(defun +ad-ediff-reveal-org-content-around-hunk (&rest _)
  "Reveal org heading around current hunk in ediff buffers."
  (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (derived-mode-p 'org-mode)
          (org-reveal t))))))

(with-eval-after-load 'ediff
  (advice-add 'ediff-next-difference :after #'+ad-ediff-reveal-org-content-around-hunk)
  (advice-add 'ediff-previous-difference :after #'+ad-ediff-reveal-org-content-around-hunk))

(provide 'diff-init)

;;; init.el ends here
