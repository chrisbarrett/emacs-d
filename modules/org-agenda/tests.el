;;; org-agenda-tests.el --- Tests for org-agenda module  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for org-agenda configuration based on spec 024-org-agenda.md

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Capture module directory at load time
(defvar org-agenda-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the org-agenda module.")

(let ((lib-file (expand-file-name "lib.el" org-agenda-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil t)))

(let ((init-file (expand-file-name "init.el" org-agenda-test--module-dir)))
  (when (file-exists-p init-file)
    (load init-file nil t)))

;; Force deferred config to run.
(require 'org-agenda)
(require 'page-break-lines)

;;; P1: org-agenda-files points to file in org-directory

(ert-deftest org-agenda/test-p1-agenda-files-location ()
  "P1: org-agenda-files points to file in org-directory."
  (should (and (stringp org-agenda-files)
               (string-prefix-p org-directory org-agenda-files))))

;;; P3: Custom commands "p" and "w" are defined

(ert-deftest org-agenda/test-p3-custom-commands-defined ()
  "P3: Custom commands p and w are defined."
  (should (assoc "p" org-agenda-custom-commands))
  (should (assoc "w" org-agenda-custom-commands)))

;;; P4: page-break-lines-modes includes org-agenda-mode

(ert-deftest org-agenda/test-p4-page-break-lines ()
  "P4: page-break-lines-modes includes org-agenda-mode."
  (should (memq 'org-agenda-mode page-break-lines-modes)))

;;; P5: outline-path context action caches per-line

(ert-deftest org-agenda/test-p5-context-action-caches-outline-path ()
  "`+org-agenda-do-context-action-cached' computes the outline path once
per agenda line and reuses it, instead of re-walking the source heading
tree on every cursor move."
  (skip-unless (fboundp '+org-agenda-do-context-action-cached))
  (let ((src (get-buffer-create " *olp-src*"))
        (agenda (get-buffer-create " *olp-agenda*"))
        (calls 0))
    (unwind-protect
        (let (m)
          (with-current-buffer src
            (org-mode)
            (insert "* Alpha\n** Beta\n*** Gamma\n")
            (goto-char (point-min))
            (re-search-forward "Gamma")
            (setq m (copy-marker (line-beginning-position))))
          (with-current-buffer agenda
            (insert "  cat: TODO Gamma\n")
            (goto-char (point-min))
            (put-text-property (line-beginning-position) (line-end-position)
                               'org-marker m)
            (setq-local org-agenda-show-outline-path t)
            (setq-local org-agenda-follow-mode nil)
            (cl-letf* ((orig (symbol-function 'org-display-outline-path))
                       ((symbol-function 'org-display-outline-path)
                        (lambda (&rest args)
                          (setq calls (1+ calls))
                          (apply orig args))))
              ;; First visit: computes and caches.
              (+org-agenda-do-context-action-cached)
              (should (get-text-property (line-beginning-position) '+org-agenda-olp))
              (should (= calls 1))
              ;; Second visit: cache hit, no recompute.
              (+org-agenda-do-context-action-cached)
              (should (= calls 1)))))
      (when (buffer-live-p src) (kill-buffer src))
      (when (buffer-live-p agenda) (kill-buffer agenda)))))

(provide 'org-agenda-tests)

;;; org-agenda-tests.el ends here
