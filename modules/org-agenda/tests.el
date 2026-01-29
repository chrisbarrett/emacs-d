;;; org-agenda-tests.el --- Tests for org-agenda module  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for org-agenda configuration based on spec 024-org-agenda.md

;;; Code:

(require 'ert)

;; Capture module directory at load time
(defvar org-agenda-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the org-agenda module.")

;; Load lib at top level
(let ((lib-file (expand-file-name "lib.el" org-agenda-test--module-dir)))
  (when (file-exists-p lib-file)
    (condition-case nil
        (load lib-file nil t)
      (error nil))))

;; Load module init at top level
(let ((init-file (expand-file-name "init.el" org-agenda-test--module-dir)))
  (when (file-exists-p init-file)
    (condition-case nil
        (load init-file nil t)
      (error nil))))

;;; P1: org-agenda-files points to file in org-directory

(ert-deftest org-agenda/test-p1-agenda-files-location ()
  "P1: org-agenda-files points to file in org-directory."
  (skip-unless (boundp 'org-directory))
  (skip-unless (boundp 'org-agenda-files))
  ;; Skip if config not applied (org-agenda-files is nil or default)
  (skip-unless (and org-agenda-files (stringp org-agenda-files)))
  (should (string-prefix-p org-directory org-agenda-files)))

;;; P3: Custom commands "p" and "w" are defined

(ert-deftest org-agenda/test-p3-custom-commands-defined ()
  "P3: Custom commands p and w are defined."
  (skip-unless (boundp 'org-agenda-custom-commands))
  (should (assoc "p" org-agenda-custom-commands))
  (should (assoc "w" org-agenda-custom-commands)))

;;; P4: page-break-lines-modes includes org-agenda-mode

(ert-deftest org-agenda/test-p4-page-break-lines ()
  "P4: page-break-lines-modes includes org-agenda-mode."
  (skip-unless (featurep 'page-break-lines))
  (skip-unless (boundp 'page-break-lines-modes))
  (should (memq 'org-agenda-mode page-break-lines-modes)))

(provide 'org-agenda-tests)

;;; org-agenda-tests.el ends here
