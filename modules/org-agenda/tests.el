;;; org-agenda-tests.el --- Tests for org-agenda module  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for org-agenda configuration based on spec 024-org-agenda.md

;;; Code:

(require 'ert)

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

(provide 'org-agenda-tests)

;;; org-agenda-tests.el ends here
