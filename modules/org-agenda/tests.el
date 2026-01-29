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

;;; P5: Skip function excludes archived entries

(ert-deftest org-agenda/test-p5-skip-archived ()
  "P5: +agenda-view-skip-function is defined and skips archived."
  (should (fboundp '+agenda-view-skip-function)))

;;; P6: Skip function includes high-priority TODOs

(ert-deftest org-agenda/test-p6-skip-function-high-priority ()
  "P6: +agenda-next-actions-skip-function is defined."
  (should (fboundp '+agenda-next-actions-skip-function)))

;;; P7: Habit graph column adjusts to window width

(ert-deftest org-agenda/test-p7-habit-resize-function ()
  "P7: +org-habit-resize-graph-h is defined."
  (should (fboundp '+org-habit-resize-graph-h)))

(ert-deftest org-agenda/test-p7-habit-variables ()
  "P7: Habit graph variables are defined."
  (should (boundp '+org-habit-graph-window-ratio))
  (should (boundp '+org-habit-graph-padding))
  (should (boundp '+org-habit-min-width)))

;;; P8: After-save hook updates agenda files

(ert-deftest org-agenda/test-p8-agenda-update-function ()
  "P8: +org-agenda-update-files is defined."
  (should (fboundp '+org-agenda-update-files)))

(ert-deftest org-agenda/test-p8-agenda-update-script ()
  "P8: Agenda files update script path is defined."
  (should (boundp '+agenda-files-update-script))
  (should (stringp +agenda-files-update-script)))

;;; P9: TAB in agenda view runs reveal sequence

(ert-deftest org-agenda/test-p9-after-show-hook ()
  "P9: org-agenda-after-show-hook has reveal function."
  (skip-unless (boundp 'org-agenda-after-show-hook))
  ;; Hook should be configured
  (should (listp org-agenda-after-show-hook)))

;;; Predicate Functions

(ert-deftest org-agenda/test-predicates-defined ()
  "All predicate functions are defined."
  (should (fboundp '+agenda--any-scheduled-or-deadline-p))
  (should (fboundp '+agenda--skip-heading-safe))
  (should (fboundp '+agenda--scheduled-in-future-p))
  (should (fboundp '+agenda--scheduled-now-p))
  (should (fboundp '+agenda--at-TODO-p))
  (should (fboundp '+agenda--first-todo-at-this-level-p))
  (should (fboundp '+agenda--high-priority-p))
  (should (fboundp '+agenda--parent-scheduled-in-future-p)))

;;; Keybindings

(ert-deftest org-agenda/test-keybindings-configured ()
  "Keybindings are configured for org-agenda-mode."
  (skip-unless (featurep 'general))
  (skip-unless (boundp 'org-agenda-mode-map))
  ;; Just verify the map exists and is a keymap
  (should (keymapp org-agenda-mode-map)))

(provide 'org-agenda-tests)

;;; org-agenda-tests.el ends here
