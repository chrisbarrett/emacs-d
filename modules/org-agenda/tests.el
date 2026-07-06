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

;;; P5: outline-path echo caches per-line

(defmacro org-agenda-test--with-olp-fixture (marker-var &rest body)
  "Bind MARKER-VAR to an org heading marker and run BODY in a fake agenda.
Provides live `src'/`agenda' buffers; the agenda holds one item line
whose `org-marker' points at a level-3 heading (outline path
\"Alpha/Beta\"), is the selected window's buffer, and has
`org-agenda-show-outline-path' enabled.  Buffers are killed afterwards."
  (declare (indent 1))
  `(let ((src (get-buffer-create " *olp-src*"))
         (agenda (get-buffer-create " *olp-agenda*"))
         ,marker-var)
     (unwind-protect
         (progn
           (with-current-buffer src
             (org-mode)
             (insert "* Alpha\n** Beta\n*** Gamma\n")
             (goto-char (point-min))
             (re-search-forward "Gamma")
             (setq ,marker-var (copy-marker (line-beginning-position))))
           (set-window-buffer (selected-window) agenda)
           (with-current-buffer agenda
             (insert "  cat: TODO Gamma\n")
             (goto-char (point-min))
             (put-text-property (line-beginning-position) (line-end-position)
                                'org-marker ,marker-var)
             (setq-local org-agenda-show-outline-path t)
             (setq-local org-agenda-follow-mode nil)
             ,@body))
       (when (timerp +org-agenda-olp-timer) (cancel-timer +org-agenda-olp-timer))
       (when (buffer-live-p src) (kill-buffer src))
       (when (buffer-live-p agenda) (kill-buffer agenda)))))

(ert-deftest org-agenda/test-p5-context-action-caches-outline-path ()
  "`+org-agenda--show-olp' computes the outline path once per agenda line
and reuses it, instead of re-walking the source heading tree each time."
  (skip-unless (fboundp '+org-agenda--show-olp))
  (let ((calls 0))
    (org-agenda-test--with-olp-fixture m
      (cl-letf* ((orig (symbol-function 'org-display-outline-path))
                 ((symbol-function 'org-display-outline-path)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        ;; First visit: computes and caches.
        (+org-agenda--show-olp agenda)
        (should (get-text-property (line-beginning-position) '+org-agenda-olp))
        (should (= calls 1))
        ;; Second visit: cache hit, no recompute.
        (+org-agenda--show-olp agenda)
        (should (= calls 1))))))

;;; P6: context action debounces the outline-path echo

(ert-deftest org-agenda/test-p6-context-action-debounces ()
  "`+org-agenda-do-context-action-cached' defers the echo to an idle
timer: rapid calls emit nothing synchronously and coalesce into a single
pending timer (each reschedule cancels the previous)."
  (skip-unless (fboundp '+org-agenda-do-context-action-cached))
  (setq +org-agenda-olp-timer nil)
  (let ((calls 0))
    (org-agenda-test--with-olp-fixture m
      (cl-letf* ((orig (symbol-function 'org-display-outline-path))
                 ((symbol-function 'org-display-outline-path)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        (+org-agenda-do-context-action-cached)
        (+org-agenda-do-context-action-cached)
        (+org-agenda-do-context-action-cached)
        ;; Nothing computed or echoed synchronously.
        (should (= calls 0))
        ;; Exactly one pending debounce timer, not three.
        (should (timerp +org-agenda-olp-timer))
        (should (= 1 (cl-count-if
                      (lambda (tm)
                        (eq (timer--function tm) '+org-agenda--show-olp))
                      timer-idle-list)))))))

(provide 'org-agenda-tests)

;;; org-agenda-tests.el ends here
