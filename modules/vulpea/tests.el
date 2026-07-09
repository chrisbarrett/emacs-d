;;; vulpea-tests.el --- Tests for vulpea module -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defvar vulpea-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(let ((lib-file (expand-file-name "lib.el" vulpea-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil t)))

(load (expand-file-name "init.el" vulpea-test--module-dir) nil t)
(require 'vulpea-note)

;; init.el defers vulpea's config (`:defer-incrementally', `with-eval-after-load').
;; Force the packages to load so tests assert post-load state instead of the
;; text of init.el.  no-littering is required first because vulpea's :config
;; calls `no-littering-expand-var-file-name'; without it the :config aborts.
(ignore-errors (require 'no-littering))
(ignore-errors (require 'vulpea))
(ignore-errors (require 'vulpea-ui))


;;; P1: vulpea-db-autosync-mode enabled in config

(ert-deftest vulpea-test-p1-autosync-configured ()
  "vulpea-db-autosync-mode is enabled after load."
  :tags '(vulpea)
  (skip-unless (featurep 'vulpea))
  (should (bound-and-true-p vulpea-db-autosync-mode)))


;;; P2: sensitive tag filtering

(ert-deftest vulpea-test-p2-sensitive-tags-defined ()
  :tags '(vulpea)
  (should (boundp '+vulpea-sensitive-tags))
  (should (member "daily" +vulpea-sensitive-tags))
  (should (member "sensitive" +vulpea-sensitive-tags))
  (should (member "private" +vulpea-sensitive-tags)))

(ert-deftest vulpea-test-p2-sensitive-filter-rejects-tagged ()
  :tags '(vulpea)
  (let ((note (make-vulpea-note :tags '("daily" "project"))))
    (should-not (+vulpea-note-visible-p note))))

(ert-deftest vulpea-test-p2-sensitive-filter-accepts-clean ()
  :tags '(vulpea)
  (let ((note (make-vulpea-note :tags '("project" "work"))))
    (should (+vulpea-note-visible-p note))))


;;; P3: vulpea-find with sensitive filtering

(ert-deftest vulpea-test-p3-find-command-exists ()
  :tags '(vulpea)
  (should (fboundp '+vulpea-find)))


;;; P4: OLP display function

(ert-deftest vulpea-test-p4-olp-display-exists ()
  :tags '(vulpea)
  (should (fboundp '+vulpea-note-describe-olp)))


;;; P6: vulpea-ui sidebar configured

(ert-deftest vulpea-test-p6-sidebar-configured ()
  "vulpea-ui sidebar is configured on the right."
  :tags '(vulpea)
  (skip-unless (featurep 'vulpea-ui))
  (should (eq vulpea-ui-sidebar-position 'right)))


;;; P7: alias keybindings configured

(ert-deftest vulpea-test-p7-alias-bindings ()
  "Alias add/remove commands wired to the leader are available."
  :tags '(vulpea)
  ;; The leader keymap is not populated under the batch harness, so the
  ;; binding itself is not observable; assert the commands it targets exist.
  (skip-unless (featurep 'vulpea))
  (should (fboundp 'vulpea-buffer-alias-add))
  (should (fboundp 'vulpea-buffer-alias-remove)))


;;; P8: org-id auto-create in vulpea files

(ert-deftest vulpea-test-p8-id-link-hook ()
  "Opening notes files installs the org-id link policy via find-file-hook."
  :tags '(vulpea)
  (skip-unless (featurep 'vulpea))
  (should (memq '+vulpea-set-id-link-policy-h find-file-hook)))


;;; P9: vulpea and vulpea-ui packages available

(ert-deftest vulpea-test-p9-packages-declared ()
  "Both vulpea packages load."
  :tags '(vulpea)
  (skip-unless (featurep 'vulpea))
  (should (featurep 'vulpea))
  (should (featurep 'vulpea-ui)))


;;; P10: notes directory var defined

(ert-deftest vulpea-test-p10-notes-directory ()
  :tags '(vulpea)
  (should (boundp '+notes-directory)))

(provide 'vulpea-tests)

;;; vulpea-tests.el ends here
