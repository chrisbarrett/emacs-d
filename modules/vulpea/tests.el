;;; vulpea-tests.el --- Tests for vulpea module -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defvar vulpea-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(let ((lib-file (expand-file-name "lib.el" vulpea-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil t)))

(condition-case nil
    (load (expand-file-name "init.el" vulpea-test--module-dir) nil t)
  (error nil))


;;; P1: vulpea-db-autosync-mode enabled in config

(ert-deftest vulpea-test-p1-autosync-configured ()
  :tags '(vulpea)
  (let ((init-file (expand-file-name "init.el" vulpea-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "vulpea-db-autosync-mode" nil t)))))


;;; P2: sensitive tag filtering

(ert-deftest vulpea-test-p2-sensitive-tags-defined ()
  :tags '(vulpea)
  (should (boundp '+vulpea-sensitive-tags))
  (should (member "daily" +vulpea-sensitive-tags))
  (should (member "sensitive" +vulpea-sensitive-tags))
  (should (member "private" +vulpea-sensitive-tags)))

(ert-deftest vulpea-test-p2-sensitive-filter-rejects-tagged ()
  :tags '(vulpea)
  (skip-unless (featurep 'vulpea-note))
  (let ((note (make-vulpea-note :tags '("daily" "project"))))
    (should-not (+vulpea-note-visible-p note))))

(ert-deftest vulpea-test-p2-sensitive-filter-accepts-clean ()
  :tags '(vulpea)
  (skip-unless (featurep 'vulpea-note))
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
  :tags '(vulpea)
  (let ((init-file (expand-file-name "init.el" vulpea-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "vulpea-ui" nil t)))))


;;; P7: alias keybindings configured

(ert-deftest vulpea-test-p7-alias-bindings ()
  :tags '(vulpea)
  (let ((init-file (expand-file-name "init.el" vulpea-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "vulpea-buffer-alias-add" nil t))
      (should (search-forward "vulpea-buffer-alias-remove" nil t)))))


;;; P8: org-id auto-create in vulpea files

(ert-deftest vulpea-test-p8-id-link-hook ()
  :tags '(vulpea)
  (let ((init-file (expand-file-name "init.el" vulpea-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "org-id-link-to-org-use-id" nil t))
      (goto-char (point-min))
      (should (search-forward "find-file-hook" nil t)))))


;;; P9: packages.eld declares vulpea and vulpea-ui

(ert-deftest vulpea-test-p9-packages-declared ()
  :tags '(vulpea)
  (let* ((pkg-file (expand-file-name "packages.eld" vulpea-test--module-dir))
         (content (with-temp-buffer
                    (insert-file-contents pkg-file)
                    (buffer-string))))
    (should (string-match-p "vulpea" content))
    (should (string-match-p "vulpea-ui" content))))


;;; P10: notes directory var defined

(ert-deftest vulpea-test-p10-notes-directory ()
  :tags '(vulpea)
  (should (boundp '+notes-directory)))

(provide 'vulpea-tests)

;;; vulpea-tests.el ends here
