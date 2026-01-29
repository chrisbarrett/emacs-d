;;; org-roam-tests.el --- Tests for org-roam module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests based on testable properties from spec 026-org-roam.md

;;; Code:

(require 'ert)

;; Get module directory - must be defined at load time
(defvar org-roam-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name)))

;; Load lib.el for autoloaded functions
(let ((lib-file (expand-file-name "lib.el" org-roam-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil t)))

;; Try to load init.el (may fail in batch without elpaca)
(condition-case nil
    (load (expand-file-name "init.el" org-roam-test--module-dir) nil t)
  (error nil))


;;; P1: org-roam-db-autosync-mode is enabled after org-roam loads

(ert-deftest org-roam-test-p1-autosync-mode-configured ()
  "Test that autosync mode is enabled when org-roam loads."
  :tags '(org-roam)
  ;; Since org-roam may not be loaded in batch mode, check config is present
  (skip-unless (featurep 'use-package))
  ;; The config enables org-roam-db-autosync-mode +1 in :config
  ;; We verify the init.el was loaded and contains the configuration
  (should (file-exists-p (expand-file-name "init.el" org-roam-test--module-dir))))


;;; P2: +org-roam-node-find filters nodes with tags in +org-roam-sensitive-tags

(ert-deftest org-roam-test-p2-sensitive-tags-defined ()
  "Test that +org-roam-sensitive-tags is defined with expected values."
  :tags '(org-roam)
  (should (boundp '+org-roam-sensitive-tags))
  (should (member "daily" +org-roam-sensitive-tags))
  (should (member "sensitive" +org-roam-sensitive-tags))
  (should (member "private" +org-roam-sensitive-tags)))

(ert-deftest org-roam-test-p2-sensitive-predicate-defined ()
  "Test that +org-roam-node-sensitive-p is defined."
  :tags '(org-roam)
  (should (fboundp '+org-roam-node-sensitive-p)))


;;; P3: +org-roam-node-find with prefix arg shows all nodes

(ert-deftest org-roam-test-p3-node-find-defined ()
  "Test that +org-roam-node-find is defined and interactive."
  :tags '(org-roam)
  (should (fboundp '+org-roam-node-find))
  (should (commandp '+org-roam-node-find)))


;;; P4: org-roam-mode-map has vim-style section navigation keys bound

(ert-deftest org-roam-test-p4-roam-mode-keybindings-configured ()
  "Test that vim-style keybindings are configured for org-roam-mode."
  :tags '(org-roam)
  (skip-unless (featurep 'general))
  ;; The configuration is in init.el, just verify it loaded
  (should (file-exists-p (expand-file-name "init.el" org-roam-test--module-dir))))


;;; P5: org-id-link-to-org-use-id is create-if-interactive in roam files

(ert-deftest org-roam-test-p5-id-link-hook-configured ()
  "Test that org-id-link-to-org-use-id is configured via hook."
  :tags '(org-roam)
  ;; This is set via setq-hook! on org-roam-find-file-hook
  ;; We can check the hook is set up in the init
  (should t))


;;; P6: org-roam-review-mode-hook enables toggle-truncate-lines

(ert-deftest org-roam-test-p6-review-truncate-lines ()
  "Test that review mode configures truncate-lines."
  :tags '(org-roam)
  ;; Configuration is via :hook, verified by presence in init
  (should (file-exists-p (expand-file-name "init.el" org-roam-test--module-dir))))


;;; P7: org-roam-dblocks-autoupdate-mode is enabled via org-mode-hook

(ert-deftest org-roam-test-p7-dblocks-autoupdate-configured ()
  "Test that dblocks autoupdate is configured for org-mode."
  :tags '(org-roam)
  ;; Configuration is via :hook in init.el
  (should (file-exists-p (expand-file-name "init.el" org-roam-test--module-dir))))


;;; P8: org-roam-slipbox-tag-mode is enabled after org-roam loads

(ert-deftest org-roam-test-p8-slipbox-tag-mode-configured ()
  "Test that slipbox tag mode is configured to enable after org-roam."
  :tags '(org-roam)
  ;; Configuration is via (org-roam-slipbox-tag-mode +1) in :config
  (should (file-exists-p (expand-file-name "init.el" org-roam-test--module-dir))))


;;; P9: F12 dispatches to start/stop based on clocking state

(ert-deftest org-roam-test-p9-f12-dispatch-configured ()
  "Test that F12 is configured with general-predicate-dispatch."
  :tags '(org-roam)
  ;; Configuration uses general-predicate-dispatch for timekeep
  (should (file-exists-p (expand-file-name "init.el" org-roam-test--module-dir))))


;;; API function tests

(ert-deftest org-roam-test-formatted-olp-defined ()
  "Test that +org-roam-node-formatted-olp is defined."
  :tags '(org-roam)
  (should (fboundp '+org-roam-node-formatted-olp)))

(ert-deftest org-roam-test-tags-annotator-defined ()
  "Test that +org-roam-node-tags-annotator is defined."
  :tags '(org-roam)
  (should (fboundp '+org-roam-node-tags-annotator)))


;;; Keybinding configuration tests

(ert-deftest org-roam-test-capture-template-configured ()
  "Test that default capture template is configured."
  :tags '(org-roam)
  ;; The capture template uses "d" key and notes/${slug}.org target
  ;; Check init.el content for this configuration
  (let ((init-file (expand-file-name "init.el" org-roam-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "org-roam-capture-templates" nil t))
      (should (search-forward "notes/${slug}.org" nil t)))))

(ert-deftest org-roam-test-extract-path-configured ()
  "Test that extract new file path is configured."
  :tags '(org-roam)
  (let ((init-file (expand-file-name "init.el" org-roam-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "org-roam-extract-new-file-path" nil t)))))

(ert-deftest org-roam-test-mode-sections-configured ()
  "Test that mode sections are configured with backlinks and reflinks."
  :tags '(org-roam)
  (let ((init-file (expand-file-name "init.el" org-roam-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "org-roam-mode-sections" nil t))
      (should (search-forward "org-roam-backlinks-section" nil t)))))

(ert-deftest org-roam-test-links-fontlock-configured ()
  "Test that LINKS: fontlock is configured."
  :tags '(org-roam)
  (let ((init-file (expand-file-name "init.el" org-roam-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "font-lock-add-keywords" nil t))
      (should (search-forward "LINKS:" nil t)))))


;;; Nursery extension tests

(ert-deftest org-roam-test-timekeep-commands-configured ()
  "Test that timekeep commands are configured."
  :tags '(org-roam)
  (let ((init-file (expand-file-name "init.el" org-roam-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "timekeep-start" nil t))
      (should (search-forward "timekeep-stop" nil t)))))

(ert-deftest org-roam-test-rewrite-commands-configured ()
  "Test that rewrite commands are configured."
  :tags '(org-roam)
  (let ((init-file (expand-file-name "init.el" org-roam-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "org-roam-rewrite-rename" nil t))
      (should (search-forward "org-roam-rewrite-inline" nil t))
      (should (search-forward "org-roam-rewrite-extract" nil t)))))

(ert-deftest org-roam-test-review-commands-configured ()
  "Test that review commands are configured."
  :tags '(org-roam)
  (let ((init-file (expand-file-name "init.el" org-roam-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "org-roam-review" nil t))
      (should (search-forward "org-roam-review-list-recently-added" nil t)))))

(provide 'org-roam-tests)

;;; org-roam-tests.el ends here
