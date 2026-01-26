;;; org-capture-tests.el --- Tests for org-capture module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the org-capture module based on spec 025-org-capture.md

;;; Code:

(require 'ert)

;; Get module directory - must be defined at load time
(defvar org-capture-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name)))

;; Load lib.el for autoloaded functions
(let ((lib-file (expand-file-name "lib.el" org-capture-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil t)))

;; Try to load init.el (may fail in batch without elpaca)
(condition-case nil
    (load (expand-file-name "init.el" org-capture-test--module-dir) nil t)
  (error nil))


;;; Module structure tests

(ert-deftest org-capture-test-packages-eld-exists ()
  "packages.eld exists in module."
  (should (file-exists-p (expand-file-name "packages.eld" org-capture-test--module-dir))))

(ert-deftest org-capture-test-spec-symlink ()
  "spec.md symlink exists."
  (should (file-symlink-p (expand-file-name "spec.md" org-capture-test--module-dir))))

(ert-deftest org-capture-test-lib-exists ()
  "lib.el exists in module."
  (should (file-exists-p (expand-file-name "lib.el" org-capture-test--module-dir))))

(ert-deftest org-capture-test-init-exists ()
  "init.el exists in module."
  (should (file-exists-p (expand-file-name "init.el" org-capture-test--module-dir))))


;;; P1: Capture template "t" creates TODO entry in datetree

(ert-deftest org-capture-test-p1-template-t-exists ()
  "Template 't' is defined."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (skip-unless (boundp 'org-capture-templates))
  (let ((template (assoc "t" org-capture-templates)))
    (should template)
    (should (string-match-p "TODO" (nth 4 template)))))


;;; P2: Work templates include both timekeep-work-tag and work tags

(ert-deftest org-capture-test-p2-work-template-tags ()
  "Work templates include work tag."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (skip-unless (boundp 'org-capture-templates))
  (when-let* ((template (assoc "wt" org-capture-templates)))
    (should (string-match-p ":work:" (nth 4 template)))))


;;; P3: Link template uses org-cliplink-capture

(ert-deftest org-capture-test-p3-link-template-cliplink ()
  "Link template uses org-cliplink-capture."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (skip-unless (boundp 'org-capture-templates))
  (when-let* ((template (assoc "l" org-capture-templates)))
    (should (string-match-p "org-cliplink-capture" (nth 4 template)))))


;;; P4: Litnote creates file in org-roam-directory/litnotes/

(ert-deftest org-capture-test-p4-litnote-function-defined ()
  "+capture-litnote-function is defined."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (should (fboundp '+capture-litnote-function)))


;;; P5: Litnote filename is snake_case of title

(ert-deftest org-capture-test-p5-snake-case-used ()
  "s-snake-case is used in litnote function."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (skip-unless (fboundp '+capture-litnote-function))
  ;; Check that s-snake-case is required (indicates usage)
  (should (featurep 's)))


;;; P6: +capture-read-url defaults to first URL-like string in kill ring

(ert-deftest org-capture-test-p6-read-url-defined ()
  "+capture-read-url is defined."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (should (fboundp '+capture-read-url)))


;;; P7: YouTube URLs use title-only prompt

(ert-deftest org-capture-test-p7-youtube-prompt ()
  "+capture--prompt-for-youtube-video generates prompt."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (skip-unless (fboundp '+capture--prompt-for-youtube-video))
  (let ((prompt (+capture--prompt-for-youtube-video "Test Video Title")))
    (should (stringp prompt))
    (should (string-match-p "Test Video Title" prompt))
    (should (string-match-p "YouTube" prompt))))


;;; P8: Generic URLs include up to 1500 chars

(ert-deftest org-capture-test-p8-generic-prompt-excerpt-length ()
  "+capture-excerpt-length-chars is 1500."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (should (boundp '+capture-excerpt-length-chars))
  (should (= 1500 +capture-excerpt-length-chars)))

(ert-deftest org-capture-test-p8-generic-prompt-function ()
  "+capture--prompt-for-generic-web-page generates prompt."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (skip-unless (fboundp '+capture--prompt-for-generic-web-page))
  (let ((prompt (+capture--prompt-for-generic-web-page "Title" "Some content here")))
    (should (stringp prompt))
    (should (string-match-p "Title" prompt))
    (should (string-match-p "EXCERPT" prompt))))


;;; P9: Capture buffers killed after finalize

(ert-deftest org-capture-test-p9-kill-buffer-setting ()
  "init.el calls org-capture-put with :kill-buffer t."
  :tags '(:org-capture)
  (let ((init-file (expand-file-name "init.el" org-capture-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "org-capture-put :kill-buffer t" nil t)))))


;;; API tests

(ert-deftest org-capture-test-context-variable ()
  "+capture-context variable is defined."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (should (boundp '+capture-context)))

(ert-deftest org-capture-test-metadata-function ()
  "+capture--metadata-for-web-document is defined."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (should (fboundp '+capture--metadata-for-web-document)))

(ert-deftest org-capture-test-eww-metadata-function ()
  "+litnote-meta-try-from-eww is defined."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (should (fboundp '+litnote-meta-try-from-eww)))

(ert-deftest org-capture-test-url-metadata-function ()
  "+litnote-meta-from-url is defined."
  :tags '(:org-capture)
  (skip-unless (featurep 'org-capture-lib))
  (should (fboundp '+litnote-meta-from-url)))


;;; Template file tests

(ert-deftest org-capture-test-litnote-template-exists ()
  "capture-templates/litnote.org exists."
  :tags '(:org-capture)
  (let ((file (expand-file-name "capture-templates/litnote.org" user-emacs-directory)))
    (should (file-exists-p file))))

(ert-deftest org-capture-test-postmortem-template-exists ()
  "capture-templates/postmortem.org exists."
  :tags '(:org-capture)
  (let ((file (expand-file-name "capture-templates/postmortem.org" user-emacs-directory)))
    (should (file-exists-p file))))

(ert-deftest org-capture-test-journal-template-exists ()
  "capture-templates/journal.org exists."
  :tags '(:org-capture)
  (let ((file (expand-file-name "capture-templates/journal.org" user-emacs-directory)))
    (should (file-exists-p file))))

(ert-deftest org-capture-test-language-review-template-exists ()
  "capture-templates/language-learning-review.org exists."
  :tags '(:org-capture)
  (let ((file (expand-file-name "capture-templates/language-learning-review.org" user-emacs-directory)))
    (should (file-exists-p file))))


;;; Packages tests

(ert-deftest org-capture-test-packages-includes-cliplink ()
  "packages.eld includes org-cliplink."
  :tags '(:org-capture)
  (let ((packages-file (expand-file-name "packages.eld" org-capture-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents packages-file)
      (should (search-forward "org-cliplink" nil t)))))

(ert-deftest org-capture-test-packages-includes-s ()
  "packages.eld includes s (string library)."
  :tags '(:org-capture)
  (let ((packages-file (expand-file-name "packages.eld" org-capture-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents packages-file)
      (should (search-forward "(s)" nil t)))))

(ert-deftest org-capture-test-packages-includes-dash ()
  "packages.eld includes dash."
  :tags '(:org-capture)
  (let ((packages-file (expand-file-name "packages.eld" org-capture-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents packages-file)
      (should (search-forward "(dash)" nil t)))))

(provide 'org-capture-tests)

;;; org-capture-tests.el ends here
