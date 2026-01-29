;;; vcs-tests.el --- Tests for VCS module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vcs module based on spec 022-vcs.md

;;; Code:

(require 'ert)

(defconst vcs-test--module-dir
  (expand-file-name "modules/vcs/" user-emacs-directory)
  "Directory containing the VCS module.")

(defun vcs-test--load-module ()
  "Load the VCS module."
  (let ((lib-file (expand-file-name "lib.el" vcs-test--module-dir))
        (init-file (expand-file-name "init.el" vcs-test--module-dir)))
    (when (file-exists-p lib-file)
      (condition-case nil
          (load lib-file nil t)
        (error nil)))
    (when (file-exists-p init-file)
      (condition-case nil
          (load init-file nil t)
        (error nil)))))

;; Load module at test file load time
(vcs-test--load-module)

;;; P2: Magit diff buffers display in separate window in same frame

(ert-deftest vcs-p2-magit-display-buffer-function ()
  "P2: magit-display-buffer-function is set correctly."
  (skip-unless (featurep 'magit))
  (should (eq magit-display-buffer-function #'+magit-display-buffer-same-frame)))

;;; P3: Emoji shortcodes render as Unicode in git-commit-mode

(ert-deftest vcs-p3-emoji-display-hook ()
  "P3: Emoji display hook is registered for git-commit-mode."
  (skip-unless (featurep 'magit))
  (should (memq '+git-commit-enable-emoji-display git-commit-mode-hook)))

;;; P5: browse-at-remote-get-url returns correct URL in git-timemachine session
;; Note: This is tested by the advice being present

(ert-deftest vcs-p5-browse-at-remote-advice ()
  "P5: browse-at-remote-get-url has git-timemachine advice."
  (skip-unless (featurep 'browse-at-remote))
  (should (advice-member-p 'browse-at-remote-get-url@git-timemachine-integration
                           'browse-at-remote-get-url)))

;;; P8: magit-worktree-delete has cleanup advice

(ert-deftest vcs-p8-magit-worktree-delete-advised ()
  "P8: magit-worktree-delete has cleanup advice."
  (skip-unless (featurep 'magit))
  (should (advice-member-p '+magit-worktree-delete--cleanup 'magit-worktree-delete)))

(provide 'vcs-tests)

;;; vcs-tests.el ends here
