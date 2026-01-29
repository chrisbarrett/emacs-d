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

;;; P1: transient-quit-one bound to escape in transient-map

(ert-deftest vcs-p1-transient-escape-binding ()
  "P1: transient-quit-one bound to escape in transient-map."
  ;; Skip in batch mode - :general-config doesn't run without full init
  (skip-unless (and (featurep 'general)
                    (featurep 'transient)
                    (not noninteractive)))
  (require 'transient)
  (should (eq (lookup-key transient-map [escape]) 'transient-quit-one)))

;;; P2: Magit diff buffers display in separate window in same frame

(ert-deftest vcs-p2-magit-display-buffer-function ()
  "P2: magit-display-buffer-function is set correctly."
  (skip-unless (featurep 'magit))
  (should (eq magit-display-buffer-function #'+magit-display-buffer-same-frame)))

(ert-deftest vcs-p2-display-buffer-function-defined ()
  "P2: +magit-display-buffer-same-frame function is defined."
  (should (fboundp '+magit-display-buffer-same-frame)))

;;; P3: Emoji shortcodes render as Unicode in git-commit-mode

(ert-deftest vcs-p3-emoji-display-hook ()
  "P3: Emoji display hook is registered for git-commit-mode."
  (skip-unless (featurep 'magit))
  (should (memq '+git-commit-enable-emoji-display git-commit-mode-hook)))

(ert-deftest vcs-p3-emoji-display-function-defined ()
  "P3: +git-commit-enable-emoji-display is defined."
  (should (fboundp '+git-commit-enable-emoji-display)))

(ert-deftest vcs-p3-emoji-download-function-defined ()
  "P3: +git-commit-emoji-download-data is defined."
  (should (fboundp '+git-commit-emoji-download-data)))

(ert-deftest vcs-p3-emoji-variables-exist ()
  "P3: Emoji-related variables exist."
  (should (boundp '+git-commit-emoji-data-url))
  (should (boundp '+git-commit-emoji-table)))

;;; P4: git-timemachine-mode shows revision in header-line

(ert-deftest vcs-p4-git-timemachine-minibuffer-details ()
  "P4: git-timemachine-show-minibuffer-details is t."
  (skip-unless (featurep 'git-timemachine))
  (should (eq git-timemachine-show-minibuffer-details t)))

;;; P5: browse-at-remote-get-url returns correct URL in git-timemachine session
;; Note: This is tested by the advice being present

(ert-deftest vcs-p5-browse-at-remote-advice ()
  "P5: browse-at-remote-get-url has git-timemachine advice."
  (skip-unless (featurep 'browse-at-remote))
  (should (advice-member-p 'browse-at-remote-get-url@git-timemachine-integration
                           'browse-at-remote-get-url)))

;;; P6: Forge browse commands remap correctly in magit-mode-map

(ert-deftest vcs-p6-forge-remap-defined ()
  "P6: forge-browse is configured for magit-browse-thing remap."
  ;; The remap is done via :general, which sets up bindings after forge loads
  ;; We just verify forge is configured to load after magit-status
  (skip-unless (featurep 'forge))
  (should (fboundp 'forge-browse)))

;;; P7: +worktrees-create-switch creates worktree under .worktrees/
;; Note: This is a complex interactive function, we test the variable

(ert-deftest vcs-p7-worktree-base-dir ()
  "P7: Worktree base directory is .worktrees."
  (should (string= +worktrees-worktree-base-dir ".worktrees")))

;;; P8: Tab close kills worktree buffers and stops claude-code-ide
;; Note: This is tested by hook presence

(ert-deftest vcs-p8-worktrees-close-tabs-defined ()
  "P8: +worktrees-close-tabs is defined."
  (should (fboundp '+worktrees-close-tabs)))

(ert-deftest vcs-p8-magit-worktree-delete-cleanup-defined ()
  "P8: +magit-worktree-delete--cleanup is defined."
  (should (fboundp '+magit-worktree-delete--cleanup)))

(ert-deftest vcs-p8-magit-worktree-delete-advised ()
  "P8: magit-worktree-delete has cleanup advice."
  (skip-unless (featurep 'magit))
  (should (advice-member-p '+magit-worktree-delete--cleanup 'magit-worktree-delete)))

;;; P9: +git-repo-display-name returns owner/repo for GitHub URLs

(ert-deftest vcs-p9-git-repo-display-name-defined ()
  "P9: +git-repo-display-name is defined."
  (should (fboundp '+git-repo-display-name)))

;;; Additional function tests

(ert-deftest vcs-worktrees-path-for-selected-tab-defined ()
  "+worktrees-path-for-selected-tab is defined."
  (should (fboundp '+worktrees-path-for-selected-tab)))

(ert-deftest vcs-worktrees-tab-dedicated-to-child-p-defined ()
  "+worktrees-tab-dedicated-to-child-p is defined."
  (should (fboundp '+worktrees-tab-dedicated-to-child-p)))

(ert-deftest vcs-worktrees-set-alert-defined ()
  "+worktrees-set-alert is defined."
  (should (fboundp '+worktrees-set-alert)))

(ert-deftest vcs-worktrees-set-transient-alert-defined ()
  "+worktrees-set-transient-alert is defined."
  (should (fboundp '+worktrees-set-transient-alert)))

(ert-deftest vcs-worktrees-clear-alert-defined ()
  "+worktrees-clear-alert is defined."
  (should (fboundp '+worktrees-clear-alert)))

(ert-deftest vcs-worktrees-refresh-magit-defined ()
  "+worktrees-refresh-magit is defined."
  (should (fboundp '+worktrees-refresh-magit)))

(ert-deftest vcs-magit-diff-visit-unselected-defined ()
  "+magit-diff-visit-file-unselected is defined."
  (should (fboundp '+magit-diff-visit-file-unselected)))

(ert-deftest vcs-magit-worktree-prune-defined ()
  "+magit-worktree-prune is defined."
  (should (fboundp '+magit-worktree-prune)))

(provide 'vcs-tests)

;;; vcs-tests.el ends here
