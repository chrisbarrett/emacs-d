;;; +bd-issue-tests.el --- Tests for bd issue creation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author: Chris Oates
;; Maintainer: Chris Oates
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; ERT tests for +bd-issue.el
;;
;;; Code:

(require 'ert)
(require '+bd-issue)

;;; Buffer text extraction tests

(ert-deftest +bd-issue-test-get-buffer-text-empty ()
  "Test that empty buffer returns empty string."
  (with-temp-buffer
    (+bd-issue-mode)
    (should (string-empty-p (+bd-issue--get-buffer-text)))))

(ert-deftest +bd-issue-test-get-buffer-text-whitespace-only ()
  "Test that buffer with only whitespace returns empty string."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "   \n\n  \t  \n")
    (should (string-empty-p (+bd-issue--get-buffer-text)))))

(ert-deftest +bd-issue-test-get-buffer-text-simple ()
  "Test extracting simple text without comments."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "This is a bug\nIt needs to be fixed")
    (should (equal (+bd-issue--get-buffer-text)
                   "This is a bug\nIt needs to be fixed"))))

(ert-deftest +bd-issue-test-get-buffer-text-strips-comments ()
  "Test that comment lines (starting with '# ') are removed."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "# This is a comment\n")
    (insert "This is content\n")
    (insert "# Another comment\n")
    (insert "More content")
    (should (equal (+bd-issue--get-buffer-text)
                   "This is content\nMore content"))))

(ert-deftest +bd-issue-test-get-buffer-text-strips-markdown-headers ()
  "Test that single # headers (# with space) are stripped like comments.
Multi-# headers (##, ###) are preserved since they don't match the pattern."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "# Implementation\n")
    (insert "## Details\n")
    (insert "### Subsection\n")
    (insert "Some content")
    (should (equal (+bd-issue--get-buffer-text)
                   "## Details\n### Subsection\nSome content"))))

(ert-deftest +bd-issue-test-get-buffer-text-mixed-content ()
  "Test mixed content with comments.
All lines starting with '# ' are stripped, including what looks like headers."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "# This is a comment (note the space)\n")
    (insert "# This also gets stripped\n")
    (insert "# Another comment\n")
    (insert "Content here\n")
    (insert "More content")
    (should (equal (+bd-issue--get-buffer-text)
                   "Content here\nMore content"))))

(ert-deftest +bd-issue-test-get-buffer-text-template ()
  "Test that default template is fully stripped."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert +bd-issue-template)
    (should (string-empty-p (+bd-issue--get-buffer-text)))))

(ert-deftest +bd-issue-test-get-buffer-text-template-with-content ()
  "Test template with user content added."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert +bd-issue-template)
    (insert "\nFix the login bug\n")
    (insert "Users can't login with special characters in password")
    (should (equal (+bd-issue--get-buffer-text)
                   "Fix the login bug\nUsers can't login with special characters in password"))))

(ert-deftest +bd-issue-test-get-buffer-text-inline-hash ()
  "Test that # in the middle of lines is preserved."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "This references issue #123\n")
    (insert "Also see #456 for details")
    (should (equal (+bd-issue--get-buffer-text)
                   "This references issue #123\nAlso see #456 for details"))))

(ert-deftest +bd-issue-test-get-buffer-text-hash-no-space ()
  "Test that # at start without space is preserved."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "#bug\n")
    (insert "#feature-request")
    (should (equal (+bd-issue--get-buffer-text)
                   "#bug\n#feature-request"))))

;;; Buffer creation and setup tests

(ert-deftest +bd-issue-test-mode-setup ()
  "Test that +bd-issue-mode sets up buffer correctly."
  (with-temp-buffer
    (+bd-issue-mode)
    ;; Check comment syntax
    (should (equal comment-start "#"))
    (should (equal comment-start-skip (rx (one-or-more "#") (zero-or-more space))))
    ;; Check fill column
    (should (= fill-column 72))
    ;; Check auto-fill is enabled
    (should auto-fill-function)
    ;; Check kill-buffer hook is added
    (should (memq #'+bd-issue--kill-buffer-query
                  kill-buffer-query-functions))))

(ert-deftest +bd-issue-test-mode-line ()
  "Test that mode line is set up with keybindings."
  (with-temp-buffer
    (+bd-issue-mode)
    (should mode-line-misc-info)
    (should (assq '+bd-issue-mode mode-line-misc-info))))

;;; Kill buffer protection tests

(ert-deftest +bd-issue-test-kill-buffer-query ()
  "Test that kill-buffer-query prevents accidental kills."
  (with-temp-buffer
    (+bd-issue-mode)
    (should-not (+bd-issue--kill-buffer-query))))

(ert-deftest +bd-issue-test-kill-buffer-query-other-mode ()
  "Test that kill-buffer-query allows kills in other modes."
  (with-temp-buffer
    (text-mode)
    (should (+bd-issue--kill-buffer-query))))

;;; Entry point tests

(ert-deftest +bd-issue-test-create-buffer-created ()
  "Test that +bd-issue-create creates a buffer."
  (let (buf)
    (unwind-protect
        (progn
          (+bd-issue-create default-directory)
          (setq buf (get-buffer "*bd-new-issue*"))
          (should buf)
          (with-current-buffer buf
            (should (eq major-mode '+bd-issue-mode))
            (should +bd-issue--worktree-path)
            ;; Check template is inserted
            (should (string-match-p "Describe the issue"
                                    (buffer-string)))))
      ;; Cleanup
      (when (and buf (buffer-live-p buf))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest +bd-issue-test-create-sets-worktree-path ()
  "Test that +bd-issue-create sets worktree path correctly."
  (let ((test-path "/tmp/test-worktree")
        buf)
    (unwind-protect
        (progn
          (+bd-issue-create test-path)
          (setq buf (get-buffer "*bd-new-issue*"))
          (with-current-buffer buf
            (should (equal +bd-issue--worktree-path test-path))))
      ;; Cleanup
      (when (and buf (buffer-live-p buf))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

;;; Finish/Cancel tests

(ert-deftest +bd-issue-test-finish-empty-buffer-errors ()
  "Test that finishing with empty buffer raises error."
  (let ((buf (generate-new-buffer "*bd-new-issue*")))
    (unwind-protect
        (with-current-buffer buf
          (+bd-issue-mode)
          (setq-local +bd-issue--worktree-path default-directory)
          (should-error (+bd-issue-finish) :type 'user-error))
      ;; Cleanup
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest +bd-issue-test-finish-whitespace-only-errors ()
  "Test that finishing with whitespace-only buffer raises error."
  (let ((buf (generate-new-buffer "*bd-new-issue*")))
    (unwind-protect
        (with-current-buffer buf
          (+bd-issue-mode)
          (setq-local +bd-issue--worktree-path default-directory)
          (insert "   \n\n  ")
          (should-error (+bd-issue-finish) :type 'user-error))
      ;; Cleanup
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest +bd-issue-test-finish-template-only-errors ()
  "Test that finishing with only template text raises error."
  (let ((buf (generate-new-buffer "*bd-new-issue*")))
    (unwind-protect
        (with-current-buffer buf
          (+bd-issue-mode)
          (setq-local +bd-issue--worktree-path default-directory)
          (insert +bd-issue-template)
          (should-error (+bd-issue-finish) :type 'user-error))
      ;; Cleanup
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

;;; Claude instruction tests

(ert-deftest +bd-issue-test-claude-instruction-format ()
  "Test that Claude instruction constant has expected content."
  (should (stringp +bd-issue--claude-instruction))
  (should (string-match-p "beads MCP" +bd-issue--claude-instruction))
  (should (string-match-p "mcp__plugin_beads_beads__create"
                          +bd-issue--claude-instruction))
  (should (string-match-p "Description:" +bd-issue--claude-instruction)))

(provide '+bd-issue-tests)
;;; +bd-issue-tests.el ends here
