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

;;; Font-locking tests

(ert-deftest +bd-issue-test-font-locking-applied ()
  "Test that font-locking is configured correctly in +bd-issue-mode."
  (with-temp-buffer
    (+bd-issue-mode)
    ;; Insert some content with comments
    (insert "# This is a comment\n")
    (insert "This is not a comment\n")
    (insert "# Another comment")

    ;; Font-lock defaults should be configured
    (should font-lock-defaults)

    ;; Verify the font-lock keywords are set up correctly
    (should (equal (car font-lock-defaults)
                   '(("^#.*$" . font-lock-comment-face))))

    ;; Ensure font-lock is applied
    (font-lock-ensure)

    ;; Check that the comment line has the correct face
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face)
                'font-lock-comment-face))

    ;; Check that non-comment line doesn't have comment face
    (goto-char (point-min))
    (forward-line 1)
    (should-not (eq (get-text-property (point) 'face)
                    'font-lock-comment-face))))

;;; Entry point tests

(ert-deftest +bd-issue-test-create-buffer-created ()
  "Test that +bd-issue-create creates a buffer."
  (let ((+bd-issue-buffer-name "*bd-test-issue*")
        buf)
    (unwind-protect
        (progn
          (+bd-issue-create default-directory)
          (setq buf (get-buffer +bd-issue-buffer-name))
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
  (let ((+bd-issue-buffer-name "*bd-test-issue*")
        (test-path "/tmp/test-worktree")
        buf)
    (unwind-protect
        (progn
          (+bd-issue-create test-path)
          (setq buf (get-buffer +bd-issue-buffer-name))
          (with-current-buffer buf
            (should (equal +bd-issue--worktree-path test-path))))
      ;; Cleanup
      (when (and buf (buffer-live-p buf))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

;;; Finish/Cancel tests

(ert-deftest +bd-issue-test-finish-empty-buffer-errors ()
  "Test that finishing with empty buffer raises error."
  (let ((+bd-issue-buffer-name "*bd-test-issue*"))
    (let ((buf (generate-new-buffer +bd-issue-buffer-name)))
      (unwind-protect
          (with-current-buffer buf
            (+bd-issue-mode)
            (setq-local +bd-issue--worktree-path default-directory)
            (should-error (+bd-issue-finish) :type 'user-error))
        ;; Cleanup
        (when (buffer-live-p buf)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(ert-deftest +bd-issue-test-finish-whitespace-only-errors ()
  "Test that finishing with whitespace-only buffer raises error."
  (let ((+bd-issue-buffer-name "*bd-test-issue*"))
    (let ((buf (generate-new-buffer +bd-issue-buffer-name)))
      (unwind-protect
          (with-current-buffer buf
            (+bd-issue-mode)
            (setq-local +bd-issue--worktree-path default-directory)
            (insert "   \n\n  ")
            (should-error (+bd-issue-finish) :type 'user-error))
        ;; Cleanup
        (when (buffer-live-p buf)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(ert-deftest +bd-issue-test-finish-template-only-errors ()
  "Test that finishing with only template text raises error."
  (let ((+bd-issue-buffer-name "*bd-test-issue*"))
    (let ((buf (generate-new-buffer +bd-issue-buffer-name)))
      (unwind-protect
          (with-current-buffer buf
            (+bd-issue-mode)
            (setq-local +bd-issue--worktree-path default-directory)
            (insert +bd-issue-template)
            (should-error (+bd-issue-finish) :type 'user-error))
        ;; Cleanup
        (when (buffer-live-p buf)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

;;; Claude instruction tests

(ert-deftest +bd-issue-test-claude-instruction-format ()
  "Test that Claude instruction constant has expected content."
  (should (stringp +bd-issue--claude-instruction))
  (should (string-match-p "beads MCP" +bd-issue--claude-instruction))
  (should (string-match-p "mcp__plugin_beads_beads__create"
                          +bd-issue--claude-instruction))
  (should (string-match-p "Description:" +bd-issue--claude-instruction)))

;;; Message history tests

(ert-deftest +bd-issue-test-prepare-message-ring ()
  "Test that message ring is initialized correctly."
  (let ((+bd-issue-message-ring nil))
    (with-temp-buffer
      (+bd-issue-mode)
      (should +bd-issue-message-ring)
      (should (ring-p +bd-issue-message-ring))
      (should (= (ring-size +bd-issue-message-ring) +bd-issue-message-ring-size)))))

(ert-deftest +bd-issue-test-buffer-message-extracts-text ()
  "Test that +bd-issue--buffer-message extracts non-comment text."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "Fix the login bug\n")
    (insert "# This is a comment\n")
    (insert "Users can't login")
    (should (equal (+bd-issue--buffer-message)
                   "Fix the login bug\nUsers can't login"))))

(ert-deftest +bd-issue-test-buffer-message-returns-nil-when-empty ()
  "Test that +bd-issue--buffer-message returns nil for empty content."
  (with-temp-buffer
    (+bd-issue-mode)
    (insert "# Only comments\n")
    (insert "   \n")
    (should-not (+bd-issue--buffer-message))))

(ert-deftest +bd-issue-test-save-message ()
  "Test that save-message adds message to ring."
  (let ((+bd-issue-message-ring nil))
    (with-temp-buffer
      (+bd-issue-mode)
      (insert "Test issue description")
      (+bd-issue-save-message)
      (should (= (ring-length +bd-issue-message-ring) 1))
      (should (equal (ring-ref +bd-issue-message-ring 0)
                     "Test issue description")))))

(ert-deftest +bd-issue-test-save-message-removes-duplicates ()
  "Test that saving duplicate message removes old copy."
  (let ((+bd-issue-message-ring nil))
    (with-temp-buffer
      (+bd-issue-mode)
      (insert "Test issue")
      (+bd-issue-save-message)
      (should (= (ring-length +bd-issue-message-ring) 1))
      ;; Save same message again
      (+bd-issue-save-message)
      ;; Should still be only 1 entry
      (should (= (ring-length +bd-issue-message-ring) 1)))))

(ert-deftest +bd-issue-test-prev-message-cycles-backward ()
  "Test that M-p cycles backward through history."
  (let ((+bd-issue-message-ring nil))
    (with-temp-buffer
      (+bd-issue-mode)
      ;; Add two messages to history
      (insert "First message")
      (+bd-issue-save-message)
      (erase-buffer)
      (insert "Second message")
      (+bd-issue-save-message)
      (erase-buffer)

      ;; Now cycle backward
      (+bd-issue-prev-message 1)
      (let ((text (+bd-issue--get-buffer-text)))
        (should (equal text "Second message")))

      ;; Cycle backward again
      (+bd-issue-prev-message 1)
      (let ((text (+bd-issue--get-buffer-text)))
        (should (equal text "First message"))))))

(ert-deftest +bd-issue-test-next-message-cycles-forward ()
  "Test that M-n cycles forward through history."
  (let ((+bd-issue-message-ring nil))
    (with-temp-buffer
      (+bd-issue-mode)
      ;; Add two messages
      (insert "First message")
      (+bd-issue-save-message)
      (erase-buffer)
      (insert "Second message")
      (+bd-issue-save-message)
      (erase-buffer)

      ;; Go back to first
      (+bd-issue-prev-message 1)
      (+bd-issue-prev-message 1)

      ;; Now go forward
      (+bd-issue-next-message 1)
      (let ((text (+bd-issue--get-buffer-text)))
        (should (equal text "Second message"))))))

(ert-deftest +bd-issue-test-prev-message-saves-current ()
  "Test that cycling backward saves current message first."
  (let ((+bd-issue-message-ring nil))
    (with-temp-buffer
      (+bd-issue-mode)
      ;; Add one message to history
      (insert "Old message")
      (+bd-issue-save-message)
      (erase-buffer)

      ;; Type new message but don't save
      (insert "New unsaved message")

      ;; Cycle backward - should save current first
      (+bd-issue-prev-message 1)

      ;; Should have 2 messages now
      (should (= (ring-length +bd-issue-message-ring) 2))

      ;; Cycle forward to get back to the new message
      (+bd-issue-next-message 1)
      (let ((text (+bd-issue--get-buffer-text)))
        (should (equal text "New unsaved message"))))))

(ert-deftest +bd-issue-test-empty-history-rings-bell ()
  "Test that cycling with empty history rings bell."
  (let ((+bd-issue-message-ring nil))
    (with-temp-buffer
      (+bd-issue-mode)
      ;; Try to cycle with empty history - should not error
      (should-not (condition-case nil
                      (progn (+bd-issue-prev-message 1) nil)
                    (error t))))))

(provide '+bd-issue-tests)
;;; +bd-issue-tests.el ends here
