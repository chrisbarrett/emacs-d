;;; +bd-issue.el --- Buffer-based input for creating bd issues -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author: Chris Oates
;; Maintainer: Chris Oates
;; Created: January 29, 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This library provides a git-commit-like buffer interface for creating
;; beads (bd) issues. Users can type free-form text describing the issue,
;; and Claude Code interprets the text to create a structured issue via
;; the beads MCP server.
;;
;; Usage:
;;   M-x +bd-issue-create
;;
;; The buffer supports:
;;   C-c C-c - Create the issue via Claude Code
;;   C-c C-k - Cancel without creating the issue
;;
;; Evil integration:
;;   If using Evil mode, configure the initial state in your init.el:
;;     (evil-set-initial-state '+bd-issue-mode 'insert)
;;
;;; Code:

(require 'subr-x)

;;; Customization

(defvar +bd-issue-buffer-name "*bd-new-issue*"
  "Name of the buffer used for creating bd issues.")

(defvar +bd-issue-claude-program "claude"
  "Name or path of the Claude CLI program.")

(defconst +bd-issue-template
  "

# Describe the issue you want to create. Write naturally - Claude will interpret
# your description to create the issue with an appropriate title, type, and
# priority.
#
# Lines starting with '#' will be ignored.
"
  "Template for creating new bd issues.")

(defconst +bd-issue--claude-instruction
  "Create a bd issue from this description using the beads MCP server.

IMPORTANT: First call mcp__plugin_beads_beads__set_context with the
workspace_root parameter set to the current working directory. Then use
mcp__plugin_beads_beads__create to create the issue with an appropriate
title, type, and priority based on the text below.

Just create the issue and confirm it was created - no other output needed.

Description:
"
  "Instruction prompt for Claude when creating bd issues.")

;;; Buffer-local variables

(defvar-local +bd-issue--worktree-path nil
  "Path to worktree for this bd issue buffer.")

(defvar-local +bd-issue--previous-window-config nil
  "Window configuration to restore when issue creation completes.")

;;; Major mode

(defvar +bd-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+bd-issue-finish)
    (define-key map (kbd "C-c C-k") #'+bd-issue-cancel)
    map)
  "Keymap for `+bd-issue-mode'.")

(define-derived-mode +bd-issue-mode text-mode "BD-Issue"
  "Major mode for creating beads (bd) issues.

Special commands:
\\{+bd-issue-mode-map}"
  ;; Enable markdown-like features from text-mode
  (setq-local comment-start "#")
  (setq-local comment-start-skip (rx (one-or-more "#") (zero-or-more space)))
  (setq-local fill-column 72)

  ;; Set up font-locking for # comments
  (setq-local font-lock-defaults
              '((("^#.*$" . font-lock-comment-face))
                nil nil nil nil))

  ;; Enable auto-fill for description text
  (auto-fill-mode 1)

  ;; Prevent accidental buffer kills
  (add-hook 'kill-buffer-query-functions
            #'+bd-issue--kill-buffer-query nil t)

  ;; Set up helpful mode line
  (setq-local mode-line-misc-info
              `((+bd-issue-mode
                 ,(substitute-command-keys
                   " [\\[+bd-issue-finish] = create, \\[+bd-issue-cancel] = cancel]")))))

;;; Entry point

;;;###autoload
(defun +bd-issue-create (&optional worktree-path)
  "Create a new bd issue via Claude Code.

Opens a buffer with a template for describing the issue. Write
your description naturally and press \\[+bd-issue-finish] to
create the issue or \\[+bd-issue-cancel] to cancel.

If WORKTREE-PATH is provided, use that directory for creating
the issue. Otherwise, use the worktree associated with the
current tab, or fall back to `default-directory'.

The issue will be created via the beads MCP server through
Claude Code CLI, which interprets your description to determine
appropriate title, type, and priority."
  (interactive)
  (let* ((worktree (or worktree-path
                       (when (fboundp '+worktrees-path-for-selected-tab)
                         (+worktrees-path-for-selected-tab))
                       default-directory))
         (window-config (current-window-configuration))
         (buf (generate-new-buffer +bd-issue-buffer-name)))

    ;; Set up the buffer
    (with-current-buffer buf
      ;; Enable mode first (it calls kill-all-local-variables)
      (+bd-issue-mode)
      ;; Then set buffer-local variables
      (setq-local +bd-issue--worktree-path worktree)
      (setq-local +bd-issue--previous-window-config window-config)
      ;; Insert template and position cursor
      (insert +bd-issue-template)
      (goto-char (point-min)))

    ;; Display the buffer
    (pop-to-buffer buf)

    (message "%s" (substitute-command-keys
                   "Enter issue details. \\[+bd-issue-finish] to create, \\[+bd-issue-cancel] to cancel."))))

;;; Buffer content extraction

(defun +bd-issue--get-buffer-text ()
  "Extract user's issue description from buffer.
Removes comment lines - those starting with '#' followed by whitespace
or end-of-line. This strips lines like '# comment' and bare '#' lines,
but preserves '#tag' or '#123' (no space after #).
Note: This will also strip markdown headers like '# Title',
which is intentional to match the git-commit style."
  (string-trim
   (string-join
    (seq-remove (lambda (line)
                  (string-match-p (rx line-start "#" (or space line-end)) line))
                (split-string (buffer-string) "\n"))
    "\n")))

;;; Accept/Cancel handlers

(defun +bd-issue-finish ()
  "Finish creating the bd issue and create it via Claude Code."
  (interactive)
  (let ((issue-text (+bd-issue--get-buffer-text)))
    (when (string-empty-p issue-text)
      (user-error "Please describe the issue you want to create"))

    (let ((worktree-path +bd-issue--worktree-path)
          (window-config +bd-issue--previous-window-config))

      ;; Kill the buffer first
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (current-buffer)))

      ;; Restore window configuration
      (when window-config
        (set-window-configuration window-config))

      ;; Create the issue via Claude
      (+bd-issue--create-via-claude issue-text worktree-path))))

(defun +bd-issue-cancel ()
  "Cancel bd issue creation without creating the issue."
  (interactive)
  (let ((window-config +bd-issue--previous-window-config))
    (when (yes-or-no-p "Cancel issue creation? ")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (current-buffer)))
      (when window-config
        (set-window-configuration window-config))
      (message "Issue creation cancelled"))))

(defun +bd-issue--kill-buffer-query ()
  "Query function for `kill-buffer-query-functions'.
Prevents accidental buffer kill without using \\[+bd-issue-finish] or \\[+bd-issue-cancel]."
  (if (eq major-mode '+bd-issue-mode)
      (progn
        (message "%s" (substitute-command-keys
                       "Use \\[+bd-issue-finish] to create issue or \\[+bd-issue-cancel] to cancel"))
        nil) ; Prevent kill
    t)) ; Allow kill for other modes

;;; Claude Code integration

(defun +bd-issue--create-via-claude (issue-text worktree-path)
  "Create a bd issue via Claude Code CLI asynchronously.

ISSUE-TEXT is the free-form description from the user.
WORKTREE-PATH is the directory to run the command in."
  ;; Check that claude binary exists
  (cl-assert (executable-find +bd-issue-claude-program))

  (let* ((default-directory worktree-path)
         (input (concat +bd-issue--claude-instruction issue-text))
         (output-buffer (generate-new-buffer " *bd-issue-claude-output*"))
         (input-buffer (generate-new-buffer " *bd-issue-claude-input*"))
         (reporter (make-progress-reporter "Creating issue via Claude Code"))
         (timer nil))

    ;; Prepare input buffer
    (with-current-buffer input-buffer
      (insert input))

    ;; Start progress reporter timer (update every 0.5s)
    (setq timer
          (run-with-timer 0.5 0.5
                          (lambda ()
                            (progress-reporter-update reporter))))

    ;; Start async process
    (let ((proc nil))
      (unwind-protect
          (condition-case err
              (progn
                (setq proc (make-process
                            :name "bd-issue-claude"
                            :buffer output-buffer
                            :command (list +bd-issue-claude-program)
                            :connection-type 'pipe
                            :sentinel (lambda (process event)
                                        (+bd-issue--claude-sentinel process event
                                                                    input worktree-path
                                                                    input-buffer
                                                                    reporter timer))
                            :noquery t))
                ;; Send input and close stdin
                (process-send-string proc input)
                (process-send-eof proc))
            (file-error
             (when timer (cancel-timer timer))
             (progress-reporter-done reporter)
             (kill-buffer output-buffer)
             (kill-buffer input-buffer)
             (user-error "Failed to execute claude: %s"
                         (error-message-string err))))
        ;; Cleanup: if we error out before the process finishes, clean up
        (when (and proc (process-live-p proc))
          (delete-process proc)
          (when timer (cancel-timer timer))
          (kill-buffer output-buffer)
          (kill-buffer input-buffer))))))

(defun +bd-issue--claude-sentinel (process event input worktree-path input-buffer reporter timer)
  "Process sentinel for Claude CLI invocation.

PROCESS is the process object.
EVENT is the event description string.
INPUT is the text sent to Claude.
WORKTREE-PATH is the directory where the command ran.
INPUT-BUFFER is the buffer containing the input text.
REPORTER is the progress reporter object.
TIMER is the timer updating the progress reporter."
  (let ((output-buffer (process-buffer process)))
    (unwind-protect
        (progn
          ;; Cancel the progress reporter timer
          (when timer
            (cancel-timer timer))

          (cond
           ;; Process finished successfully
           ((string-match-p (rx line-start "finished") event)
            (let ((exit-code (process-exit-status process)))
              (if (zerop exit-code)
                  (progress-reporter-done reporter)
                ;; Non-zero exit with "finished" status
                (let ((error-output (with-current-buffer output-buffer
                                      (buffer-string))))
                  (progress-reporter-done reporter)
                  (message "Issue creation failed (exit code %d)" exit-code)
                  (+bd-issue--display-error exit-code error-output input worktree-path)))))

           ;; Process failed or was killed
           ((string-match-p (rx line-start (or "exited" "failed" "killed" "terminated")) event)
            (let ((exit-code (process-exit-status process))
                  (error-output (with-current-buffer output-buffer
                                  (buffer-string))))
              (progress-reporter-done reporter)
              (message "Issue creation with Claude failed: %s" (string-trim event))
              (+bd-issue--display-error exit-code error-output input worktree-path)))

           ;; Unexpected event
           (t
            (progress-reporter-done reporter)
            (message "Claude process event: %s" event))))

      ;; Cleanup buffers
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer))
      (when (buffer-live-p input-buffer)
        (kill-buffer input-buffer)))))

(defun +bd-issue--display-error (exit-code error-output input worktree-path)
  "Display error information in the *bd-issue-errors* buffer.

EXIT-CODE is the process exit code.
ERROR-OUTPUT is the stderr/stdout from the process.
INPUT is the text sent to Claude.
WORKTREE-PATH is the directory where the command ran."
  (with-current-buffer (get-buffer-create "*bd-issue-errors*")
    (unless (eq major-mode 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n\n" (current-time-string) "\n")
      (insert "Exit code: " (number-to-string exit-code) "\n")
      (insert "Command: claude (async via make-process)\n")
      (insert "Working directory: " worktree-path "\n")
      (insert "\n--- Input ---\n" input "\n")
      (insert "\n--- Output ---\n" error-output))
    (display-buffer (current-buffer))))

(provide '+bd-issue)
;;; +bd-issue.el ends here
