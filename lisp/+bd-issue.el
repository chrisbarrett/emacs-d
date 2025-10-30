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
;;   M-p     - Cycle backward through issue description history
;;   M-n     - Cycle forward through issue description history
;;
;; Evil integration:
;;   If using Evil mode, configure the initial state in your init.el:
;;     (evil-set-initial-state '+bd-issue-mode 'insert)
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ring)

;;; Customization

(defvar +bd-issue-buffer-name "*bd-new-issue*"
  "Name of the buffer used for creating bd issues.")

(defvar +bd-issue-claude-program "claude"
  "Name or path of the Claude CLI program.")

(defcustom +bd-issue-message-ring-size 32
  "Maximum number of issue descriptions to save in the history ring."
  :type 'integer
  :group 'bd-issue)

(defvar +bd-issue-message-ring nil
  "Ring of previous bd issue descriptions for history cycling.")

(defvar-local +bd-issue-message-ring-index nil
  "Index into `+bd-issue-message-ring' for the current buffer.")

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
    (define-key map (kbd "M-p") #'+bd-issue-prev-message)
    (define-key map (kbd "M-n") #'+bd-issue-next-message)
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
                   " [\\[+bd-issue-finish] = create, \\[+bd-issue-cancel] = cancel, \\[+bd-issue-prev-message] = prev]"))))

  ;; Initialize message ring for history cycling
  (+bd-issue--prepare-message-ring))

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

    ;; Save message to history before finishing
    (+bd-issue-save-message)

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
  "Cancel bd issue creation without creating the issue.
Saves the current message to history before cancelling."
  (interactive)
  ;; Save message to history even when cancelling
  (+bd-issue-save-message)
  (let ((window-config +bd-issue--previous-window-config))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (current-buffer)))
    (when window-config
      (set-window-configuration window-config))
    (message "Issue creation cancelled. Message saved to history")))

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
         (input-buffer (generate-new-buffer " *bd-issue-claude-input*")))

    ;; Prepare input buffer
    (with-current-buffer input-buffer
      (insert input))

    (condition-case err
        (let ((proc (make-process
                     :name "bd-issue-claude"
                     :buffer output-buffer
                     :command (list +bd-issue-claude-program)
                     :connection-type 'pipe
                     :sentinel (lambda (process event)
                                 (+bd-issue--claude-sentinel process event
                                                             input worktree-path
                                                             input-buffer))
                     :noquery t)))
          ;; Send input and close stdin
          (process-send-string proc input)
          (process-send-eof proc)
          (message "Creating issue with claude..." ))

      (file-error
       (kill-buffer output-buffer)
       (kill-buffer input-buffer)
       (user-error "Failed to execute claude: %s"
                   (error-message-string err))))))

(defun +bd-issue--claude-sentinel (process event input worktree-path input-buffer)
  "Process sentinel for Claude CLI invocation.

PROCESS is the process object.
EVENT is the event description string.
INPUT is the text sent to Claude.
WORKTREE-PATH is the directory where the command ran.
INPUT-BUFFER is the buffer containing the input text."
  (let ((output-buffer (process-buffer process)))
    (unwind-protect
        (cond
         ;; Process finished successfully
         ((string-match-p (rx line-start "finished") event)
          (let ((exit-code (process-exit-status process)))
            (if (zerop exit-code)
                (message "Issue created.")
              ;; Non-zero exit with "finished" status
              (let ((error-output (with-current-buffer output-buffer
                                    (buffer-string))))
                (message "Issue creation failed (exit code %d)" exit-code)
                (+bd-issue--display-error exit-code error-output input worktree-path)))))

         ;; Process failed or was killed
         ((string-match-p (rx line-start (or "exited" "failed" "killed" "terminated")) event)
          (let ((exit-code (process-exit-status process))
                (error-output (with-current-buffer output-buffer
                                (buffer-string))))
            (message "Issue creation with Claude failed: %s" (string-trim event))
            (+bd-issue--display-error exit-code error-output input worktree-path)))

         ;; Unexpected event
         (t
          (message "Claude process event: %s" event)))

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

;;; Message History

(defun +bd-issue--prepare-message-ring ()
  "Initialize the message ring for this buffer if needed."
  (unless +bd-issue-message-ring
    (setq +bd-issue-message-ring (make-ring +bd-issue-message-ring-size)))
  (make-local-variable '+bd-issue-message-ring-index))

(defun +bd-issue--buffer-message ()
  "Extract the user's issue description from the buffer.
Returns nil if the buffer contains only whitespace and/or comments."
  (let ((text (+bd-issue--get-buffer-text)))
    (and (not (string-match-p "\\`[ \t\n\r]*\\'" text))
         text)))

(defun +bd-issue--new-message-index (offset len)
  "Calculate new message ring index.
OFFSET is the relative movement (positive = backward, negative = forward).
LEN is the ring length."
  (if +bd-issue-message-ring-index
      (mod (+ +bd-issue-message-ring-index offset) len)
    (1- len)))

(defun +bd-issue-prev-message (arg)
  "Cycle backward through message history, after saving current message.
With a numeric prefix ARG, go back ARG messages."
  (interactive "*p")
  (+bd-issue--prepare-message-ring)
  (let ((len (ring-length +bd-issue-message-ring)))
    (if (<= len 0)
        (progn (message "Empty issue description history") (ding))
      ;; Save the current non-empty message to the ring
      (when-let ((message (+bd-issue--buffer-message)))
        (unless (ring-member +bd-issue-message-ring message)
          (ring-insert +bd-issue-message-ring message)
          (cl-incf arg)
          (setq len (ring-length +bd-issue-message-ring))))
      ;; Delete the current text but not comment lines
      (delete-region (point-min) (point-max))
      ;; Calculate new index and insert the message
      (setq +bd-issue-message-ring-index (+bd-issue--new-message-index arg len))
      (message "Issue description %d" (1+ +bd-issue-message-ring-index))
      (insert (ring-ref +bd-issue-message-ring +bd-issue-message-ring-index))
      ;; Save position before comments for cursor placement
      (let ((end-of-message (point)))
        ;; Re-insert template comments at the end (template already starts with newlines)
        (goto-char (point-max))
        (insert +bd-issue-template)
        ;; Position cursor on the newline before the template comments
        (goto-char (1+ end-of-message))))))

(defun +bd-issue-next-message (arg)
  "Cycle forward through message history, after saving current message.
With a numeric prefix ARG, go forward ARG messages."
  (interactive "*p")
  (+bd-issue-prev-message (- arg)))

(defun +bd-issue-save-message ()
  "Save current issue description to the history ring."
  (interactive)
  (+bd-issue--prepare-message-ring)
  (if-let ((message (+bd-issue--buffer-message)))
      (progn
        (when-let ((index (ring-member +bd-issue-message-ring message)))
          (ring-remove +bd-issue-message-ring index))
        (ring-insert +bd-issue-message-ring message)
        (message "Issue description saved to history"))
    (message "Only whitespace and/or comments; not saved")))

(provide '+bd-issue)
;;; +bd-issue.el ends here
