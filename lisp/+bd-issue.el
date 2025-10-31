;;; +bd-issue.el --- Buffer-based input for creating bd issues -*- lexical-binding: t; -*-

;;; Commentary:

;; This library provides a git-commit-like buffer interface for creating
;; beads (bd) issues. Users can type free-form text describing the issue,
;; and Claude Code interprets the text.
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ring)
(require '+bd-process)

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

(defvar-local +bd-issue--worktree-path nil
  "Path to worktree for this bd issue buffer.")

(defvar-local +bd-issue--previous-window-config nil
  "Window configuration to restore when issue creation completes.")


;;; Major mode

(defvar +bd-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+bd-issue-finish)
    (define-key map (kbd "C-c C-k") #'+bd-issue-cancel)
    (define-key map (kbd "C-c C-p") #'+bd-issue-prev-message)
    (define-key map (kbd "M-p") #'+bd-issue-prev-message)
    (define-key map (kbd "C-c C-n") #'+bd-issue-next-message)
    (define-key map (kbd "M-n") #'+bd-issue-next-message)
    map)
  "Keymap for `+bd-issue-mode'.")

(define-derived-mode +bd-issue-mode text-mode "BD-Issue"
  "Major mode for creating beads (bd) issues.

Special commands:
\\{+bd-issue-mode-map}"
  (setq-local comment-start "#")
  (setq-local comment-start-skip (rx (one-or-more "#") (zero-or-more space)))
  (setq-local fill-column 72)
  (setq-local font-lock-defaults '((("^#.*$" . font-lock-comment-face)) nil nil nil nil))

  (auto-fill-mode 1)

  (add-hook 'kill-buffer-query-functions #'+bd-issue--kill-buffer-query nil t)

  (setq-local mode-line-misc-info
              `((+bd-issue-mode
                 ,(substitute-command-keys
                   " [\\[+bd-issue-finish] = create, \\[+bd-issue-cancel] = cancel, \\[+bd-issue-prev-message] = prev]"))))

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

    (+bd-issue-save-message)

    (let ((worktree-path +bd-issue--worktree-path)
          (window-config +bd-issue--previous-window-config))

      (let ((kill-buffer-query-functions nil))
        (kill-buffer (current-buffer)))

      (when window-config
        (set-window-configuration window-config))

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
;;
;; Use claude code to process a natural language issue description into
;; something more structured for bd create.
;; The flow is: claude command (via +bd-process-call) -> parse output ->
;; bd create command (via +bd-process-call in callback).

(defun +bd-issue--make-prompt (issue-text)
  (format "
<instructions>
    You receive free text input from a user and must interpret it into a LISP
    S-Expression structure for use in a program.

    The user will provide a description of an issue they would like to create.
    You must interpret it into an alist with a specific set of allowed keys and
    value types, as per the structure definition below.

    You return ONLY the `read'-able alist, without markdown code fencing or any
    other prose.
</instructions>

<structure>
    <input>
        ARBITRARY FREE TEXT ENTERED BY USER
    </input>
    <output>
        ((arguments . (\"title\"))                          ; Required: positional arguments (just the title)
         (flags . ((type . \"bug|feature|task|epic|chore\") ; Optional (default: task)
                   (priority . 2)                           ; Optional: 0=highest, 2=default
                   (description . \"string\")               ; Optional: issue description
                   (labels . (\"string\" ...))              ; Optional: keyword labels for querying
                   (deps . (\"string\" ...))                ; Optional: Dependencies in format 'type:id' or 'id'
                   (design . \"string\")                    ; Optional: design notes
                   (acceptance . \"string\")                ; Optional: acceptance criteria
                   (external-ref . \"string\"))))           ; Optional: external ticket ID
    </output>
</structure>

<input>
    Fix the login button - it's not responding to clicks on mobile
</input>
<output
    ((arguments . (\"Fix login button on mobile\"))
     (flags . ((type . \"bug\")
               (priority . 1)
               (description . \"Login button not responding to clicks on mobile devices\"))))
</output>

<input>
%s
</input>
<output>
```
" (string-join (seq-map (lambda (line)
                          (format "    %s" line))
                        (string-lines issue-text))
               "\n")))

(defun +bd-issue--create-via-claude (issue-text worktree-path)
  "Create a bd issue via Claude Code CLI asynchronously.

ISSUE-TEXT is the free-form description from the user.
WORKTREE-PATH is the directory to run the command in."
  ;; Check that claude binary exists
  (cl-assert (executable-find +bd-issue-claude-program))

  (let* ((default-directory worktree-path)
         (prompt (+bd-issue--make-prompt issue-text)))

    (message "Analysing input with Claude...")
    (+bd-process-call
     (list :command +bd-issue-claude-program
           :arguments '("--model" "haiku")
           :stdin prompt
           :callback (lambda (output)
                       (+bd-issue--parse-and-create output worktree-path issue-text))))))


(defun +bd-issue--parse-and-create (output worktree-path input)
  "Parse alist from OUTPUT and create issue via bd command.

OUTPUT is the raw output from Claude (should be an alist).
WORKTREE-PATH is the directory to run bd in.
INPUT is the original user input (for error reporting)."
  (condition-case err
      (let* ((cleaned-output (string-trim (string-replace "```" "" output)))
             (issue-alist (read cleaned-output))
             (default-directory worktree-path))
        (debug)
        (+bd-process-call
         (list :bd "create"
               :arguments (alist-get 'arguments issue-alist)
               :flags (alist-get 'flags issue-alist)
               :callback (lambda (result)
                           (message "Issue created successfully: %s" result)))))

    (invalid-read-syntax
     (debug err)
     (+bd-issue--display-error 1 output input worktree-path))

    (error
     (debug err)
     (+bd-issue--display-error 1 (format "%s\n\nOriginal output:\n%s"
                                         (error-message-string err)
                                         output)
                               input worktree-path))))

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
      (insert "Command: claude (async via +bd-process-call)\n")
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
      (when-let* ((message (+bd-issue--buffer-message)))
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
  (when-let* ((message (+bd-issue--buffer-message)))
    (when-let* ((index (ring-member +bd-issue-message-ring message)))
      (ring-remove +bd-issue-message-ring index))
    (ring-insert +bd-issue-message-ring message)))

(provide '+bd-issue)
;;; +bd-issue.el ends here
