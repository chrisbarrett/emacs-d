;;; claude-prompt.el --- git-commit-style editing for Claude prompt files -*- lexical-binding: t; -*-

;;; Commentary:

;; Edit Claude Code prompts in Emacs the way you edit a Git commit message.
;;
;; When you ask Claude Code to open your prompt in an editor, it hands the
;; file to Emacs and waits.  `claude-prompt-mode' turns that buffer into a
;; proper editing surface:
;;
;; - `C-c C-c' sends the prompt back to Claude; `C-c C-k' cancels and keeps
;;   your original prompt.
;; - `M-p' / `M-n' walk back and forth through prompts you have submitted
;;   before from this repository.
;; - `C-r' searches your prompt history: `[r]' for this repository (the
;;   default), `[g]' for every project.  Pick one and it is inserted.
;;
;; History comes from Claude's own log (`~/.claude/history.jsonl'), so every
;; prompt you send shows up next time -- nothing extra to maintain.

;;; Code:

(require 'rx)
(require 'subr-x)

(declare-function with-editor-mode "with-editor")
(declare-function consult--multi "consult")

(eval-when-compile
  (require 'with-editor nil t)
  (require 'consult nil t))


;;; Customisation

(defgroup claude-prompt nil
  "Editing surface for Claude Code prompt files."
  :group 'external
  :prefix "claude-prompt-")

(defcustom claude-prompt-history-file
  (expand-file-name "~/.claude/history.jsonl")
  "Path to Claude Code's prompt log.
Each line is a JSON object with `display' (prompt text) and
`project' (working directory) fields.  Read-only input."
  :type 'file
  :group 'claude-prompt)

(defconst claude-prompt-filename-regexp
  (rx "/claude-" (+ digit)
      "/claude-prompt-" (+ (any alnum "-"))
      ".md" eos)
  "Regexp matching a Claude Code prompt file.
Matches a `claude-prompt-<guid>.md' file inside a `claude-<uid>'
directory under the system temporary directory, e.g.
`/private/tmp/claude-503/claude-prompt-abc.md'.")


;;; Repo classification

(defconst claude-prompt--worktree-suffix-regexp
  (rx (or "__worktrees" "/.worktrees")
      "/" (+ (not (any "/")))
      eos)
  "Regexp matching a trailing worktree segment on a project path.
Strips a workmux `__worktrees/<name>' or `/.worktrees/<name>'
suffix so linked worktrees normalise to their main repository.")

(defun claude-prompt--normalise-repo (path)
  "Normalise PATH to its main-worktree repository root.
Strips any trailing worktree segment and a trailing slash."
  (when path
    (let ((stripped (replace-regexp-in-string
                     claude-prompt--worktree-suffix-regexp "" path)))
      (directory-file-name stripped))))

(defun claude-prompt--repo-member-p (project repo-root)
  "Return non-nil if PROJECT belongs to REPO-ROOT.
Both are normalised by stripping any trailing worktree segment, so
prompts from deleted worktrees still classify into the repo scope."
  (and project repo-root
       (string-equal (claude-prompt--normalise-repo project)
                     (claude-prompt--normalise-repo repo-root))))


;;; History log

(defvar claude-prompt--history-cache nil
  "Cache of the parsed history log.
A cons of (MTIME . ENTRIES), where ENTRIES is a list of
\(DISPLAY . PROJECT) pairs, most-recent-first and de-duplicated.")

(defun claude-prompt--parse-history (file)
  "Parse the history log at FILE into (DISPLAY . PROJECT) pairs.
Result is most-recent-first and de-duplicated by display text.
Returns nil if FILE is absent or unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      ;; Lines are appended chronologically, so walk backwards for
      ;; most-recent-first, keeping the first occurrence of each display.
      (let ((seen (make-hash-table :test 'equal))
            (entries nil))
        (while (not (bobp))
          (forward-line -1)
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p (string-trim line))
              (when-let* ((obj (ignore-errors
                                 (json-parse-string line :object-type 'alist
                                                    :null-object nil)))
                          (display (alist-get 'display obj)))
                (unless (gethash display seen)
                  (puthash display t seen)
                  (push (cons display (alist-get 'project obj)) entries))))))
        (nreverse entries)))))

(defun claude-prompt-history (&optional file)
  "Return the parsed history from FILE (default `claude-prompt-history-file').
A list of (DISPLAY . PROJECT) pairs, most-recent-first and
de-duplicated.  Cached and re-parsed only when FILE's modification
time changes."
  (let* ((file (or file claude-prompt-history-file))
         (mtime (file-attribute-modification-time (file-attributes file))))
    (if (and claude-prompt--history-cache
             (equal (car claude-prompt--history-cache) mtime))
        (cdr claude-prompt--history-cache)
      (let ((entries (claude-prompt--parse-history file)))
        (setq claude-prompt--history-cache (cons mtime entries))
        entries))))

(defun claude-prompt-repo-prompts (repo-root &optional file)
  "Return history prompts belonging to REPO-ROOT from FILE.
A list of (DISPLAY . PROJECT) pairs, most-recent-first."
  (seq-filter (pcase-lambda (`(,_ . ,project))
                (claude-prompt--repo-member-p project repo-root))
              (claude-prompt-history file)))


;;; Repo context side-channel

(defvar claude-prompt--context-table (make-hash-table :test 'equal)
  "Map of prompt-file truename to its git-derived repository root.
Populated by `claude-prompt-register-context' before the file is
visited, and popped at setup time.")

(defvar-local claude-prompt--repo-root nil
  "Repository root for the current prompt buffer, for history scoping.")

;;;###autoload
(defun claude-prompt-register-context (file repo-root)
  "Register REPO-ROOT for the prompt FILE, keyed by its truename.
Called by the editor wrapper via `emacsclient --eval' before the
blocking `emacsclient -nw -c FILE' opens the buffer.  Keyed by path
so the two calls cannot race."
  (puthash (file-truename file) repo-root claude-prompt--context-table)
  repo-root)

(defun claude-prompt--resolve-repo-root ()
  "Resolve the repository root for the current prompt buffer.
Prefers the registered side-channel context, popping it once
consumed.  Falls back to the newest history entry's project."
  (let* ((key (and buffer-file-name (file-truename buffer-file-name)))
         (registered (and key (gethash key claude-prompt--context-table 'none))))
    (cond
     ((and key (not (eq registered 'none)))
      (remhash key claude-prompt--context-table)
      registered)
     (t (cdr (car (claude-prompt-history)))))))


;;; History ring

(defvar-local claude-prompt--ring nil
  "Repo-scoped prompt strings for ring cycling, most-recent-first.")

(defvar-local claude-prompt--ring-index -1
  "Current position in `claude-prompt--ring'.
-1 means the buffer shows its original body rather than a ring entry.")

(defvar-local claude-prompt--original-body nil
  "Buffer body captured before the first ring cycle, for restoration.")

(defun claude-prompt--replace-body (text)
  "Replace the whole buffer body with TEXT."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert text)))

(defun claude-prompt--ring-goto (n)
  "Show ring entry N, or the original body when N is -1."
  (unless claude-prompt--ring
    (user-error "No prompt history for this repository"))
  (let ((n (max -1 (min n (1- (length claude-prompt--ring))))))
    (when (and (= claude-prompt--ring-index -1) (/= n -1))
      (setq claude-prompt--original-body (buffer-string)))
    (claude-prompt--replace-body
     (if (= n -1)
         (or claude-prompt--original-body "")
       (nth n claude-prompt--ring)))
    (setq claude-prompt--ring-index n)))

(defun claude-prompt-previous ()
  "Replace the buffer body with the previous (older) prompt in the ring."
  (interactive)
  (when (>= claude-prompt--ring-index (1- (length claude-prompt--ring)))
    (user-error "No earlier prompt"))
  (claude-prompt--ring-goto (1+ claude-prompt--ring-index)))

(defun claude-prompt-next ()
  "Replace the buffer body with the next (newer) prompt in the ring."
  (interactive)
  (when (< claude-prompt--ring-index 0)
    (user-error "No later prompt"))
  (claude-prompt--ring-goto (1- claude-prompt--ring-index)))


;;; Recall picker (consult)

(defun claude-prompt--insert (text)
  "Insert TEXT (a recalled prompt) into the current buffer."
  (insert (substring-no-properties text)))

(defun claude-prompt--source (prompts narrow name default)
  "Build a `consult--multi' source over PROMPTS.
NARROW is the narrowing character, NAME the source name, and DEFAULT
marks the default source.  PROMPTS is a list of (DISPLAY . PROJECT)."
  (list :name name
        :narrow narrow
        :category 'claude-prompt
        :default default
        :annotate (lambda (cand) (get-text-property 0 'claude-prompt-project cand))
        :action #'claude-prompt--insert
        :items (mapcar (pcase-lambda (`(,display . ,project))
                         (propertize display 'claude-prompt-project
                                     (or project "")))
                       prompts)))

(defun claude-prompt--consult-sources (repo-root &optional file)
  "Return the `[r]' repo and `[g]' global recall sources.
REPO-ROOT scopes the default repo source; FILE overrides the log."
  (list (claude-prompt--source (claude-prompt-repo-prompts repo-root file)
                               ?r "Repo prompts" t)
        (claude-prompt--source (claude-prompt-history file)
                               ?g "All prompts" nil)))

(defun claude-prompt-recall ()
  "Recall a prior prompt via `consult', inserting the selection.
`[r]' narrows to the current repository (default); `[g]' spans all
projects."
  (interactive)
  (require 'consult)
  (consult--multi (claude-prompt--consult-sources claude-prompt--repo-root)
                  :prompt "Recall prompt: "
                  :require-match t
                  :sort nil))


;;; Mode

(defvar-keymap claude-prompt-mode-map
  :doc "Keymap for `claude-prompt-mode'.
Finish (\\`C-c C-c') and cancel (\\`C-c C-k') come from `with-editor-mode'."
  "M-p" #'claude-prompt-previous
  "M-n" #'claude-prompt-next
  "C-r" #'claude-prompt-recall)

(defconst claude-prompt-usage-message "\
Type \\[with-editor-finish] to send the prompt to Claude, \
\\[with-editor-cancel] to cancel, \
\\[claude-prompt-previous] and \\[claude-prompt-next] to cycle prior prompts, \
and \\[claude-prompt-recall] to search history"
  "Help message shown when a Claude prompt buffer opens.")

;;;###autoload
(define-minor-mode claude-prompt-mode
  "Minor mode for editing a Claude Code prompt file.

Layered on the file's existing major mode (normally `gfm-mode') and
on `with-editor-mode', which provides finish (\\`C-c C-c'), cancel
(\\`C-c C-k'), and kill-buffer protection.  Adds a repo-scoped history
ring (\\`M-p' / \\`M-n') and a `consult' recall picker (\\`C-r')."
  :lighter " Claude"
  :keymap claude-prompt-mode-map
  :interactive nil
  (when claude-prompt-mode
    (require 'with-editor)
    (setq claude-prompt--repo-root (claude-prompt--resolve-repo-root))
    (setq claude-prompt--ring
          (mapcar #'car (claude-prompt-repo-prompts claude-prompt--repo-root)))
    (setq claude-prompt--ring-index -1)
    ;; Show our richer help message in place of with-editor's default.
    (setq-local with-editor-usage-message claude-prompt-usage-message)
    (unless (bound-and-true-p with-editor-mode)
      (with-editor-mode 1))))

(defun claude-prompt--registered-p ()
  "Return non-nil if the current buffer's file has a registered context.
The editor wrapper calls `claude-prompt-register-context' before
opening the file, so a registered entry marks a wrapper-driven edit."
  (and buffer-file-name
       (gethash (file-truename buffer-file-name)
                claude-prompt--context-table)))

;;;###autoload
(defun claude-prompt-setup-check-buffer ()
  "Enable `claude-prompt-mode' for a wrapper-opened Claude prompt buffer.

Intended for `server-visit-hook', which runs only for files opened
through the Emacs server (i.e. via the `emacsclient' editor wrapper),
so ordinary file visits incur no cost.  The mode is enabled when the
wrapper has registered a context for this file (the primary path) or,
as a fallback, when the filename matches `claude-prompt-filename-regexp'."
  (when (and buffer-file-name
             (or (claude-prompt--registered-p)
                 (string-match-p claude-prompt-filename-regexp
                                 buffer-file-name)))
    (claude-prompt-mode 1)))

(provide 'claude-prompt)

;;; claude-prompt.el ends here
