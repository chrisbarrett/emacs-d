;;; claude-prompt-tests.el --- Tests for claude-prompt -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for `claude-prompt'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'claude-prompt)
(require 'with-editor)

;;; Fixtures

(defconst claude-prompt-tests--history-lines
  ;; Chronological order (oldest first), as Claude appends them.
  '("{\"display\":\"first\",\"project\":\"/repo\"}"
    "{\"display\":\"second\",\"project\":\"/repo__worktrees/x\"}"
    "{\"display\":\"first\",\"project\":\"/other\"}"
    "{\"display\":\"third\",\"project\":\"/elsewhere\"}")
  "Fixture history lines; \"first\" recurs to exercise de-duplication.")

(defmacro claude-prompt-tests--with-history (var &rest body)
  "Write the fixture history to a temp file, bind its path to VAR, run BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "claude-prompt-history-" nil ".jsonl"))
         (claude-prompt--history-cache nil))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (dolist (line claude-prompt-tests--history-lines)
               (insert line "\n")))
           ,@body)
       (delete-file ,var))))


;;; Prompt-file detection

(ert-deftest claude-prompt/regexp-matches-prompt-file ()
  "The filename regexp matches a Claude prompt path."
  (should (string-match-p claude-prompt-filename-regexp
                          "/private/tmp/claude-503/claude-prompt-abc123.md"))
  (should (string-match-p claude-prompt-filename-regexp
                          "/tmp/claude-1000/claude-prompt-1a2b-3c4d.md")))

(ert-deftest claude-prompt/regexp-rejects-ordinary-markdown ()
  "The filename regexp rejects an ordinary markdown file."
  (should-not (string-match-p claude-prompt-filename-regexp "/home/me/notes.md"))
  (should-not (string-match-p claude-prompt-filename-regexp
                              "/tmp/claude-503/other.md")))


;;; Repo classification

(ert-deftest claude-prompt/normalise-strips-worktree-suffix ()
  "Plain, `__worktrees', and `.worktrees' paths normalise to the repo root."
  (should (equal (claude-prompt--normalise-repo "/repo") "/repo"))
  (should (equal (claude-prompt--normalise-repo "/repo/") "/repo"))
  (should (equal (claude-prompt--normalise-repo "/repo__worktrees/x") "/repo"))
  (should (equal (claude-prompt--normalise-repo "/repo/.worktrees/x") "/repo")))

(ert-deftest claude-prompt/repo-membership ()
  "Worktree entries classify into their repo; others are excluded."
  (should (claude-prompt--repo-member-p "/repo" "/repo"))
  (should (claude-prompt--repo-member-p "/repo__worktrees/feature" "/repo"))
  (should (claude-prompt--repo-member-p "/repo/.worktrees/feature" "/repo"))
  (should-not (claude-prompt--repo-member-p "/other" "/repo"))
  (should-not (claude-prompt--repo-member-p nil "/repo")))


;;; History log

(ert-deftest claude-prompt/history-most-recent-first-deduped ()
  "Parsing yields (display . project) pairs, newest-first, de-duplicated."
  (claude-prompt-tests--with-history file
    (should (equal (claude-prompt-history file)
                   '(("third" . "/elsewhere")
                     ("first" . "/other")
                     ("second" . "/repo__worktrees/x"))))))

(ert-deftest claude-prompt/history-cached-by-mtime ()
  "A second call with an unchanged file returns the cached list object."
  (claude-prompt-tests--with-history file
    (let ((first (claude-prompt-history file)))
      (should (eq first (claude-prompt-history file))))))

(ert-deftest claude-prompt/repo-prompts-scoped ()
  "Repo prompts include worktree entries and exclude foreign projects."
  (claude-prompt-tests--with-history file
    ;; "first"/repo was de-duplicated to its newer "first"/other occurrence,
    ;; so only the worktree entry "second" remains in /repo.
    (should (equal (claude-prompt-repo-prompts "/repo" file)
                   '(("second" . "/repo__worktrees/x"))))))


;;; Repo context side-channel

(ert-deftest claude-prompt/register-and-resolve-context ()
  "A registered context is resolved by truename and popped once consumed."
  (let ((claude-prompt--context-table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (setq buffer-file-name "/private/tmp/claude-503/claude-prompt-z.md")
      (claude-prompt-register-context buffer-file-name "/my/repo")
      (should (equal (claude-prompt--resolve-repo-root) "/my/repo"))
      ;; Popped: a second resolve no longer finds it.
      (should-not (equal (claude-prompt--resolve-repo-root) "/my/repo")))))

(ert-deftest claude-prompt/resolve-falls-back-to-newest-history ()
  "Absent a registered context, the repo is the newest history project."
  (claude-prompt-tests--with-history file
    (let ((claude-prompt--context-table (make-hash-table :test 'equal))
          (claude-prompt-history-file file))
      (with-temp-buffer
        (setq buffer-file-name "/private/tmp/claude-503/claude-prompt-unknown.md")
        (should (equal (claude-prompt--resolve-repo-root) "/elsewhere"))))))


;;; History ring

(ert-deftest claude-prompt/ring-cycles-previous-and-next ()
  "`M-p' walks to older prompts; `M-n' walks back to the original body."
  (with-temp-buffer
    (insert "orig")
    (setq claude-prompt--ring '("a" "b" "c")
          claude-prompt--ring-index -1)
    (claude-prompt-previous)
    (should (equal (buffer-string) "a"))
    (claude-prompt-previous)
    (should (equal (buffer-string) "b"))
    (claude-prompt-next)
    (should (equal (buffer-string) "a"))
    (claude-prompt-next)
    (should (equal (buffer-string) "orig"))
    (should (= claude-prompt--ring-index -1))))

(ert-deftest claude-prompt/ring-bounds ()
  "Cycling past either end signals rather than wrapping."
  (with-temp-buffer
    (insert "orig")
    (setq claude-prompt--ring '("a") claude-prompt--ring-index -1)
    (should-error (claude-prompt-next))
    (claude-prompt-previous)
    (should (equal (buffer-string) "a"))
    (should-error (claude-prompt-previous))))


;;; Recall picker (consult)

(ert-deftest claude-prompt/consult-sources-shape ()
  "Two sources: `[r]' repo (default) and `[g]' global, scoped correctly."
  (claude-prompt-tests--with-history file
    (let* ((sources (claude-prompt--consult-sources "/repo" file))
           (repo (car sources))
           (global (cadr sources)))
      (should (= (length sources) 2))
      (should (eq (plist-get repo :narrow) ?r))
      (should (eq (plist-get repo :default) t))
      (should (equal (plist-get repo :items) '("second")))
      (should (eq (plist-get global :narrow) ?g))
      (should-not (plist-get global :default))
      (should (= (length (plist-get global :items)) 3))
      ;; Candidates carry their originating project for annotation.
      (should (equal (get-text-property 0 'claude-prompt-project
                                        (car (plist-get repo :items)))
                     "/repo__worktrees/x")))))

(ert-deftest claude-prompt/recall-action-inserts ()
  "The source action inserts the selected prompt text."
  (claude-prompt-tests--with-history file
    (let* ((sources (claude-prompt--consult-sources "/repo" file))
           (action (plist-get (car sources) :action))
           (cand (car (plist-get (car sources) :items))))
      (with-temp-buffer
        (funcall action cand)
        (should (equal (buffer-string) "second"))))))


;;; Mode activation

(ert-deftest claude-prompt/setup-enables-mode-on-prompt-file ()
  "Visiting a prompt path enables `claude-prompt-mode' and `with-editor-mode',
binds finish/cancel, and protects against `kill-buffer'."
  (claude-prompt-tests--with-history file
    (let ((claude-prompt-history-file file)
          (claude-prompt--context-table (make-hash-table :test 'equal))
          (with-editor-show-usage nil))
      (with-temp-buffer
        (setq buffer-file-name "/private/tmp/claude-503/claude-prompt-abc.md")
        (unwind-protect
            (progn
              (claude-prompt-setup-check-buffer)
              (should claude-prompt-mode)
              (should (bound-and-true-p with-editor-mode))
              (should (eq (key-binding (kbd "C-c C-c")) #'with-editor-finish))
              (should (eq (key-binding (kbd "C-c C-k")) #'with-editor-cancel))
              (should (eq (key-binding (kbd "M-p")) #'claude-prompt-previous))
              (should (eq (key-binding (kbd "C-r")) #'claude-prompt-recall))
              (should (memq #'with-editor-kill-buffer-noop
                            kill-buffer-query-functions))
              ;; Our richer usage message replaces with-editor's default.
              (should (equal with-editor-usage-message
                             claude-prompt-usage-message)))
          (setq kill-buffer-query-functions nil)
          (set-buffer-modified-p nil))))))

(ert-deftest claude-prompt/setup-activates-on-registered-context ()
  "A wrapper-registered context activates the mode regardless of filename.
This is the primary, wrapper-driven path (no filename pattern needed)."
  (claude-prompt-tests--with-history file
    (let ((claude-prompt-history-file file)
          (claude-prompt--context-table (make-hash-table :test 'equal))
          (with-editor-show-usage nil))
      (with-temp-buffer
        ;; A path that does NOT match the prompt-file pattern.
        (setq buffer-file-name "/tmp/scratch/anything.md")
        (claude-prompt-register-context buffer-file-name "/repo")
        (unwind-protect
            (progn
              (claude-prompt-setup-check-buffer)
              (should claude-prompt-mode)
              (should (equal claude-prompt--repo-root "/repo")))
          (setq kill-buffer-query-functions nil)
          (set-buffer-modified-p nil))))))

(ert-deftest claude-prompt/setup-skips-ordinary-file ()
  "An ordinary markdown file with no registered context does not activate."
  (let ((claude-prompt--context-table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (setq buffer-file-name "/home/me/notes.md")
      (claude-prompt-setup-check-buffer)
      (should-not claude-prompt-mode))))

(provide 'claude-prompt-tests)

;;; claude-prompt-tests.el ends here
