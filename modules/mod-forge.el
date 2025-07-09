;;; mod-forge.el --- Forge configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides LLM-powered pull request creation that automates
;; branch management and generates intelligent PR titles and descriptions.
;;
;; The main entry point is `+forge-create-smart-pullreq` which provides
;; a streamlined workflow:
;;
;; 1. **Branch Detection & Creation**:
;;    - If on main branch: Creates a new feature branch with LLM-generated name
;;    - If on feature branch: Uses existing branch for PR creation
;;    - Automatically stages unstaged files if user confirms
;;
;; 2. **Intelligent Branch Naming**:
;;    - Analyzes recent commits to generate descriptive branch names
;;    - Uses kebab-case format (e.g., "fix-user-authentication")
;;    - Avoids generic names and follows naming conventions
;;
;; 3. **Automated Push Management**:
;;    - Sets upstream tracking for new branches
;;    - Pushes only when there are unpushed commits
;;    - Handles remote branch creation automatically
;;
;; 4. **LLM-Generated PR Content**:
;;    - Analyzes git diff between feature and main branch
;;    - Generates concise, imperative-mood titles (<72 chars)
;;    - Creates markdown-formatted descriptions with context
;;    - Pre-fills forge PR buffer for review before submission
;;
;; 5. **Graceful Fallbacks**:
;;    - Falls back to standard forge PR creation if LLM fails
;;    - Provides helpful error messages for edge cases
;;    - Maintains compatibility with existing forge workflows
;;
;; Usage: Bind `+forge-create-smart-pullreq` to a key or use via the forge
;; dispatch transient (c l). The command works from any git repository state
;; and guides you through the entire PR creation process.

;;; Code:

(require '+corelib)
(require 'forge)
(require 'general)

(autoload 'gptel-request "gptel")

;;; Smart LLM-powered Pull Request Creation

(defvar +forge-pr-llm-prompt "
Generate a pull request title and description for these changes.

Requirements:
- Title: Concise, imperative mood, under 72 characters
- Description: Clear summary of what changed and why
- Use markdown formatting for the description
- Include relevant context about the changes
- Consider both the commit messages and code diff for context

Format your response as:
Title: [your title here]

Description:
[your description here]

Only respond with the title and description, no additional text.
")

(defvar +forge-branch-name-llm-prompt "
Generate a terse, descriptive branch name based on these recent commits.

Requirements:
- Use kebab-case (lowercase with hyphens)
- Max 30 characters
- Be descriptive but concise
- No prefixes like feature/ or fix/
- Avoid generic names like \"update\" or \"changes\"

Recent commits:
%s

Respond with only the branch name, no explanation.
")

(defun +forge-create-smart-pullreq ()
  "Smart PR creation with branch management and LLM generation."
  (interactive)
  (let ((current-branch (magit-get-current-branch))
        (main-branch (magit-main-branch)))
    (if (equal current-branch main-branch)
        (+forge--create-branch-and-pr)
      (+forge--push-and-create-pr current-branch))))

(defun +forge--create-branch-and-pr ()
  "Create new branch with LLM name, then create PR."
  (let* ((main-branch (magit-main-branch))
         (upstream (magit-get-upstream-branch main-branch))
         (unpushed-commits (and upstream
                                (magit-git-lines "log" "--oneline" (concat upstream "..HEAD"))))
         (staged-files (magit-git-lines "diff" "--cached" "--name-only"))
         (modified-files (magit-git-lines "diff" "--name-only"))
         (untracked-files (magit-git-lines "ls-files" "--others" "--exclude-standard")))
    (cond
     ;; Has unpushed commits on main - spin them out
     ((not (null unpushed-commits))
      (+forge--generate-branch-name-and-create))
     ;; No unpushed commits but has staged files - proceed
     ((not (null staged-files))
      (+forge--generate-branch-name-and-create))
     ;; Has unstaged changes - prompt to stage
     ((or modified-files untracked-files)
      (if (y-or-n-p (format "Stage %s files for PR? "
                            (+ (length modified-files) (length untracked-files))))
          (progn
            (when modified-files
              (magit-run-git-async "add" modified-files))
            (when untracked-files
              (magit-run-git-async "add" untracked-files))
            (run-at-time 1.0 nil #'+forge--generate-branch-name-and-create))
        (user-error "Cannot create PR without staged changes")))
     ;; No changes at all
     (t (user-error "No changes to create a branch from")))))

(defun +forge--generate-branch-name-and-create ()
  "Generate branch name with LLM and create branch."
  (let* ((main-branch (magit-main-branch))
         (upstream (magit-get-upstream-branch main-branch))
         (merge-base (and upstream (magit-git-string "merge-base" upstream "HEAD")))
         (commits (if merge-base
                      (magit-git-lines "log" "--oneline" (concat merge-base "..HEAD"))
                    (magit-git-lines "log" "--oneline" "--max-count=5" "HEAD")))
         (spinner (make-progress-reporter "Generating branch name"))
         (timer nil))
    (if (null commits)
        (user-error "No commits found to create branch from")
      (setq timer (run-at-time 0.1 0.1 (lambda () (progress-reporter-update spinner))))
      (gptel-request (format +forge-branch-name-llm-prompt (string-join commits "\n"))
        :callback (lambda (response _)
                    (when timer
                      (cancel-timer timer))
                    (progress-reporter-done spinner)
                    (let ((branch-name (if response
                                           (string-trim response)
                                         (format "branch-%s" (format-time-string "%Y%m%d-%H%M%S")))))
                      (+forge--spinout-and-continue branch-name)))))))

(defun +forge--spinout-and-continue (branch-name)
  "Create branch and continue with push and PR creation."
  (condition-case err
      (progn
        (magit-branch-spinoff branch-name)
        (message "Spun off branch: %s" branch-name)
        (+forge--push-and-create-pr branch-name))
    (error
     (message "Failed to spin off branch '%s': %s" branch-name (error-message-string err))
     (forge-create-pullreq branch-name (magit-main-branch)))))

(defun +forge--push-and-create-pr (branch)
  "Push branch if needed, then create PR."
  (let ((upstream (magit-get-upstream-branch branch))
        (remote (or (magit-get-push-remote branch) "origin")))
    (cond
     ;; No upstream - set it and push
     ((not upstream)
      (let ((spinner (make-progress-reporter (format "Pushing %s with upstream" branch)))
            (timer nil))
        (setq timer (run-at-time 0.1 0.1 (lambda () (progress-reporter-update spinner))))
        ;; Use explicit remote:branch format to avoid upstream mismatch
        (magit-run-git-async "push" "--set-upstream" remote (format "%s:%s" branch branch))
        ;; Wait a bit for push to complete, then create PR
        (run-at-time 2.0 nil
                     (lambda ()
                       (when timer (cancel-timer timer))
                       (progress-reporter-done spinner)
                       (+forge--create-pr-with-llm branch)))))
     ;; Has upstream, check if we need to push
     (t
      (if (magit-git-lines "log" "--oneline" (concat upstream "..HEAD"))
          (progn
            (message "Pushing %s" branch)
            ;; Push to the same-named branch on remote
            (magit-run-git-async "push" remote (format "%s:%s" branch branch))
            (run-at-time 1.5 nil (lambda () (+forge--create-pr-with-llm branch))))
        ;; Already up to date
        (+forge--create-pr-with-llm branch))))))

(defun +forge--create-pr-with-llm (branch)
  "Create PR with LLM-generated content for current branch."
  (let* ((main-branch (magit-main-branch))
         (diff (shell-command-to-string (format "git diff %s..%s" main-branch branch)))
         (commits (shell-command-to-string (format "git log --pretty=format:'%%h %%s%%n%%n%%b' %s..%s" main-branch branch))))
    (if (string-empty-p (string-trim diff))
        (message "No changes between %s and %s" main-branch branch)
      (let ((spinner (make-progress-reporter "Generating PR title and description"))
            (timer nil)
            (context (concat +forge-pr-llm-prompt
                            "\n\nCommits:\n" commits
                            "\n\nDiff:\n" diff)))
        (setq timer (run-at-time 0.1 0.1 (lambda () (progress-reporter-update spinner))))
        (gptel-request context
          :callback (lambda (response _)
                      (when timer
                        (cancel-timer timer))
                      (progress-reporter-done spinner)
                      (if response
                          (+forge--create-pullreq-with-content branch main-branch response)
                        (message "Failed to generate PR content, falling back to regular creation")
                        (forge-create-pullreq branch main-branch))))))))

(defun +forge--create-pullreq-with-content (source target llm-response)
  "Create a pull request with pre-filled content from LLM response."
  (let* ((lines (split-string (string-trim llm-response) "\n"))
         (title-line (seq-find (lambda (line) (string-match-p "^Title:" line)) lines))
         (title (if title-line
                    (string-trim (substring title-line 6))
                  ""))
         (desc-start (seq-position lines "Description:"))
         (description (if desc-start
                          (string-join (seq-drop lines (1+ desc-start)) "\n")
                        llm-response))
         (draft-file (expand-file-name ".git/magit/posts/new-pullreq" (magit-toplevel))))
    ;; Clear any existing buffer for this file to avoid conflict
    (when-let ((existing-buf (find-buffer-visiting draft-file)))
      (with-current-buffer existing-buf
        (set-buffer-modified-p nil))
      (kill-buffer existing-buf))
    ;; Write content directly to the draft file
    (make-directory (file-name-directory draft-file) t)
    (with-temp-file draft-file
      (insert "# " title "\n\n" description))
    ;; Use standard forge API to create PR buffer which will load the draft
    ;; Temporarily override the resume prompt to always resume
    (cl-letf (((symbol-function 'forge--post-resume-p)
               (lambda (file _buffer)
                 (and (file-exists-p file)
                      (> (file-attribute-size (file-attributes file)) 0)))))
      (forge-create-pullreq source target))
    (message "PR edit buffer ready - review and submit with C-c C-c")))


(general-def :keymaps 'forge-topic-list-mode-map :states 'normal
  "q" #'kill-current-buffer)

;; Add keybinding to forge dispatch transient
(with-eval-after-load 'forge-commands
  (transient-append-suffix 'forge-dispatch "c p"
    '("c l" "Smart LLM PR" +forge-create-smart-pullreq)))

(provide 'mod-forge)

;;; mod-forge.el ends here
