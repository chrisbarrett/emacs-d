;;; +beads.el --- Beads issue tracker integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration with bd (beads) issue tracker for picking and managing issues.

;;; Code:

(require 'json)

(defun +beads-format-issue-for-completion (issue)
  "Format ISSUE for display in `completing-read'.
Returns a cons cell (DISPLAY-STRING . ISSUE)."
  (let* ((id (alist-get 'id issue))
         (title (alist-get 'title issue))
         (priority (alist-get 'priority issue))
         (issue-type (alist-get 'issue_type issue))
         (priority-str (pcase priority
                         (0 "🔴")
                         (1 "🟠")
                         (2 "🟡")
                         (3 "🟢")
                         (4 "⚪")
                         (_ "  ")))
         (type-str (pcase issue-type
                     ("bug" "🐛")
                     ("feature" "✨")
                     ("task" "☑️")
                     ("epic" "📘")
                     ("chore" "🔧")
                     (_ "  ")))
         (display (format "%s %s %-12s %s"
                          priority-str
                          type-str
                          id
                          title)))
    (cons display issue)))

(defun +beads--issue-sort-key (issue)
  "Generate a sort key for ISSUE (priority, then issue number).
Returns a cons cell (PRIORITY . NUMBER) for comparison."
  (let* ((id (alist-get 'id issue))
         (priority (alist-get 'priority issue))
         ;; Extract number from issue ID (e.g., "emacs-4" -> 4)
         (number (if (string-match "-\\([0-9]+\\)$" id)
                     (string-to-number (match-string 1 id))
                   0)))
    (cons priority number)))

(defun +beads-pick-issue ()
  "Prompt user to select an issue from beads.
Returns the selected issue alist, or nil if cancelled."
  (let* ((issues-json (shell-command-to-string "bd ready --json"))
         (issues (condition-case nil
                     (json-parse-string issues-json
                                        :object-type 'alist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)
                   (error nil))))
    (unless issues
      (user-error "No ready issues found or bd command failed"))

    ;; Sort by priority (ascending), then by issue number (ascending)
    (setq issues (sort issues
                       (lambda (a b)
                         (let ((key-a (+beads--issue-sort-key a))
                               (key-b (+beads--issue-sort-key b)))
                           (or (< (car key-a) (car key-b))
                               (and (= (car key-a) (car key-b))
                                    (< (cdr key-a) (cdr key-b))))))))

    (let* ((formatted-issues (mapcar #'+beads-format-issue-for-completion issues))
           (completion-table
            (lambda (string pred action)
              (if (eq action 'metadata)
                  `(metadata (display-sort-function . ,#'identity))
                (complete-with-action action formatted-issues string pred))))
           (selection (completing-read "Select issue: "
                                       completion-table
                                       nil t)))
      (when selection
        (cdr (assoc selection formatted-issues))))))

(provide '+beads)

;;; +beads.el ends here
