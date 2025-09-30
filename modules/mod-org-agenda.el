;;; mod-org-agenda.el --- Configuration for org-agenda -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+agenda)
(require '+corelib)
(require 'general)
(require 'org-agenda)
(require 'org-habit)

(general-def :keymaps 'org-agenda-mode-map :states 'motion
  [remap save-buffer] #'org-save-all-org-buffers
  "J" #'org-agenda-goto-date
  "C-n" #'org-agenda-later
  "C-p" #'org-agenda-earlier)

(setq org-agenda-files (file-name-concat org-directory "org-agenda-files"))
(setq org-agenda-text-search-extra-files `(agenda-archives ,(file-name-concat org-directory "archive.org")))
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-search-view-always-boolean t)
(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-tags-column 0)
(setq org-archive-tag "ARCHIVED")
(setq org-agenda-inhibit-startup nil)

(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-span 'day)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-start-day nil)
(setq org-agenda-include-diary nil)

(setq org-agenda-insert-diary-extract-time nil)
(setq org-agenda-show-inherited-tags nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-sorting-strategy
      '((agenda time-up habit-up priority-down category-up priority-down todo-state-up)
        (todo priority-down category-up scheduled-up)
        (tags priority-down category-up)
        (search category-up)))

(setq org-agenda-use-time-grid nil)
(setq org-agenda-show-future-repeats nil)
(setq org-agenda-ignore-properties '(effort appt))
(setq org-agenda-clock-report-header "\nClocking")
(setq org-agenda-clock-consistency-checks
      '(:gap-ok-around ("12:20" "12:40" "4:00")
        :max-duration "10:00"
        :min-duration 0
        :max-gap 0))

(setq org-agenda-custom-commands
      (let ((sections
             '((agenda ""
                ((org-agenda-overriding-header "Agenda")
                 (org-agenda-use-time-grid t)
                 (org-agenda-clockreport-parameter-plist '(:compact t
                                                           :link t
                                                           :maxlevel 3
                                                           :fileskip0 t
                                                           :filetitle t))
                 (org-agenda-skip-function #'+agenda-view-skip-function)
                 (org-super-agenda-groups
                  `((:name "Agenda" :time-grid t)
                    (:name "Forming" :and (:habit t :regexp ,(rx "->")))
                    (:name "French Study" :category "french")
                    (:name "Cooking" :and (:habit t :tag "cooking"))
                    (:name "Chores" :and (:habit t :tag "chore"))
                    (:name "Habits" :habit t)
                    (:name "Birthdays" :category "birthdays")
                    (:name "Delegated" :todo "WAIT")
                    (:name "Tickler" :tag "tickler")))))

               (tags-todo "-tickler-inbox+TODO=\"TODO\""
                          ((org-agenda-overriding-header "Next Actions")
                           (org-agenda-dim-blocked-tasks 'invisible)
                           (org-agenda-skip-function #'+agenda-next-actions-skip-function)))

               (tags-todo "+inbox+TODO=\"TODO\""
                          ((org-agenda-overriding-header "Inbox")
                           (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))

               (todo "WAIT"
                     ((org-agenda-overriding-header "Delegated")
                      (org-agenda-remove-tags nil)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))

               (tags-todo "+TODO=\"PROJECT\""
                          ((org-agenda-overriding-header "Projects"))))))

        `(("p" "personal agenda" ,sections
           ((org-agenda-tag-filter-preset '("-work" "-ignore"))))

          ("w" "work agenda" ,sections
           ((org-agenda-tag-filter-preset (list "-ignore" (format "+%s" (timekeep-work-tag)))))))))



;; Use page-break separator for sections

(setq org-agenda-block-separator (char-to-string ?\f))

(cl-eval-when (compile)
  (require 'page-break-lines))

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'org-agenda-mode)

  (define-advice org-agenda (:after (&rest _) draw-separator)
    (page-break-lines--update-display-tables))

  (define-advice org-agenda-redo (:after (&rest _) draw-separator)
    (page-break-lines--update-display-tables)))


;; Automatically set agenda-files by scanning `org-directory' for files that
;; have todo keywords.
;;
;; This could be made more efficient by computing incrementally, but ripgrep is
;; so fast that I'm not too worried.

(defvar +org--agenda-update-process nil)

(defconst +agenda-files-update-script
  (file-name-concat user-emacs-directory "scripts/update-agenda-files.sh"))

(defun +org-agenda-update-files ()
  (unless (and +org--agenda-update-process (process-live-p +org--agenda-update-process))
    (setq +org--agenda-update-process
          (start-process "update-org-agenda-files" nil +agenda-files-update-script))))

(add-hook! 'org-mode-hook
  (add-hook 'after-save-hook #'+org-agenda-update-files nil t))


;; Reveal context around item on TAB
(add-hook! 'org-agenda-after-show-hook
  (org-overview)
  (org-reveal)
  (org-fold-show-subtree)
  (org-display-outline-path))


;;; org-habit

;; Declare certain tasks in the agenda as 'habits'; these have a graph
;; displayed beside them to help visualise your consistency.

(setq org-habit-graph-column 72)
(setq org-habit-today-glyph ?!)
(setq org-habit-completed-glyph ?*)

(defvar +org-habit-graph-window-ratio 0.2
  "The ratio of the consistency graphs relative to the window width.")

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph.")

(defvar +org-habit-min-width 30
  "Hide the consistency graph if `org-habit-graph-column' is less than this.")

(add-hook 'org-agenda-mode-hook
          (defun +org-habit-resize-graph-h ()
            "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
            (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
                   (preceding-days-ratio (/ org-habit-preceding-days total-days))
                   (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
                   (preceding-days (floor (* graph-width preceding-days-ratio)))
                   (following-days (- graph-width preceding-days))
                   (graph-column (- (window-width) (+ preceding-days following-days)))
                   (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                              (- graph-column +org-habit-graph-padding)
                                            nil)))
              (setq-local org-habit-preceding-days preceding-days)
              (setq-local org-habit-following-days following-days)
              (setq-local org-habit-graph-column graph-column-adjusted))))

(provide 'mod-org-agenda)

;;; mod-org-agenda.el ends here
