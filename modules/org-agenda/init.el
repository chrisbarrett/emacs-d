;;; org-agenda-init.el --- Org agenda configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Org agenda views, habit tracking, and agenda file management.

;;; Code:

(require '+autoloads)

(require '+corelib)

(with-eval-after-load 'org-agenda
  (require 'org-habit)

  ;; Keybindings
  (with-eval-after-load 'general
    (general-def :keymaps 'org-agenda-mode-map :states 'motion
      [remap save-buffer] #'org-save-all-org-buffers
      "J"                 #'org-agenda-goto-date
      "C-n"               #'org-agenda-later
      "C-p"               #'org-agenda-earlier))

  ;; Agenda files
  (setq org-agenda-files (file-name-concat org-directory "org-agenda-files"))
  (setq org-agenda-text-search-extra-files `(agenda-archives ,(file-name-concat org-directory "archive.org")))
  (setq org-agenda-skip-unavailable-files t)

  ;; View configuration
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-show-inherited-tags nil)
  (setq org-agenda-tags-column 0)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-use-time-grid nil)
  (setq org-agenda-show-future-repeats nil)
  (setq org-agenda-inhibit-startup nil)
  (setq org-agenda-search-view-always-boolean t)
  (setq org-agenda-insert-diary-extract-time nil)

  ;; Skip logic
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-agenda-todo-ignore-scheduled 'future)
  (setq org-agenda-ignore-properties '(effort appt))

  ;; Sorting strategy
  (setq org-agenda-sorting-strategy
        '((agenda time-up habit-up priority-down category-up priority-down todo-state-up)
          (todo priority-down category-up scheduled-up)
          (tags priority-down category-up)
          (search category-up)))

  ;; Archive tag
  (setq org-archive-tag "ARCHIVED")

  ;; Clock consistency checks
  (setq org-agenda-clock-report-header "\nClocking")
  (setq org-agenda-clock-consistency-checks
        '(:gap-ok-around ("12:20" "12:40" "4:00")
          :max-duration "10:00"
          :min-duration 0
          :max-gap 0))

  ;; Custom commands
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

  ;; Avoid running possibly expensive ${major-mode}-local-vars-hook while building
  ;; the agenda.
  (autoload '+inhibit-local-var-hooks-a "+hooks")
  (advice-add 'org-get-agenda-file-buffer :around #'+inhibit-local-var-hooks-a)

  ;; Page-break separator for sections
  (setq org-agenda-block-separator (char-to-string ?\f))

  ;; Habit settings
  (setq org-habit-graph-column 72)
  (setq org-habit-today-glyph ?!)
  (setq org-habit-completed-glyph ?*)

  ;; Resize habit graph on agenda mode
  (add-hook 'org-agenda-mode-hook #'+org-habit-resize-graph-h)

  ;; Reveal context around item on TAB
  (add-hook 'org-agenda-after-show-hook
            (lambda ()
              (org-overview)
              (org-reveal)
              (org-fold-show-subtree)
              (org-display-outline-path))))

;; Auto-update agenda files on save in org-mode
(add-hook! 'org-mode-local-vars-hook
  (add-hook 'after-save-hook #'+org-agenda-update-files nil t))

;; Page-break-lines for agenda separators
(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'org-agenda-mode)
  (define-advice org-agenda (:after (&rest _) draw-separator)
    (page-break-lines--update-display-tables))
  (define-advice org-agenda-redo (:after (&rest _) draw-separator)
    (page-break-lines--update-display-tables)))


