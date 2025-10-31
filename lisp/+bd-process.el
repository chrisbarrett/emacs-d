;;; +bd-process.el --- Process management and logging for bd commands -*- lexical-binding: t; -*-

;;; Commentary:

;; This library provides asynchronous process execution for bd commands
;; with a magit-process-style log buffer for debugging.

;;; Code:

(require 'dash)
(require 'seq)
(require 'map)
(require 'magit-section)

(defvar +bd-program "bd")

;;; Log Buffer

(defvar +bd-process-log-buffer-name "*bd-process*"
  "Name of the buffer used for logging bd command execution.")

(define-derived-mode +bd-process-log-mode magit-section-mode "BD-Process"
  "Major mode for viewing bd command execution log."
  :group 'bd-process
  (setq-local magit-imenu-item-types 'bd-process)
  (setq buffer-read-only t)
  (hack-dir-local-variables-non-file-buffer))

(defun +bd-process-log-buffer ()
  "Get or create the bd-process log buffer."
  (let ((buffer (get-buffer-create +bd-process-log-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode '+bd-process-log-mode)
        (+bd-process-log-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-insert-section (bd-process-logbuf)
            (insert "\n")))))
    buffer))

;;;###autoload
(defun +bd-process-show-log ()
  "Display the bd-process log buffer."
  (interactive)
  (pop-to-buffer (+bd-process-log-buffer)))

;;; Command Queue

(defvar +bd-process-commands-queue nil
  "FIFO queue of command groups received by `+bd-process-call'.

Each element is a list of command plists, as received by the function.")

(defun +bd-process-call (&rest commands)
  "Schedule COMMANDS for asynchronous execution.

Each element of COMMANDS is a plist, with the following keys:

:bd - the bd subcommand to run, e.g. `create' (mutually exclusive with :command)

:command - arbitrary command to run instead of bd (mutually exclusive with :bd)

:arguments (optional) - list of positional arguments

:flags (optional) - alist of flag names (symbols) to values.
  Values can be atoms or lists. Lists are joined with commas.
  Underscores in flag names are converted to dashes.

:stdin (optional) - string to send to the process's stdin

:callback (optional) - callback to run with the string output of
successful execution

COMMANDS will be called asynchronously, but in sequence - for each
command it is guaranteed that the proceeding one completed with a
non-zero exit code. If any command fails, the subsequent commands will
not be called."
  (when commands
    (push commands +bd-process-commands-queue)
    (when (eq 1 (length +bd-process-commands-queue))
      (+bd-process--pull-work-from-calls-queue))
    t))

(defun +bd-process--pull-work-from-calls-queue ()
  "Take some work off the calls queue and run it."
  (when-let* ((commands (-last-item +bd-process-commands-queue)))
    (setq +bd-process-commands-queue (butlast +bd-process-commands-queue))
    (+bd-process--run-sequentially commands)))

(defvar +bd-proc-buffer " *bd-proc-buffer*")

(defun +bd-process-command-to-process-args (command)
  "Convert COMMAND plist to process arguments list.
COMMAND is a plist with :bd or :command, :arguments, and :flags.
If :bd is present, runs `bd <subcommand>'.
If :command is present, runs that command directly.
Returns a list suitable for passing to `make-process' :command parameter."
  (let* ((bd-subcommand (plist-get command :bd))
         (arbitrary-command (plist-get command :command))
         (arguments (plist-get command :arguments))
         (flags (plist-get command :flags))
         (args (cond
                (bd-subcommand (list +bd-program bd-subcommand))
                (arbitrary-command (list arbitrary-command))
                (t (error "Command must specify either :bd or :command")))))
    ;; Add positional arguments
    (when arguments
      (setq args (append args arguments)))
    ;; Add flag arguments
    (when flags
      (dolist (entry flags)
        (let ((key (car entry))
              (value (cdr entry)))
          (when value
            ;; Convert key to flag name (replace _ with -)
            (let ((flag (concat "--" (replace-regexp-in-string "_" "-" (symbol-name key)))))
              (cond
               ;; Lists: join with commas
               ((listp value)
                (setq args (append args (list flag (string-join value ",")))))
               ;; Numbers: convert to string
               ((numberp value)
                (setq args (append args (list flag (number-to-string value)))))
               ;; Strings: use as-is
               ((stringp value)
                (setq args (append args (list flag value))))))))))
    args))

(defun +bd-process--run-sequentially (commands)
  "Run COMMANDS sequentially.

If a command in the sequence fails, subsequent commands are not run.

Schedules the next sequence of commands once there's no more to run."
  (pcase commands
    (`()
     (+bd-process--pull-work-from-calls-queue))

    (`(,cur-command . ,next-commands)
     (pcase-let* (((map :callback :stdin) cur-command)
                  (buf (get-buffer-create +bd-proc-buffer))
                  (process-args (+bd-process-command-to-process-args cur-command))
                  (process-sentinel
                   (lambda (proc status)
                     (condition-case err
                         (let ((exit-code (process-exit-status proc)))
                           (if (zerop exit-code)
                               (progn
                                 (+bd-process-log--complete-command proc buf)
                                 (when callback
                                   (with-current-buffer buf
                                     (funcall callback (string-trim (buffer-string)))))
                                 (+bd-process--run-sequentially next-commands))
                             (+bd-process-log--fail-command proc buf status)
                             (+bd-process--pull-work-from-calls-queue)))
                       (error
                        (+bd-process-log--fail-command proc buf status)
                        (signal (car err) (cdr err)))))))
       (with-current-buffer buf
         (erase-buffer))
       (let ((proc (make-process :name "bd"
                                 :buffer buf
                                 :command process-args
                                 :connection-type 'pipe
                                 :sentinel process-sentinel)))
         (+bd-process-log--start-command cur-command proc)
         (when stdin
           (process-send-string proc stdin)
           (process-send-eof proc)))))))


;; Log buffer implementation using magit-section

(defvar-local +bd-process-log--section-cache nil
  "Alist mapping process objects to their magit-section objects.")

(defun +bd-process-log--start-command (command proc)
  "Log the start of a COMMAND execution.

COMMAND is the command plist, PROC is the process object, PROC-BUF is
the process buffer."
  (with-current-buffer (+bd-process-log-buffer)
    (let ((inhibit-read-only t)
          (magit-insert-section--current nil)
          (magit-insert-section--parent magit-root-section)
          (magit-insert-section--oldroot nil))
      (goto-char (1- (point-max)))
      (let* ((bd-subcommand (plist-get command :bd))
             (arbitrary-command (plist-get command :command))
             (cmd-display (if bd-subcommand
                              (format "%s %s" +bd-program bd-subcommand)
                            arbitrary-command))
             (arguments (plist-get command :arguments))
             (flags (plist-get command :flags))
             (section
              (magit-insert-section (bd-process)
                ;; Insert "run" marker initially
                (insert "run ")
                (insert (format-time-string "%Y-%m-%d %H:%M:%S") " ")
                (magit-insert-heading
                  (propertize cmd-display
                              'font-lock-face 'magit-section-heading))
                ;; Insert structured input
                (when (or arguments flags)
                  (insert (propertize "Input:\n" 'font-lock-face 'magit-section-secondary-heading))
                  ;; Show positional arguments first
                  (when arguments
                    (insert "  " (string-join arguments " ") "\n"))
                  ;; Show flags as key: value
                  (when flags
                    (dolist (entry flags)
                      (let ((key (car entry))
                            (value (cdr entry)))
                        (when value
                          (insert "  " (symbol-name key) ": ")
                          (insert (cond
                                   ((listp value) (string-join value ", "))
                                   ((numberp value) (number-to-string value))
                                   (t value)))
                          (insert "\n")))))
                  (insert "\n"))
                (insert "\n"))))
        ;; Store section for later updates
        (push (cons proc section) +bd-process-log--section-cache)))))

(defun +bd-process-log--complete-command (proc proc-buf)
  "Log successful completion of process PROC.
PROC-BUF is the buffer containing the process output."
  (when-let* ((section (alist-get proc +bd-process-log--section-cache)))
    (with-current-buffer (+bd-process-log-buffer)
      (let ((inhibit-read-only t)
            (output (with-current-buffer proc-buf (buffer-string))))
        (save-excursion
          (goto-char (oref section start))
          ;; Replace "run" with exit code
          (when (looking-at (rx "run "))
            (delete-char 4)
            (insert (propertize "  0 " 'font-lock-face 'magit-process-ok)))
          ;; Insert output if non-empty
          (when (and output (not (string-empty-p output)))
            (goto-char (oref section end))
            (forward-line -1)  ; Before the trailing newline
            (insert (propertize "Output:\n" 'font-lock-face 'magit-section-secondary-heading))
            (insert output)
            (unless (string-suffix-p "\n" output)
              (insert "\n"))))))
    ;; Remove from cache
    (setq +bd-process-log--section-cache
          (assq-delete-all proc +bd-process-log--section-cache))))

(defun +bd-process-log--fail-command (proc proc-buf status)
  "Log failure of process PROC.
PROC-BUF is the buffer containing the process output.
STATUS is the status string from the sentinel."
  (when-let* ((section (alist-get proc +bd-process-log--section-cache)))
    (with-current-buffer (+bd-process-log-buffer)
      (let ((inhibit-read-only t)
            (exit-code (process-exit-status proc))
            (output (with-current-buffer proc-buf (buffer-string))))
        (save-excursion
          (goto-char (oref section start))
          ;; Replace "run" with exit code
          (when (looking-at (rx "run "))
            (delete-char 4)
            (insert (propertize (format "%3d " exit-code)
                                'font-lock-face 'magit-process-ng)))
          ;; Insert failure info
          (goto-char (oref section end))
          (forward-line -1)  ; Before the trailing newline
          (insert (propertize (format "Status: %s\n" (string-trim status))
                              'font-lock-face 'error))
          ;; Insert output if non-empty
          (when (and output (not (string-empty-p output)))
            (insert (propertize "Output:\n" 'font-lock-face 'magit-section-secondary-heading))
            (insert output)
            (unless (string-suffix-p "\n" output)
              (insert "\n"))))))
    ;; Remove from cache
    (setq +bd-process-log--section-cache
          (assq-delete-all proc +bd-process-log--section-cache))))

(provide '+bd-process)

;;; +bd-process.el ends here
