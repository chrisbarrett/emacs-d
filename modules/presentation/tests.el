;;; tests.el --- Presentation sessions tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the presentation module.  Tmux interaction is tested by
;; asserting on emitted effects; no live tmux process is invoked.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (let* ((this (or load-file-name buffer-file-name byte-compile-current-file))
         (dir (and this (file-name-directory this))))
    (when dir
      (load (expand-file-name "lib.el" dir) nil t))))

;; Declare defvars from `lib.el' as special so `let'-bindings in tests
;; are treated as dynamic (avoids unused-lexical warnings under
;; `lexical-binding').
(defvar +presentation--sessions)
(defvar +presentation-narrative-major-mode)
(defvar +presentation-spawn-poll-attempts)
(defvar +presentation-spawn-poll-interval)

(defun +presentation-tests--with-clean-frame (frame fn)
  "Save FRAME's presentation parameters, run FN, then restore them."
  (let ((prev-key (frame-parameter frame 'presentation-key))
        (prev-origin (frame-parameter frame 'presentation-origin)))
    (unwind-protect (funcall fn)
      (modify-frame-parameters frame
                               `((presentation-key . ,prev-key)
                                 (presentation-origin . ,prev-origin))))))

(defun +presentation-tests--fake-process-file-buffer (dest)
  "Resolve DEST argument of `process-file' to the buffer to write into."
  (cond ((bufferp dest) dest)
        ((eq dest t) (current-buffer))
        ((consp dest) (when (bufferp (car dest)) (car dest)))))


;;; 2. Effect data types

(ert-deftest +presentation/effect-shell-record ()
  (let ((e (make-+presentation-effect-shell :argv '("tmux" "list-panes"))))
    (should (+presentation-effect-shell-p e))
    (should (equal (+presentation-effect-shell-argv e) '("tmux" "list-panes")))))

(ert-deftest +presentation/effect-elisp-record ()
  (let ((e (make-+presentation-effect-elisp :thunk (lambda () 42))))
    (should (+presentation-effect-elisp-p e))
    (should (= (funcall (+presentation-effect-elisp-thunk e)) 42))))

(ert-deftest +presentation/run-effects-shells-out-and-runs-thunks ()
  "Runner dispatches shell effects via `process-file' and elisp via thunk.
Results are returned in execution order."
  (let* ((calls nil)
         (shell-effect (make-+presentation-effect-shell
                        :argv '("tmux" "list-panes" "-t" "s:1")))
         (elisp-effect (make-+presentation-effect-elisp
                        :thunk (lambda () (push 'thunk calls) 'thunk-result))))
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile dest _display &rest args)
                 (push (cons program args) calls)
                 (let ((target (cond ((bufferp dest) dest)
                                     ((eq dest t) (current-buffer)))))
                   (when target
                     (with-current-buffer target
                       (insert "%pane1\t/dev/ttys001\n"))))
                 0)))
      (let ((results (+presentation--run-effects
                      (list shell-effect elisp-effect))))
        (should (equal results '("%pane1\t/dev/ttys001\n" thunk-result)))
        (should (member 'thunk calls))
        (should (assoc "tmux" calls))))))


;;; 3. Tmux command builders

(ert-deftest +presentation/cmd-list-panes-emits-exact-argv ()
  (let ((e (+presentation--cmd-list-panes "sess" "1")))
    (should (+presentation-effect-shell-p e))
    (should (equal (+presentation-effect-shell-argv e)
                   '("tmux" "list-panes"
                     "-t" "sess:1"
                     "-F" "#{pane_id}\t#{pane_tty}")))))

(ert-deftest +presentation/cmd-split-window-horizontal ()
  (let ((e (+presentation--cmd-split-window "sess" "1" 'horizontal "/tmp/sock")))
    (should (equal (+presentation-effect-shell-argv e)
                   '("tmux" "split-window" "-h"
                     "-t" "sess:1"
                     "--" "emacsclient" "-t" "-s" "/tmp/sock")))))

(ert-deftest +presentation/cmd-split-window-vertical ()
  (let ((e (+presentation--cmd-split-window "sess" "1" 'vertical "/tmp/sock")))
    (should (equal (+presentation-effect-shell-argv e)
                   '("tmux" "split-window" "-v"
                     "-t" "sess:1"
                     "--" "emacsclient" "-t" "-s" "/tmp/sock")))))

(ert-deftest +presentation/cmd-kill-pane ()
  (let ((e (+presentation--cmd-kill-pane "%42")))
    (should (equal (+presentation-effect-shell-argv e)
                   '("tmux" "kill-pane" "-t" "%42")))))

(ert-deftest +presentation/parse-list-panes-output ()
  (should (equal
           (+presentation--parse-list-panes-output
            "%1\t/dev/ttys001\n%2\t/dev/ttys002\n")
           '(("%1" . "/dev/ttys001")
             ("%2" . "/dev/ttys002"))))
  (should (equal (+presentation--parse-list-panes-output "") nil))
  (should (equal (+presentation--parse-list-panes-output nil) nil)))

(ert-deftest +presentation/diff-panes-returns-only-new ()
  (let ((before '(("%1" . "/dev/ttys001")))
        (after  '(("%1" . "/dev/ttys001") ("%2" . "/dev/ttys002"))))
    (should (equal (+presentation--diff-panes before after)
                   '(("%2" . "/dev/ttys002"))))))


;;; 4. State store + key generation

(ert-deftest +presentation/make-key-is-fresh-each-call ()
  (let ((a (+presentation--make-key))
        (b (+presentation--make-key)))
    (should (stringp a))
    (should (stringp b))
    (should-not (equal a b))))

(ert-deftest +presentation/register-session-stores-and-tags-frame ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (key "test-key"))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (+presentation--register-session
        key (list :frame frame :origin 'created :worktree "/tmp"))
       (should (equal (plist-get (gethash key +presentation--sessions) :worktree)
                      "/tmp"))
       (should (equal (frame-parameter frame 'presentation-key) key))
       (should (eq (frame-parameter frame 'presentation-origin) 'created))))))

(ert-deftest +presentation/get-session-returns-plist-for-known-key ()
  (let ((+presentation--sessions (make-hash-table :test 'equal)))
    (puthash "k1" (list :origin 'reused) +presentation--sessions)
    (should (eq (plist-get (+presentation--get-session "k1") :origin) 'reused))))

(ert-deftest +presentation/get-session-errors-for-unknown-key ()
  (let ((+presentation--sessions (make-hash-table :test 'equal)))
    (should-error (+presentation--get-session "nope") :type 'user-error)))


;;; 5. Frame discovery

(ert-deftest +presentation/find-frame-by-tty-matches-on-frame-parameter ()
  "Faked `frame-list'/`frame-parameter' return the matching frame."
  (let* ((f-a (cons 'frame "a"))
         (f-b (cons 'frame "b"))
         (params `((,f-a . ((tty . "/dev/ttys001")))
                   (,f-b . ((tty . "/dev/ttys002"))))))
    (cl-letf (((symbol-function 'frame-list) (lambda () (list f-a f-b)))
              ((symbol-function 'frame-parameter)
               (lambda (f param)
                 (alist-get param (alist-get f params nil nil #'eq)))))
      (should (eq (+presentation--find-frame-by-tty "/dev/ttys002") f-b))
      (should (null (+presentation--find-frame-by-tty "/dev/ttys999"))))))

(ert-deftest +presentation/find-existing-frame-joins-panes-and-frames ()
  (let* ((f (cons 'frame "match")))
    (cl-letf (((symbol-function 'frame-list) (lambda () (list f)))
              ((symbol-function 'frame-parameter)
               (lambda (frame param)
                 (when (and (eq frame f) (eq param 'tty)) "/dev/ttys003"))))
      (should (eq (+presentation--find-existing-frame
                   '(("%1" . "/dev/ttys001")
                     ("%9" . "/dev/ttys003")))
                  f))
      (should (null (+presentation--find-existing-frame
                     '(("%1" . "/dev/ttys001"))))))))


;;; 6. Splash buffer

(ert-deftest +presentation/make-splash-buffer-contents ()
  (let* ((key "abc")
         (buf (+presentation--make-splash-buffer key "/tmp/wt")))
    (unwind-protect
        (with-current-buffer buf
          (should (equal (buffer-name) "*presentation: abc*"))
          (should (string-match-p (regexp-quote key) (buffer-string)))
          (should (string-match-p (regexp-quote "/tmp/wt") (buffer-string)))
          (should (string-match-p "Awaiting first slide" (buffer-string))))
      (kill-buffer buf))))


;;; 7. Narrative slide rendering

(ert-deftest +presentation/render-slide-narrative-replaces-and-sets-mode ()
  (let ((buf (generate-new-buffer "*present-test*"))
        (+presentation-narrative-major-mode 'fundamental-mode))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "stale"))
          (+presentation--render-slide
           buf '(:kind "narrative" :markdown "# Hello"))
          (with-current-buffer buf
            (should (equal (buffer-string) "# Hello"))
            (should (eq major-mode 'fundamental-mode))))
      (kill-buffer buf))))


;;; 8. Reuse path planner

(ert-deftest +presentation/plan-reuse-emits-elisp-effect ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (effects (+presentation--plan-reuse frame "k-reuse" "/tmp/wt")))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (should (= (length effects) 1))
       (should (+presentation-effect-elisp-p (car effects)))
       (cl-letf (((symbol-function 'current-window-configuration)
                  (lambda () 'fake-wc))
                 ((symbol-function 'set-register)
                  (lambda (_r _v) nil)))
         (funcall (+presentation-effect-elisp-thunk (car effects)))
         (let ((sess (gethash "k-reuse" +presentation--sessions)))
           (should (eq (plist-get sess :origin) 'reused))
           (should (eq (plist-get sess :saved-config) 'fake-wc))
           (should (null (plist-get sess :tmux-pane)))
           (should (eq (plist-get sess :frame) frame))))))))

(ert-deftest +presentation/plan-reuse-pushes-config-to-register-P ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (effects (+presentation--plan-reuse frame "k-reg" "/tmp/wt"))
         (registered nil))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (cl-letf (((symbol-function 'current-window-configuration)
                  (lambda () 'fake-wc))
                 ((symbol-function 'set-register)
                  (lambda (r v) (push (cons r v) registered))))
         (funcall (+presentation-effect-elisp-thunk (car effects)))
         (should (assoc ?P registered)))))))


;;; 9. Spawn path planner

(ert-deftest +presentation/plan-spawn-effect-order ()
  "Plan-spawn emits, in order: list-panes, split-window, poll-elisp, tag-elisp."
  (let* ((effects (+presentation--plan-spawn
                   "k-spawn" "sess" "1" 'horizontal "/tmp/sock" "/tmp/wt")))
    (should (= (length effects) 4))
    (should (+presentation-effect-shell-p (nth 0 effects)))
    (should (equal (+presentation-effect-shell-argv (nth 0 effects))
                   '("tmux" "list-panes" "-t" "sess:1"
                     "-F" "#{pane_id}\t#{pane_tty}")))
    (should (+presentation-effect-shell-p (nth 1 effects)))
    (should (equal (car (+presentation-effect-shell-argv (nth 1 effects)))
                   "tmux"))
    (should (member "split-window" (+presentation-effect-shell-argv (nth 1 effects))))
    (should (+presentation-effect-elisp-p (nth 2 effects)))
    (should (+presentation-effect-elisp-p (nth 3 effects)))))

(ert-deftest +presentation/diff-panes-after-split-discovers-new-pane ()
  "Post-split diff picks the pane that's in `after' but not in `before'."
  (let ((before '(("%1" . "/dev/ttys001")))
        (after  '(("%1" . "/dev/ttys001") ("%7" . "/dev/ttys017"))))
    (should (equal (car (+presentation--diff-panes before after))
                   '("%7" . "/dev/ttys017")))))


;;; 10. start_presentation entry point

(ert-deftest +presentation/start-takes-reuse-path-when-frame-exists ()
  "When tmux discovery yields an existing frame, start uses the reuse path."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame)))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (cl-letf
           (((symbol-function 'process-file)
             (lambda (_program _infile dest _display &rest _args)
               (when-let* ((b (+presentation-tests--fake-process-file-buffer dest)))
                 (with-current-buffer b (insert "%9\t/dev/ttys099\n")))
               0))
            ((symbol-function 'frame-list) (lambda () (list frame)))
            ((symbol-function 'frame-parameter)
             (lambda (f param)
               (cond
                ((eq param 'tty) (when (eq f frame) "/dev/ttys099"))
                (t nil))))
            ((symbol-function 'current-window-configuration)
             (lambda () 'fake-wc))
            ((symbol-function 'set-register) (lambda (_r _v) nil))
            ((symbol-function 'switch-to-buffer) (lambda (_b) nil)))
         (let ((key (+presentation-start
                     :worktree "/tmp/wt"
                     :tmux-session "sess"
                     :tmux-window "1"
                     :socket "/tmp/sock")))
           (should (stringp key))
           (let ((sess (gethash key +presentation--sessions)))
             (should (eq (plist-get sess :origin) 'reused)))))))))

(ert-deftest +presentation/start-takes-spawn-path-when-no-frame ()
  "With no existing emacsclient frame the spawn path is taken."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (split-happened nil)
         (spawned-frame (selected-frame))
         (+presentation-spawn-poll-attempts 2)
         (+presentation-spawn-poll-interval 0))
    (+presentation-tests--with-clean-frame
     spawned-frame
     (lambda ()
       (cl-letf
           (((symbol-function 'process-file)
             (lambda (_program _infile dest _display &rest args)
               (when-let* ((b (+presentation-tests--fake-process-file-buffer dest)))
                 (with-current-buffer b
                   (cond
                    ((member "split-window" args)
                     (setq split-happened t))
                    ((and (member "list-panes" args) (not split-happened))
                     (insert "%1\t/dev/ttys001\n"))
                    ((member "list-panes" args)
                     (insert "%1\t/dev/ttys001\n%2\t/dev/ttys002\n"))
                    (t nil))))
               0))
            ((symbol-function 'frame-list) (lambda () (list spawned-frame)))
            ((symbol-function 'frame-parameter)
             (lambda (f param)
               (when (and (eq f spawned-frame) (eq param 'tty))
                 "/dev/ttys002")))
            ((symbol-function 'switch-to-buffer) (lambda (_b) nil)))
         (let ((key (+presentation-start
                     :worktree "/tmp/wt"
                     :tmux-session "sess"
                     :tmux-window "1"
                     :socket "/tmp/sock")))
           (should (stringp key))
           (let ((sess (gethash key +presentation--sessions)))
             (should (eq (plist-get sess :origin) 'created))
             (should (equal (plist-get sess :tmux-pane) "%2")))))))))

(ert-deftest +presentation/start-renders-initial-slide-into-splash-buffer ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (+presentation-narrative-major-mode 'fundamental-mode))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (cl-letf
           (((symbol-function 'process-file)
             (lambda (_program _infile dest _display &rest _args)
               (when-let* ((b (+presentation-tests--fake-process-file-buffer dest)))
                 (with-current-buffer b (insert "%9\t/dev/ttys099\n")))
               0))
            ((symbol-function 'frame-list) (lambda () (list frame)))
            ((symbol-function 'frame-parameter)
             (lambda (f param)
               (when (and (eq f frame) (eq param 'tty)) "/dev/ttys099")))
            ((symbol-function 'current-window-configuration)
             (lambda () 'fake-wc))
            ((symbol-function 'set-register) (lambda (_r _v) nil))
            ((symbol-function 'switch-to-buffer)
             (lambda (_b) nil)))
         (let ((key (+presentation-start
                     :worktree "/tmp/wt"
                     :tmux-session "sess"
                     :tmux-window "1"
                     :socket "/tmp/sock"
                     :initial-slide '(:kind "narrative" :markdown "# Slide one"))))
           (should (stringp key))
           (should (bufferp (get-buffer (format "*presentation: %s*" key))))
           (with-current-buffer (get-buffer (format "*presentation: %s*" key))
             (should (string-match-p "# Slide one" (buffer-string)))
             (should-not (string-match-p "Awaiting first slide"
                                         (buffer-string))))))))))


;;; 11. end_presentation entry point

(ert-deftest +presentation/end-on-reused-restores-window-config-and-drops-state ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (calls nil))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (puthash "k-end-r"
                (list :frame frame :origin 'reused
                      :saved-config 'fake-wc
                      :tmux-pane nil
                      :worktree "/tmp/wt"
                      :started-at 0)
                +presentation--sessions)
       (modify-frame-parameters frame
                                '((presentation-key . "k-end-r")
                                  (presentation-origin . reused)))
       (cl-letf (((symbol-function 'set-window-configuration)
                  (lambda (wc) (push (cons 'set-wc wc) calls))))
         (+presentation-end "k-end-r")
         (should (assoc 'set-wc calls))
         (should (eq (cdar calls) 'fake-wc))
         (should (null (gethash "k-end-r" +presentation--sessions)))
         (should (null (frame-parameter frame 'presentation-key))))))))

(ert-deftest +presentation/end-on-created-emits-kill-pane ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (calls nil))
    (puthash "k-end-c"
             (list :frame nil :origin 'created
                   :saved-config nil
                   :tmux-pane "%42"
                   :worktree "/tmp/wt"
                   :started-at 0)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile _dest _display &rest args)
                 (push (cons program args) calls)
                 0)))
      (+presentation-end "k-end-c")
      (let ((tmux-call (assoc "tmux" calls)))
        (should tmux-call)
        (should (member "kill-pane" tmux-call))
        (should (member "%42" tmux-call))))))

(ert-deftest +presentation/end-on-unknown-key-signals-user-error ()
  (let ((+presentation--sessions (make-hash-table :test 'equal)))
    (should-error (+presentation-end "no-such-key") :type 'user-error)))


;;; 12. Frame-deletion cleanup hook

(ert-deftest +presentation/frame-deleted-h-clears-matching-entry ()
  "Hook matches sessions by `:frame' identity (frame parameters are
unreliable: tty-client disconnects fire `delete-frame-functions' after
the frame's parameters have already been wiped)."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame)))
    (puthash "k-del" (list :frame frame :origin 'created)
             +presentation--sessions)
    (+presentation--frame-deleted-h frame)
    (should (null (gethash "k-del" +presentation--sessions)))))

(ert-deftest +presentation/frame-deleted-h-noop-on-non-presentation-frame ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame)))
    (puthash "k-other" (list :frame 'some-other-frame :origin 'created)
             +presentation--sessions)
    (+presentation--frame-deleted-h frame)
    (should (gethash "k-other" +presentation--sessions))))


;;; 13. display-buffer protection

(ert-deftest +presentation/display-buffer-suppress-predicate ()
  "When the selected frame carries `presentation-key' the suppress
predicate returns non-nil."
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_f param)
               (when (eq param 'presentation-key) "fake"))))
    (should (+presentation--display-buffer-suppress-p (current-buffer) nil)))
  (cl-letf (((symbol-function 'frame-parameter) (lambda (_f _p) nil)))
    (should-not
     (+presentation--display-buffer-suppress-p (current-buffer) nil))))


;;; 14. MCP tool registration

(ert-deftest +presentation/mcp-tools-registered-after-init ()
  (skip-unless (require 'claude-code-ide-mcp-server nil t))
  (let ((init (expand-file-name "modules/presentation/init.el"
                                user-emacs-directory)))
    (skip-unless (file-exists-p init))
    (condition-case nil (load init nil t) (error nil))
    (when (fboundp 'claude-code-ide-mcp-server-get-tool-names)
      (let ((names (claude-code-ide-mcp-server-get-tool-names)))
        (should (member "start_presentation" names))
        (should (member "end_presentation" names))))))


;;; get_presentation info

(ert-deftest +presentation/info-returns-public-fields ()
  "`+presentation-info' returns an alist suitable for JSON encoding."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame)))
    (puthash "k-info"
             (list :frame frame :origin 'created
                   :saved-config nil :tmux-pane "%99"
                   :worktree "/tmp/wt" :started-at 1700000000.0)
             +presentation--sessions)
    (let ((info (+presentation-info "k-info")))
      (should (equal (alist-get 'origin info) "created"))
      (should (eq (alist-get 'frame_live info) t))
      (should (equal (alist-get 'tmux_pane info) "%99"))
      (should (equal (alist-get 'worktree info) "/tmp/wt"))
      (should (numberp (alist-get 'started_at info)))
      (should (equal (alist-get 'key info) "k-info")))))

(ert-deftest +presentation/info-reports-dead-frame ()
  (let ((+presentation--sessions (make-hash-table :test 'equal)))
    (puthash "k-dead"
             (list :frame nil :origin 'reused :tmux-pane nil
                   :worktree "/tmp/wt" :started-at 0)
             +presentation--sessions)
    (let ((info (+presentation-info "k-dead")))
      (should (eq (alist-get 'frame_live info) :json-false)))))

(ert-deftest +presentation/info-errors-on-unknown-key ()
  (let ((+presentation--sessions (make-hash-table :test 'equal)))
    (should-error (+presentation-info "no-such") :type 'user-error)))


;;; alist-to-plist helper

(ert-deftest +presentation/alist-to-plist-symbol-keys ()
  (should (equal (+presentation--alist-to-plist '((kind . "narrative")
                                                  (markdown . "# Hi")))
                 '(:kind "narrative" :markdown "# Hi"))))

;;; tests.el ends here
