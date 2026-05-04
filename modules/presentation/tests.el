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
(defvar +presentation--effect-results)
(defvar +presentation-mode-map)
(defvar +presentation--session-key)

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

(ert-deftest +presentation/render-narrative-writes-into-key-buffer ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (+presentation-narrative-major-mode 'fundamental-mode)
         (key "narr-key"))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck [] :current-slide-index nil)
             +presentation--sessions)
    (let ((buf (+presentation--render-narrative
                key '(:kind "narrative" :markdown "# Hello"))))
      (unwind-protect
          (with-current-buffer buf
            (should (equal (buffer-name) (format "*presentation: %s*" key)))
            (should (string-match-p "# Hello" (buffer-string)))
            (should (eq major-mode 'fundamental-mode)))
        (kill-buffer buf)))))


;;; 8. Reuse path planner

(ert-deftest +presentation/plan-reuse-emits-elisp-effect ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (effects (+presentation--plan-reuse frame "k-reuse" "/tmp/wt"
                                              "sess" "1")))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (should (= (length effects) 2))
       (should (+presentation-effect-shell-p (nth 0 effects)))
       (should (+presentation-effect-elisp-p (nth 1 effects)))
       (cl-letf (((symbol-function 'current-window-configuration)
                  (lambda () 'fake-wc))
                 ((symbol-function 'set-register)
                  (lambda (_r _v) nil)))
         (let ((+presentation--effect-results '("L0")))
           (funcall (+presentation-effect-elisp-thunk (nth 1 effects))))
         (let ((sess (gethash "k-reuse" +presentation--sessions)))
           (should (eq (plist-get sess :origin) 'reused))
           (should (eq (plist-get sess :saved-config) 'fake-wc))
           (should (null (plist-get sess :tmux-pane)))
           (should (eq (plist-get sess :frame) frame))))))))

(ert-deftest +presentation/plan-reuse-pushes-config-to-register-P ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (effects (+presentation--plan-reuse frame "k-reg" "/tmp/wt"
                                              "sess" "1"))
         (registered nil))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (cl-letf (((symbol-function 'current-window-configuration)
                  (lambda () 'fake-wc))
                 ((symbol-function 'set-register)
                  (lambda (r v) (push (cons r v) registered))))
         (let ((+presentation--effect-results '("L0")))
           (funcall (+presentation-effect-elisp-thunk (nth 1 effects))))
         (should (assoc ?P registered)))))))


;;; 9. Spawn path planner

(ert-deftest +presentation/plan-spawn-effect-order ()
  "Plan-spawn emits, in order: display-message (capture window layout),
list-panes, split-window, poll-elisp, tag-elisp."
  (let* ((effects (+presentation--plan-spawn
                   "k-spawn" "sess" "1" 'horizontal "/tmp/sock" "/tmp/wt")))
    (should (= (length effects) 5))
    (should (+presentation-effect-shell-p (nth 0 effects)))
    (should (equal (+presentation-effect-shell-argv (nth 0 effects))
                   '("tmux" "display-message" "-p"
                     "-t" "sess:1" "#{window_layout}")))
    (should (+presentation-effect-shell-p (nth 1 effects)))
    (should (equal (+presentation-effect-shell-argv (nth 1 effects))
                   '("tmux" "list-panes" "-t" "sess:1"
                     "-F" "#{pane_id}\t#{pane_tty}")))
    (should (+presentation-effect-shell-p (nth 2 effects)))
    (should (member "split-window"
                    (+presentation-effect-shell-argv (nth 2 effects))))
    (should (+presentation-effect-elisp-p (nth 3 effects)))
    (should (+presentation-effect-elisp-p (nth 4 effects)))))

(ert-deftest +presentation/plan-spawn-captures-window-layout-into-session ()
  "Running plan-spawn end-to-end stashes the captured layout on the
session plist as `:tmux-saved-layout'."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (+presentation-spawn-poll-attempts 2)
         (+presentation-spawn-poll-interval 0)
         (split-happened nil)
         (frame (selected-frame))
         (effects (+presentation--plan-spawn
                   "k-cap-s" "sess" "1" 'horizontal "/tmp/sock" "/tmp/wt")))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (cl-letf
           (((symbol-function 'process-file)
             (lambda (_program _infile dest _display &rest args)
               (when-let* ((b (+presentation-tests--fake-process-file-buffer
                               dest)))
                 (with-current-buffer b
                   (cond
                    ((member "display-message" args)
                     (insert "saved-layout-foo"))
                    ((member "split-window" args)
                     (setq split-happened t))
                    ((and (member "list-panes" args) (not split-happened))
                     (insert "%1\t/dev/ttys001\n"))
                    ((member "list-panes" args)
                     (insert "%1\t/dev/ttys001\n%2\t/dev/ttys002\n"))
                    (t nil))))
               0))
            ((symbol-function 'frame-list) (lambda () (list frame)))
            ((symbol-function 'frame-parameter)
             (lambda (f param)
               (when (and (eq f frame) (eq param 'tty)) "/dev/ttys002"))))
         (+presentation--run-effects effects)
         (let ((sess (gethash "k-cap-s" +presentation--sessions)))
           (should (equal (plist-get sess :tmux-saved-layout)
                          "saved-layout-foo"))
           (should (equal (plist-get sess :tmux-session) "sess"))
           (should (equal (plist-get sess :tmux-window) "1"))))))))

(ert-deftest +presentation/plan-reuse-emits-window-layout-capture-first ()
  (let* ((frame (selected-frame))
         (effects (+presentation--plan-reuse frame "k-reuse-cap" "/tmp/wt"
                                              "sess" "1")))
    (should (>= (length effects) 2))
    (let ((first (nth 0 effects)))
      (should (+presentation-effect-shell-p first))
      (should (equal (+presentation-effect-shell-argv first)
                     '("tmux" "display-message" "-p"
                       "-t" "sess:1" "#{window_layout}"))))))

(ert-deftest +presentation/plan-reuse-captures-window-layout-into-session ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (effects (+presentation--plan-reuse frame "k-cap-r" "/tmp/wt"
                                              "sess" "1")))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (cl-letf
           (((symbol-function 'process-file)
             (lambda (_program _infile dest _display &rest _args)
               (when-let* ((b (+presentation-tests--fake-process-file-buffer
                               dest)))
                 (with-current-buffer b (insert "saved-layout-bar")))
               0))
            ((symbol-function 'current-window-configuration)
             (lambda () 'fake-wc))
            ((symbol-function 'set-register) (lambda (_r _v) nil)))
         (+presentation--run-effects effects)
         (let ((sess (gethash "k-cap-r" +presentation--sessions)))
           (should (equal (plist-get sess :tmux-saved-layout)
                          "saved-layout-bar"))
           (should (equal (plist-get sess :tmux-session) "sess"))
           (should (equal (plist-get sess :tmux-window) "1"))))))))

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

(ert-deftest +presentation/end-restores-saved-layout-before-kill-pane ()
  "On `created' origin: select-layout runs before kill-pane."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (calls nil))
    (puthash "k-rl-c"
             (list :frame nil :origin 'created
                   :saved-config nil
                   :tmux-pane "%42"
                   :tmux-session "sess" :tmux-window "1"
                   :tmux-saved-layout "fake-layout-str"
                   :worktree "/tmp/wt" :started-at 0)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile _dest _display &rest args)
                 (push (cons program args) calls) 0)))
      (+presentation-end "k-rl-c")
      (let ((order (nreverse calls)))
        (should (equal (nth 0 order)
                       '("tmux" "select-layout" "-t" "sess:1"
                         "fake-layout-str")))
        (should (equal (nth 1 order)
                       '("tmux" "kill-pane" "-t" "%42")))))))

(ert-deftest +presentation/end-restores-saved-layout-before-window-config ()
  "On `reused' origin: select-layout runs before set-window-configuration."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame))
         (calls nil))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (puthash "k-rl-r"
                (list :frame frame :origin 'reused
                      :saved-config 'fake-wc
                      :tmux-pane nil
                      :tmux-session "sess" :tmux-window "1"
                      :tmux-saved-layout "fake-layout"
                      :worktree "/tmp/wt" :started-at 0)
                +presentation--sessions)
       (cl-letf (((symbol-function 'process-file)
                  (lambda (program _infile _dest _display &rest args)
                    (push (cons program args) calls) 0))
                 ((symbol-function 'set-window-configuration)
                  (lambda (_wc) (push 'set-wc calls))))
         (+presentation-end "k-rl-r")
         (let ((order (nreverse calls)))
           (should (equal (nth 0 order)
                          '("tmux" "select-layout" "-t" "sess:1"
                            "fake-layout")))
           (should (eq (nth 1 order) 'set-wc))))))))

(ert-deftest +presentation/end-skips-restore-when-saved-layout-nil ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (calls nil))
    (puthash "k-no-sl"
             (list :frame nil :origin 'created
                   :saved-config nil :tmux-pane "%5"
                   :tmux-session "sess" :tmux-window "1"
                   :tmux-saved-layout nil
                   :worktree "/tmp/wt" :started-at 0)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile _dest _display &rest args)
                 (push (cons program args) calls) 0)))
      (+presentation-end "k-no-sl")
      (should (null (cl-find-if
                     (lambda (c) (member "select-layout" c))
                     calls))))))

(ert-deftest +presentation/end-skips-restore-when-saved-layout-empty ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (calls nil))
    (puthash "k-empty-sl"
             (list :frame nil :origin 'created
                   :saved-config nil :tmux-pane "%6"
                   :tmux-session "sess" :tmux-window "1"
                   :tmux-saved-layout ""
                   :worktree "/tmp/wt" :started-at 0)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile _dest _display &rest args)
                 (push (cons program args) calls) 0)))
      (+presentation-end "k-empty-sl")
      (should (null (cl-find-if
                     (lambda (c) (member "select-layout" c))
                     calls))))))


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

(defun +presentation-tests--load-init ()
  "Load `modules/presentation/init.el' for MCP-tool tests.
Returns t if loaded, nil if the file or MCP package is unavailable."
  (when (require 'claude-code-ide-mcp-server nil t)
    (let ((init (expand-file-name "modules/presentation/init.el"
                                  user-emacs-directory)))
      (when (file-exists-p init)
        (condition-case nil (load init nil t) (error nil))
        t))))

(defun +presentation-tests--tool-spec (name)
  "Return the registered MCP tool spec named NAME, or nil."
  (when (boundp 'claude-code-ide-mcp-server-tools)
    (cl-find-if (lambda (s)
                  (equal (plist-get s :name) name))
                claude-code-ide-mcp-server-tools)))

(ert-deftest +presentation/push-slide-tool-declares-set_current-arg ()
  "`push_slide' MCP tool registers an optional boolean `set_current' arg."
  (skip-unless (+presentation-tests--load-init))
  (let* ((spec (+presentation-tests--tool-spec "push_slide"))
         (args (plist-get spec :args))
         (sc (cl-find-if (lambda (a) (equal (plist-get a :name) "set_current"))
                         args)))
    (should spec)
    (should sc)
    (should (eq (plist-get sc :type) 'boolean))
    (should (plist-get sc :optional))))

(ert-deftest +presentation/push-slide-tool-coerces-set_current-to-set-current ()
  "Calling the registered push_slide function with set_current: t advances."
  (skip-unless (+presentation-tests--load-init))
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-mcp-push-sc")
         (renders 0)
         (spec (+presentation-tests--tool-spec "push_slide"))
         (fn (plist-get spec :function)))
    (puthash key (list :deck [] :current-slide-index nil)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) (cl-incf renders))))
      (funcall fn key '((kind . "narrative") (markdown . "x")) t)
      (let ((s (gethash key +presentation--sessions)))
        (should (= (length (plist-get s :deck)) 1))
        (should (= (plist-get s :current-slide-index) 0)))
      (should (= renders 1))
      (funcall fn key '((kind . "narrative") (markdown . "y")) nil)
      (let ((s (gethash key +presentation--sessions)))
        (should (= (length (plist-get s :deck)) 2))
        (should (= (plist-get s :current-slide-index) 0)))
      (should (= renders 1)))))

(ert-deftest +presentation/mcp-tools-registered-after-init ()
  (skip-unless (require 'claude-code-ide-mcp-server nil t))
  (let ((init (expand-file-name "modules/presentation/init.el"
                                user-emacs-directory)))
    (skip-unless (file-exists-p init))
    (condition-case nil (load init nil t) (error nil))
    (when (fboundp 'claude-code-ide-mcp-server-get-tool-names)
      (let ((names (claude-code-ide-mcp-server-get-tool-names)))
        (should (member "start_presentation" names))
        (should (member "end_presentation" names))
        (should (member "get_presentation" names))
        (should (member "push_slide" names))
        (should (member "replace_slide" names))
        (should (member "truncate_after" names))
        (should (member "goto_slide" names))
        (should (member "get_deck" names))))))


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

(ert-deftest +presentation/info-includes-slide-count-and-current-index ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-info-deck"))
    (puthash key (list :frame nil :origin 'created
                       :tmux-pane nil :worktree "/tmp/wt"
                       :started-at 0
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 1)
             +presentation--sessions)
    (let ((info (+presentation-info key)))
      (should (= (alist-get 'slide_count info) 2))
      (should (= (alist-get 'current_slide_index info) 1)))))

(ert-deftest +presentation/info-empty-deck-reports-zero-and-null-index ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-info-empty"))
    (puthash key (list :frame nil :origin 'created
                       :tmux-pane nil :worktree "/tmp/wt"
                       :started-at 0
                       :deck [] :current-slide-index nil)
             +presentation--sessions)
    (let ((info (+presentation-info key)))
      (should (= (alist-get 'slide_count info) 0))
      (should (null (alist-get 'current_slide_index info))))))


;;; get_deck

(ert-deftest +presentation/deck-info-empty ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-deck-e"))
    (puthash key (list :deck [] :current-slide-index nil)
             +presentation--sessions)
    (let ((info (+presentation-deck-info key)))
      (should (equal (alist-get 'key info) key))
      (should (null (alist-get 'current_slide_index info)))
      (should (equal (alist-get 'slides info) [])))))

(ert-deftest +presentation/deck-info-non-empty ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-deck-n"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a"
                                       :title "Intro")
                                     '(:kind "file" :path "x.el"))
                       :current-slide-index 0)
             +presentation--sessions)
    (let* ((info (+presentation-deck-info key))
           (slides (alist-get 'slides info)))
      (should (= (length slides) 2))
      (let ((s0 (aref slides 0)))
        (should (= (alist-get 'index s0) 0))
        (should (equal (alist-get 'kind s0) "narrative"))
        (should (equal (alist-get 'title s0) "Intro")))
      (let ((s1 (aref slides 1)))
        (should (= (alist-get 'index s1) 1))
        (should (equal (alist-get 'kind s1) "file"))
        (should (null (alist-get 'title s1)))))))


;;; alist-to-plist helper

(ert-deftest +presentation/alist-to-plist-symbol-keys ()
  (should (equal (+presentation--alist-to-plist '((kind . "narrative")
                                                  (markdown . "# Hi")))
                 '(:kind "narrative" :markdown "# Hi"))))


;;; Slide-spec coercion

(ert-deftest +presentation/coerce-slide-deep-converts-nested-alists ()
  (should (equal
           (+presentation--coerce-slide
            '((kind . "narrative") (markdown . "# Hi")))
           '(:kind "narrative" :markdown "# Hi"))))

(ert-deftest +presentation/coerce-slide-walks-vectors-of-alists ()
  (should (equal
           (+presentation--coerce-slide
            '((kind . "narrative") (markdown . "x")
              (annotations . [((line . 1) (text . "a") (position . "after"))
                              ((line . 2) (text . "b"))])))
           '(:kind "narrative" :markdown "x"
             :annotations [(:line 1 :text "a" :position "after")
                           (:line 2 :text "b")]))))

(ert-deftest +presentation/coerce-slide-recurses-into-layout-panes ()
  (let ((coerced (+presentation--coerce-slide
                  '((kind . "layout") (split . "horizontal")
                    (panes . [((kind . "narrative") (markdown . "L"))
                              ((kind . "narrative") (markdown . "R"))])))))
    (should (equal (plist-get coerced :kind) "layout"))
    (let ((panes (plist-get coerced :panes)))
      (should (vectorp panes))
      (should (equal (plist-get (aref panes 0) :markdown) "L"))
      (should (equal (plist-get (aref panes 1) :markdown) "R")))))


;;; Slide validation

(ert-deftest +presentation/validate-slide-narrative-ok ()
  (should (+presentation--validate-slide
           '(:kind "narrative" :markdown "x"))))

(ert-deftest +presentation/validate-slide-narrative-requires-markdown ()
  (should-error (+presentation--validate-slide '(:kind "narrative"))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-file-ok ()
  (should (+presentation--validate-slide
           '(:kind "file" :path "lib.el"))))

(ert-deftest +presentation/validate-slide-file-requires-path ()
  (should-error (+presentation--validate-slide '(:kind "file"))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-file-line-ranges-positive ()
  (should-error (+presentation--validate-slide
                 '(:kind "file" :path "x" :start-line 0))
                :type 'user-error)
  (should-error (+presentation--validate-slide
                 '(:kind "file" :path "x" :end-line -1))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-file-focus-shape ()
  (should (+presentation--validate-slide
           '(:kind "file" :path "x" :focus [10 20])))
  (should-error (+presentation--validate-slide
                 '(:kind "file" :path "x" :focus [10]))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-diff-empty-ok ()
  (should (+presentation--validate-slide '(:kind "diff"))))

(ert-deftest +presentation/validate-slide-diff-rejects-half-range ()
  (should-error (+presentation--validate-slide
                 '(:kind "diff" :base "main"))
                :type 'user-error)
  (should-error (+presentation--validate-slide
                 '(:kind "diff" :head "feat"))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-layout-ok ()
  (should (+presentation--validate-slide
           '(:kind "layout" :split "horizontal"
             :panes [(:kind "narrative" :markdown "L")
                     (:kind "narrative" :markdown "R")]))))

(ert-deftest +presentation/validate-slide-layout-rejects-nested ()
  (should-error
   (+presentation--validate-slide
    '(:kind "layout" :split "horizontal"
      :panes [(:kind "layout" :split "vertical"
               :panes [(:kind "narrative" :markdown "a")
                       (:kind "narrative" :markdown "b")])
              (:kind "narrative" :markdown "R")]))
   :type 'user-error))

(ert-deftest +presentation/validate-slide-layout-requires-two-panes ()
  (should-error
   (+presentation--validate-slide
    '(:kind "layout" :split "horizontal"
      :panes [(:kind "narrative" :markdown "x")]))
   :type 'user-error))

(ert-deftest +presentation/validate-slide-layout-rejects-bad-split ()
  (should-error
   (+presentation--validate-slide
    '(:kind "layout" :split "diagonal"
      :panes [(:kind "narrative" :markdown "L")
              (:kind "narrative" :markdown "R")]))
   :type 'user-error))

(ert-deftest +presentation/validate-slide-rejects-unknown-kind ()
  (should-error (+presentation--validate-slide '(:kind "mystery"))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-annotation-line-positive ()
  (should-error
   (+presentation--validate-slide
    '(:kind "narrative" :markdown "x"
      :annotations [(:line 0 :text "t")]))
   :type 'user-error)
  (should-error
   (+presentation--validate-slide
    '(:kind "narrative" :markdown "x"
      :annotations [(:line "5" :text "t")]))
   :type 'user-error))

(ert-deftest +presentation/validate-slide-annotation-position-enum ()
  (should (+presentation--validate-slide
           '(:kind "narrative" :markdown "x"
             :annotations [(:line 1 :text "t" :position "before")
                           (:line 2 :text "u" :position "after")
                           (:line 3 :text "v")])))
  (should-error
   (+presentation--validate-slide
    '(:kind "narrative" :markdown "x"
      :annotations [(:line 1 :text "t" :position "elsewhere")]))
   :type 'user-error))


;;; pane_layout validation

(ert-deftest +presentation/validate-slide-pane-layout-tall-ok ()
  (should (+presentation--validate-slide
           '(:kind "narrative" :markdown "x" :pane-layout "tall")))
  (should (+presentation--validate-slide
           '(:kind "file" :path "x" :pane-layout "tall")))
  (should (+presentation--validate-slide
           '(:kind "diff" :pane-layout "tall")))
  (should (+presentation--validate-slide
           '(:kind "layout" :split "horizontal" :pane-layout "tall"
             :panes [(:kind "narrative" :markdown "L")
                     (:kind "narrative" :markdown "R")]))))

(ert-deftest +presentation/validate-slide-pane-layout-wide-ok ()
  (should (+presentation--validate-slide
           '(:kind "narrative" :markdown "x" :pane-layout "wide"))))

(ert-deftest +presentation/validate-slide-pane-layout-rejects-other-string ()
  (should-error (+presentation--validate-slide
                 '(:kind "narrative" :markdown "x" :pane-layout "huge"))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-pane-layout-rejects-non-string ()
  (should-error (+presentation--validate-slide
                 '(:kind "narrative" :markdown "x" :pane-layout tall))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-pane-layout-rejects-empty ()
  (should-error (+presentation--validate-slide
                 '(:kind "narrative" :markdown "x" :pane-layout ""))
                :type 'user-error))

(ert-deftest +presentation/validate-slide-pane-layout-absent-ok ()
  (should (+presentation--validate-slide '(:kind "narrative" :markdown "x")))
  (should (+presentation--validate-slide '(:kind "file" :path "x")))
  (should (+presentation--validate-slide '(:kind "diff")))
  (should (+presentation--validate-slide
           '(:kind "layout" :split "horizontal"
             :panes [(:kind "narrative" :markdown "L")
                     (:kind "narrative" :markdown "R")]))))

;;; pane_layout effect planner

(ert-deftest +presentation/pane-layout-effects-tall ()
  (let ((effects (+presentation--pane-layout-effects 'tall nil "sess" "1")))
    (should (= (length effects) 2))
    (should (equal (+presentation-effect-shell-argv (nth 0 effects))
                   '("tmux" "select-layout"
                     "-t" "sess:1" "main-horizontal")))
    (should (equal (+presentation-effect-shell-argv (nth 1 effects))
                   '("tmux" "set-window-option"
                     "-t" "sess:1" "main-pane-height" "25%")))))

(ert-deftest +presentation/pane-layout-effects-wide ()
  (let ((effects (+presentation--pane-layout-effects 'wide 'tall "sess" "1")))
    (should (= (length effects) 2))
    (should (equal (+presentation-effect-shell-argv (nth 0 effects))
                   '("tmux" "select-layout"
                     "-t" "sess:1" "main-vertical")))
    (should (equal (+presentation-effect-shell-argv (nth 1 effects))
                   '("tmux" "set-window-option"
                     "-t" "sess:1" "main-pane-width" "33%")))))

(ert-deftest +presentation/pane-layout-effects-idempotent ()
  (should (null (+presentation--pane-layout-effects 'tall 'tall "sess" "1")))
  (should (null (+presentation--pane-layout-effects 'wide 'wide "sess" "1")))
  (should (null (+presentation--pane-layout-effects nil nil "sess" "1"))))


(ert-deftest +presentation/coerce-slide-converts-pane-layout-snake-to-kebab ()
  (should (equal (plist-get
                  (+presentation--coerce-slide
                   '((kind . "narrative")
                     (markdown . "x")
                     (pane_layout . "tall")))
                  :pane-layout)
                 "tall")))


;;; Deck mutation helpers

(ert-deftest +presentation/deck-push-default-leaves-current-and-skips-render ()
  "Default `+presentation--deck-push' appends without touching current/render."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-push-default")
         (renders 0))
    (puthash key (list :deck [] :current-slide-index nil)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) (cl-incf renders))))
      (let ((idx (+presentation--deck-push
                  key '(:kind "narrative" :markdown "a"))))
        (should (= idx 0))
        (let ((s (gethash key +presentation--sessions)))
          (should (= (length (plist-get s :deck)) 1))
          (should (null (plist-get s :current-slide-index)))))
      (let ((idx (+presentation--deck-push
                  key '(:kind "narrative" :markdown "b"))))
        (should (= idx 1))
        (let ((s (gethash key +presentation--sessions)))
          (should (= (length (plist-get s :deck)) 2))
          (should (null (plist-get s :current-slide-index)))))
      (should (= renders 0)))))

(ert-deftest +presentation/deck-push-set-current-advances-and-renders ()
  "`+presentation--deck-push' with `:set-current' t sets current and renders."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-push-sc")
         (renders 0))
    (puthash key (list :deck [] :current-slide-index nil)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) (cl-incf renders))))
      (let ((idx (+presentation--deck-push
                  key '(:kind "narrative" :markdown "a")
                  :set-current t)))
        (should (= idx 0))
        (let ((s (gethash key +presentation--sessions)))
          (should (= (length (plist-get s :deck)) 1))
          (should (= (plist-get s :current-slide-index) 0))))
      (let ((idx (+presentation--deck-push
                  key '(:kind "narrative" :markdown "b")
                  :set-current t)))
        (should (= idx 1))
        (should (= (plist-get (gethash key +presentation--sessions)
                              :current-slide-index)
                   1)))
      (should (= renders 2)))))

(ert-deftest +presentation/deck-replace-mutates-without-changing-current ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-rep"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 1)
             +presentation--sessions)
    (+presentation--deck-replace key 0 '(:kind "narrative" :markdown "A"))
    (let ((s (gethash key +presentation--sessions)))
      (should (equal (plist-get (aref (plist-get s :deck) 0) :markdown) "A"))
      (should (= (plist-get s :current-slide-index) 1)))))

(ert-deftest +presentation/deck-replace-out-of-range-errors ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-rep-oob"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a"))
                       :current-slide-index 0)
             +presentation--sessions)
    (should-error (+presentation--deck-replace
                   key 5 '(:kind "narrative" :markdown "x"))
                  :type 'user-error)))

(ert-deftest +presentation/deck-replace-on-empty-errors ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-rep-empty"))
    (puthash key (list :deck [] :current-slide-index nil)
             +presentation--sessions)
    (should-error (+presentation--deck-replace
                   key 0 '(:kind "narrative" :markdown "x"))
                  :type 'user-error)))

(ert-deftest +presentation/deck-truncate-drops-trailing-and-resets-current ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-trunc"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b")
                                     '(:kind "narrative" :markdown "c"))
                       :current-slide-index 2)
             +presentation--sessions)
    (+presentation--deck-truncate key 0)
    (let ((s (gethash key +presentation--sessions)))
      (should (= (length (plist-get s :deck)) 1))
      (should (= (plist-get s :current-slide-index) 0)))))

(ert-deftest +presentation/deck-truncate-keeps-current-when-not-past ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-trunc-keep"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b")
                                     '(:kind "narrative" :markdown "c"))
                       :current-slide-index 0)
             +presentation--sessions)
    (+presentation--deck-truncate key 1)
    (let ((s (gethash key +presentation--sessions)))
      (should (= (length (plist-get s :deck)) 2))
      (should (= (plist-get s :current-slide-index) 0)))))

(ert-deftest +presentation/deck-truncate-minus-one-clears-deck ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-trunc-clear"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a"))
                       :current-slide-index 0)
             +presentation--sessions)
    (+presentation--deck-truncate key -1)
    (let ((s (gethash key +presentation--sessions)))
      (should (= (length (plist-get s :deck)) 0))
      (should (null (plist-get s :current-slide-index))))))

(ert-deftest +presentation/deck-truncate-out-of-range-errors ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-trunc-oob"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a"))
                       :current-slide-index 0)
             +presentation--sessions)
    (should-error (+presentation--deck-truncate key 5) :type 'user-error)))

(ert-deftest +presentation/deck-goto-sets-current-without-mutating-deck ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-goto"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 0)
             +presentation--sessions)
    (+presentation--deck-goto key 1)
    (let ((s (gethash key +presentation--sessions)))
      (should (= (length (plist-get s :deck)) 2))
      (should (= (plist-get s :current-slide-index) 1)))))

(ert-deftest +presentation/deck-goto-out-of-range-errors ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-goto-oob"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a"))
                       :current-slide-index 0)
             +presentation--sessions)
    (should-error (+presentation--deck-goto key 5) :type 'user-error)))


;;; Render dispatch

(ert-deftest +presentation/render-slide-clears-prior-overlays-before-dispatch ()
  "render-slide deletes overlays from `:render-state' before rendering."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-cleanup")
         (sentinel-buf (generate-new-buffer "*sentinel*"))
         (ov (with-current-buffer sentinel-buf
               (make-overlay (point-min) (point-min))))
         (deleted nil))
    (unwind-protect
        (progn
          (puthash key (list :worktree "/tmp" :frame nil
                             :render-state (list :overlays (list ov)))
                   +presentation--sessions)
          (cl-letf (((symbol-function '+presentation--dispatch-slide)
                     (lambda (_key _slide)
                       (setq deleted (not (overlay-buffer ov)))
                       sentinel-buf)))
            (+presentation--render-slide
             key '(:kind "narrative" :markdown "x")))
          (should deleted)
          (should (null (+presentation--session-prop key :render-state))))
      (when (overlay-buffer ov) (delete-overlay ov))
      (kill-buffer sentinel-buf))))

(ert-deftest +presentation/render-slide-runs-pane-layout-effects-and-stores ()
  "Render runs layout effects when slide hint differs from session slot,
then writes the new value into `:pane-layout' after success."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-pl-render")
         (calls nil))
    (puthash key (list :worktree "/tmp" :frame nil
                       :tmux-session "sess" :tmux-window "1"
                       :pane-layout nil :render-state nil
                       :deck [] :current-slide-index nil)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile _dest _display &rest args)
                 (push (cons program args) calls) 0))
              ((symbol-function '+presentation--dispatch-slide)
               (lambda (_k _s) (generate-new-buffer " *tmp*"))))
      (+presentation--render-slide
       key '(:kind "narrative" :markdown "x" :pane-layout "tall"))
      (let ((tmux-calls (cl-count-if (lambda (c) (equal (car c) "tmux"))
                                     calls)))
        (should (= tmux-calls 2)))
      (should (eq (plist-get (gethash key +presentation--sessions)
                             :pane-layout)
                  'tall)))))

(ert-deftest +presentation/render-slide-pane-layout-idempotent ()
  "Three slides all marked `tall' invoke tmux exactly once."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-pl-id")
         (calls nil))
    (puthash key (list :worktree "/tmp" :frame nil
                       :tmux-session "sess" :tmux-window "1"
                       :pane-layout nil :render-state nil
                       :deck [] :current-slide-index nil)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile _dest _display &rest args)
                 (push (cons program args) calls) 0))
              ((symbol-function '+presentation--dispatch-slide)
               (lambda (_k _s) (generate-new-buffer " *tmp*"))))
      (dotimes (_ 3)
        (+presentation--render-slide
         key '(:kind "narrative" :markdown "x" :pane-layout "tall")))
      (let ((tmux-calls (cl-count-if (lambda (c) (equal (car c) "tmux"))
                                     calls)))
        (should (= tmux-calls 2))))))

(ert-deftest +presentation/render-slide-without-pane-layout-leaves-state ()
  "Slide without `:pane-layout' leaves session slot and tmux untouched."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-pl-none")
         (calls nil))
    (puthash key (list :worktree "/tmp" :frame nil
                       :tmux-session "sess" :tmux-window "1"
                       :pane-layout 'wide :render-state nil
                       :deck [] :current-slide-index nil)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (program _infile _dest _display &rest args)
                 (push (cons program args) calls) 0))
              ((symbol-function '+presentation--dispatch-slide)
               (lambda (_k _s) (generate-new-buffer " *tmp*"))))
      (+presentation--render-slide
       key '(:kind "narrative" :markdown "x"))
      (should (null (cl-find-if (lambda (c) (equal (car c) "tmux")) calls)))
      (should (eq (plist-get (gethash key +presentation--sessions)
                             :pane-layout)
                  'wide)))))


(ert-deftest +presentation/dispatch-slide-rejects-unknown-kind ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-unknown"))
    (puthash key (list :worktree "/tmp" :frame nil) +presentation--sessions)
    (should-error (+presentation--dispatch-slide key '(:kind "mystery")))))


;;; start_presentation deck wiring

(ert-deftest +presentation/start-with-initial-slide-populates-deck-zero ()
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
            ((symbol-function 'switch-to-buffer) (lambda (_b) nil)))
         (let* ((key (+presentation-start
                      :worktree "/tmp/wt"
                      :tmux-session "sess"
                      :tmux-window "1"
                      :socket "/tmp/sock"
                      :initial-slide '(:kind "narrative" :markdown "# Hi")))
                (sess (gethash key +presentation--sessions)))
           (should (= (length (plist-get sess :deck)) 1))
           (should (= (plist-get sess :current-slide-index) 0))
           (should (equal (plist-get (aref (plist-get sess :deck) 0) :markdown)
                          "# Hi"))))))))

(defmacro +presentation-tests--with-temp-file (var content &rest body)
  "Bind VAR to a temp file containing CONTENT for BODY's duration."
  (declare (indent 2))
  `(let ((,var (make-temp-file "present-test-" nil ".txt" ,content)))
     (unwind-protect (progn ,@body)
       (when (get-file-buffer ,var)
         (with-current-buffer (get-file-buffer ,var)
           (set-buffer-modified-p nil))
         (kill-buffer (get-file-buffer ,var)))
       (delete-file ,var))))


;;; file slide

(ert-deftest +presentation/render-file-resolves-relative-against-worktree ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-file"))
    (+presentation-tests--with-temp-file
        path "alpha\nbeta\ngamma\n"
      (let ((dir (file-name-directory path))
            (rel (file-name-nondirectory path)))
        (puthash key (list :worktree dir :frame nil) +presentation--sessions)
        (let ((buf (+presentation--render-file
                    key (list :kind "file" :path rel))))
          (should (bufferp buf))
          (should (equal (buffer-file-name buf) path)))))))

(ert-deftest +presentation/render-file-narrows-to-line-range ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-file-narrow"))
    (+presentation-tests--with-temp-file
        path "one\ntwo\nthree\nfour\nfive\n"
      (puthash key (list :worktree (file-name-directory path) :frame nil)
               +presentation--sessions)
      (let ((buf (+presentation--render-file
                  key (list :kind "file" :path path
                            :start-line 2 :end-line 4))))
        (with-current-buffer buf
          (should (equal (buffer-substring-no-properties (point-min)
                                                         (point-max))
                         "two\nthree\nfour\n")))))))

(ert-deftest +presentation/render-file-focus-creates-region-overlay ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-file-focus"))
    (+presentation-tests--with-temp-file
        path "one\ntwo\nthree\nfour\nfive\n"
      (puthash key (list :worktree (file-name-directory path) :frame nil)
               +presentation--sessions)
      (let ((buf (+presentation--render-file
                  key (list :kind "file" :path path :focus [2 3]))))
        (with-current-buffer buf
          (let ((ovs (cl-remove-if-not
                      (lambda (o) (eq (overlay-get o 'face) 'region))
                      (overlays-in (point-min) (point-max)))))
            (should (= (length ovs) 1))))))))

;;; diff slide

(ert-deftest +presentation/diff-argv-working-tree ()
  (should (equal (+presentation--diff-argv "/tmp/wt" nil nil nil)
                 '("git" "-C" "/tmp/wt" "diff"))))

(ert-deftest +presentation/diff-argv-range ()
  (should (equal (+presentation--diff-argv "/tmp/wt" "main" "feat" nil)
                 '("git" "-C" "/tmp/wt" "diff" "main..feat"))))

(ert-deftest +presentation/diff-argv-range-with-path ()
  (should (equal (+presentation--diff-argv "/tmp/wt" "main" "feat" "src/x.el")
                 '("git" "-C" "/tmp/wt" "diff"
                   "main..feat" "--" "src/x.el"))))

(ert-deftest +presentation/diff-argv-working-tree-with-path ()
  (should (equal (+presentation--diff-argv "/tmp/wt" nil nil "src/x.el")
                 '("git" "-C" "/tmp/wt" "diff" "--" "src/x.el"))))

(ert-deftest +presentation/diff-argv-half-range-errors ()
  (should-error (+presentation--diff-argv "/tmp/wt" "main" nil nil)
                :type 'user-error)
  (should-error (+presentation--diff-argv "/tmp/wt" nil "feat" nil)
                :type 'user-error))

(ert-deftest +presentation/render-diff-inserts-into-per-session-buffer ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-diff"))
    (puthash key (list :worktree "/tmp/wt" :frame nil)
             +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile dest _display &rest _args)
                 (when-let* ((b (+presentation-tests--fake-process-file-buffer
                                 dest)))
                   (with-current-buffer b
                     (insert "diff --git a/x b/x\n--- a/x\n+++ b/x\n")))
                 0)))
      (let ((buf (+presentation--render-diff
                 key '(:kind "diff"))))
        (unwind-protect
            (with-current-buffer buf
              (should (equal (buffer-name)
                             (format "*presentation-diff: %s*" key)))
              (should (string-match-p "diff --git" (buffer-string)))
              (should (eq major-mode 'diff-mode)))
          (kill-buffer buf))))))


;;; layout slide

(ert-deftest +presentation/render-layout-horizontal-splits-right ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-layout-h")
         (frame (selected-frame))
         (split-args nil)
         (window-buffers nil))
    (puthash key (list :worktree "/tmp" :frame frame)
             +presentation--sessions)
    (cl-letf (((symbol-function 'delete-other-windows) (lambda () nil))
              ((symbol-function 'split-window)
               (lambda (&rest args) (push args split-args) 'fake-window))
              ((symbol-function 'set-window-buffer)
               (lambda (w b) (push (cons w b) window-buffers))))
      (let ((result (+presentation--render-layout
                     key '(:kind "layout" :split "horizontal"
                           :panes [(:kind "narrative" :markdown "L")
                                   (:kind "narrative" :markdown "R")]))))
        (should (eq (car result) :layout))
        (should (= (length window-buffers) 2))
        (let ((args (car split-args)))
          (should (memq 'right args)))))))

(ert-deftest +presentation/render-layout-vertical-splits-below ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-layout-v")
         (frame (selected-frame))
         (split-args nil))
    (puthash key (list :worktree "/tmp" :frame frame)
             +presentation--sessions)
    (cl-letf (((symbol-function 'delete-other-windows) (lambda () nil))
              ((symbol-function 'split-window)
               (lambda (&rest args) (push args split-args) 'fake-window))
              ((symbol-function 'set-window-buffer) (lambda (_w _b) nil)))
      (+presentation--render-layout
       key '(:kind "layout" :split "vertical"
             :panes [(:kind "narrative" :markdown "T")
                     (:kind "narrative" :markdown "B")]))
      (let ((args (car split-args)))
        (should (memq 'below args))))))

(ert-deftest +presentation/deck-push-rejects-nested-layout ()
  "Validation runs at the public push path; nested layout is rejected."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-layout-nest"))
    (puthash key (list :deck [] :current-slide-index nil :worktree "/tmp"
                       :frame nil)
             +presentation--sessions)
    (should-error
     (+presentation--deck-push
      key '(:kind "layout" :split "horizontal"
            :panes [(:kind "layout" :split "vertical"
                     :panes [(:kind "narrative" :markdown "x")
                             (:kind "narrative" :markdown "y")])
                    (:kind "narrative" :markdown "R")]))
     :type 'user-error)))


;;; annotations

(ert-deftest +presentation/apply-annotations-overlay-after-string ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-ann")
         (buf (generate-new-buffer "*ann-buf*")))
    (puthash key (list :worktree "/tmp" :frame nil)
             +presentation--sessions)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "line1\nline2\nline3\nline4\nline5\n"))
          (+presentation--apply-annotations
           key buf [(:line 3 :text "note" :position "after")])
          (let* ((rs (+presentation--session-prop key :render-state))
                 (ovs (plist-get rs :overlays)))
            (should (= (length ovs) 1))
            (let ((ov (car ovs)))
              (should (eq (overlay-buffer ov) buf))
              (should (string-match-p "note" (overlay-get ov 'after-string))))))
      (kill-buffer buf))))

(ert-deftest +presentation/apply-annotations-before-string ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-ann-b")
         (buf (generate-new-buffer "*ann-b*")))
    (puthash key (list :worktree "/tmp") +presentation--sessions)
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "a\nb\nc\n"))
          (+presentation--apply-annotations
           key buf [(:line 2 :text "B" :position "before")])
          (let* ((rs (+presentation--session-prop key :render-state))
                 (ov (car (plist-get rs :overlays))))
            (should (string-match-p "B" (overlay-get ov 'before-string)))
            (should-not (overlay-get ov 'after-string))))
      (kill-buffer buf))))

(ert-deftest +presentation/render-slide-applies-annotations-via-narrative ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-ann-narr")
         (+presentation-narrative-major-mode 'fundamental-mode))
    (puthash key (list :worktree "/tmp" :frame nil)
             +presentation--sessions)
    (let ((buf (+presentation--render-slide
                key '(:kind "narrative" :markdown "x\ny\nz\n"
                      :annotations [(:line 2 :text "T")]))))
      (unwind-protect
          (let* ((rs (+presentation--session-prop key :render-state))
                 (ovs (plist-get rs :overlays)))
            (should (= (length ovs) 1))
            (should (eq (overlay-buffer (car ovs)) buf)))
        (kill-buffer buf)))))

(ert-deftest +presentation/overlays-deleted-on-slide-change ()
  "Push slide A with annotations; pushing slide B clears A's overlays."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-ov-clear")
         (+presentation-narrative-major-mode 'fundamental-mode))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck [] :current-slide-index nil)
             +presentation--sessions)
    (+presentation--deck-push
     key '(:kind "narrative" :markdown "a\nb\n"
           :annotations [(:line 1 :text "x")])
     :set-current t)
    (let* ((rs (+presentation--session-prop key :render-state))
           (old-ov (car (plist-get rs :overlays))))
      (should (overlayp old-ov))
      (+presentation--deck-push
       key '(:kind "narrative" :markdown "c\nd\n")
       :set-current t)
      (should-not (overlay-buffer old-ov)))))

(ert-deftest +presentation/end-presentation-deletes-overlays ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-end-ov")
         (+presentation-narrative-major-mode 'fundamental-mode))
    (puthash key (list :worktree "/tmp" :frame nil :origin 'reused
                       :saved-config nil
                       :deck [] :current-slide-index nil)
             +presentation--sessions)
    (+presentation--deck-push
     key '(:kind "narrative" :markdown "a\n"
           :annotations [(:line 1 :text "x")])
     :set-current t)
    (let* ((rs (+presentation--session-prop key :render-state))
           (ov (car (plist-get rs :overlays))))
      (should (overlayp ov))
      (+presentation-end key)
      (should-not (overlay-buffer ov)))))


(ert-deftest +presentation/render-file-read-only-restored-on-cleanup ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-file-ro"))
    (+presentation-tests--with-temp-file
        path "x\ny\n"
      (puthash key (list :worktree (file-name-directory path) :frame nil)
               +presentation--sessions)
      (let ((buf (+presentation--render-file
                  key (list :kind "file" :path path))))
        (with-current-buffer buf
          (should buffer-read-only))
        (+presentation--cleanup-render-state key)
        (with-current-buffer buf
          (should-not buffer-read-only))))))


(ert-deftest +presentation/start-without-initial-slide-leaves-deck-empty ()
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
               (when (and (eq f frame) (eq param 'tty)) "/dev/ttys099")))
            ((symbol-function 'current-window-configuration)
             (lambda () 'fake-wc))
            ((symbol-function 'set-register) (lambda (_r _v) nil))
            ((symbol-function 'switch-to-buffer) (lambda (_b) nil)))
         (let* ((key (+presentation-start
                      :worktree "/tmp/wt"
                      :tmux-session "sess"
                      :tmux-window "1"
                      :socket "/tmp/sock"))
                (sess (gethash key +presentation--sessions)))
           (should (equal (plist-get sess :deck) []))
           (should (null (plist-get sess :current-slide-index)))))))))

;;; Renderer hookup of +presentation-mode

(ert-deftest +presentation/render-narrative-enables-presentation-mode ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-narr-mode")
         (+presentation-narrative-major-mode 'fundamental-mode))
    (puthash key (list :worktree "/tmp" :frame nil) +presentation--sessions)
    (let ((buf (+presentation--render-narrative
                key '(:kind "narrative" :markdown "x"))))
      (unwind-protect
          (with-current-buffer buf
            (should +presentation-mode)
            (should (equal +presentation--session-key key))
            (should (local-variable-p '+presentation--session-key)))
        (kill-buffer buf)))))

(ert-deftest +presentation/render-file-enables-presentation-mode ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-file-mode"))
    (+presentation-tests--with-temp-file
        path "x\n"
      (puthash key (list :worktree (file-name-directory path) :frame nil)
               +presentation--sessions)
      (let ((buf (+presentation--render-file
                  key (list :kind "file" :path path))))
        (with-current-buffer buf
          (should +presentation-mode)
          (should (equal +presentation--session-key key)))))))

(ert-deftest +presentation/render-diff-enables-presentation-mode ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-diff-mode"))
    (puthash key (list :worktree "/tmp/wt" :frame nil) +presentation--sessions)
    (cl-letf (((symbol-function 'process-file)
               (lambda (_p _i dest _d &rest _a)
                 (when-let* ((b (+presentation-tests--fake-process-file-buffer
                                 dest)))
                   (with-current-buffer b (insert "")))
                 0)))
      (let ((buf (+presentation--render-diff
                  key '(:kind "diff"))))
        (unwind-protect
            (with-current-buffer buf
              (should +presentation-mode)
              (should (equal +presentation--session-key key)))
          (kill-buffer buf))))))

(ert-deftest +presentation/render-layout-enables-mode-on-both-panes ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-layout-mode")
         (frame (selected-frame))
         (+presentation-narrative-major-mode 'fundamental-mode))
    (puthash key (list :worktree "/tmp" :frame frame) +presentation--sessions)
    (cl-letf (((symbol-function 'delete-other-windows) (lambda () nil))
              ((symbol-function 'split-window)
               (lambda (&rest _) 'fake-window))
              ((symbol-function 'set-window-buffer) (lambda (_w _b) nil)))
      (let* ((result (+presentation--render-layout
                      key '(:kind "layout" :split "horizontal"
                            :panes [(:kind "narrative" :markdown "L")
                                    (:kind "narrative" :markdown "R")])))
             (buf1 (nth 2 result))
             (buf2 (nth 3 result)))
        (with-current-buffer buf1
          (should +presentation-mode)
          (should (equal +presentation--session-key key)))
        (with-current-buffer buf2
          (should +presentation-mode)
          (should (equal +presentation--session-key key)))))))

(ert-deftest +presentation/splash-buffer-has-presentation-mode ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (frame (selected-frame)))
    (+presentation-tests--with-clean-frame
     frame
     (lambda ()
       (cl-letf
           (((symbol-function 'process-file)
             (lambda (_p _i dest _d &rest _a)
               (when-let* ((b (+presentation-tests--fake-process-file-buffer
                               dest)))
                 (with-current-buffer b (insert "%9\t/dev/ttys099\n")))
               0))
            ((symbol-function 'frame-list) (lambda () (list frame)))
            ((symbol-function 'frame-parameter)
             (lambda (f param)
               (when (and (eq f frame) (eq param 'tty)) "/dev/ttys099")))
            ((symbol-function 'current-window-configuration)
             (lambda () 'fake-wc))
            ((symbol-function 'set-register) (lambda (_r _v) nil))
            ((symbol-function 'switch-to-buffer) (lambda (_b) nil)))
         (let* ((key (+presentation-start
                      :worktree "/tmp/wt"
                      :tmux-session "sess"
                      :tmux-window "1"
                      :socket "/tmp/sock"))
                (buf (get-buffer (format "*presentation: %s*" key))))
           (with-current-buffer buf
             (should +presentation-mode)
             (should (equal +presentation--session-key key)))))))))


;;; +presentation-mode minor mode

(ert-deftest +presentation/mode-keymap-bindings ()
  "C-n and C-f are bound to next-slide; C-p and C-b to previous-slide."
  (should (eq (lookup-key +presentation-mode-map (kbd "C-n"))
              '+presentation-next-slide))
  (should (eq (lookup-key +presentation-mode-map (kbd "C-f"))
              '+presentation-next-slide))
  (should (eq (lookup-key +presentation-mode-map (kbd "C-p"))
              '+presentation-previous-slide))
  (should (eq (lookup-key +presentation-mode-map (kbd "C-b"))
              '+presentation-previous-slide)))

(ert-deftest +presentation/mode-keymap-binds-quit ()
  "C-c q is bound to +presentation-quit."
  (should (eq (lookup-key +presentation-mode-map (kbd "C-c q"))
              '+presentation-quit)))

(ert-deftest +presentation/quit-calls-end-with-buffer-key ()
  "+presentation-quit invokes +presentation-end with the buffer-local key."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-quit")
         (end-calls nil))
    (puthash key (list :worktree "/tmp" :frame nil)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation-end)
               (lambda (k) (push k end-calls) 'done)))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-quit)
        (should (equal end-calls (list key)))))))

(ert-deftest +presentation/quit-noop-when-key-nil ()
  "+presentation-quit is a no-op when buffer-local key is nil."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (end-calls nil))
    (cl-letf (((symbol-function '+presentation-end)
               (lambda (k) (push k end-calls) 'done)))
      (with-temp-buffer
        (setq-local +presentation--session-key nil)
        (+presentation-quit)
        (should (null end-calls))))))

(ert-deftest +presentation/quit-noop-when-session-stale ()
  "+presentation-quit is a no-op when the buffer-local key references no session."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (end-calls nil))
    (cl-letf (((symbol-function '+presentation-end)
               (lambda (k) (push k end-calls) 'done)))
      (with-temp-buffer
        (setq-local +presentation--session-key "k-stale")
        (+presentation-quit)
        (should (null end-calls))))))

(ert-deftest +presentation/next-slide-advances-deck ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-nav-n"))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b")
                                     '(:kind "narrative" :markdown "c"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil)))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-next-slide)
        (should (= (plist-get (gethash key +presentation--sessions)
                              :current-slide-index)
                   1))
        (+presentation-next-slide)
        (should (= (plist-get (gethash key +presentation--sessions)
                              :current-slide-index)
                   2))))))

(ert-deftest +presentation/next-slide-noop-at-end ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-nav-end"))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 1)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil)))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-next-slide)
        (should (= (plist-get (gethash key +presentation--sessions)
                              :current-slide-index)
                   1))))))

(ert-deftest +presentation/previous-slide-retreats-deck ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-nav-p"))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 1)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil)))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-previous-slide)
        (should (= (plist-get (gethash key +presentation--sessions)
                              :current-slide-index)
                   0))))))

(ert-deftest +presentation/previous-slide-noop-at-zero ()
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-nav-p0"))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil)))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-previous-slide)
        (should (= (plist-get (gethash key +presentation--sessions)
                              :current-slide-index)
                   0))))))

;;; Cursor position memory across nav

(ert-deftest +presentation/deck-goto-saves-and-restores-point ()
  "Navigating away then back restores the prior cursor position."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-pt")
         (frame (selected-frame))
         (win (selected-window))
         (buf (generate-new-buffer "*pt-buf*"))
         (window-pt 17)
         (set-calls nil))
    (with-current-buffer buf
      (insert (make-string 200 ?a)))
    (unwind-protect
        (progn
          (puthash key (list :frame frame :worktree "/tmp"
                             :deck (vector '(:kind "narrative" :markdown "a")
                                           '(:kind "narrative" :markdown "b"))
                             :current-slide-index 0)
                   +presentation--sessions)
          (cl-letf (((symbol-function '+presentation--render-current)
                     (lambda (_k) nil))
                    ((symbol-function 'frame-selected-window) (lambda (_f) win))
                    ((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-point) (lambda (_w) window-pt))
                    ((symbol-function 'set-window-point)
                     (lambda (_w pt) (push pt set-calls) (setq window-pt pt)))
                    ((symbol-function 'window-buffer) (lambda (_w) buf)))
            (+presentation--deck-goto key 1)
            (let* ((sess (gethash key +presentation--sessions))
                   (alist (plist-get sess :slide-points)))
              (should (equal (alist-get 0 alist) 17)))
            (setq window-pt 99)
            (+presentation--deck-goto key 0)
            (should (member 17 set-calls))))
      (kill-buffer buf))))

(ert-deftest +presentation/deck-goto-clamps-restored-point-to-buffer ()
  "Restored point is clamped to (point-max) of the destination buffer."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-pt-clamp")
         (frame (selected-frame))
         (win (selected-window))
         (small-buf (generate-new-buffer "*small*"))
         (set-calls nil))
    (with-current-buffer small-buf (insert "abc"))
    (unwind-protect
        (progn
          (puthash key (list :frame frame :worktree "/tmp"
                             :deck (vector '(:kind "narrative" :markdown "x")
                                           '(:kind "narrative" :markdown "y"))
                             :current-slide-index 1
                             :slide-points (list (cons 0 9999)))
                   +presentation--sessions)
          (cl-letf (((symbol-function '+presentation--render-current)
                     (lambda (_k) nil))
                    ((symbol-function 'frame-selected-window) (lambda (_f) win))
                    ((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-point) (lambda (_w) 1))
                    ((symbol-function 'window-buffer) (lambda (_w) small-buf))
                    ((symbol-function 'set-window-point)
                     (lambda (_w pt) (push pt set-calls))))
            (+presentation--deck-goto key 0)
            (should (= (car set-calls)
                       (with-current-buffer small-buf (point-max))))))
      (kill-buffer small-buf))))

(ert-deftest +presentation/deck-goto-no-saved-point-leaves-window-untouched ()
  "First visit to a slide does not call `set-window-point'."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-pt-fresh")
         (frame (selected-frame))
         (win (selected-window))
         (set-called nil))
    (puthash key (list :frame frame :worktree "/tmp"
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil))
              ((symbol-function 'frame-selected-window) (lambda (_f) win))
              ((symbol-function 'window-live-p) (lambda (_w) t))
              ((symbol-function 'window-point) (lambda (_w) 5))
              ((symbol-function 'window-buffer) (lambda (_w) (current-buffer)))
              ((symbol-function 'set-window-point)
               (lambda (_w _pt) (setq set-called t))))
      (+presentation--deck-goto key 1)
      (should-not set-called))))


;;; Channel capability registration

(ert-deftest +presentation/inject-channel-capability-splices-experimental ()
  "Filter-return advice splices `experimental.claude/channel: {}'."
  (let* ((response
          `((jsonrpc . "2.0")
            (id . 1)
            (result . ((protocolVersion . "2024-11-05")
                       (capabilities . ((tools . ((listChanged . t)))))
                       (serverInfo . ((name . "x") (version . "0.1")))))))
         (out (+presentation--inject-channel-capability response))
         (caps (alist-get 'capabilities (alist-get 'result out)))
         (exp (alist-get 'experimental caps)))
    (should (consp exp))
    (should (eq (alist-get 'claude/channel exp) :json-empty))
    (should (alist-get 'tools caps))))

(ert-deftest +presentation/inject-channel-capability-merges-existing-experimental ()
  "Existing `experimental' entries are preserved alongside `claude/channel'."
  (let* ((response
          `((jsonrpc . "2.0")
            (id . 1)
            (result . ((capabilities . ((experimental . ((other . :json-empty))))) ))))
         (out (+presentation--inject-channel-capability response))
         (exp (alist-get 'experimental
                         (alist-get 'capabilities
                                    (alist-get 'result out)))))
    (should (eq (alist-get 'claude/channel exp) :json-empty))
    (should (eq (alist-get 'other exp) :json-empty))))

(ert-deftest +presentation/register-channel-capability-noop-when-fn-unbound ()
  "Registration silently no-ops when MCP handler symbol is unbound."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym)
               (cond ((eq sym 'claude-code-ide-mcp--handle-initialize) nil)
                     (t (funcall #'fboundp sym))))))
    (should-not (+presentation--register-channel-capability))))

(ert-deftest +presentation/register-channel-capability-installs-advice ()
  "When the handler exists, registration adds filter-return advice."
  (let ((sym (gensym "fake-init-")))
    (defalias sym (lambda (id _params)
                    `((jsonrpc . "2.0") (id . ,id)
                      (result . ((capabilities . ((tools . t))))))))
    (unwind-protect
        (progn
          (+presentation--register-channel-capability sym)
          (let* ((resp (funcall sym 7 nil))
                 (caps (alist-get 'capabilities (alist-get 'result resp))))
            (should (eq (alist-get 'claude/channel
                                   (alist-get 'experimental caps))
                        :json-empty))))
      (advice-remove sym #'+presentation--inject-channel-capability)
      (fmakunbound sym))))


;;; Channel notification emission

(ert-deftest +presentation/emit-nav-channel-forward-content-and-meta ()
  "Forward emission composes the advance content string and string meta."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-emit-fwd")
         (sent nil))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a"
                                       :title "First")
                                     '(:kind "file" :path "x" :title "Second")
                                     '(:kind "narrative" :markdown "c"))
                       :current-slide-index 1)
             +presentation--sessions)
    (cl-letf (((symbol-function 'claude-code-ide-mcp--send-notification)
               (lambda (method params) (setq sent (list method params)))))
      (+presentation--emit-nav-channel key 0 1)
      (let* ((method (nth 0 sent))
             (params (nth 1 sent))
             (meta (alist-get 'meta params)))
        (should (equal method "notifications/claude/channel"))
        (should (equal (alist-get 'content params)
                       "User advanced to slide 1 of 3."))
        (should (equal (alist-get 'key meta) key))
        (should (equal (alist-get 'current_slide meta) "1"))
        (should (equal (alist-get 'prior_slide meta) "0"))
        (should (equal (alist-get 'kind meta) "file"))
        (should (equal (alist-get 'title meta) "Second"))
        (dolist (cell meta)
          (should (stringp (cdr cell))))))))

(ert-deftest +presentation/emit-nav-channel-backward-content ()
  "Backward emission composes the retreat content string."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-emit-bwd")
         (sent nil))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b")
                                     '(:kind "narrative" :markdown "c"))
                       :current-slide-index 1)
             +presentation--sessions)
    (cl-letf (((symbol-function 'claude-code-ide-mcp--send-notification)
               (lambda (_m params) (setq sent params))))
      (+presentation--emit-nav-channel key 2 1)
      (should (equal (alist-get 'content sent)
                     "User retreated to slide 1 of 3.")))))

(ert-deftest +presentation/emit-nav-channel-omits-title-when-absent ()
  "When the destination slide has no `:title', `meta.title' is omitted."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-emit-no-title")
         (sent nil))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function 'claude-code-ide-mcp--send-notification)
               (lambda (_m params) (setq sent params))))
      (+presentation--emit-nav-channel key 0 1)
      (let ((meta (alist-get 'meta sent)))
        (should-not (assoc 'title meta))
        (should (equal (alist-get 'kind meta) "narrative"))))))

(ert-deftest +presentation/emit-nav-channel-no-op-when-sender-unbound ()
  "Emission silently no-ops when `claude-code-ide-mcp--send-notification' missing."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-emit-unbound"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'claude-code-ide-mcp--send-notification) nil
                   (funcall #'fboundp sym)))))
      (should-not (+presentation--emit-nav-channel key 0 1)))))

(ert-deftest +presentation/emit-nav-channel-swallows-sender-errors ()
  "Errors from the sender are swallowed; emission returns nil."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-emit-err"))
    (puthash key (list :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function 'claude-code-ide-mcp--send-notification)
               (lambda (_m _p) (error "boom"))))
      (should-not (+presentation--emit-nav-channel key 0 1)))))


;;; Nav command emission wiring

(ert-deftest +presentation/next-slide-emits-channel-after-goto ()
  "`+presentation-next-slide' emits a forward channel notification."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-nav-emit")
         (calls nil))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil))
              ((symbol-function '+presentation--emit-nav-channel)
               (lambda (k prior current) (push (list k prior current) calls))))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-next-slide)
        (should (equal calls (list (list key 0 1))))))))

(ert-deftest +presentation/previous-slide-emits-channel-after-goto ()
  "`+presentation-previous-slide' emits a backward channel notification."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-nav-emit-prev")
         (calls nil))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 1)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil))
              ((symbol-function '+presentation--emit-nav-channel)
               (lambda (k prior current) (push (list k prior current) calls))))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-previous-slide)
        (should (equal calls (list (list key 1 0))))))))

(ert-deftest +presentation/nav-noop-at-end-does-not-emit ()
  "No-op nav at the deck boundary does not call the emitter."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-nav-noop")
         (calls 0))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 1)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil))
              ((symbol-function '+presentation--emit-nav-channel)
               (lambda (&rest _) (cl-incf calls))))
      (with-temp-buffer
        (setq-local +presentation--session-key key)
        (+presentation-next-slide)
        (should (= calls 0))))))

(ert-deftest +presentation/agent-driven-mutation-does-not-emit ()
  "`goto_slide', `push_slide :set-current t', and `replace_slide' on
current do not emit channel notifications."
  (let* ((+presentation--sessions (make-hash-table :test 'equal))
         (key "k-agent")
         (calls 0))
    (puthash key (list :worktree "/tmp" :frame nil
                       :deck (vector '(:kind "narrative" :markdown "a")
                                     '(:kind "narrative" :markdown "b"))
                       :current-slide-index 0)
             +presentation--sessions)
    (cl-letf (((symbol-function '+presentation--render-current)
               (lambda (_k) nil))
              ((symbol-function '+presentation--emit-nav-channel)
               (lambda (&rest _) (cl-incf calls))))
      (+presentation--deck-goto key 1)
      (+presentation--deck-push key '(:kind "narrative" :markdown "c")
                                :set-current t)
      (+presentation--deck-replace
       key 2 '(:kind "narrative" :markdown "C"))
      (should (= calls 0)))))


(ert-deftest +presentation/session-key-is-buffer-local ()
  (with-temp-buffer
    (setq-local +presentation--session-key "K1")
    (should (equal +presentation--session-key "K1"))
    (should (local-variable-p '+presentation--session-key))
    (with-temp-buffer
      (should (or (null +presentation--session-key)
                  (not (equal +presentation--session-key "K1")))))))


;;; tests.el ends here
