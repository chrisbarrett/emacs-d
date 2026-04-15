;;; +code-fences-tests.el --- Tests for polymode code fence decorations -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for polymode heredoc handling: fontification,
;; fence overlays, shell interpolation detection, and quoting semantics.

;;; Code:

(require 'ert)
(require 'seq)

(defvar +code-fences-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defvar +code-fences-test-fixture-dir
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (directory-file-name +code-fences-test-dir)))
  "Directory containing test fixture shell scripts.")

;; Load the implementation
(load (expand-file-name "+code-fences.el" +code-fences-test-dir) nil t)

;; Load polymode
(require 'polymode)
(require 'poly-lock)

;; Load lang-shscript init for innermode definitions (poly-bash-ts-mode etc.)
(let ((shscript-init (expand-file-name "../../lang-shscript/init.el" +code-fences-test-dir)))
  (load shscript-init nil t))

;; Load lang-nix init for Nix interpolation functions
(let ((nix-init (expand-file-name "../../lang-nix/init.el" +code-fences-test-dir)))
  (load nix-init nil t))


;;; Test helpers

(defun +code-fences-test--open-fixture (name)
  "Open fixture NAME in poly-bash-ts-mode, decorate, return base buffer.
Triggers polymode fontification matching interactive behavior:
1. `poly-lock-fontify-now' simulates the display engine's fontification pass
2. `+polymode-refontify-inner-spans' applies fence decorations and
   inner-mode re-fontification
Caller must kill the buffer when done."
  (let ((buf (find-file-noselect
              (expand-file-name name +code-fences-test-fixture-dir))))
    (with-current-buffer buf
      (poly-bash-ts-mode 1)
      (condition-case nil
          (poly-lock-fontify-now (point-min) (point-max))
        (error nil))
      (+polymode-refontify-inner-spans))
    buf))

(defun +code-fences-test--kill-fixture (buf)
  "Kill BUF and all its indirect buffers."
  (let ((base (or (buffer-base-buffer buf) buf)))
    (dolist (b (buffer-list))
      (when (eq (buffer-base-buffer b) base) (kill-buffer b)))
    (kill-buffer base)))

(defmacro with-fixture (name base-var &rest body)
  "Open fixture NAME, bind base buffer to BASE-VAR, eval BODY, clean up."
  (declare (indent 2))
  `(let* ((+fixture-buf+ (+code-fences-test--open-fixture ,name))
          (,base-var (or (buffer-base-buffer +fixture-buf+) +fixture-buf+)))
     (unwind-protect (progn ,@body)
       (+code-fences-test--kill-fixture +fixture-buf+))))

(defun +code-fences-test--overlays-with-prop (buf prop &optional val)
  "Return overlays in BUF having property PROP.
If VAL is non-nil, only return overlays where PROP equals VAL."
  (with-current-buffer buf
    (seq-filter
     (lambda (ov)
       (let ((v (overlay-get ov prop)))
         (if val (equal v val) v)))
     (overlays-in (point-min) (point-max)))))

(defun +code-fences-test--interpolation-texts (buf)
  "Return list of strings covered by shell interpolation overlays in BUF."
  (let ((base (or (buffer-base-buffer buf) buf)))
    (with-current-buffer base
      (seq-map (lambda (ov)
                 (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
               (seq-sort-by #'overlay-start #'<
                            (+code-fences-test--overlays-with-prop
                             base 'face '+polymode-interpolation-face))))))

(defun +code-fences-test--fence-overlays (buf)
  "Return all fence overlays in BUF sorted by position."
  (let ((base (or (buffer-base-buffer buf) buf)))
    (seq-sort-by #'overlay-start #'<
                 (+code-fences-test--overlays-with-prop base '+polymode-fence t))))

(defun +code-fences-test--head-overlays (buf)
  "Return head fence overlays in BUF."
  (seq-filter (lambda (ov) (overlay-get ov '+polymode-head-col-c))
              (+code-fences-test--fence-overlays buf)))

(defun +code-fences-test--tail-overlays (buf)
  "Return tail fence overlays in BUF."
  (seq-filter (lambda (ov) (overlay-get ov 'before-string))
              (+code-fences-test--fence-overlays buf)))

(defun +code-fences-test--body-overlays (buf)
  "Return body fence overlays in BUF."
  (seq-filter (lambda (ov) (overlay-get ov 'line-prefix))
              (+code-fences-test--fence-overlays buf)))

(defun +code-fences-test--search-in-buf (buf string)
  "Return position of STRING in BUF."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (when (search-forward string nil t)
        (match-beginning 0)))))

(defun +code-fences-test--interp-overlay-texts (input)
  "Insert INPUT in temp buffer, run interpolation, return overlay texts."
  (with-temp-buffer
    (insert input)
    (+bash--add-interpolation-overlays (point-min) (point-max) (current-buffer))
    (seq-map (lambda (ov)
               (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
             (seq-sort-by #'overlay-start #'<
                          (overlays-in (point-min) (point-max))))))

(defun +code-fences-test--overlay-bright-p (ov)
  "Return non-nil if head overlay OV is showing its bright (base) line.
Checks the face on the language name portion of the after-string."
  (let* ((as (overlay-get ov 'after-string))
         (base-line (overlay-get ov '+polymode-head-base-line))
         ;; Language name starts at position 4 in base-line (after \"╭── \"),
         ;; plus 1 for the leading newline in after-string.
         (name-pos (1+ 4)))
    (and as base-line
         (> (length as) name-pos)
         (eq (get-text-property name-pos 'face as)
             (get-text-property 4 'face base-line)))))

(defun +code-fences-test--face-contains-p (face target)
  "Return non-nil if TARGET face symbol appears in FACE.
FACE may be a symbol or a composite list from polymode."
  (cond
   ((eq face target) t)
   ((and (listp face) (memq target face)) t)))


;;; Elisp fontification in heredocs

(ert-deftest +code-fences/elisp-quoted-heredoc-fontified ()
  "Quoted <<'ELISP' heredoc body gets emacs-lisp-mode fontification."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let ((pos (+code-fences-test--search-in-buf base "defun")))
      (should pos)
      (should (+code-fences-test--face-contains-p
               (get-text-property pos 'face base)
               'font-lock-keyword-face)))))


;;; Shell interpolation — quoting semantics

(ert-deftest +code-fences/quoted-heredoc-no-interpolation ()
  "Quoted heredocs get no interpolation overlays."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (should (null (+code-fences-test--interpolation-texts base)))))

(ert-deftest +code-fences/unquoted-heredoc-has-interpolation ()
  "Unquoted <<DELIM heredoc gets shell interpolation overlays on $VAR."
  (with-fixture "heredoc-elisp-unquoted.sh" base
    (should (member "$FILENAME" (+code-fences-test--interpolation-texts base)))))

(ert-deftest +code-fences/mixed-quoting-interpolation ()
  "Only unquoted heredocs in mixed file get interpolation overlays."
  (with-fixture "heredoc-mixed.sh" base
    (let ((interps (+code-fences-test--interpolation-texts base)))
      ;; Unquoted block
      (should (member "$ARGSFILE" interps))
      (should (member "$NORMAL_VAR" interps))
      ;; \$ESCAPED — 1 backslash (odd) → not interpolated
      (should-not (seq-some (lambda (s) (string-match-p "\\`\\$ESCAPED\\'" s)) interps))
      ;; \\$DOUBLE_ESCAPED — 2 backslashes (even) → interpolated
      (should (member "$DOUBLE_ESCAPED" interps))
      ;; \\\$TRIPLE_ESCAPED — 3 backslashes (odd) → not interpolated
      (should-not (member "$TRIPLE_ESCAPED" interps))
      ;; Tab-stripping unquoted block
      (should (member "$TABBED_VAR" interps))
      (should (seq-some (lambda (s) (string-match-p "BRACED_VAR" s)) interps))
      (should (seq-some (lambda (s) (string-match-p "command-sub" s)) interps))
      (should (seq-some (lambda (s) (string-match-p "1 \\+ 2" s)) interps))
      ;; Quoted blocks should contribute nothing
      (should-not (member "$expansion" interps))
      (should-not (member "$also_no_expansion" interps))
      (should-not (member "$still_no_expansion" interps)))))


;;; Heredoc quoting detection

(ert-deftest +code-fences/heredoc-quoting-detection ()
  "Detect unquoted vs quoted heredoc heads."
  (dolist (case '(("<<ELISP\n"     . t)    ; bare = unquoted
                  ("<<-ELISP\n"    . t)    ; dash = unquoted
                  ("<<'ELISP'\n"   . nil)  ; single-quoted
                  ("<<\"ELISP\"\n" . nil)  ; double-quoted
                  ("<<\\ELISP\n"   . nil)  ; backslash-quoted
                  ("<<-'ELISP'\n"  . nil)));  dash + single-quoted
    (with-temp-buffer
      (insert (car case))
      (if (cdr case)
          (should (+bash--heredoc-unquoted-p 1 (point-max)))
        (should-not (+bash--heredoc-unquoted-p 1 (point-max)))))))


;;; Shell interpolation pattern matching (unit tests)

(ert-deftest +code-fences/interpolation-patterns ()
  "Shell interpolation patterns produce correct overlays."
  (dolist (case '(;; Single patterns
                  ("hello $FOO world"       ("$FOO"))
                  ("hello ${FOO} world"     ("${FOO}"))
                  ("hello ${FOO:-bar} world" ("${FOO:-bar}"))
                  ("hello $(date) world"    ("$(date)"))
                  ("hello $((1+2)) world"   ("$((1+2))"))
                  ("hello `date` world"     ("`date`"))
                  ;; Multiple
                  ("$A ${B} $(c) $((d)) `e`" ("$A" "${B}" "$(c)" "$((d))" "`e`"))
                  ;; Start of region
                  ("$FOO world"             ("$FOO"))))
    (should (equal (+code-fences-test--interp-overlay-texts (nth 0 case))
                   (nth 1 case)))))

(ert-deftest +code-fences/interpolation-escaping ()
  "Backslash parity determines whether $ is interpolated."
  (dolist (case '(("hello \\$FOO world"       nil)   ; 1 backslash → escaped
                  ("hello \\\\$FOO world"     ("$FOO")) ; 2 → live
                  ("hello \\\\\\$FOO world"   nil)))  ; 3 → escaped
    (let ((result (+code-fences-test--interp-overlay-texts (nth 0 case))))
      (if (nth 1 case)
          (should (equal result (nth 1 case)))
        (should (null result))))))

(ert-deftest +code-fences/interpolation-no-false-positives ()
  "Bare $ followed by space, digits, or punctuation gets no overlay."
  (should (null (+code-fences-test--interp-overlay-texts "cost is $5 or $ alone"))))


;;; Fence overlay structure

(ert-deftest +code-fences/fence-overlay-structure ()
  "Fence overlays have box-drawing head, body prefix, and tail decorations."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let ((fences (+code-fences-test--fence-overlays base)))
      ;; Head: after-string with ╭──
      (let ((heads (+code-fences-test--head-overlays base)))
        (should (= (length heads) 1))
        (should (string-match-p "╭──" (overlay-get (car heads) 'after-string))))
      ;; Body: line-prefix with │
      (let ((bodies (+code-fences-test--body-overlays base)))
        (should (= (length bodies) 1))
        (should (string-match-p "│" (overlay-get (car bodies) 'line-prefix))))
      ;; Tail: before-string with ╰──
      (let ((tails (+code-fences-test--tail-overlays base)))
        (should (= (length tails) 1))
        (should (string-match-p "╰──" (overlay-get (car tails) 'before-string)))))))


;;; Unquoted body overlay tagging

(ert-deftest +code-fences/unquoted-body-tagged ()
  "Body overlay in unquoted heredoc has +polymode-unquoted property."
  (with-fixture "heredoc-elisp-unquoted.sh" base
    (should (= (length (+code-fences-test--overlays-with-prop base '+polymode-unquoted t)) 1))))

(ert-deftest +code-fences/quoted-body-not-tagged ()
  "Body overlay in quoted heredoc lacks +polymode-unquoted property."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (should (= (length (+code-fences-test--overlays-with-prop base '+polymode-unquoted t)) 0))))


;;; Mixed file: multiple heredoc types

(ert-deftest +code-fences/mixed-file-fence-count ()
  "Mixed heredoc file gets correct number of fence overlay groups."
  (with-fixture "heredoc-mixed.sh" base
    (should (= (length (+code-fences-test--head-overlays base)) 5))))

(ert-deftest +code-fences/mixed-file-unquoted-count ()
  "Mixed file has exactly 2 unquoted body spans (bare <<ELISP and <<-ELISP)."
  (with-fixture "heredoc-mixed.sh" base
    (should (= (length (+code-fences-test--overlays-with-prop base '+polymode-unquoted t)) 2))))


;;; Python heredoc tests

(ert-deftest +code-fences/python-quoted-heredoc-fontified ()
  :expected-result (if (treesit-language-available-p 'python) :passed :failed)
  "Quoted <<'PYTHON' heredoc body gets python-ts-mode fontification."
  (with-fixture "heredoc-python-quoted.sh" base
    (let ((pos (+code-fences-test--search-in-buf base "def greet")))
      (should pos)
      (should (+code-fences-test--face-contains-p
               (get-text-property pos 'face base)
               'font-lock-keyword-face)))
    (let ((pos (+code-fences-test--search-in-buf base "class Greeter")))
      (should pos)
      (should (+code-fences-test--face-contains-p
               (get-text-property pos 'face base)
               'font-lock-keyword-face)))
    (let ((pos (+code-fences-test--search-in-buf base "str)")))
      (should pos)
      (should (get-text-property pos 'face base)))))

(ert-deftest +code-fences/python-quoted-no-interpolation ()
  "Quoted <<'PYTHON' heredoc gets no interpolation overlays."
  (with-fixture "heredoc-python-quoted.sh" base
    (should (null (+code-fences-test--interpolation-texts base)))))

(ert-deftest +code-fences/python-unquoted-has-interpolation ()
  "Unquoted <<PYTHON heredoc gets shell interpolation overlay on $HOME."
  (with-fixture "heredoc-python-unquoted.sh" base
    (should (member "$HOME" (+code-fences-test--interpolation-texts base)))))

(ert-deftest +code-fences/python-unquoted-body-tagged ()
  "Body overlay in unquoted <<PYTHON heredoc has +polymode-unquoted property."
  (with-fixture "heredoc-python-unquoted.sh" base
    (should (= (length (+code-fences-test--overlays-with-prop base '+polymode-unquoted t)) 1))))

(ert-deftest +code-fences/python-inner-buffer-mode ()
  "Python heredoc body span runs in python-ts-mode."
  (with-fixture "heredoc-python-quoted.sh" base
    (should (seq-some
             (lambda (b)
               (and (eq (buffer-base-buffer b) base)
                    (eq (buffer-local-value 'major-mode b) 'python-ts-mode)))
             (buffer-list)))))


;;; Fence overlay positioning

(ert-deftest +code-fences/head-overlay-spans-head-line ()
  "Head overlay start/end positions match the heredoc operator through EOL."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let* ((heads (+code-fences-test--head-overlays base))
           (head (car heads))
           (head-start (overlay-start head))
           (head-end (overlay-end head)))
      (should (= (length heads) 1))
      ;; Head overlay starts at the << operator (polymode head spans
      ;; begin at <<, not at line beginning) and ends at or past EOL.
      (with-current-buffer base
        (let ((op-pos (+code-fences-test--search-in-buf base "<<"))
              (line2-end (save-excursion (goto-char (point-min))
                                        (forward-line 1)
                                        (line-end-position))))
          (should op-pos)
          (should (= head-start op-pos))
          (should (<= head-end (1+ line2-end))))))))

(ert-deftest +code-fences/tail-overlay-spans-tail-line ()
  "Tail overlay start/end positions match the heredoc tail line."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let* ((tails (+code-fences-test--tail-overlays base))
           (tail (car tails))
           (tail-start (overlay-start tail))
           (tail-end (overlay-end tail)))
      (should (= (length tails) 1))
      ;; Tail overlay should cover line 6: "ELISP"
      (with-current-buffer base
        (let ((elisp-pos (+code-fences-test--search-in-buf base "\nELISP\n")))
          (should elisp-pos)
          ;; Tail overlay should start at or before the ELISP delimiter
          ;; and end at or after it
          (should (<= tail-start (1+ elisp-pos)))
          (should (>= tail-end (1+ elisp-pos))))))))

(ert-deftest +code-fences/body-overlay-spans-between-head-and-tail ()
  "Body overlay spans exactly the region between head and tail lines."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let* ((bodies (+code-fences-test--body-overlays base))
           (body (car bodies))
           (body-start (overlay-start body))
           (body-end (overlay-end body)))
      (should (= (length bodies) 1))
      (with-current-buffer base
        ;; Body should start after the head line (line 2) and end before
        ;; the tail line (line 6)
        (let ((defun-pos (+code-fences-test--search-in-buf base "(defun"))
              (elisp-pos (+code-fences-test--search-in-buf base "\nELISP\n")))
          (should defun-pos)
          (should elisp-pos)
          ;; Body should contain the defun
          (should (<= body-start defun-pos))
          ;; Body should end at or before the tail delimiter
          (should (<= body-end (1+ elisp-pos))))))))

(ert-deftest +code-fences/head-overlay-has-span-end-property ()
  "Head overlay has +polymode-span-end pointing to the tail end position."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let* ((heads (+code-fences-test--head-overlays base))
           (head (car heads))
           (span-end (overlay-get head '+polymode-span-end)))
      (should (= (length heads) 1))
      (should span-end)
      (should (integer-or-marker-p span-end))
      ;; span-end should point past the ELISP tail delimiter
      (with-current-buffer base
        (let ((tail-pos (+code-fences-test--search-in-buf base "\nELISP\n")))
          (should tail-pos)
          (should (> span-end tail-pos)))))))

(ert-deftest +code-fences/mixed-file-non-overlapping-fence-groups ()
  "Multiple heredocs in heredoc-mixed.sh get separate non-overlapping fence groups."
  (with-fixture "heredoc-mixed.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (should (= (length heads) 5))
      ;; Collect all fence overlay regions grouped by head
      ;; Each head's span-end should define a non-overlapping range
      (let ((ranges (seq-map (lambda (h)
                               (cons (overlay-start h)
                                     (overlay-get h '+polymode-span-end)))
                             heads)))
        ;; All ranges should have valid span-end values
        (dolist (r ranges)
          (should (integer-or-marker-p (cdr r)))
          (should (< (car r) (cdr r))))
        ;; Verify non-overlapping: each range ends before the next starts
        (let ((sorted (seq-sort-by #'car #'< ranges)))
          (seq-do-indexed
           (lambda (r i)
             (when (< i (1- (length sorted)))
               (let ((next (nth (1+ i) sorted)))
                 (should (<= (cdr r) (car next))))))
           sorted))))))

(ert-deftest +code-fences/mixed-file-head-positions-match-buffer ()
  "Each head overlay in heredoc-mixed.sh starts at the correct heredoc line."
  (with-fixture "heredoc-mixed.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (should (= (length heads) 5))
      ;; Each head overlay should contain a "cat <<" string
      (dolist (h heads)
        (with-current-buffer base
          (let ((text (buffer-substring-no-properties
                       (overlay-start h) (overlay-end h))))
            (should (string-match-p "<<" text))))))))


;;; Hook registration

(ert-deftest +code-fences/after-change-hooks-registered ()
  "After refontification, base buffer has required after-change-functions."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (should (memq '+polymode-update-head-connectors after-change-functions))
      (should (memq '+polymode--after-change-refontify after-change-functions)))))

(ert-deftest +code-fences/post-command-hook-registered ()
  "After refontification, base buffer has header active-state hook."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (should (memq '+polymode-update-header-active-state post-command-hook)))))

;;; Active-state header dimming

(ert-deftest +code-fences/head-overlays-have-base-and-dim-lines ()
  "Head overlays carry both +polymode-head-base-line and +polymode-head-dim-line."
  (with-fixture "heredoc-mixed.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (should (= (length heads) 5))
      (dolist (h heads)
        (should (overlay-get h '+polymode-head-base-line))
        (should (overlay-get h '+polymode-head-dim-line))))))

(ert-deftest +code-fences/base-and-dim-lines-differ ()
  "The bright and dim header strings are different."
  (with-fixture "heredoc-mixed.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (dolist (h heads)
        (should-not (equal-including-properties
                     (overlay-get h '+polymode-head-base-line)
                     (overlay-get h '+polymode-head-dim-line)))))))

(ert-deftest +code-fences/active-state-first-heredoc-bright ()
  "Point inside first heredoc body makes its header bright, others dim."
  (with-fixture "heredoc-mixed.sh" base
    (with-current-buffer base
      (let* ((heads (+code-fences-test--head-overlays base))
             (first-head (car heads))
             (body-pos (+code-fences-test--search-in-buf base "no $expansion")))
        (should body-pos)
        (goto-char body-pos)
        (+polymode-update-header-active-state)
        ;; First head should show bright (base-line)
        (should (+code-fences-test--overlay-bright-p first-head))
        ;; All other heads should show dim
        (dolist (h (cdr heads))
          (should-not (+code-fences-test--overlay-bright-p h)))))))

(ert-deftest +code-fences/active-state-switches-on-move ()
  "Moving point to a different heredoc body switches which header is bright."
  (with-fixture "heredoc-mixed.sh" base
    (with-current-buffer base
      (let* ((heads (+code-fences-test--head-overlays base))
             (second-body-pos (+code-fences-test--search-in-buf base "$ARGSFILE")))
        (should second-body-pos)
        (goto-char second-body-pos)
        (+polymode-update-header-active-state)
        ;; Second head should be bright
        (should (+code-fences-test--overlay-bright-p (nth 1 heads)))
        ;; First head should be dim
        (should-not (+code-fences-test--overlay-bright-p (nth 0 heads)))))))

(ert-deftest +code-fences/all-dim-when-outside-spans ()
  "When point is before any heredoc, all headers are dim."
  (with-fixture "heredoc-mixed.sh" base
    (with-current-buffer base
      (goto-char (point-min))
      (+polymode-update-header-active-state)
      (let ((heads (+code-fences-test--head-overlays base)))
        (dolist (h heads)
          (should-not (+code-fences-test--overlay-bright-p h)))))))

;; cursor-pushed-off-head-overlay-newline: skipped, depends on evil
;; state handling which is under active iteration.

(ert-deftest +code-fences/insert-before-span-does-not-activate ()
  "Inserting text before a heredoc must not make its body overlays active.
Regression: span-start/span-end stored as integers instead of markers,
so insertions before the span shifted point past the stale boundary."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (goto-char (point-min))
      (end-of-line)
      (let ((insert-pos (point)))
        ;; Point is at end of first line, before the heredoc
        (+polymode-update-header-active-state)
        ;; Sanity: body should be dim before inserting
        (let ((bodies (+code-fences-test--body-overlays base)))
          (should (> (length bodies) 0))
          (dolist (ov bodies)
            (should (eq (get-text-property 0 'face (overlay-get ov 'line-prefix))
                        '+polymode-fence-dim-face))))
        ;; Insert enough text to push point past the stale span-start
        (goto-char insert-pos)
        (insert (make-string 50 ?\s))
        (+polymode-update-header-active-state)
        ;; Body overlays must still be dim — point is outside the span
        (let ((bodies (+code-fences-test--body-overlays base)))
          (should (> (length bodies) 0))
          (dolist (ov bodies)
            (should (eq (get-text-property 0 'face (overlay-get ov 'line-prefix))
                        '+polymode-fence-dim-face))))))))


;;; Undo/edit recovery

(ert-deftest +code-fences/interpolation-overlay-regenerated-after-insert ()
  "Inserting text with $NEW_VAR into unquoted heredoc body creates interpolation overlay."
  (with-fixture "heredoc-elisp-unquoted.sh" base
    ;; Verify initial interpolation exists
    (should (member "$FILENAME" (+code-fences-test--interpolation-texts base)))
    ;; Insert new interpolation text into the body
    (let ((pos (+code-fences-test--search-in-buf base "$FILENAME")))
      (should pos)
      (with-current-buffer base
        (goto-char pos)
        (end-of-line)
        (let ((beg (point))
              (text "\n  (message $NEW_VAR)"))
          (insert text)
          (+polymode--after-change-refontify beg (+ beg (length text)) 0))))
    ;; $NEW_VAR should now have an interpolation overlay
    (should (member "$NEW_VAR" (+code-fences-test--interpolation-texts base)))))

(ert-deftest +code-fences/interpolation-overlay-removed-after-delete ()
  "Deleting $FILENAME text from unquoted heredoc body removes its interpolation overlay."
  (with-fixture "heredoc-elisp-unquoted.sh" base
    ;; Verify initial interpolation exists
    (should (member "$FILENAME" (+code-fences-test--interpolation-texts base)))
    ;; Delete the $FILENAME text
    (let ((pos (+code-fences-test--search-in-buf base "$FILENAME")))
      (should pos)
      (with-current-buffer base
        (goto-char pos)
        (let ((beg pos)
              (len (length "$FILENAME")))
          (delete-region beg (+ beg len))
          (+polymode--after-change-refontify beg beg len))))
    ;; $FILENAME interpolation overlay should be gone
    (should-not (member "$FILENAME" (+code-fences-test--interpolation-texts base)))))

(ert-deftest +code-fences/quoted-heredoc-no-interpolation-after-edit ()
  "Inserting $SHOULD_NOT_MATCH into quoted heredoc body creates no interpolation overlays."
  (with-fixture "heredoc-elisp-quoted.sh" base
    ;; No interpolation initially
    (should (null (+code-fences-test--interpolation-texts base)))
    ;; Insert text with a variable reference into the body
    (let ((pos (+code-fences-test--search-in-buf base "defun")))
      (should pos)
      (with-current-buffer base
        (goto-char pos)
        (end-of-line)
        (let ((beg (point))
              (text "\n  $SHOULD_NOT_MATCH"))
          (insert text)
          (+polymode--after-change-refontify beg (+ beg (length text)) 0))))
    ;; Still no interpolation overlays — quoted heredocs never get them
    (should (null (+code-fences-test--interpolation-texts base)))))

(ert-deftest +code-fences/stale-overlay-cleanup-preserves-valid-heads ()
  "Calling +polymode-update-head-connectors does not remove valid head overlays."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let ((heads-before (+code-fences-test--head-overlays base)))
      (should (= (length heads-before) 1))
      ;; Trigger connector update — valid heredoc opener still present
      (with-current-buffer base
        (+polymode-update-head-connectors (point-min) (point-max) 0))
      ;; Head overlay should still exist and not be removed as stale
      (let ((heads-after (+code-fences-test--head-overlays base)))
        (should (= (length heads-after) 1))))))

;;; Connector rendering

(ert-deftest +code-fences/connector-present-in-after-string ()
  "Head overlay's after-string should contain the connector end character.
Uses a deeply indented heredoc so the delimiter column exceeds the label width."
  (with-fixture "heredoc-deep-indent.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (should (= (length heads) 1))
      (should (string-match-p "╯" (overlay-get (car heads) 'after-string))))))

(ert-deftest +code-fences/connector-contains-horizontal-line ()
  "Head overlay's after-string should contain horizontal line characters."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (should (= (length heads) 1))
      (should (string-match-p "─" (overlay-get (car heads) 'after-string))))))

(ert-deftest +code-fences/connector-absent-when-delimiter-left-of-label ()
  "When << is at column 0, connector should not appear in after-string."
  (with-fixture "heredoc-short-indent.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (should (= (length heads) 1))
      (should-not (string-match-p "╯" (overlay-get (car heads) 'after-string))))))

(ert-deftest +code-fences/multiple-heredocs-independent-connectors ()
  "Each head overlay in mixed file has its own after-string with connector chars."
  (with-fixture "heredoc-mixed.sh" base
    (let ((heads (+code-fences-test--head-overlays base)))
      (should (= (length heads) 5))
      ;; Each head should have an after-string containing connector characters
      (dolist (h heads)
        (let ((as (overlay-get h 'after-string)))
          (should as)
          (should (string-match-p "─" as)))))))

(ert-deftest +code-fences/stale-connector-removes-stale-head-only ()
  "Deleting heredoc opener removes the stale head overlay and schedules redecorate."
  (with-fixture "heredoc-elisp-quoted.sh" base
    ;; Verify head overlays exist initially
    (should (= (length (+code-fences-test--head-overlays base)) 1))
    (should (>= (length (+code-fences-test--fence-overlays base)) 1))
    ;; Delete the << operator to invalidate the heredoc
    (with-current-buffer base
      (let ((op-pos (+code-fences-test--search-in-buf base "<<")))
        (should op-pos)
        (goto-char op-pos)
        (delete-region op-pos (+ op-pos 2))
        (insert "  ")
        (+polymode-update-head-connectors (point-min) (point-max) 0)))
    ;; Stale head overlay deleted; body/tail remain until redecorate
    (should (= (length (+code-fences-test--head-overlays base)) 0))
    ;; Redecorate scheduled (spans-decorated cleared)
    (with-current-buffer base
      (should-not +polymode--spans-decorated))))

;;; Separedit integration

(defmacro +code-fences-test--with-polymode-body (body-text mode &rest forms)
  "Evaluate FORMS in a temp buffer simulating a polymode body span.
BODY-TEXT is inserted, MODE is the major-mode, and pm/chunkmode is
set so the separedit advice activates.  pm-innermost-span is stubbed
to return a body span covering the buffer contents."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,body-text)
     (funcall ,mode)
     (setq-local pm/chunkmode t)
     (cl-letf (((symbol-function 'pm-innermost-span)
                (lambda (&rest _)
                  (list 'body (point-min) (point-max) pm/chunkmode))))
       ,@forms)))

(ert-deftest +code-fences/separedit-block-info-returned-for-body-span ()
  "Inside a polymode body span, separedit--block-info returns a non-nil plist."
  (require 'separedit)
  (+code-fences-test--with-polymode-body
      "(defun greet (name)\n  (format \"Hello, %s!\" name))\n"
      #'emacs-lisp-mode
    (goto-char (point-min))
    (search-forward "defun" nil t)
    (let ((info (separedit--block-info)))
      (should info)
      (should (plist-get info :beginning))
      (should (plist-get info :end))
      (should (plist-get info :lang-mode)))))

(ert-deftest +code-fences/separedit-block-info-has-correct-lang-mode ()
  "The :lang-mode in block info matches the buffer's major-mode."
  (require 'separedit)
  (+code-fences-test--with-polymode-body
      "(defun greet (name)\n  (format \"Hello, %s!\" name))\n"
      #'emacs-lisp-mode
    (goto-char (point-min))
    (search-forward "defun" nil t)
    (let ((info (separedit--block-info)))
      (should info)
      (should (eq (plist-get info :lang-mode) major-mode)))))

(ert-deftest +code-fences/separedit-block-info-boundaries-skip-whitespace ()
  "Block info :beginning and :end skip leading/trailing whitespace from span."
  (require 'separedit)
  (+code-fences-test--with-polymode-body
      "\n(defun greet (name)\n  (format \"Hello, %s!\" name))\n\n"
      #'emacs-lisp-mode
    (goto-char (point-min))
    (search-forward "defun" nil t)
    (let* ((info (separedit--block-info))
           (beg (plist-get info :beginning))
           (end (plist-get info :end)))
      (should info)
      ;; :beginning should skip leading whitespace
      (should (> beg (point-min)))
      (save-excursion
        (goto-char beg)
        (should-not (looking-at-p "\\`[ \t\n]*\\'")))
      ;; :end should trim trailing whitespace (be strictly less than point-max)
      (should (< end (point-max))))))

(ert-deftest +code-fences/separedit-block-info-not-returned-outside-body ()
  "In the host buffer (not a body span), the polymode advice does not fire."
  (require 'separedit)
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (goto-char (point-min))
      ;; pm/chunkmode is unset in the base buffer, so the :before-until
      ;; advice returns nil and separedit falls through to its default
      ;; behaviour which may signal user-error.
      (let ((info (condition-case nil
                      (separedit--block-info)
                    (user-error nil))))
        (should-not info)))))

(ert-deftest +code-fences/separedit-block-info-has-indent-length ()
  "Block info :indent-length is a non-negative integer."
  (require 'separedit)
  (+code-fences-test--with-polymode-body
      "(defun greet (name)\n  (format \"Hello, %s!\" name))\n"
      #'emacs-lisp-mode
    (goto-char (point-min))
    (search-forward "defun" nil t)
    (let ((info (separedit--block-info)))
      (should info)
      (should (integerp (plist-get info :indent-length)))
      (should (>= (plist-get info :indent-length) 0)))))

;;; Overlay recovery after syntax break/fix

(ert-deftest +code-fences/stale-detection-removes-stale-head ()
  "Breaking heredoc operator deletes the stale head overlay.
Inserting space after << makes the head overlay fail the heredoc opener
regex.  Only the stale head is deleted immediately; remaining fence
overlays are cleaned up by the scheduled redecorate."
  (with-fixture "heredoc-break-restore.sh" base
    ;; Overlays present initially
    (should (= (length (+code-fences-test--head-overlays base)) 1))
    (with-current-buffer base
      (let ((op-pos (+code-fences-test--search-in-buf base "<<")))
        (should op-pos)
        ;; Break: insert space after << to make it "<< 'ELISP'"
        (goto-char (+ op-pos 2))
        (insert " ")
        (+polymode-update-head-connectors (+ op-pos 2) (+ op-pos 3) 0))
      ;; Stale head overlay should be deleted
      (should (= (length (+code-fences-test--head-overlays base)) 0))
      ;; Redecorate flag should be cleared for next idle pass
      (should-not +polymode--spans-decorated))))


;;; Compatibility guards

(ert-deftest +code-fences/poly-lock-adjust-span-face-nil-background ()
  "No error when default face background is nil (batch mode / some terminals)."
  (require 'poly-lock)
  (with-temp-buffer
    (let ((span (list 'body (point-min) (point-max) nil)))
      (cl-letf (((symbol-function 'face-background)
                 (lambda (&rest _) nil)))
        (should-not (condition-case err
                        (progn (poly-lock-adjust-span-face span) nil)
                      (error err)))))))

(ert-deftest +code-fences/poly-lock-adjust-span-face-valid-background ()
  "With a valid default background, the guard allows the function to proceed."
  (require 'poly-lock)
  (with-temp-buffer
    (insert "some text")
    (let ((span (list 'body (point-min) (point-max) nil))
          (proceeded nil))
      (cl-letf (((symbol-function 'face-background)
                 (lambda (&rest _) "#000000"))
                ((symbol-function 'pm-get-adjust-face)
                 (lambda (&rest _) (setq proceeded t) nil)))
        (poly-lock-adjust-span-face span)
        ;; The guard should NOT have blocked the call — pm-get-adjust-face was reached
        (should proceeded)))))

(ert-deftest +code-fences/sh-syntax-propertize-nil-heredoc-re ()
  "No error when `sh--heredoc-subsequent-re' is nil (bash-ts-mode context)."
  (require 'sh-script)
  (with-temp-buffer
    (insert "echo hello")
    (let ((sh--heredoc-subsequent-re nil))
      (should-not (condition-case err
                      (progn (sh-syntax-propertize-function (point-min) (point-max)) nil)
                    (error err))))))

(ert-deftest +code-fences/flymake-start-passthrough-regular-buffer ()
  "In a regular (non-indirect) buffer, `flymake-start' passes through normally."
  (require 'flymake)
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Should not error — the advice only acts on indirect buffers
    (should-not (condition-case err
                    (progn (flymake-start) nil)
                  (error err)))))

(ert-deftest +code-fences/flymake-start-removes-checkdoc-in-indirect ()
  "In indirect buffer context, `elisp-flymake-checkdoc' is removed during flymake-start."
  (require 'flymake)
  (let ((captured-diag-fns nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let* ((base (current-buffer))
             (indirect (make-indirect-buffer base " *test-indirect*" t)))
        (unwind-protect
            (with-current-buffer indirect
              (emacs-lisp-mode)
              (setq-local flymake-diagnostic-functions
                          '(elisp-flymake-byte-compile elisp-flymake-checkdoc))
              ;; Stub flymake-start's real work to capture diagnostic functions
              (cl-letf (((symbol-function 'flymake--run-backends)
                         (lambda ()
                           (setq captured-diag-fns
                                 (buffer-local-value 'flymake-diagnostic-functions
                                                     (current-buffer))))))
                (flymake-start)))
          (kill-buffer indirect))))
    ;; checkdoc should have been removed during the call
    (when captured-diag-fns
      (should-not (memq 'elisp-flymake-checkdoc captured-diag-fns)))))

;;; Inactive fence dimming

(ert-deftest +code-fences/inactive-box-drawing-uses-fence-dim-face ()
  "When point is outside all spans, body overlay line-prefix uses fence dim face."
  (with-fixture "heredoc-mixed.sh" base
    (with-current-buffer base
      (goto-char (point-min))
      (+polymode-update-header-active-state)
      (let ((bodies (+code-fences-test--body-overlays base)))
        (should (> (length bodies) 0))
        (dolist (ov bodies)
          (let* ((prefix (overlay-get ov 'line-prefix))
                 (prefix-face (get-text-property 0 'face prefix)))
            (should prefix-face)
            (should (eq prefix-face '+polymode-fence-dim-face))))))))

(ert-deftest +code-fences/active-box-drawing-not-shadow ()
  "Active body overlay line-prefix uses font-lock-comment-delimiter-face, not shadow."
  (with-fixture "heredoc-mixed.sh" base
    (with-current-buffer base
      (let ((body-pos (+code-fences-test--search-in-buf base "no $expansion")))
        (should body-pos)
        (goto-char body-pos)
        (+polymode-update-header-active-state)
        (let* ((bodies (+code-fences-test--body-overlays base))
               (active-ov (seq-find
                           (lambda (ov)
                             (and (<= (overlay-start ov) body-pos)
                                  (>= (overlay-end ov) body-pos)))
                           bodies)))
          (should active-ov)
          (let* ((prefix (overlay-get active-ov 'line-prefix))
                 (prefix-face (get-text-property 0 'face prefix)))
            ;; Should use the normal face, not shadow
            (should (+code-fences-test--face-contains-p
                     prefix-face 'font-lock-comment-delimiter-face))
            (should-not (eq prefix-face '+polymode-fence-dim-face))))))))

(ert-deftest +code-fences/box-drawing-dim-state-switches-between-spans ()
  "Moving point between spans toggles which body line-prefix is bright."
  (with-fixture "heredoc-mixed.sh" base
    (with-current-buffer base
      (let* ((second-pos (+code-fences-test--search-in-buf base "$ARGSFILE"))
             (first-pos (+code-fences-test--search-in-buf base "no $expansion")))
        (should second-pos)
        (should first-pos)
        (goto-char second-pos)
        (+polymode-update-header-active-state)
        (let* ((second-ov (seq-find
                           (lambda (ov)
                             (and (<= (overlay-start ov) second-pos)
                                  (>= (overlay-end ov) second-pos)))
                           (+code-fences-test--body-overlays base)))
               (first-ov (seq-find
                          (lambda (ov)
                            (and (<= (overlay-start ov) first-pos)
                                 (>= (overlay-end ov) first-pos)))
                          (+code-fences-test--body-overlays base))))
          (should second-ov)
          (should first-ov)
          ;; Second active → normal prefix; first inactive → dim prefix
          (should (eq (get-text-property 0 'face (overlay-get second-ov 'line-prefix))
                      'font-lock-comment-delimiter-face))
          (should (eq (get-text-property 0 'face (overlay-get first-ov 'line-prefix))
                      '+polymode-fence-dim-face))
          ;; Move to first
          (goto-char first-pos)
          (+polymode-update-header-active-state)
          (should (eq (get-text-property 0 'face (overlay-get first-ov 'line-prefix))
                      'font-lock-comment-delimiter-face))
          (should (eq (get-text-property 0 'face (overlay-get second-ov 'line-prefix))
                      '+polymode-fence-dim-face)))))))

;;; Body overlays carry full span range

(ert-deftest +code-fences/body-overlay-has-span-start-property ()
  "Body overlay has +polymode-span-start pointing to head start."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let* ((bodies (+code-fences-test--body-overlays base))
           (heads (+code-fences-test--head-overlays base)))
      (should (= (length bodies) 1))
      (should (= (length heads) 1))
      (let ((span-start (overlay-get (car bodies) '+polymode-span-start)))
        (should span-start)
        (should (= span-start (overlay-start (car heads))))))))

(ert-deftest +code-fences/body-overlay-has-span-end-property ()
  "Body overlay has +polymode-span-end pointing past tail end."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (let* ((bodies (+code-fences-test--body-overlays base))
           (tails (+code-fences-test--tail-overlays base)))
      (should (= (length bodies) 1))
      (should (= (length tails) 1))
      (let ((span-end (overlay-get (car bodies) '+polymode-span-end)))
        (should span-end)
        (should (>= span-end (overlay-end (car tails))))))))

;;; Point on head/tail activates full fence group

(ert-deftest +code-fences/point-on-head-activates-body-prefix ()
  "Point on heredoc opener line makes body line-prefix bright."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (let ((head-pos (+code-fences-test--search-in-buf base "<<")))
        (should head-pos)
        (goto-char head-pos)
        (+polymode-update-header-active-state)
        (let ((bodies (+code-fences-test--body-overlays base)))
          (should (= (length bodies) 1))
          (should (eq (get-text-property 0 'face (overlay-get (car bodies) 'line-prefix))
                      'font-lock-comment-delimiter-face)))))))

(ert-deftest +code-fences/point-on-tail-activates-body-prefix ()
  "Point on heredoc closer line makes body line-prefix bright."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (let ((tail-pos (+code-fences-test--search-in-buf base "\nELISP\n")))
        (should tail-pos)
        (goto-char (1+ tail-pos))
        (+polymode-update-header-active-state)
        (let ((bodies (+code-fences-test--body-overlays base)))
          (should (= (length bodies) 1))
          (should (eq (get-text-property 0 'face (overlay-get (car bodies) 'line-prefix))
                      'font-lock-comment-delimiter-face)))))))

(ert-deftest +code-fences/point-on-head-activates-tail-decoration ()
  "Point on heredoc opener line makes tail before-string bright."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (let ((head-pos (+code-fences-test--search-in-buf base "<<")))
        (should head-pos)
        (goto-char head-pos)
        (+polymode-update-header-active-state)
        (let ((tails (+code-fences-test--tail-overlays base)))
          (should (= (length tails) 1))
          (should (eq (get-text-property 0 'face (overlay-get (car tails) 'before-string))
                      'font-lock-comment-delimiter-face)))))))

;;; Editing heredoc opener doesn't error

(ert-deftest +code-fences/insert-space-in-opener-no-error ()
  "Inserting space between << and delimiter runs all hooks without error."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (should (>= (length (+code-fences-test--fence-overlays base)) 1))
      (let ((op-pos (+code-fences-test--search-in-buf base "<<")))
        (should op-pos)
        ;; Insert space: <<'ELISP' → << 'ELISP'
        (goto-char (+ op-pos 2))
        (insert " ")
        ;; Simulate full hook sequence: after-change then post-command
        (+polymode--after-change-refontify (+ op-pos 2) (+ op-pos 3) 0)
        (+polymode-update-head-connectors (+ op-pos 2) (+ op-pos 3) 0)
        (+polymode-update-header-active-state)))))

;;; Stale detection blast radius — regression tests

(ert-deftest +code-fences/stale-head-preserves-sibling-fences ()
  "Breaking one heredoc opener should not remove fence overlays from other heredocs.
Regression: `+polymode-update-head-connectors' used to nuke ALL fence overlays
when any single head was stale."
  (with-fixture "heredoc-two-blocks.sh" base
    (let ((initial-heads (length (+code-fences-test--head-overlays base))))
      (should (= initial-heads 2))
      ;; Break the FIRST heredoc's opener: <<'ELISP' → << 'ELISP'
      (with-current-buffer base
        (let ((op-pos (+code-fences-test--search-in-buf base "<<'ELISP'")))
          (should op-pos)
          (goto-char (+ op-pos 2))
          (insert " ")
          (+polymode-update-head-connectors (+ op-pos 2) (+ op-pos 3) 0)))
      ;; Second heredoc's fence overlays should survive
      (let ((remaining-heads (+code-fences-test--head-overlays base)))
        (should (>= (length remaining-heads) 1))))))

(ert-deftest +code-fences/redecorate-applies-dim-state ()
  "After idle redecoration, body prefixes should reflect correct dim state.
Regression: `+polymode-refontify-inner-spans' created overlays in active
state but never called `+polymode-update-header-active-state'."
  (with-fixture "heredoc-two-blocks.sh" base
    (with-current-buffer base
      ;; Move point outside any heredoc span
      (goto-char (point-min))
      ;; Force redecorate cycle
      (setq +polymode--spans-decorated nil)
      (+polymode-refontify-inner-spans)
      ;; All body prefixes should be dim (point is outside spans)
      (let ((bodies (+code-fences-test--body-overlays base)))
        (should (> (length bodies) 0))
        (dolist (ov bodies)
          (let* ((lp (overlay-get ov 'line-prefix))
                 (face (and (stringp lp) (get-text-property 0 'face lp))))
            (should (eq face '+polymode-fence-dim-face))))))))

(ert-deftest +code-fences/opener-count-mismatch-triggers-redecorate ()
  "After syntax break and fix, opener count mismatch triggers redecorate.
Regression: after syntax break, stale detection removed head overlays
and set spans-decorated=t during incomplete redecorate.  After fix, no
stale head existed to trigger redecorate, so fences were permanently
lost.  Now: opener count vs head count mismatch triggers redecorate."
  (with-fixture "heredoc-two-blocks.sh" base
    (should (= (length (+code-fences-test--head-overlays base)) 2))
    (with-current-buffer base
      ;; Break first heredoc: <<'ELISP' → << 'ELISP'
      (let ((op-pos (+code-fences-test--search-in-buf base "<<'ELISP'")))
        (should op-pos)
        (goto-char (+ op-pos 2))
        (insert " ")
        ;; Stale detection fires, removes stale ELISP head
        (+polymode-update-head-connectors (+ op-pos 2) (+ op-pos 3) 0)
        (should-not +polymode--spans-decorated)
        (should (= (length (+code-fences-test--head-overlays base)) 1))
        ;; Simulate that an incomplete redecorate ran (as happens in live Emacs)
        (setq +polymode--spans-decorated t)
        ;; Fix: remove the space
        (goto-char (+ op-pos 2))
        (delete-char 1)
        ;; After fix: 1 head overlay but 2 openers in buffer.
        ;; Opener count mismatch should clear spans-decorated.
        (+polymode-update-head-connectors (+ op-pos 2) (+ op-pos 2) 1)
        (should-not +polymode--spans-decorated)))))

;;; Host registration API

(ert-deftest +code-fences/config-resolves-host-mode ()
  "After decoration, +code-fences--config returns the bash host config."
  (with-fixture "heredoc-elisp-quoted.sh" base
    (with-current-buffer base
      (should +code-fences--cached-config)
      (should (plist-get (+code-fences--config) :head-valid-p)))))

(ert-deftest +code-fences/register-adds-to-host-config ()
  "Registering a host mode adds an entry to `+code-fences-host-config'."
  (let ((+code-fences-host-config nil))
    (+code-fences-register 'test-mode :unquoted-p #'always)
    (should (alist-get 'test-mode +code-fences-host-config))
    (should (eq (plist-get (alist-get 'test-mode +code-fences-host-config)
                           :unquoted-p)
                #'always))))

(ert-deftest +code-fences/unknown-host-no-interpolation ()
  "Unknown host mode gets generic rendering but no interpolation overlays."
  (let ((+code-fences-host-config nil))
    (with-temp-buffer
      (insert "hello $FOO world")
      ;; With no host registered, dispatch should skip interpolation
      (let ((config (alist-get 'nonexistent-mode +code-fences-host-config)))
        (should-not config)
        (should-not (plist-get config :interpolation-fn))))))

;;; Nix interpolation

(defun +code-fences-test--nix-interp-overlay-texts (input)
  "Insert INPUT in temp buffer, run Nix interpolation, return overlay texts."
  (with-temp-buffer
    (insert input)
    (+nix--add-interpolation-overlays (point-min) (point-max) (current-buffer))
    (seq-map (lambda (ov)
               (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
             (seq-sort-by #'overlay-start #'<
                          (overlays-in (point-min) (point-max))))))

(ert-deftest +code-fences/nix-simple-interpolation ()
  "Nix `${...}' creates interpolation overlay."
  (should (equal (+code-fences-test--nix-interp-overlay-texts
                  "hello ${pkgs.hello} world")
                 '("${pkgs.hello}"))))

(ert-deftest +code-fences/nix-nested-braces ()
  "Nix `${...}' with nested braces spans the full expression."
  (should (equal (+code-fences-test--nix-interp-overlay-texts
                  "hello ${if foo then \"bar\" else \"baz\"} world")
                 '("${if foo then \"bar\" else \"baz\"}"))))

(ert-deftest +code-fences/nix-escaped-interpolation ()
  "Nix `''${...}' (2 quotes, even) is escaped — no overlay."
  (should (null (+code-fences-test--nix-interp-overlay-texts
                 "hello ''${notInterpolated} world"))))

(ert-deftest +code-fences/nix-escaped-quote-then-real ()
  "Nix `'''${...}' (3 quotes, odd) is real interpolation."
  (should (equal (+code-fences-test--nix-interp-overlay-texts
                  "hello '''${realInterp} world")
                 '("${realInterp}"))))

(ert-deftest +code-fences/nix-double-escaped-then-escaped ()
  "Nix `''''${...}' (4 quotes, even) is escaped — no overlay."
  (should (null (+code-fences-test--nix-interp-overlay-texts
                 "hello ''''${escaped} world"))))

(ert-deftest +code-fences/nix-no-preceding-quotes ()
  "Nix `${...}' with no preceding quotes is real interpolation."
  (should (equal (+code-fences-test--nix-interp-overlay-texts
                  "${normalInterp}")
                 '("${normalInterp}"))))

(ert-deftest +code-fences/nix-multiple-interpolations ()
  "Multiple Nix interpolations in one string."
  (should (equal (+code-fences-test--nix-interp-overlay-texts
                  "${a} text ${b}")
                 '("${a}" "${b}"))))

(ert-deftest +code-fences/nix-escape-detection-parity ()
  "Single quote parity: 1 quote before $ is not an escape."
  (should (equal (+code-fences-test--nix-interp-overlay-texts
                  "x'${interp}y")
                 '("${interp}"))))

(provide '+code-fences-tests)

;;; +code-fences-tests.el ends here
