;;; tests.el --- Presentation tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the presentation module.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (let* ((this (or load-file-name buffer-file-name byte-compile-current-file))
         (dir (and this (file-name-directory this))))
    (when dir
      (load (expand-file-name "lib.el" dir) nil t))))


;;; Module-init negative assertions

(defun +presentation-tests--load-init ()
  "Load `modules/presentation/init.el'.
Returns t if loaded, nil if the file is unavailable."
  (let ((init (expand-file-name "modules/presentation/init.el"
                                user-emacs-directory)))
    (when (file-exists-p init)
      (condition-case nil (load init nil t) (error nil))
      t)))

(ert-deftest +presentation/no-mcp-tools-registered ()
  "Module init no longer registers presentation MCP tools."
  (skip-unless (+presentation-tests--load-init))
  (when (boundp 'claude-code-ide-mcp-server-tools)
    (let ((names (mapcar (lambda (s) (plist-get s :name))
                         claude-code-ide-mcp-server-tools)))
      (dolist (n '("start_presentation" "present_document" "get_presentation"
                   "end_presentation" "push_slide" "replace_slide"
                   "truncate_after" "goto_slide" "get_deck"))
        (should-not (member n names))))))


;;; Surviving surface

(ert-deftest +presentation/focus-face-defined ()
  "`+presentation-focus-face' is defined."
  (should (facep '+presentation-focus-face)))

(ert-deftest +presentation/mode-map-defined ()
  "`+presentation-mode-map' is a keymap."
  (should (keymapp +presentation-mode-map)))

(ert-deftest +presentation/mode-is-buffer-local ()
  "`+presentation-mode' is a buffer-local minor mode."
  (with-temp-buffer
    (+presentation-mode 1)
    (should +presentation-mode)
    (+presentation-mode -1)
    (should-not +presentation-mode)))


;;; §3 Heading-narrowed slide model

(defun +presentation-tests--narrowed-first-line ()
  "Return the text of the first line in the narrowed region."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

(ert-deftest +presentation/heading-region-encloses-current-h1 ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (goto-char (point-min))
    (search-forward "body B")
    (let ((region (+presentation--heading-region (point))))
      (should region)
      (should (string-prefix-p "# B"
                               (buffer-substring-no-properties
                                (car region)
                                (min (cdr region) (+ (car region) 3))))))))

(ert-deftest +presentation/heading-region-treats-fence-as-opaque ()
  (with-temp-buffer
    (insert "# Real\n```\n# Fake\n```\n# Next\n")
    (should (= 2 (length (+presentation--all-h1-positions))))))

(ert-deftest +presentation/all-h1-positions-document-order ()
  (with-temp-buffer
    (insert "# A\n# B\n# C\n")
    (let ((positions (+presentation--all-h1-positions)))
      (should (= 3 (length positions)))
      (should (apply #'< positions)))))

(ert-deftest +presentation/heading-slug-downcases-and-collapses ()
  (should (equal "auth-tokens-v2"
                 (+presentation--heading-slug "Auth & Tokens (v2)")))
  (should (equal "setup"
                 (+presentation--heading-slug "!! Setup !!")))
  (should (equal "hello-world"
                 (+presentation--heading-slug "Hello   World"))))

(ert-deftest +presentation/narrow-to-heading-at-narrows ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (search-backward "body B")
    (+presentation--narrow-to-heading-at (point))
    (should (equal "# B" (+presentation-tests--narrowed-first-line)))))

(ert-deftest +presentation/narrow-before-first-h1-falls-back-to-first ()
  (with-temp-buffer
    (insert "preamble\n# A\nbody A\n# B\nbody B\n")
    (goto-char (point-min))
    (+presentation--narrow-to-heading-at (point))
    (should (equal "# A" (+presentation-tests--narrowed-first-line)))))

(ert-deftest +presentation/narrow-after-last-h1-stays-on-last ()
  (with-temp-buffer
    (insert "# A\n# B\n# C\nlast body\n")
    (goto-char (point-max))
    (+presentation--narrow-to-heading-at (point))
    (should (equal "# C" (+presentation-tests--narrowed-first-line)))))

(ert-deftest +presentation/next-slide-advances ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (search-backward "body A")
    (+presentation--narrow-to-heading-at (point))
    (+presentation-next-slide)
    (should (equal "# B" (+presentation-tests--narrowed-first-line)))))

(ert-deftest +presentation/next-slide-noop-at-last ()
  (with-temp-buffer
    (insert "# A\n# B\n# C\nlast\n")
    (search-backward "last")
    (+presentation--narrow-to-heading-at (point))
    (let ((before (point-min)))
      (+presentation-next-slide)
      (should (= before (point-min))))))

(ert-deftest +presentation/previous-slide-retreats ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (search-backward "body B")
    (+presentation--narrow-to-heading-at (point))
    (+presentation-previous-slide)
    (should (equal "# A" (+presentation-tests--narrowed-first-line)))))

(ert-deftest +presentation/previous-slide-noop-at-first ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\n")
    (search-backward "body A")
    (+presentation--narrow-to-heading-at (point))
    (let ((before (point-min)))
      (+presentation-previous-slide)
      (should (= before (point-min))))))


;;; §4 Minor mode + entry point

(defmacro +presentation-tests--with-temp-md (text path-var &rest body)
  "Write TEXT to a fresh markdown temp file, bind its path to PATH-VAR.
Run BODY; afterwards kill any buffer visiting the file and delete the file."
  (declare (indent 2))
  `(let ((,path-var (make-temp-file "pres-test-" nil ".md" ,text)))
     (unwind-protect (progn ,@body)
       (let ((b (get-file-buffer ,path-var)))
         (when (and b (buffer-live-p b))
           (with-current-buffer b (set-buffer-modified-p nil))
           (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
       (when (file-exists-p ,path-var) (delete-file ,path-var)))))

(ert-deftest +presentation/mode-keymap-binds-required-keys ()
  (should (eq #'+presentation-next-slide
              (lookup-key +presentation-mode-map (kbd "C-n"))))
  (should (eq #'+presentation-next-slide
              (lookup-key +presentation-mode-map (kbd "C-f"))))
  (should (eq #'+presentation-previous-slide
              (lookup-key +presentation-mode-map (kbd "C-p"))))
  (should (eq #'+presentation-previous-slide
              (lookup-key +presentation-mode-map (kbd "C-b"))))
  (should (eq #'+presentation-quit
              (lookup-key +presentation-mode-map (kbd "C-c q"))))
  (should (eq #'+presentation-follow-link
              (lookup-key +presentation-mode-map (kbd "RET")))))

(ert-deftest +presentation/mode-enable-narrows-to-enclosing-h1 ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\n")
    (search-backward "body B")
    (+presentation-mode 1)
    (should (equal "# B" (+presentation-tests--narrowed-first-line)))))

(ert-deftest +presentation/mode-disable-widens ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\n")
    (search-backward "body B")
    (+presentation-mode 1)
    (should (< (- (point-max) (point-min)) (buffer-size)))
    (+presentation-mode -1)
    (should (= (- (point-max) (point-min)) (buffer-size)))))

(ert-deftest +presentation/keymap-callable-from-evil-normal ()
  (skip-unless (require 'evil nil t))
  (with-temp-buffer
    (insert "# A\nbody A\n# B\n")
    (goto-char (point-min))
    (when (fboundp 'evil-local-mode) (evil-local-mode 1))
    (when (fboundp 'evil-normal-state) (evil-normal-state))
    (+presentation-mode 1)
    (should (eq #'+presentation-next-slide (key-binding (kbd "C-n"))))))

(ert-deftest +present-markdown/opens-and-enables-mode ()
  (+presentation-tests--with-temp-md "# A\nbody A\n# B\n" path
    (+present-markdown path)
    (let ((b (get-file-buffer path)))
      (should b)
      (with-current-buffer b
        (should +presentation-mode)
        (should (equal "# A" (+presentation-tests--narrowed-first-line)))))))

(ert-deftest +present-markdown/missing-file-signals-user-error ()
  (should-error (+present-markdown "/no/such/file/exists.md") :type 'user-error))

(ert-deftest +present-markdown/marks-owned-buffer-on-fresh-open ()
  (+presentation-tests--with-temp-md "# A\n" path
    (+present-markdown path)
    (with-current-buffer (get-file-buffer path)
      (should +presentation--owned-buffer))))

(ert-deftest +present-markdown/does-not-mark-when-buffer-pre-existed ()
  (+presentation-tests--with-temp-md "# A\n" path
    (find-file-noselect path)
    (+present-markdown path)
    (with-current-buffer (get-file-buffer path)
      (should-not +presentation--owned-buffer))))

(ert-deftest +presentation/quit-buries-non-owned-buffer ()
  (+presentation-tests--with-temp-md "# A\n" path
    (find-file-noselect path)
    (+present-markdown path)
    (let ((b (get-file-buffer path)))
      (with-current-buffer b
        (+presentation-quit)
        (should (buffer-live-p b))
        (should-not +presentation-mode)
        (should (= (- (point-max) (point-min)) (buffer-size)))))))

(ert-deftest +presentation/quit-kills-owned-buffer ()
  (+presentation-tests--with-temp-md "# A\n" path
    (+present-markdown path)
    (let ((b (get-file-buffer path)))
      (with-current-buffer b
        (let ((kill-buffer-query-functions nil)) (+presentation-quit)))
      (should-not (buffer-live-p b)))))


;;; §5 Heading-text in-doc links

(ert-deftest +presentation/parse-heading-link-extracts-slug ()
  (should (equal "auth-flow" (+presentation--parse-heading-link "#auth-flow")))
  (should (equal "section-1" (+presentation--parse-heading-link "#section-1")))
  (should-not (+presentation--parse-heading-link "/path"))
  (should-not (+presentation--parse-heading-link "https://example.com"))
  (should-not (+presentation--parse-heading-link "diff:a...b"))
  (should-not (+presentation--parse-heading-link nil)))

(ert-deftest +presentation/dispatch-heading-link-finds-h2-and-encloses ()
  (with-temp-buffer
    (insert "# First\nbody\n# Auth flow\n## Refresh tokens\nbody\n# Other\n")
    (let ((result (+presentation--dispatch-heading-link "refresh-tokens")))
      (should (consp result))
      (let ((heading-pos (car result))
            (region (cdr result)))
        (save-excursion
          (goto-char heading-pos)
          (should (looking-at "## Refresh tokens")))
        (save-excursion
          (goto-char (car region))
          (should (looking-at "# Auth flow")))))))

(ert-deftest +presentation/dispatch-heading-link-finds-h1 ()
  (with-temp-buffer
    (insert "# Token validation\nbody\n# Other\n")
    (let ((result (+presentation--dispatch-heading-link "token-validation")))
      (should (consp result))
      (save-excursion
        (goto-char (car result))
        (should (looking-at "# Token validation"))))))

(ert-deftest +presentation/dispatch-heading-link-non-match-returns-pass-through ()
  (with-temp-buffer
    (insert "# Foo\n# Bar\n")
    (should (eq 'pass-through (+presentation--dispatch-heading-link "missing")))))

(ert-deftest +presentation/follow-link-heading-narrows-and-pushes-mark ()
  (with-temp-buffer
    (insert "# Top\nSee [refresh](#refresh-tokens) here.\n# Auth flow\n## Refresh tokens\nbody\n")
    (goto-char (point-min))
    (+presentation-mode 1)
    (search-forward "[refresh]")
    (backward-char 3)
    (let ((click (point)))
      (+presentation-follow-link)
      (should (equal "# Auth flow" (+presentation-tests--narrowed-first-line)))
      (should (looking-at "## Refresh tokens"))
      (should (member click (mapcar #'marker-position
                                    (cons (mark-marker) mark-ring)))))))

(ert-deftest +presentation/follow-link-non-matching-slug-passes-through ()
  (let ((called 0))
    (cl-letf (((symbol-function 'markdown-follow-link-at-point)
               (lambda () (interactive) (cl-incf called))))
      (with-temp-buffer
        (insert "# Top\n[link](#nope) text.\n# Other\n")
        (goto-char (point-min))
        (+presentation-mode 1)
        (search-forward "[link]")
        (backward-char 2)
        (+presentation-follow-link)
        (should (= 1 called))))))


;;; §6 Source-range link preview overlay

(defmacro +presentation-tests--with-temp-source (lines path-var &rest body)
  "Write LINES (a list of strings) to a `.txt' temp file at PATH-VAR.
Run BODY then kill any visiting buffer and delete the file.
`.txt' is used to avoid loading language major modes that may prompt
for tree-sitter grammars and confuse downstream tests."
  (declare (indent 2))
  `(let ((,path-var (make-temp-file "src-test-" nil ".txt"
                                    (concat (mapconcat #'identity ,lines "\n")
                                            "\n"))))
     (unwind-protect (progn ,@body)
       (let ((b (find-buffer-visiting ,path-var)))
         (when b
           (with-current-buffer b (set-buffer-modified-p nil))
           (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
       (when (file-exists-p ,path-var) (delete-file ,path-var)))))

(ert-deftest +presentation/parse-source-link-with-range ()
  (let ((r (+presentation--parse-source-link "modules/auth.rs#L42-L67")))
    (should (equal "modules/auth.rs" (car r)))
    (should (= 42 (cadr r)))
    (should (= 67 (cddr r)))))

(ert-deftest +presentation/parse-source-link-single-line ()
  (let ((r (+presentation--parse-source-link "foo.el#L7")))
    (should (equal "foo.el" (car r)))
    (should (= 7 (cadr r)))
    (should (= 7 (cddr r)))))

(ert-deftest +presentation/parse-source-link-rejects-other-forms ()
  (should-not (+presentation--parse-source-link "modules/auth.rs"))
  (should-not (+presentation--parse-source-link "https://example.com"))
  (should-not (+presentation--parse-source-link "#auth-flow"))
  (should-not (+presentation--parse-source-link "diff:a...b"))
  (should-not (+presentation--parse-source-link nil)))

(ert-deftest +presentation/language-from-extension ()
  (should (equal "rust" (+presentation--language-from-extension "x.rs")))
  (should (equal "elisp" (+presentation--language-from-extension "x.el")))
  (should (equal "python" (+presentation--language-from-extension "x.py")))
  (should (equal "markdown" (+presentation--language-from-extension "x.md")))
  (should (equal "text" (+presentation--language-from-extension "x.unknown")))
  (should (equal "text" (+presentation--language-from-extension nil))))

(ert-deftest +presentation/read-line-range-small ()
  (+presentation-tests--with-temp-source '("line1" "line2" "line3" "line4") tmp
    (let ((r (+presentation--read-line-range tmp 2 3)))
      (should (equal '("line2" "line3") (car r)))
      (should (= 0 (cdr r))))))

(ert-deftest +presentation/read-line-range-truncates-and-reports-extra ()
  (+presentation-tests--with-temp-source
      (mapcar (lambda (n) (format "line%d" n)) (number-sequence 1 20)) tmp
    (let ((r (+presentation--read-line-range tmp 1 15)))
      (should (= 10 (length (car r))))
      (should (= 5 (cdr r))))))

(ert-deftest +presentation/read-line-range-missing-file ()
  (should (eq 'file-not-found
              (+presentation--read-line-range "/no/such/file" 1 5))))

(ert-deftest +presentation/read-line-range-invalid-range ()
  (+presentation-tests--with-temp-source '("line1" "line2") tmp
    (should (eq 'invalid-range (+presentation--read-line-range tmp 0 5)))
    (should (eq 'invalid-range (+presentation--read-line-range tmp 5 2)))
    (should (eq 'invalid-range (+presentation--read-line-range tmp 100 200)))))

(ert-deftest +presentation/build-preview-fence-no-footer ()
  (let ((s (+presentation--build-preview-fence
            "rust" "fn" "p" 1 5 "line1\nline2" 0)))
    (should (string-match-p "```rust fn · p:1-5" s))
    (should (string-match-p "line1\nline2" s))
    (should (string-match-p "```\\'" s))
    (should-not (string-match-p "more lines" s))))

(ert-deftest +presentation/build-preview-fence-with-footer ()
  (let ((s (+presentation--build-preview-fence
            "rust" "fn" "p" 1 30 "body" 25)))
    (should (string-match-p "\\+25 more lines · click to open" s))))

(ert-deftest +presentation/source-preview-missing-file-fence ()
  (let ((s (+presentation--source-preview-fence "/no/such/file" 1 5 "lbl")))
    (should (string-match-p "(file not found: /no/such/file)" s))))

(ert-deftest +presentation/source-preview-invalid-range-fence ()
  (+presentation-tests--with-temp-source '("one") tmp
    (let ((s (+presentation--source-preview-fence tmp 100 200 "lbl")))
      (should (string-match-p "(invalid range)" s)))))

(ert-deftest +presentation/render-link-previews-creates-overlay ()
  (+presentation-tests--with-temp-source '("a" "b" "c" "d") tmp
    (with-temp-buffer
      (insert (format "# Slide\nSee [foo](%s#L2-L3) here.\n" tmp))
      (+presentation--render-link-previews)
      (should (= 1 (length +presentation--preview-overlays)))
      (let* ((ov (car +presentation--preview-overlays))
             (display (overlay-get ov 'display)))
        (should display)
        (should (string-match-p "b\nc" display))))))

(ert-deftest +presentation/render-link-previews-does-not-mutate-buffer ()
  (+presentation-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1) text\n" tmp))
      (let ((before-text (buffer-string)))
        (+presentation--render-link-previews)
        (should (equal before-text (buffer-string)))))))

(ert-deftest +presentation/clear-link-previews-removes-overlays ()
  (+presentation-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1) text\n" tmp))
      (+presentation--render-link-previews)
      (should (= 1 (length +presentation--preview-overlays)))
      (+presentation--clear-link-previews)
      (should-not +presentation--preview-overlays))))


;;; §7 Diff-range link preview overlay

(defmacro +presentation-tests--with-fake-call-process (output exit &rest body)
  "Stub `call-process' to insert OUTPUT and return EXIT during BODY."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (_p _i _d _disp &rest _) (insert ,output) ,exit)))
     ,@body))

(ert-deftest +presentation/parse-diff-link-with-path ()
  (let ((r (+presentation--parse-diff-link "diff:main...HEAD#auth.rs")))
    (should (equal "main"    (plist-get r :base)))
    (should (equal "HEAD"    (plist-get r :head)))
    (should (equal "auth.rs" (plist-get r :path)))))

(ert-deftest +presentation/parse-diff-link-without-path ()
  (let ((r (+presentation--parse-diff-link "diff:HEAD~1...HEAD")))
    (should (equal "HEAD~1" (plist-get r :base)))
    (should (equal "HEAD"   (plist-get r :head)))
    (should-not (plist-get r :path))))

(ert-deftest +presentation/parse-diff-link-handles-dotted-refs ()
  (let ((r (+presentation--parse-diff-link "diff:v1.0...v2.0")))
    (should (equal "v1.0" (plist-get r :base)))
    (should (equal "v2.0" (plist-get r :head)))))

(ert-deftest +presentation/parse-diff-link-rejects-non-diff ()
  (should-not (+presentation--parse-diff-link "https://example.com"))
  (should-not (+presentation--parse-diff-link "p.rs#L1"))
  (should-not (+presentation--parse-diff-link "diff:abc"))
  (should-not (+presentation--parse-diff-link "diff:..."))
  (should-not (+presentation--parse-diff-link nil)))

(ert-deftest +presentation/diff-preview-argv-no-path ()
  (should (equal '("git" "-C" "/wt" "diff" "B...H")
                 (+presentation--diff-preview-argv "/wt" "B" "H"))))

(ert-deftest +presentation/diff-preview-argv-with-path ()
  (should (equal '("git" "-C" "/wt" "diff" "B...H" "--" "p.rs")
                 (+presentation--diff-preview-argv "/wt" "B" "H" "p.rs"))))

(ert-deftest +presentation/run-diff-preview-empty ()
  (+presentation-tests--with-fake-call-process "" 0
    (let ((r (+presentation--run-diff-preview "/wt" "B" "H")))
      (should (eq 'no-changes (plist-get r :status))))))

(ert-deftest +presentation/run-diff-preview-small ()
  (+presentation-tests--with-fake-call-process "line1\nline2\nline3\n" 0
    (let ((r (+presentation--run-diff-preview "/wt" "B" "H")))
      (should (eq 'ok (plist-get r :status)))
      (should (= 0 (plist-get r :extra)))
      (should (string-match-p "line1\nline2\nline3" (plist-get r :body))))))

(ert-deftest +presentation/run-diff-preview-oversized-truncates ()
  (let ((output (concat (mapconcat (lambda (n) (format "L%d" n))
                                   (number-sequence 1 15) "\n")
                        "\n")))
    (+presentation-tests--with-fake-call-process output 0
      (let ((r (+presentation--run-diff-preview "/wt" "B" "H")))
        (should (= 5 (plist-get r :extra)))
        (should (= 10 (length (split-string (plist-get r :body) "\n"))))))))

(ert-deftest +presentation/diff-preview-fence-empty-says-no-changes ()
  (+presentation-tests--with-fake-call-process "" 0
    (let ((s (+presentation--diff-preview-fence "/wt" "B" "H" nil "lbl")))
      (should (string-match-p "(no changes)" s)))))

(ert-deftest +presentation/diff-preview-fence-error-shows-first-line ()
  (+presentation-tests--with-fake-call-process "fatal: bad object\nmore\n" 128
    (let ((s (+presentation--diff-preview-fence "/wt" "B" "H" nil "lbl")))
      (should (string-match-p "(git error: fatal: bad object)" s)))))

(ert-deftest +presentation/render-link-previews-handles-diff-link ()
  (+presentation-tests--with-fake-call-process "diff line 1\n" 0
    (with-temp-buffer
      (insert "# Slide\n[change](diff:HEAD~1...HEAD)\n")
      (+presentation--render-link-previews)
      (should (= 1 (length +presentation--preview-overlays)))
      (let ((display (overlay-get (car +presentation--preview-overlays) 'display)))
        (should (string-match-p "diff line 1" display))))))


;;; §8 Preview refresh on slide entry

(ert-deftest +presentation/mode-enable-rebuilds-previews ()
  (+presentation-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1-L2)\n" tmp))
      (goto-char (point-min))
      (+presentation-mode 1)
      (should (= 1 (length +presentation--preview-overlays))))))

(ert-deftest +presentation/next-slide-rebuilds-previews ()
  (+presentation-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# A\nfirst\n# B\n[foo](%s#L1-L2)\n" tmp))
      (goto-char (point-min))
      (+presentation-mode 1)
      (should (= 0 (length +presentation--preview-overlays)))
      (+presentation-next-slide)
      (should (= 1 (length +presentation--preview-overlays))))))

(ert-deftest +presentation/previous-slide-rebuilds-previews ()
  (+presentation-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# A\n[foo](%s#L1-L2)\n# B\nbody\n" tmp))
      (goto-char (point-max))
      (+presentation-mode 1)
      (should (= 0 (length +presentation--preview-overlays)))
      (+presentation-previous-slide)
      (should (= 1 (length +presentation--preview-overlays))))))

(ert-deftest +presentation/heading-link-follow-rebuilds-previews ()
  (+presentation-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# Top\n[jump](#target)\n# Other\n## Target\n[foo](%s#L1-L2)\n"
                      tmp))
      (goto-char (point-min))
      (+presentation-mode 1)
      (search-forward "[jump]")
      (backward-char 2)
      (+presentation-follow-link)
      (should (= 1 (length +presentation--preview-overlays))))))

(ert-deftest +presentation/no-file-notify-watchers ()
  (let ((src (with-temp-buffer
               (insert-file-contents
                (expand-file-name "modules/presentation/lib.el"
                                  user-emacs-directory))
               (buffer-string))))
    (should-not (string-match-p "file-notify-add-watch" src))))

(ert-deftest +presentation/no-idle-timers-for-preview-refresh ()
  (let ((src (with-temp-buffer
               (insert-file-contents
                (expand-file-name "modules/presentation/lib.el"
                                  user-emacs-directory))
               (buffer-string))))
    (should-not (string-match-p "run-with-idle-timer" src))))


;;; §13 Detached narrowed-source renderer

(ert-deftest +presentation/render-narrowed-source-narrows-and-readonly ()
  (with-temp-buffer
    (insert "L1\nL2\nL3\nL4\nL5\n")
    (+presentation--render-narrowed-source (current-buffer) 2 4)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "L2")))
    (should (= 3 (count-lines (point-min) (point-max))))
    (should buffer-read-only)
    (should +presentation--source-restorer)))

(ert-deftest +presentation/render-narrowed-source-applies-focus-overlays ()
  (with-temp-buffer
    (insert "L1\nL2\nL3\nL4\nL5\n")
    (+presentation--render-narrowed-source (current-buffer) 1 5 2 4)
    (let ((focus-ovs (cl-remove-if-not
                      (lambda (o) (eq (overlay-get o 'face) '+presentation-focus-face))
                      +presentation--source-overlays)))
      (should (= 3 (length focus-ovs))))))

(ert-deftest +presentation/render-narrowed-source-cleans-on-rerender ()
  (with-temp-buffer
    (insert "L1\nL2\nL3\nL4\nL5\n")
    (+presentation--render-narrowed-source (current-buffer) 1 5 1 2)
    (should (= 2 (length +presentation--source-overlays)))
    (+presentation--render-narrowed-source (current-buffer) 1 5 3 5)
    (should (= 3 (length +presentation--source-overlays)))))

(ert-deftest +presentation/render-narrowed-source-focus-bounded-to-eol ()
  (with-temp-buffer
    (insert "shortline\nlonger second line\n")
    (+presentation--render-narrowed-source (current-buffer) 1 2 1 1)
    (let ((ov (car +presentation--source-overlays)))
      (should ov)
      (should (= (overlay-end ov)
                 (save-excursion
                   (goto-char (overlay-start ov))
                   (line-end-position)))))))

(ert-deftest +presentation/render-narrowed-source-cleanup-restores-state ()
  (let ((buf (generate-new-buffer "*pres-render-test*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "L1\nL2\nL3\n")
          (+presentation--render-narrowed-source buf 1 3)
          (should buffer-read-only)
          (+presentation--cleanup-source-render)
          (should-not buffer-read-only)
          (should (= (- (point-max) (point-min)) (buffer-size))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest +presentation/focus-face-no-extend ()
  (should-not (eq t (face-attribute '+presentation-focus-face :extend nil t))))


;;; §9 Click escape: source-range link

(defmacro +presentation-tests--with-source-link-click (lines path link &rest body)
  "Set up a presentation buffer containing LINK pointing into PATH (LINES).
LINK is a string fragment like \"#L2-L3\".  Stubs `pop-to-buffer'
to merely set the current buffer.  Inside BODY: cursor is on the link,
mode is enabled, and BODY can call `+presentation-follow-link'."
  (declare (indent 3))
  `(+presentation-tests--with-temp-source ,lines ,path
     (with-temp-buffer
       (insert (format "# Slide\n[foo](%s%s)\n" ,path ,link))
       (goto-char (point-min))
       (+presentation-mode 1)
       (search-forward "[foo]")
       (backward-char 2)
       (cl-letf (((symbol-function 'pop-to-buffer)
                  (lambda (buffer &rest _) (set-buffer buffer) buffer)))
         ,@body))))

(ert-deftest +presentation/source-link-click-pushes-mark ()
  (+presentation-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1-L2)\n" tmp))
      (goto-char (point-min))
      (+presentation-mode 1)
      (search-forward "[foo]")
      (backward-char 2)
      (let ((click (point))
            (doc-buffer (current-buffer)))
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (set-buffer buffer) buffer)))
          (+presentation-follow-link))
        (with-current-buffer doc-buffer
          (should (member click (mapcar #'marker-position
                                        (cons (mark-marker) mark-ring)))))))))

(ert-deftest +presentation/source-link-click-opens-narrowed-readonly ()
  (+presentation-tests--with-source-link-click
      '("a" "b" "c" "d" "e") tmp "#L2-L3"
    (+presentation-follow-link)
    (let ((src (find-buffer-visiting tmp)))
      (should src)
      (with-current-buffer src
        (should buffer-read-only)
        (should-not +presentation-mode)
        (should (= 2 (- (line-number-at-pos (point-max))
                        (line-number-at-pos (point-min)))))
        (save-excursion
          (goto-char (point-min))
          (should (looking-at "b")))))))

(ert-deftest +presentation/source-link-click-uses-display-buffer ()
  (let (called)
    (+presentation-tests--with-temp-source '("a" "b") tmp
      (with-temp-buffer
        (insert (format "# Slide\n[foo](%s#L1)\n" tmp))
        (goto-char (point-min))
        (+presentation-mode 1)
        (search-forward "[foo]")
        (backward-char 2)
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq called t) (set-buffer buffer))))
          (+presentation-follow-link))
        (should called)))))

(ert-deftest +presentation/source-link-click-applies-focus-overlay ()
  (+presentation-tests--with-source-link-click
      '("a" "b" "c" "d") tmp "#L2-L3"
    (+presentation-follow-link)
    (with-current-buffer (find-buffer-visiting tmp)
      (let ((focus-ovs (cl-remove-if-not
                        (lambda (o) (eq (overlay-get o 'face)
                                        '+presentation-focus-face))
                        (overlays-in (point-min) (point-max)))))
        (should focus-ovs)
        (should (= 2 (length focus-ovs)))))))

(ert-deftest +presentation/source-link-click-installs-kill-hook ()
  (+presentation-tests--with-source-link-click
      '("a" "b") tmp "#L1"
    (+presentation-follow-link)
    (with-current-buffer (find-buffer-visiting tmp)
      (should (memq #'+presentation--cleanup-source-render kill-buffer-hook)))))

(ert-deftest +presentation/source-link-click-leaves-presentation-mode-off ()
  (+presentation-tests--with-source-link-click
      '("a" "b") tmp "#L1"
    (+presentation-follow-link)
    (with-current-buffer (find-buffer-visiting tmp)
      (should-not +presentation-mode))))


;;; §10 Click escape: diff-range link

(defmacro +presentation-tests--with-magit-stub (captured-var &rest body)
  "Stub `require' to succeed for `magit' and `magit-diff-range' to capture args."
  (declare (indent 1))
  `(let ((,captured-var nil)
         (orig-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'require)
                (lambda (feat &rest args)
                  (if (eq feat 'magit) t (apply orig-require feat args))))
               ((symbol-function 'magit-diff-range)
                (lambda (range &optional _args files)
                  (setq ,captured-var (list range files)))))
       ,@body)))

(ert-deftest +presentation/diff-link-click-calls-magit-and-pushes-mark ()
  (+presentation-tests--with-magit-stub captured
    (with-temp-buffer
      (insert "# Slide\n[change](diff:main...HEAD)\n")
      (goto-char (point-min))
      (+presentation-mode 1)
      (search-forward "[change]")
      (backward-char 2)
      (let ((click (point)))
        (+presentation-follow-link)
        (should (equal "main...HEAD" (car captured)))
        (should-not (cadr captured))
        (should (member click (mapcar #'marker-position
                                      (cons (mark-marker) mark-ring))))))))

(ert-deftest +presentation/diff-link-click-passes-path-when-scoped ()
  (+presentation-tests--with-magit-stub captured
    (with-temp-buffer
      (insert "# Slide\n[change](diff:main...HEAD#auth.rs)\n")
      (goto-char (point-min))
      (+presentation-mode 1)
      (search-forward "[change]")
      (backward-char 2)
      (+presentation-follow-link)
      (should (equal "main...HEAD" (car captured)))
      (should (equal '("auth.rs") (cadr captured))))))

(ert-deftest +presentation/follow-link-https-passes-through ()
  (let ((called 0))
    (cl-letf (((symbol-function 'markdown-follow-link-at-point)
               (lambda () (interactive) (cl-incf called))))
      (with-temp-buffer
        (insert "# Slide\n[ex](https://example.com) text\n")
        (goto-char (point-min))
        (+presentation-mode 1)
        (search-forward "[ex]")
        (backward-char 2)
        (+presentation-follow-link)
        (should (= 1 called))))))

(ert-deftest +presentation/follow-link-plain-path-passes-through ()
  (let ((called 0))
    (cl-letf (((symbol-function 'markdown-follow-link-at-point)
               (lambda () (interactive) (cl-incf called))))
      (with-temp-buffer
        (insert "# Slide\n[file](modules/auth.rs) text\n")
        (goto-char (point-min))
        (+presentation-mode 1)
        (search-forward "[file]")
        (backward-char 2)
        (+presentation-follow-link)
        (should (= 1 called))))))

(ert-deftest +presentation/diff-link-click-no-magit-signals ()
  (let ((orig-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feat &rest args)
                 (if (eq feat 'magit) nil (apply orig-require feat args)))))
      (with-temp-buffer
        (insert "# Slide\n[change](diff:main...HEAD)\n")
        (goto-char (point-min))
        (+presentation-mode 1)
        (search-forward "[change]")
        (backward-char 2)
        (should-error (+presentation-follow-link) :type 'user-error)))))


;;; §12 Document revert resilience

(ert-deftest +presentation/remember-position-captures-anchor-fields ()
  (with-temp-buffer
    (insert "# Top\nbody\n# Auth flow\nfingerprint here\n# Other\n")
    (goto-char (point-min))
    (+presentation-mode 1)
    (+presentation-next-slide)
    (search-forward "fingerprint")
    (backward-char (length "fingerprint"))
    (+presentation--remember-position)
    (let ((a +presentation--revert-anchor))
      (should (equal "auth-flow" (plist-get a :slug)))
      (should (= 1 (plist-get a :index)))
      (should (string-prefix-p "fingerprint" (plist-get a :fingerprint)))
      (should (<= (length (plist-get a :fingerprint)) 80))
      (should (numberp (plist-get a :window-start-offset))))))

(ert-deftest +presentation/remember-position-fingerprint-capped-at-80 ()
  (with-temp-buffer
    (insert "# A\n")
    (insert (make-string 200 ?x))
    (goto-char (point-min))
    (forward-line 1)
    (+presentation--remember-position)
    (should (= 80 (length (plist-get +presentation--revert-anchor :fingerprint))))))

(ert-deftest +presentation/restore-position-by-slug ()
  (with-temp-buffer
    (insert "# Auth flow\nbody\n# Other\nx\n")
    (setq +presentation--revert-anchor
          (list :slug "auth-flow" :index 0
                :fingerprint nil :window-start-offset 0))
    (+presentation--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# Auth flow")))))

(ert-deftest +presentation/restore-position-falls-back-to-index ()
  (with-temp-buffer
    (insert "# Renamed\nbody\n# Two\nx\n")
    (setq +presentation--revert-anchor
          (list :slug "auth-flow" :index 0
                :fingerprint nil :window-start-offset 0))
    (+presentation--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# Renamed")))))

(ert-deftest +presentation/restore-position-clamps-index-to-last ()
  (with-temp-buffer
    (insert "# A\n# B\n")
    (setq +presentation--revert-anchor
          (list :slug "missing" :index 5
                :fingerprint nil :window-start-offset 0))
    (+presentation--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# B")))))

(ert-deftest +presentation/restore-position-falls-back-to-first-when-slug-and-index-fail ()
  (with-temp-buffer
    (insert "# Only\nbody\n")
    (setq +presentation--revert-anchor
          (list :slug nil :index nil :fingerprint nil :window-start-offset 0))
    (+presentation--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# Only")))))

(ert-deftest +presentation/restore-position-no-h1s-leaves-widened ()
  (with-temp-buffer
    (insert "preamble only\n")
    (setq +presentation--revert-anchor
          (list :slug "x" :index 0 :fingerprint nil :window-start-offset 0))
    (+presentation--restore-position)
    (should (= (- (point-max) (point-min)) (buffer-size)))))

(ert-deftest +presentation/restore-position-finds-fingerprint ()
  (with-temp-buffer
    (insert "# Top\nbefore\nUNIQUEFINGERPRINT\nafter\n")
    (setq +presentation--revert-anchor
          (list :slug "top" :index 0
                :fingerprint "UNIQUEFINGERPRINT"
                :window-start-offset 0))
    (+presentation--restore-position)
    (should (looking-at "UNIQUEFINGERPRINT"))))

(ert-deftest +presentation/restore-position-rebuilds-previews ()
  (+presentation-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1)\n" tmp))
      (+presentation-mode 1)
      (setq +presentation--revert-anchor
            (list :slug "slide" :index 0 :fingerprint nil :window-start-offset 0))
      (+presentation--clear-link-previews)
      (+presentation--restore-position)
      (should (= 1 (length +presentation--preview-overlays))))))

(ert-deftest +presentation/mode-installs-and-removes-revert-hooks ()
  (with-temp-buffer
    (+presentation-mode 1)
    (should (memq #'+presentation--remember-position before-revert-hook))
    (should (memq #'+presentation--restore-position after-revert-hook))
    (+presentation-mode -1)
    (should-not (memq #'+presentation--remember-position before-revert-hook))
    (should-not (memq #'+presentation--restore-position after-revert-hook))))


;;; §14 Module-level cleanup

(defun +presentation-tests--read-lib ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/presentation/lib.el"
                       user-emacs-directory))
    (buffer-string)))

(defun +presentation-tests--read-init ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/presentation/init.el"
                       user-emacs-directory))
    (buffer-string)))

(ert-deftest +presentation/init-el-is-small-and-has-no-mcp-calls ()
  (let ((src (+presentation-tests--read-init)))
    (should-not (string-match-p "claude-code-ide-make-tool" src))
    (should-not (string-match-p "delete-frame-functions" src))
    (should-not (string-match-p "claude-code-ide-mcp" src))))

(ert-deftest +presentation/lib-el-exports-public-symbols ()
  (should (fboundp '+present-markdown))
  (should (fboundp '+presentation-mode))
  (should (fboundp '+presentation-next-slide))
  (should (fboundp '+presentation-previous-slide))
  (should (fboundp '+presentation-quit))
  (should (fboundp '+presentation-follow-link))
  (should (facep '+presentation-focus-face)))

(ert-deftest +presentation/no-references-to-deleted-symbols ()
  (let ((src (+presentation-tests--read-lib))
        (deleted '("+presentation--sessions"
                   "+presentation--effect-shell"
                   "+presentation--effect-elisp"
                   "+presentation--run-effects"
                   "+presentation--register-session"
                   "+presentation--get-session"
                   "+presentation--make-key"
                   "+presentation--deck-push"
                   "+presentation--deck-replace"
                   "+presentation--deck-truncate"
                   "+presentation--deck-goto"
                   "+presentation--render-current"
                   "+presentation--render-slide"
                   "+presentation--dispatch-slide"
                   "+presentation--validate-slide"
                   "+presentation--coerce-slide"
                   "+presentation--emit-nav-channel"
                   "+presentation--inject-channel-capability"
                   "+presentation--register-channel-capability")))
    (dolist (sym deleted)
      (should-not (string-match-p (regexp-quote sym) src)))))

;;; tests.el ends here
