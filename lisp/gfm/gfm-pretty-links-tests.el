;;; gfm-pretty-links-tests.el --- gfm-pretty-links tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the gfm-pretty-links library.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'markdown-mode)
(require 'gfm-pretty-links)


;;; Anchor jump under narrowing

(ert-deftest gfm-pretty-links/jump-to-anchor-resolves-off-region-heading ()
  "Anchor jump finds a heading outside the current narrowing and widens."
  (with-temp-buffer
    (insert "# Slide one\nbody one\n\n# Filter shape change\ndetails\n")
    (let ((slide-end (save-excursion
                       (goto-char (point-min))
                       (search-forward "body one")
                       (line-end-position))))
      (narrow-to-region (point-min) slide-end)
      (goto-char (point-min))
      (gfm-pretty-links--jump-to-anchor "#filter-shape-change")
      (should-not (buffer-narrowed-p))
      (should (looking-at "# Filter shape change")))))

(ert-deftest gfm-pretty-links/jump-to-anchor-runs-hook-with-target-pos ()
  "Successful jump runs the hook once with the target buffer position."
  (with-temp-buffer
    (insert "# Top\nbody\n# Target\nmore\n")
    (let* ((received nil)
           (gfm-pretty-links-after-anchor-jump-functions
            (list (lambda (pos) (push pos received)))))
      (goto-char (point-min))
      (gfm-pretty-links--jump-to-anchor "#target")
      (should (= 1 (length received)))
      (should (= (point) (car received)))
      (save-excursion
        (goto-char (car received))
        (should (looking-at "# Target"))))))

(ert-deftest gfm-pretty-links/jump-to-anchor-hook-runs-after-widen-and-goto ()
  "Hook observes the post-jump state: widened buffer, point at heading."
  (with-temp-buffer
    (insert "# Slide one\nbody one\n\n# Off slide\ndetails\n")
    (let* ((observed nil)
           (gfm-pretty-links-after-anchor-jump-functions
            (list (lambda (_pos)
                    (push (list :narrowed (buffer-narrowed-p)
                                :at-heading (looking-at "# Off slide"))
                          observed)))))
      (narrow-to-region (point-min)
                        (save-excursion
                          (goto-char (point-min))
                          (search-forward "body one")
                          (line-end-position)))
      (goto-char (point-min))
      (gfm-pretty-links--jump-to-anchor "#off-slide")
      (should (= 1 (length observed)))
      (should-not (plist-get (car observed) :narrowed))
      (should (plist-get (car observed) :at-heading)))))

(ert-deftest gfm-pretty-links/jump-to-anchor-miss-does-not-run-hook ()
  "User-error miss leaves the hook untouched."
  (with-temp-buffer
    (insert "# Top\nbody\n# Other\n")
    (let* ((called 0)
           (gfm-pretty-links-after-anchor-jump-functions
            (list (lambda (_pos) (cl-incf called)))))
      (goto-char (point-min))
      (should-error (gfm-pretty-links--jump-to-anchor "#missing")
                    :type 'user-error)
      (should (= 0 called)))))

(ert-deftest gfm-pretty-links/jump-to-anchor-miss-leaves-narrowing-intact ()
  "On miss, the buffer's prior narrowing is unchanged."
  (with-temp-buffer
    (insert "# Top\nbody\n# Other\n")
    (let ((beg (point-min))
          (end (save-excursion
                 (goto-char (point-min))
                 (search-forward "body")
                 (line-end-position))))
      (narrow-to-region beg end)
      (goto-char (point-min))
      (should-error (gfm-pretty-links--jump-to-anchor "#missing")
                    :type 'user-error)
      (should (buffer-narrowed-p))
      (should (= (point-min) beg))
      (should (= (point-max) end)))))

(ert-deftest gfm-pretty-links/jump-to-anchor-pushes-mark-at-click-site ()
  "Successful jump pushes the click site onto the mark ring."
  (with-temp-buffer
    (insert "# Top\nclickpoint here\n# Target\n")
    (let ((gfm-pretty-links-after-anchor-jump-functions nil))
      (goto-char (point-min))
      (search-forward "clickpoint")
      (let ((click (point)))
        (gfm-pretty-links--jump-to-anchor "#target")
        (should (member click (mapcar #'marker-position
                                      (cons (mark-marker) mark-ring))))))))

(ert-deftest gfm-pretty-links/jump-to-anchor-records-better-jumper-at-click-site ()
  "Successful jump records the click site in better-jumper's jump list."
  (with-temp-buffer
    (insert "# Top\nclickpoint here\n# Target\n")
    (let ((gfm-pretty-links-after-anchor-jump-functions nil)
          (recorded nil))
      (cl-letf (((symbol-function 'better-jumper-set-jump)
                 (lambda (&optional pos) (push (or pos (point)) recorded))))
        (goto-char (point-min))
        (search-forward "clickpoint")
        (let ((click (point)))
          (gfm-pretty-links--jump-to-anchor "#target")
          (should (= 1 (length recorded)))
          (should (= click (car recorded))))))))


;;; URL-form skip (source-range / diff URLs owned by gfm-present)

(defun gfm-pretty-links-tests--blocks (text)
  "Return link records discovered in TEXT.
Loads TEXT into a fresh `markdown-mode' buffer (hooks delayed),
rebuilds the reference-definition alist, and runs the discovery filter
over the whole buffer."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (markdown-mode))
    (gfm-pretty-links--build-ref-def-alist)
    (gfm-pretty-links--blocks-in-range (point-min) (point-max))))

(ert-deftest gfm-pretty-links/skip-inline-source-range-link ()
  "Inline `path#L<a>-L<b>' link produces no decoration record."
  (should (null (gfm-pretty-links-tests--blocks
                 "see [snippet](/repo/foo.yml#L13-L22) inline\n"))))

(ert-deftest gfm-pretty-links/skip-inline-source-link-single-line ()
  "Single-line `path#L<n>' link is also skipped."
  (should (null (gfm-pretty-links-tests--blocks
                 "see [line](/repo/x.el#L42)\n"))))

(ert-deftest gfm-pretty-links/skip-inline-diff-link ()
  "Inline `diff:<base>...<head>' link produces no decoration record."
  (should (null (gfm-pretty-links-tests--blocks
                 "see [changed](diff:main...feature)\n"))))

(ert-deftest gfm-pretty-links/skip-inline-diff-link-with-file-scope ()
  "Diff link with `#path' suffix is skipped."
  (should (null (gfm-pretty-links-tests--blocks
                 "see [changed](diff:main...feature#path/to/file.el)\n"))))

(ert-deftest gfm-pretty-links/skip-reference-link-resolving-to-source-range ()
  "Reference link whose definition resolves to a source-range URL is skipped."
  (should (null (gfm-pretty-links-tests--blocks
                 "see [snippet][src] inline\n\n[src]: /repo/foo.yml#L13-L22\n"))))

(ert-deftest gfm-pretty-links/plain-file-link-still-decorated ()
  "Plain file link (no `#L...' suffix) still produces a `file' record."
  (let ((records (gfm-pretty-links-tests--blocks
                  "run [ops](./scripts/x.sh)\n")))
    (should (= 1 (length records)))
    (should (eq 'file (gfm-pretty-links--link-class (car records))))))

(ert-deftest gfm-pretty-links/plain-anchor-link-still-decorated ()
  "Plain anchor link (no source-range shape) still produces an `anchor' record."
  (let ((records (gfm-pretty-links-tests--blocks
                  "see [Setup](#setup)\n")))
    (should (= 1 (length records)))
    (should (eq 'anchor (gfm-pretty-links--link-class (car records))))))


;;; URL-side hiding for file-class links

(defun gfm-pretty-links-tests--rebuild-in (text)
  "Insert TEXT into a fresh `markdown-mode' buffer and rebuild overlays.
Returns the buffer (caller is inside `with-temp-buffer')."
  (insert text)
  (delay-mode-hooks (markdown-mode))
  (gfm-pretty-links--rebuild))

(defun gfm-pretty-links-tests--overlay-at (pos side)
  "Return the gfm-pretty-links overlay covering POS on SIDE, or nil."
  (cl-find-if (lambda (o)
                (and (overlay-get o 'gfm-pretty-links-class)
                     (eq side (overlay-get o 'gfm-pretty-links-side))
                     (<= (overlay-start o) pos)
                     (< pos (overlay-end o))))
              (overlays-in (point-min) (point-max))))

(ert-deftest gfm-pretty-links/file-link-url-overlay-hides-span ()
  "File-class inline link gets a URL-side overlay that replaces the path span."
  (with-temp-buffer
    (gfm-pretty-links-tests--rebuild-in "run [ops](./scripts/x.sh)\n")
    (let* ((url-pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "(./scripts/x.sh)")
                      (1- (point))))
           (ov (gfm-pretty-links-tests--overlay-at url-pos 'url)))
      (should ov)
      (should (stringp (overlay-get ov 'display)))
      (should (eq 'file (overlay-get ov 'gfm-pretty-links-class)))
      (should (equal "./scripts/x.sh"
                     (overlay-get ov 'gfm-pretty-links-url))))))

(ert-deftest gfm-pretty-links/file-link-with-code-label-hides-url-span ()
  "Code-styled label preserved on title side; URL-side overlay hides path."
  (with-temp-buffer
    (gfm-pretty-links-tests--rebuild-in
     "see [`pretty`](../../path/x.hcl)\n")
    (let* ((title-pos (save-excursion
                       (goto-char (point-min))
                       (search-forward "[`pretty`]")
                       (- (point) 2)))
           (url-pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "(../../path/x.hcl)")
                      (1- (point))))
           (title-ov (gfm-pretty-links-tests--overlay-at title-pos 'title))
           (url-ov (gfm-pretty-links-tests--overlay-at url-pos 'url)))
      (should title-ov)
      (should (equal "pretty"
                     (substring-no-properties
                      (overlay-get title-ov 'display))))
      (should (eq 'gfm-pretty-links-file-face
                  (get-text-property 0 'face
                                     (overlay-get title-ov 'display))))
      (should url-ov)
      (should (stringp (overlay-get url-ov 'display)))
      (should (eq 'file (overlay-get url-ov 'gfm-pretty-links-class))))))

(ert-deftest gfm-pretty-links/file-link-icon-fallback-without-nerd-icons ()
  "File link's URL-side overlay falls back to `\"\"' when nerd-icons absent."
  (with-temp-buffer
    (cl-letf (((symbol-function 'gfm-pretty-links--call-nerd)
               (lambda (_fn _arg) nil)))
      (gfm-pretty-links-tests--rebuild-in "run [ops](./scripts/x.sh)\n")
      (let* ((url-pos (save-excursion
                        (goto-char (point-min))
                        (search-forward "(./scripts/x.sh)")
                        (1- (point))))
             (ov (gfm-pretty-links-tests--overlay-at url-pos 'url)))
        (should ov)
        (should (equal "" (overlay-get ov 'display)))
        (should (eq 'file (overlay-get ov 'gfm-pretty-links-class)))))))


;;; Wrapping backtick stripping on title side

(ert-deftest gfm-pretty-links/title-strips-wrapping-backticks ()
  "Fully backtick-wrapped label shows without backticks; metadata keeps them."
  (with-temp-buffer
    (gfm-pretty-links-tests--rebuild-in "see [`pretty`](./x.hcl)\n")
    (let* ((title-pos (save-excursion
                       (goto-char (point-min))
                       (search-forward "[`pretty`]")
                       (- (point) 2)))
           (ov (gfm-pretty-links-tests--overlay-at title-pos 'title)))
      (should ov)
      (should (equal "pretty"
                     (substring-no-properties (overlay-get ov 'display))))
      (should (equal "`pretty`"
                     (overlay-get ov 'gfm-pretty-links-label))))))

(ert-deftest gfm-pretty-links/title-keeps-interior-backticks ()
  "Interior backticks in the label are not stripped from the display."
  (with-temp-buffer
    (gfm-pretty-links-tests--rebuild-in "see [say `hi` world](./x.md)\n")
    (let* ((title-pos (save-excursion
                       (goto-char (point-min))
                       (search-forward "[say `hi` world]")
                       (- (point) 2)))
           (ov (gfm-pretty-links-tests--overlay-at title-pos 'title)))
      (should ov)
      (should (equal "say `hi` world"
                     (substring-no-properties (overlay-get ov 'display)))))))

(ert-deftest gfm-pretty-links/strip-wrapping-backticks-helper ()
  "Unit checks on the strip helper."
  (should (equal "x" (gfm-pretty-links--strip-wrapping-backticks "`x`")))
  (should (equal "" (gfm-pretty-links--strip-wrapping-backticks "``")))
  (should (equal "plain"
                 (gfm-pretty-links--strip-wrapping-backticks "plain")))
  (should (equal "a `b` c"
                 (gfm-pretty-links--strip-wrapping-backticks "a `b` c")))
  (should (equal "`only-leading"
                 (gfm-pretty-links--strip-wrapping-backticks "`only-leading")))
  (should (equal "only-trailing`"
                 (gfm-pretty-links--strip-wrapping-backticks "only-trailing`"))))

(provide 'gfm-pretty-links-tests)
;;; gfm-pretty-links-tests.el ends here
