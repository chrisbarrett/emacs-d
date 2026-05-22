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

(provide 'gfm-pretty-links-tests)
;;; gfm-pretty-links-tests.el ends here
