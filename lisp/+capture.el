;;; +capture.el --- Supporting functions for org capture templates. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 's)
(require 'dash)

(autoload 'gptel-request "gptel")
(defvar gptel-model nil)

(cl-eval-when (compile)
  (require 'eww)
  (require 'gptel)
  (require 'org-roam))

(defvar +capture-context nil
  "A plist containing extra information about the thing being captured.")

(defvar +capture-fast-llm-model 'claude-3-haiku-20240307)


;;; Prompt builders

(defun +capture--prompt-for-youtube-video (title)
  (cl-assert (stringp title))
  (format "
Below is the title for a YouTube video:
<TITLE>
%s
</TITLE>

Inspect this title and see whether you can infer information from it suitable for a citation.

Return a Lisp plist, suitable for use with `read', with the following symbol keys:

- :title -- the title of the work, with any unrelated information removed
- :author -- the name of the primary author if available, or nil
- :year -- the year the video was published.

Values that cannot be reasonably inferred from the document excerpt should be omitted from the plist.

Do not return any prose; only return the alist (unquoted) so that it may be passed to `read'.
" title))

(defun +capture--prompt-for-generic-web-page (title rendered-content)
  (cl-assert (stringp title))
  (cl-assert (stringp rendered-content))
  (let ((parts `("
Inspect this excerpt from a web page to return metadata about it.

Return a Lisp plist, suitable for use with `read', with the following symbol keys:

- :title -- the title of the document, with any case irregularities or website junk removed so it is suitable for use in a citation.
- :authors -- a list of people that are understood to be the authors of the document. Fall back to the company or institution if no specific author can be found.
- :author -- the family name of the primary author from the :authors list, with 'et al.' appended if there were several.
- :year -- the year the document was published.

Values that cannot be reasonably inferred from the document excerpt should be omitted from the plist.

Do not return any prose; only return the alist (unquoted) so that it may be passed to `read'.
"
                 ,@(when title
                     `("The title of the page from the HTML header is below:"
                       "<TITLE>"
                       ,title
                       "</TITLE>"))

                 "The excerpt of the page text is below:"
                 "<EXCERPT>"
                 ,(substring-no-properties rendered-content
                                           0
                                           (min (length rendered-content) 1000))
                 "</EXCERPT>")
               ))
    (string-join (seq-map #'string-trim parts) "\n")))



(defun +capture--metadata-for-web-document (url title rendered-content)
  (let* ((gptel-model +capture-fast-llm-model)
         (gptel-use-context nil)
         (gptel-use-tools nil)
         (reporter (make-progress-reporter "Interpreting page for metadata"))
         (prompt
          (cond ((string-prefix-p "https://www.youtube.com/" url)
                 (+capture--prompt-for-youtube-video title))
                (t
                 (+capture--prompt-for-generic-web-page title rendered-content))))
         (result))

    (gptel-request prompt
      :system nil
      :callback (lambda (response _info)
                  (setq result response)))

    (progress-reporter-update reporter)
    (while (null result)
      (sleep-for 0.05)
      (progress-reporter-update reporter))
    (prog1 (append (list :url url) (read result))
      (progress-reporter-done reporter))))

(defun +litnote-meta-try-from-eww ()
  (when-let* ((eww-buffer (seq-find (lambda (buf)
                                      (when (get-buffer-window buf)
                                        (with-current-buffer buf
                                          (derived-mode-p 'eww-mode))))
                                    (buffer-list))))
    (with-current-buffer eww-buffer
      (+capture--metadata-for-web-document (plist-get eww-data :url)
                                           (plist-get eww-data :title)
                                           (buffer-string)))))

(defun +capture-read-url ()
  (let ((default (seq-find (lambda (it)
                             (string-match-p (rx bos "http" (? "s") "://") it))
                           kill-ring)))
    (read-string (if default
                     (format "URL [default: %s]: " default)
                   "URL: ")
                 nil
                 nil
                 default)))

(defun +litnote-meta-from-url ()
  "Return metadata plist for the URL in the clipboard."
  (let ((url (+capture-read-url)))
    (with-current-buffer (url-retrieve-synchronously url t t 5)
      (goto-char (point-min))

      (search-forward "\n\n") ; skip headers
      (search-forward-regexp (rx bol "<")) ; make sure we're at the XML.
      (goto-char (line-beginning-position))

      (unwind-protect
          (let* ((dom (libxml-parse-html-region (point) (point-max)))
                 (title
                  (-some-> dom
                    (dom-by-tag 'head)
                    (dom-by-tag 'title)
                    (car)
                    (dom-text))))
            (erase-buffer)
            (shr-insert-document dom)
            (+capture--metadata-for-web-document url title (buffer-string)))

        (kill-buffer)))))

(defun +capture-litnote-function ()
  (let* ((context (or (+litnote-meta-try-from-eww)
                      (+litnote-meta-from-url))))

    (unless (plist-get context :title)
      (plist-put context :title (read-string "Title: ")))

    (unless (plist-get context :author)
      (plist-put context :author (read-string "Author: ")))

    (plist-put context
               :authors-string
               (if-let* ((authors (plist-get context :authors)))
                   (string-join authors ", ")
                 (plist-get context :author)))

    (plist-put context
               :year-string
               (if-let* ((year (plist-get +capture-context :year)))
                   (number-to-string year)
                 "unknown"))

    (setq +capture-context context)
    (let ((filename (format "%s.org" (s-snake-case (plist-get context :title)))))
      (find-file (file-name-concat org-roam-directory "litnotes" filename)))))

(provide '+capture)

;;; +capture.el ends here
