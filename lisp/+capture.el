;;; +capture.el --- Supporting functions for org capture templates. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 's)

(autoload 'gptel-request "gptel")
(defvar gptel-model nil)

(cl-eval-when (compile)
  (require 'eww)
  (require 'org-roam))

(defvar +capture-context nil
  "A plist containing extra information about the thing being captured.")

(defvar +capture-fast-llm-model 'claude-3-haiku-20240307)

(defun +capture--metadata-for-web-document (url excerpt &optional title)
  (let* ((gptel-model +capture-fast-llm-model)
         (prompt-parts
          `("
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
                  title
                  "</TITLE>"))

            "The excerpt of the page text is below:"
            "<EXCERPT>"
            ,excerpt
            "</EXCERPT>"))
         (prompt (string-join (seq-map #'string-trim prompt-parts) "\n")))

    (let ((result)
          (reporter (make-progress-reporter "Interpreting page for metadata")))

      (gptel-request prompt
        :callback (lambda (response _info)
                    (setq result response)))

      (progress-reporter-update reporter)
      (while (null result)
        (sleep-for 0.05)
        (progress-reporter-update reporter))
      (prog1 (append (list :url url) (read result))
        (progress-reporter-done reporter)))))

(defun +litnote-meta-try-from-eww ()
  (when-let* ((eww-buffer (seq-find (lambda (buf)
                                      (when (get-buffer-window buf)
                                        (with-current-buffer buf
                                          (derived-mode-p 'eww-mode))))
                                    (buffer-list))))
    (with-current-buffer eww-buffer
      (+capture--metadata-for-web-document (plist-get eww-data :url)
                                           (buffer-substring (point-min)
                                                             (min (point-max) 1000))
                                           (plist-get eww-data :title)))))

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
      (unwind-protect
          (let ((parsed (libxml-parse-html-region (point-min) (point-max))))
            (erase-buffer)
            (shr-insert-document parsed)
            (+capture--metadata-for-web-document url
                                                 (buffer-substring (point-min)
                                                                   (min (point-max) 1000))))
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
