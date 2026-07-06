;;; tests.el --- Tests for dired module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the dired module's hide-details eldoc integration.

;;; Code:

(require 'ert)
(require 'dired)
(require 'eldoc)

(defconst dired-test--module-dir
  (expand-file-name "modules/dired/" user-emacs-directory)
  "Directory containing the dired module.")

(defun dired-test--load-module ()
  "Load the dired module's library functions."
  (let ((lib-file (expand-file-name "lib.el" dired-test--module-dir)))
    (when (file-exists-p lib-file)
      (load lib-file nil t))))

(dired-test--load-module)

(defmacro dired-test--in-listing (&rest body)
  "Run BODY in a dired buffer over a temp dir containing `hello.txt'.
Point is left on the file line and `dired-hide-details-mode' is
active."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "dired-test" t))
          (file (expand-file-name "hello.txt" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert "hi"))
           (let ((buf (dired-noselect dir)))
             (unwind-protect
                 (with-current-buffer buf
                   (dired-hide-details-mode 1)
                   (dired-goto-file file)
                   ,@body)
               (kill-buffer buf))))
       (delete-directory dir t))))

;;; P1: extract the concealed ls detail columns for a file line

(ert-deftest dired/details-at-point-returns-columns ()
  "`+dired-details-at-point' returns the ls columns preceding the file."
  (dired-test--in-listing
    (let ((details (+dired-details-at-point)))
      (should (stringp details))
      ;; Leading file-type char plus nine permission bits.
      (should (string-match-p (rx bol (any "-dl") (= 9 (any "rwxsStT-")))
                              details)))))

;;; P1b: extracted details keep font-lock faces, drop `invisible'

(ert-deftest dired/details-preserve-faces-strip-invisible ()
  "Extracted details keep buffer faces but drop the `invisible' prop.
Preserving `invisible' would hide the text in the echo area; the
faces (e.g. diredfl's) should survive so the message is colourised."
  (dired-test--in-listing
    ;; Stand in for diredfl having fontified a detail column.
    (save-excursion
      (beginning-of-line)
      (let ((bol (point)))
        (dired-move-to-filename)
        (with-silent-modifications
          (put-text-property bol (point) 'face 'diredfl-number))))
    (let ((details (+dired-details-at-point)))
      (should (eq (get-text-property 0 'face details) 'diredfl-number))
      (should-not (text-property-not-all 0 (length details)
                                         'invisible nil details)))))

;;; P2: non-file lines yield nil

(ert-deftest dired/details-at-point-nil-off-file ()
  "`+dired-details-at-point' returns nil on the directory header line."
  (dired-test--in-listing
    (goto-char (point-min))
    (should-not (+dired-details-at-point))))

;;; P3: eldoc returns the details while hide-details is active

(ert-deftest dired/eldoc-returns-details-when-hidden ()
  "The eldoc function returns the file's details as its doc string."
  (dired-test--in-listing
    (let ((reported (+dired-hide-details-eldoc-function #'ignore)))
      (should (stringp reported))
      (should (string-match-p (rx (any "-dl") (= 9 (any "rwxsStT-")))
                              reported)))))

;;; P3b: eldoc uses the sync return path only (no doubled message)

(ert-deftest dired/eldoc-does-not-invoke-callback ()
  "The eldoc function must not also call its callback.
Returning the string and calling the callback registers the doc
twice, doubling the echo-area message."
  (dired-test--in-listing
    (let ((calls 0))
      (+dired-hide-details-eldoc-function (lambda (&rest _) (setq calls (1+ calls))))
      (should (= calls 0)))))

;;; P4: eldoc stays silent when details are already visible

(ert-deftest dired/eldoc-silent-when-details-shown ()
  "The eldoc function reports nothing when details are not hidden."
  (dired-test--in-listing
    (dired-hide-details-mode -1)
    (let ((called nil))
      (+dired-hide-details-eldoc-function (lambda (&rest _) (setq called t)))
      (should-not called))))

;;; P5: dired point-movement commands trigger an eldoc refresh

(ert-deftest dired/enable-registers-movement-commands ()
  "Enabling registers dired movement commands as eldoc-triggering.
Without this, eldoc never refreshes as point moves between files."
  (dired-test--in-listing
    (+dired-enable-details-eldoc)
    (should (eldoc--message-command-p 'dired-next-line))
    (should (eldoc--message-command-p 'dired-previous-line))))

;;; tests.el ends here
