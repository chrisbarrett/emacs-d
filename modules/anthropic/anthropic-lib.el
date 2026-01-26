;;; lib.el --- Anthropic Claude API client -*- lexical-binding: t; -*-

;;; Commentary:

;; A simple Anthropic Claude API client for Emacs.
;;
;; Ported from https://github.com/mark-watson/anthropic/blob/main/anthropic.lisp
;; Updated to use the modern Messages API with Emacs url library.
;;
;; Setup:
;;   Add to ~/.authinfo or ~/.authinfo.gpg:
;;     machine api.anthropic.com login apikey password sk-ant-api...
;;
;; Usage:
;;   ;; Synchronous
;;   (+anthropic-message "What is 2+2?")
;;
;;   ;; Asynchronous
;;   (+anthropic-message-async "What is 2+2?"
;;     (lambda (response) (message "Got: %s" response)))

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(defgroup anthropic nil
  "Anthropic Claude API client."
  :group 'external
  :prefix "+anthropic-")

(defcustom +anthropic-api-url "https://api.anthropic.com/v1/messages"
  "URL endpoint for the Anthropic Messages API."
  :type 'string
  :group 'anthropic)

(defcustom +anthropic-api-version "2023-06-01"
  "Anthropic API version header value.
See URL `https://docs.anthropic.com/en/api/versioning'."
  :type 'string
  :group 'anthropic)

(defcustom +anthropic-default-model "claude-sonnet-4-5-20250929"
  "Default model to use for API requests.
See URL `https://platform.claude.com/docs/en/about-claude/models/overview'."
  :type 'string
  :group 'anthropic)

(defcustom +anthropic-default-max-tokens 1024
  "Default maximum tokens for API responses."
  :type 'integer
  :group 'anthropic)

(defcustom +anthropic-auth-source-host "api.anthropic.com"
  "Host to use when looking up API key in auth-source.
The auth-source entry should have the form:
  machine api.anthropic.com login apikey password YOUR-API-KEY"
  :type 'string
  :group 'anthropic)


;;; API Key Retrieval

;;;###autoload
(defun +anthropic-get-api-key ()
  "Retrieve the Anthropic API key from auth-source.
Looks up credentials for `+anthropic-auth-source-host'."
  (if-let* ((auth (car (auth-source-search :host +anthropic-auth-source-host
                                           :require '(:secret))))
            (secret (plist-get auth :secret)))
      (if (functionp secret)
          (funcall secret)
        secret)
    (error "No API key found in auth-source for host: %s"
           +anthropic-auth-source-host)))


;;; Request Construction

;;;###autoload
(cl-defun +anthropic-build-request-body (messages &key
                                                  (model +anthropic-default-model)
                                                  (max-tokens +anthropic-default-max-tokens)
                                                  system)
  "Build the JSON request body for the Messages API.
MESSAGES is a list of message plists with :role and :content keys.
MODEL specifies which Claude model to use.
MAX-TOKENS limits the response length.
SYSTEM is an optional system prompt string."
  (let ((body `((model . ,model)
                (max_tokens . ,max-tokens)
                (messages . ,(vconcat
                              (mapcar (lambda (msg)
                                        `((role . ,(plist-get msg :role))
                                          (content . ,(plist-get msg :content))))
                                      messages))))))
    (when system
      (push `(system . ,system) body))
    body))

;;;###autoload
(defun +anthropic-build-request-headers (&optional api-key)
  "Build HTTP headers for the Anthropic API request.
API-KEY overrides the key from auth-source."
  (let ((key (or api-key (+anthropic-get-api-key))))
    `(("Content-Type" . "application/json")
      ("Accept" . "application/json")
      ("anthropic-version" . ,+anthropic-api-version)
      ("x-api-key" . ,key))))


;;; Response Parsing

;;;###autoload
(defun +anthropic-parse-response (response-string)
  "Parse RESPONSE-STRING from the API and return the assistant's message.
Returns the text content from the first content block, or signals an error
if the response indicates a failure."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (response (json-read-from-string response-string)))
    (if-let* ((error-info (alist-get 'error response)))
        (error "Anthropic API error: %s" (alist-get 'message error-info))
      (let ((content (alist-get 'content response)))
        (when content
          (let ((first-block (car content)))
            (alist-get 'text first-block)))))))

(defun +anthropic--extract-response-body ()
  "Extract the response body from the current url buffer.
Assumes point is at the beginning of the buffer with HTTP headers."
  (goto-char (point-min))
  (re-search-forward "^\r?\n" nil t)
  (buffer-substring-no-properties (point) (point-max)))


;;; Synchronous Execution

;;;###autoload
(cl-defun +anthropic-request (messages &key
                                       (model +anthropic-default-model)
                                       (max-tokens +anthropic-default-max-tokens)
                                       system
                                       api-key)
  "Send MESSAGES to the Anthropic API synchronously.
Returns the response text.

MESSAGES is a list of message plists with :role and :content keys.
MODEL specifies which Claude model to use.
MAX-TOKENS limits the response length.
SYSTEM is an optional system prompt string.
API-KEY overrides the key from auth-source."
  (let* ((url-request-method "POST")
         (url-request-extra-headers (+anthropic-build-request-headers api-key))
         (url-request-data (json-encode
                            (+anthropic-build-request-body messages
                                                           :model model
                                                           :max-tokens max-tokens
                                                           :system system)))
         (buffer (url-retrieve-synchronously +anthropic-api-url t)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((response-body (+anthropic--extract-response-body)))
            (+anthropic-parse-response response-body)))
      (kill-buffer buffer))))

;;;###autoload
(cl-defun +anthropic-message (text &key
                                   (model +anthropic-default-model)
                                   (max-tokens +anthropic-default-max-tokens)
                                   system)
  "Send a single user message TEXT synchronously.
Returns the assistant's response.

MODEL specifies which Claude model to use.
MAX-TOKENS limits the response length.
SYSTEM is an optional system prompt string."
  (+anthropic-request (list (list :role "user" :content text))
                      :model model
                      :max-tokens max-tokens
                      :system system))


;;; Asynchronous Execution

;;;###autoload
(cl-defun +anthropic-request-async (messages callback &key
                                             (model +anthropic-default-model)
                                             (max-tokens +anthropic-default-max-tokens)
                                             system
                                             api-key
                                             error-callback)
  "Send MESSAGES to the Anthropic API asynchronously.
CALLBACK is called with the response text on success.
ERROR-CALLBACK is called with the error message on failure.

MESSAGES is a list of message plists with :role and :content keys.
MODEL specifies which Claude model to use.
MAX-TOKENS limits the response length.
SYSTEM is an optional system prompt string.
API-KEY overrides the key from auth-source."
  (let* ((url-request-method "POST")
         (url-request-extra-headers (+anthropic-build-request-headers api-key))
         (url-request-data (json-encode
                            (+anthropic-build-request-body messages
                                                           :model model
                                                           :max-tokens max-tokens
                                                           :system system))))
    (url-retrieve
     +anthropic-api-url
     (lambda (status)
       (let ((error-callback (or error-callback #'error)))
         (if-let* ((err (plist-get status :error)))
             (funcall error-callback (format "HTTP error: %S" err))
           (condition-case err
               (let ((response-body (+anthropic--extract-response-body)))
                 (funcall callback (+anthropic-parse-response response-body)))
             (error (funcall error-callback (error-message-string err)))))
         (kill-buffer (current-buffer))))
     nil t)))

;;;###autoload
(cl-defun +anthropic-message-async (text callback &key
                                         (model +anthropic-default-model)
                                         (max-tokens +anthropic-default-max-tokens)
                                         system
                                         error-callback)
  "Send a single user message TEXT asynchronously.
CALLBACK is called with the assistant's response on success.
ERROR-CALLBACK is called with the error message on failure.

MODEL specifies which Claude model to use.
MAX-TOKENS limits the response length.
SYSTEM is an optional system prompt string."
  (+anthropic-request-async (list (list :role "user" :content text))
                            callback
                            :model model
                            :max-tokens max-tokens
                            :system system
                            :error-callback error-callback))

(provide 'anthropic-lib)

;;; lib.el ends here
