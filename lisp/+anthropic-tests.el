;;; +anthropic-tests.el --- Tests for +anthropic.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require '+anthropic)

;;; Request Body Construction

(ert-deftest +anthropic-build-request-body--single-message ()
  "Test building request body with a single user message."
  (let* ((+anthropic-default-model "claude-sonnet-4-5-20250929")
         (+anthropic-default-max-tokens 1024)
         (body (+anthropic-build-request-body
                (list (list :role "user" :content "Hello, Claude!")))))
    (should (equal "claude-sonnet-4-5-20250929" (alist-get 'model body)))
    (should (equal 1024 (alist-get 'max_tokens body)))
    (should-not (alist-get 'system body))
    (let ((messages (append (alist-get 'messages body) nil)))
      (should (= 1 (length messages)))
      (should (equal "user" (alist-get 'role (car messages))))
      (should (equal "Hello, Claude!" (alist-get 'content (car messages)))))))

(ert-deftest +anthropic-build-request-body--multiple-messages ()
  "Test building request body with a conversation."
  (let* ((+anthropic-default-model "claude-sonnet-4-5-20250929")
         (+anthropic-default-max-tokens 1024)
         (body (+anthropic-build-request-body
                (list (list :role "user" :content "What is 2+2?")
                      (list :role "assistant" :content "4")
                      (list :role "user" :content "And 3+3?"))))
         (messages (append (alist-get 'messages body) nil)))
    (should (= 3 (length messages)))
    (should (equal "user" (alist-get 'role (nth 0 messages))))
    (should (equal "What is 2+2?" (alist-get 'content (nth 0 messages))))
    (should (equal "assistant" (alist-get 'role (nth 1 messages))))
    (should (equal "4" (alist-get 'content (nth 1 messages))))
    (should (equal "user" (alist-get 'role (nth 2 messages))))
    (should (equal "And 3+3?" (alist-get 'content (nth 2 messages))))))

(ert-deftest +anthropic-build-request-body--custom-model ()
  "Test building request body with a custom model."
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi"))
               :model "claude-3-haiku-20240307")))
    (should (equal "claude-3-haiku-20240307" (alist-get 'model body)))))

(ert-deftest +anthropic-build-request-body--custom-max-tokens ()
  "Test building request body with custom max tokens."
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi"))
               :max-tokens 4096)))
    (should (equal 4096 (alist-get 'max_tokens body)))))

(ert-deftest +anthropic-build-request-body--with-system-prompt ()
  "Test building request body with a system prompt."
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi"))
               :system "You are a helpful assistant.")))
    (should (equal "You are a helpful assistant." (alist-get 'system body)))))

(ert-deftest +anthropic-build-request-body--without-system-prompt ()
  "Test that system key is absent when no system prompt given."
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi")))))
    (should-not (assq 'system body))))

(ert-deftest +anthropic-build-request-body--json-encodes-correctly ()
  "Test that the request body JSON-encodes to valid structure."
  (let* ((+anthropic-default-model "claude-sonnet-4-5-20250929")
         (+anthropic-default-max-tokens 100)
         (body (+anthropic-build-request-body
                (list (list :role "user" :content "Test"))
                :system "Be concise"))
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (encoded (json-encode body))
         (decoded (json-read-from-string encoded)))
    (should (equal "claude-sonnet-4-5-20250929" (alist-get 'model decoded)))
    (should (equal 100 (alist-get 'max_tokens decoded)))
    (should (equal "Be concise" (alist-get 'system decoded)))
    (should (= 1 (length (alist-get 'messages decoded))))))


;;; API Key Retrieval

(ert-deftest +anthropic-get-api-key--retrieves-from-auth-source ()
  "Test that API key is retrieved from auth-source."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _)
               (list (list :host "api.anthropic.com"
                           :secret "test-secret-key")))))
    (should (equal "test-secret-key" (+anthropic-get-api-key)))))

(ert-deftest +anthropic-get-api-key--handles-function-secret ()
  "Test that API key handles auth-source returning a function for :secret."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _)
               (list (list :host "api.anthropic.com"
                           :secret (lambda () "lazy-secret-key"))))))
    (should (equal "lazy-secret-key" (+anthropic-get-api-key)))))

(ert-deftest +anthropic-get-api-key--errors-when-not-found ()
  "Test that error is signaled when no auth-source entry found."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) nil)))
    (should-error (+anthropic-get-api-key) :type 'error)))


;;; Request Headers Construction

(ert-deftest +anthropic-build-request-headers--has-required-headers ()
  "Test that headers include all required fields."
  (let* ((+anthropic-api-version "2023-06-01")
         (headers (+anthropic-build-request-headers "test-api-key")))
    (should (equal "application/json" (alist-get "Content-Type" headers nil nil #'equal)))
    (should (equal "application/json" (alist-get "Accept" headers nil nil #'equal)))
    (should (equal "2023-06-01" (alist-get "anthropic-version" headers nil nil #'equal)))
    (should (equal "test-api-key" (alist-get "x-api-key" headers nil nil #'equal)))))

(ert-deftest +anthropic-build-request-headers--uses-auth-source ()
  "Test that headers use API key from auth-source when not provided."
  (cl-letf (((symbol-function '+anthropic-get-api-key)
             (lambda () "auth-source-key")))
    (let* ((+anthropic-api-version "2023-06-01")
           (headers (+anthropic-build-request-headers)))
      (should (equal "auth-source-key" (alist-get "x-api-key" headers nil nil #'equal))))))

(ert-deftest +anthropic-build-request-headers--explicit-key-overrides-auth-source ()
  "Test that explicit API key overrides auth-source."
  (cl-letf (((symbol-function '+anthropic-get-api-key)
             (lambda () "auth-source-key")))
    (let* ((+anthropic-api-version "2023-06-01")
           (headers (+anthropic-build-request-headers "explicit-key")))
      (should (equal "explicit-key" (alist-get "x-api-key" headers nil nil #'equal))))))

(ert-deftest +anthropic-build-request-headers--uses-custom-api-version ()
  "Test that headers respect custom API version."
  (let* ((+anthropic-api-version "2024-01-01")
         (headers (+anthropic-build-request-headers "test-key")))
    (should (equal "2024-01-01" (alist-get "anthropic-version" headers nil nil #'equal)))))


;;; Response Parsing

(ert-deftest +anthropic-parse-response--extracts-text ()
  "Test parsing a successful API response."
  (let ((response "{\"content\":[{\"type\":\"text\",\"text\":\"Hello!\"}],\"model\":\"claude-sonnet-4-5-20250929\",\"stop_reason\":\"end_turn\"}"))
    (should (equal "Hello!" (+anthropic-parse-response response)))))

(ert-deftest +anthropic-parse-response--handles-multiline-text ()
  "Test parsing response with multiline text content."
  (let ((response "{\"content\":[{\"type\":\"text\",\"text\":\"Line 1\\nLine 2\\nLine 3\"}]}"))
    (should (equal "Line 1\nLine 2\nLine 3" (+anthropic-parse-response response)))))

(ert-deftest +anthropic-parse-response--returns-first-content-block ()
  "Test that only the first content block text is returned."
  (let ((response "{\"content\":[{\"type\":\"text\",\"text\":\"First\"},{\"type\":\"text\",\"text\":\"Second\"}]}"))
    (should (equal "First" (+anthropic-parse-response response)))))

(ert-deftest +anthropic-parse-response--signals-api-error ()
  "Test that API errors are signaled properly."
  (let ((response "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"Invalid API key\"}}"))
    (should-error (+anthropic-parse-response response)
                  :type 'error)))

(ert-deftest +anthropic-parse-response--handles-empty-content ()
  "Test parsing response with empty content array."
  (let ((response "{\"content\":[]}"))
    (should-not (+anthropic-parse-response response))))


;;; Integration (body + headers)

(ert-deftest +anthropic-integration--full-request-construction ()
  "Test full request construction pipeline."
  (let* ((+anthropic-default-model "claude-sonnet-4-5-20250929")
         (+anthropic-default-max-tokens 100)
         (+anthropic-api-version "2023-06-01")
         (body (+anthropic-build-request-body
                (list (list :role "user" :content "Test message"))
                :system "Be concise"))
         (headers (+anthropic-build-request-headers "test-key")))
    ;; Verify body structure
    (should (equal "claude-sonnet-4-5-20250929" (alist-get 'model body)))
    (should (equal 100 (alist-get 'max_tokens body)))
    (should (equal "Be concise" (alist-get 'system body)))
    ;; Verify headers
    (should (equal "test-key" (alist-get "x-api-key" headers nil nil #'equal)))
    (should (equal "2023-06-01" (alist-get "anthropic-version" headers nil nil #'equal)))))

;;; +anthropic-tests.el ends here
