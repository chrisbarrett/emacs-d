;;; tests.el --- Tests for anthropic module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests based on spec 033-anthropic.md properties:
;; P1: +anthropic-get-api-key returns string when credentials exist
;; P2: +anthropic-get-api-key signals error when no credentials
;; P3: +anthropic-build-request-body includes model and max_tokens
;; P4: +anthropic-build-request-body includes system when provided
;; P5: +anthropic-parse-response extracts text from valid response
;; P6: +anthropic-parse-response signals error on API error
;; P7: +anthropic-build-request-headers includes x-api-key and anthropic-version

;;; Code:

(require 'ert)

;; Load the module lib.el
(let ((lib-file (expand-file-name "modules/anthropic/lib.el" user-emacs-directory)))
  (condition-case nil
      (load lib-file nil t)
    (error nil)))

;;; P1: API key retrieval with credentials

(ert-deftest anthropic-test-p1-get-api-key-returns-string ()
  "P1: +anthropic-get-api-key returns string when credentials exist."
  (skip-unless (fboundp '+anthropic-get-api-key))
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _)
               (list (list :host "api.anthropic.com"
                           :secret "test-secret-key")))))
    (should (stringp (+anthropic-get-api-key)))
    (should (equal "test-secret-key" (+anthropic-get-api-key)))))

(ert-deftest anthropic-test-p1-get-api-key-handles-function-secret ()
  "P1: Handles auth-source returning a function for :secret."
  (skip-unless (fboundp '+anthropic-get-api-key))
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _)
               (list (list :host "api.anthropic.com"
                           :secret (lambda () "lazy-secret-key"))))))
    (should (equal "lazy-secret-key" (+anthropic-get-api-key)))))

;;; P2: API key retrieval error

(ert-deftest anthropic-test-p2-get-api-key-errors-when-not-found ()
  "P2: +anthropic-get-api-key signals error when no credentials."
  (skip-unless (fboundp '+anthropic-get-api-key))
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) nil)))
    (should-error (+anthropic-get-api-key) :type 'error)))

;;; P3: Request body includes model and max_tokens

(ert-deftest anthropic-test-p3-build-request-body-includes-model ()
  "P3: +anthropic-build-request-body includes model."
  (skip-unless (fboundp '+anthropic-build-request-body))
  (let* ((+anthropic-default-model "claude-sonnet-4-5-20250929")
         (+anthropic-default-max-tokens 1024)
         (body (+anthropic-build-request-body
                (list (list :role "user" :content "Hello")))))
    (should (alist-get 'model body))
    (should (equal "claude-sonnet-4-5-20250929" (alist-get 'model body)))))

(ert-deftest anthropic-test-p3-build-request-body-includes-max-tokens ()
  "P3: +anthropic-build-request-body includes max_tokens."
  (skip-unless (fboundp '+anthropic-build-request-body))
  (let* ((+anthropic-default-model "claude-sonnet-4-5-20250929")
         (+anthropic-default-max-tokens 1024)
         (body (+anthropic-build-request-body
                (list (list :role "user" :content "Hello")))))
    (should (alist-get 'max_tokens body))
    (should (equal 1024 (alist-get 'max_tokens body)))))

(ert-deftest anthropic-test-p3-build-request-body-includes-messages ()
  "P3: +anthropic-build-request-body includes messages array."
  (skip-unless (fboundp '+anthropic-build-request-body))
  (let* ((+anthropic-default-model "claude-sonnet-4-5-20250929")
         (+anthropic-default-max-tokens 1024)
         (body (+anthropic-build-request-body
                (list (list :role "user" :content "Test"))))
         (messages (append (alist-get 'messages body) nil)))
    (should (= 1 (length messages)))
    (should (equal "user" (alist-get 'role (car messages))))
    (should (equal "Test" (alist-get 'content (car messages))))))

(ert-deftest anthropic-test-p3-build-request-body-custom-model ()
  "P3: +anthropic-build-request-body accepts custom model."
  (skip-unless (fboundp '+anthropic-build-request-body))
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi"))
               :model "claude-3-haiku-20240307")))
    (should (equal "claude-3-haiku-20240307" (alist-get 'model body)))))

(ert-deftest anthropic-test-p3-build-request-body-custom-max-tokens ()
  "P3: +anthropic-build-request-body accepts custom max-tokens."
  (skip-unless (fboundp '+anthropic-build-request-body))
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi"))
               :max-tokens 4096)))
    (should (equal 4096 (alist-get 'max_tokens body)))))

;;; P4: Request body includes system when provided

(ert-deftest anthropic-test-p4-build-request-body-includes-system ()
  "P4: +anthropic-build-request-body includes system when provided."
  (skip-unless (fboundp '+anthropic-build-request-body))
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi"))
               :system "You are a helpful assistant.")))
    (should (alist-get 'system body))
    (should (equal "You are a helpful assistant." (alist-get 'system body)))))

(ert-deftest anthropic-test-p4-build-request-body-omits-system-when-nil ()
  "P4: +anthropic-build-request-body omits system when not provided."
  (skip-unless (fboundp '+anthropic-build-request-body))
  (let ((body (+anthropic-build-request-body
               (list (list :role "user" :content "Hi")))))
    (should-not (assq 'system body))))

;;; P5: Response parsing extracts text

(ert-deftest anthropic-test-p5-parse-response-extracts-text ()
  "P5: +anthropic-parse-response extracts text from valid response."
  (skip-unless (fboundp '+anthropic-parse-response))
  (let ((response "{\"content\":[{\"type\":\"text\",\"text\":\"Hello!\"}],\"model\":\"claude-sonnet-4-5-20250929\",\"stop_reason\":\"end_turn\"}"))
    (should (equal "Hello!" (+anthropic-parse-response response)))))

(ert-deftest anthropic-test-p5-parse-response-handles-multiline ()
  "P5: +anthropic-parse-response handles multiline text."
  (skip-unless (fboundp '+anthropic-parse-response))
  (let ((response "{\"content\":[{\"type\":\"text\",\"text\":\"Line 1\\nLine 2\\nLine 3\"}]}"))
    (should (equal "Line 1\nLine 2\nLine 3" (+anthropic-parse-response response)))))

(ert-deftest anthropic-test-p5-parse-response-first-content-block ()
  "P5: +anthropic-parse-response returns first content block."
  (skip-unless (fboundp '+anthropic-parse-response))
  (let ((response "{\"content\":[{\"type\":\"text\",\"text\":\"First\"},{\"type\":\"text\",\"text\":\"Second\"}]}"))
    (should (equal "First" (+anthropic-parse-response response)))))

;;; P6: Response parsing signals error on API error

(ert-deftest anthropic-test-p6-parse-response-signals-api-error ()
  "P6: +anthropic-parse-response signals error on API error."
  (skip-unless (fboundp '+anthropic-parse-response))
  (let ((response "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"Invalid API key\"}}"))
    (should-error (+anthropic-parse-response response) :type 'error)))

(ert-deftest anthropic-test-p5-parse-response-handles-empty-content ()
  "P5: +anthropic-parse-response handles empty content array."
  (skip-unless (fboundp '+anthropic-parse-response))
  (let ((response "{\"content\":[]}"))
    (should-not (+anthropic-parse-response response))))

;;; P7: Request headers include x-api-key and anthropic-version

(ert-deftest anthropic-test-p7-build-request-headers-includes-api-key ()
  "P7: +anthropic-build-request-headers includes x-api-key."
  (skip-unless (fboundp '+anthropic-build-request-headers))
  (let ((headers (+anthropic-build-request-headers "test-api-key")))
    (should (alist-get "x-api-key" headers nil nil #'equal))
    (should (equal "test-api-key" (alist-get "x-api-key" headers nil nil #'equal)))))

(ert-deftest anthropic-test-p7-build-request-headers-includes-version ()
  "P7: +anthropic-build-request-headers includes anthropic-version."
  (skip-unless (fboundp '+anthropic-build-request-headers))
  (let* ((+anthropic-api-version "2023-06-01")
         (headers (+anthropic-build-request-headers "test-api-key")))
    (should (alist-get "anthropic-version" headers nil nil #'equal))
    (should (equal "2023-06-01" (alist-get "anthropic-version" headers nil nil #'equal)))))

(ert-deftest anthropic-test-p7-build-request-headers-includes-content-type ()
  "P7: +anthropic-build-request-headers includes Content-Type."
  (skip-unless (fboundp '+anthropic-build-request-headers))
  (let ((headers (+anthropic-build-request-headers "test-api-key")))
    (should (equal "application/json" (alist-get "Content-Type" headers nil nil #'equal)))))

(ert-deftest anthropic-test-p7-build-request-headers-uses-auth-source ()
  "P7: +anthropic-build-request-headers uses auth-source when key not provided."
  (skip-unless (fboundp '+anthropic-build-request-headers))
  (skip-unless (fboundp '+anthropic-get-api-key))
  (cl-letf (((symbol-function '+anthropic-get-api-key)
             (lambda () "auth-source-key")))
    (let ((headers (+anthropic-build-request-headers)))
      (should (equal "auth-source-key" (alist-get "x-api-key" headers nil nil #'equal))))))

;;; Module structure tests

(ert-deftest anthropic-test-module-has-lib ()
  "Module has lib.el file."
  (let ((lib-file (expand-file-name "modules/anthropic/lib.el" user-emacs-directory)))
    (should (file-exists-p lib-file))))

(ert-deftest anthropic-test-module-has-spec ()
  "Module has spec.md symlink."
  (let ((spec-file (expand-file-name "modules/anthropic/spec.md" user-emacs-directory)))
    (should (file-exists-p spec-file))
    (should (file-symlink-p spec-file))))

(ert-deftest anthropic-test-module-has-packages ()
  "Module has packages.eld file."
  (let ((packages-file (expand-file-name "modules/anthropic/packages.eld" user-emacs-directory)))
    (should (file-exists-p packages-file))))

;;; tests.el ends here
