# Feature: Anthropic API Client

Synchronous and asynchronous Claude API client using auth-source credentials.

## Dependencies

### Built-in
- auth-source
- cl-lib
- json
- url
- url-http

### External
None (pure Emacs Lisp using built-in HTTP)

## Files

| File              | Purpose                                  |
| :---------------- | :--------------------------------------- |
| `lisp/+anthropic.el` | API client implementation            |

## Behavior

### B1: API Key Retrieval

**Given** auth-source configured with api.anthropic.com credentials
**When** `+anthropic-get-api-key` is called
**Then** returns the secret from auth-source
**And** signals error if no credentials found

Auth-source entry format:
```
machine api.anthropic.com login apikey password sk-ant-api...
```

### B2: Synchronous Request

**Given** valid API credentials
**When** `+anthropic-message` is called with text
**Then** blocks until response received
**And** returns assistant's text content

### B3: Asynchronous Request

**Given** valid API credentials
**When** `+anthropic-message-async` is called with text and callback
**Then** returns immediately
**And** callback receives response text on success
**And** error-callback receives error message on failure

### B4: Request Body Construction

**Given** a list of message plists
**When** `+anthropic-build-request-body` is called
**Then** returns alist with model, max_tokens, messages
**And** includes system prompt if provided

### B5: Response Parsing

**Given** API response JSON
**When** `+anthropic-parse-response` is called
**Then** extracts text from first content block
**And** signals error if API error response

## Provided API

### Functions

| Function                       | Description                          |
| :----------------------------- | :----------------------------------- |
| `+anthropic-message`           | Single sync message                  |
| `+anthropic-message-async`     | Single async message with callback   |
| `+anthropic-request`           | Multi-message sync request           |
| `+anthropic-request-async`     | Multi-message async request          |
| `+anthropic-get-api-key`       | Retrieve API key from auth-source    |
| `+anthropic-build-request-body`| Construct Messages API payload      |
| `+anthropic-build-request-headers` | Construct HTTP headers          |
| `+anthropic-parse-response`    | Parse API response JSON              |

### Variables

| Variable                         | Default                        | Description                    |
| :------------------------------- | :----------------------------- | :----------------------------- |
| `+anthropic-api-url`             | api.anthropic.com/v1/messages  | API endpoint                   |
| `+anthropic-api-version`         | 2023-06-01                     | API version header             |
| `+anthropic-default-model`       | claude-sonnet-4-5-20250929     | Default model                  |
| `+anthropic-default-max-tokens`  | 1024                           | Default max tokens             |
| `+anthropic-auth-source-host`    | api.anthropic.com              | Auth-source lookup host        |

## Properties to Verify

1. `+anthropic-get-api-key` returns string when credentials exist
2. `+anthropic-get-api-key` signals error when no credentials
3. `+anthropic-build-request-body` includes model and max_tokens
4. `+anthropic-build-request-body` includes system when provided
5. `+anthropic-parse-response` extracts text from valid response
6. `+anthropic-parse-response` signals error on API error
7. `+anthropic-build-request-headers` includes x-api-key and anthropic-version
