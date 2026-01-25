# Feature: Test Runner

Batch-mode test runner with Elpaca load-path setup.

## Usage

```bash
emacs -Q --batch -l tests.el                     # run all tests
emacs -Q --batch -l tests.el lisp/+compile       # run tests for file
emacs -Q --batch -l tests.el "prefix-*"          # run tests matching pattern
```

## Behavior

### Load Path Setup

**Given** tests.el is loaded
**When** load paths are configured
**Then** `lisp/` is added to load-path
**And** all `elpaca/builds/*/` directories are added to load-path

### Test Discovery

| Argument         | Interpretation                          |
| ---------------- | --------------------------------------- |
| (none)           | Run all `*-tests.el` files              |
| `path/to/file`   | Run `path/to/file-tests.el`             |
| `"pattern"`      | Pass pattern to `ert-run-tests-batch`   |

**Given** no arguments
**When** tests run
**Then** all files matching `**/*-tests.el` are loaded and executed

**Given** a file path argument (without `-tests.el` suffix)
**When** tests run
**Then** corresponding `-tests.el` file is loaded and executed

**Given** a quoted pattern argument
**When** tests run
**Then** pattern is passed to ERT selector

### Exit Behavior

**Given** tests complete
**When** all tests pass
**Then** exit code is 0

**Given** tests complete
**When** any test fails
**Then** exit code is non-zero
**And** failure summary is printed

## Implementation Notes

- Use `command-line-args-left` for argument parsing
- Use `directory-files-recursively` for test discovery
- Clear `command-line-args-left` after parsing to prevent Emacs processing

## Provided API

| Symbol                  | Type     | Description                    |
| ----------------------- | -------- | ------------------------------ |
| `+test-runner-run`      | function | Main entry point               |
| `+test-runner-load-paths` | function | Set up Elpaca load paths     |

## Tasks

- [ ] Implement load-path setup
- [ ] Implement argument parsing
- [ ] Implement test file discovery
- [ ] Integrate with ERT batch runner
- [ ] Update AGENTS.md with new command
