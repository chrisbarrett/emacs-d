# Integration Testing Specification

Verify the Emacs configuration works correctly as a complete system.

## Problem Statement

Individual modules are tested in isolation, but integration issues arise:
- Bootstrap files may reference moved/renamed files
- Module load order may cause missing dependencies
- Evil mode may not activate in initial buffers
- Theme loading may fail before frame exists

## Testing Approach

Use `emacs -nw` with isolated server for reproducible integration tests.

### Server Isolation

Each test session uses a unique server socket to avoid interference:

```bash
EMACS_SOCKET_NAME="${TMPDIR:-/tmp}/emacs-test-$$"
emacs -nw --daemon="$EMACS_SOCKET_NAME"
```

### Inspection via emacsclient

Query running state without disrupting the session:

```bash
emacsclient -s "$EMACS_SOCKET_NAME" --eval '(expression)'
```

### Cleanup

```bash
emacsclient -s "$EMACS_SOCKET_NAME" --eval '(kill-emacs)'
```

## Integration Checks

### IC-1: Startup Completes

| Given | When | Then |
|-------|------|------|
| Clean environment | `emacs --batch -l init.el` | Exit code 0 |

### IC-2: No Missing Files

| Given | When | Then |
|-------|------|------|
| Clean environment | Start Emacs | No "Cannot open load file" errors |

### IC-3: Evil Mode Active

| Given | When | Then |
|-------|------|------|
| Emacs started | Check `*scratch*` buffer | `evil-state` is `normal` |
| Emacs started | Check `*Messages*` buffer | `evil-state` is `normal` |

```elisp
(with-current-buffer "*scratch*"
  (eq evil-state 'normal))
```

### IC-4: Theme Loaded

| Given | When | Then |
|-------|------|------|
| After init | Query themes | `custom-enabled-themes` non-nil |

```elisp
(not (null custom-enabled-themes))
```

### IC-5: Leader Keys Bound

| Given | When | Then |
|-------|------|------|
| After init | Check SPC binding | Bound to leader keymap |

```elisp
(keymapp (key-binding (kbd "SPC") nil t))
```

### IC-6: Module Packages Installed

| Given | When | Then |
|-------|------|------|
| After init | Query elpaca | All module packages queued |

```elisp
(cl-every #'elpaca-get '(evil vertico consult))
```

### IC-7: Autoloads Registered

| Given | When | Then |
|-------|------|------|
| After init | Query fboundp | Module autoloads defined |

```elisp
(and (fboundp '+theme-update)
     (fboundp '+compile-project))
```

## Bootstrap Load-Path Setup

Module directories must be on load-path before requiring module files.
Early-init.el adds validated module directories to load-path:

```elisp
(defun +module-directory-p (dir)
  "Return non-nil if DIR contains module system files."
  (and (file-directory-p dir)
       (cl-some (lambda (file)
                  (file-exists-p (file-name-concat dir file)))
                '("init.el" "lib.el" "packages.eld"))))

(dolist (dir (directory-files +modules-directory t "\\`[^.]"))
  (when (+module-directory-p dir)
    (add-to-list 'load-path dir)))
```

This validates directories contain actual module files before adding,
preventing work-in-progress or non-module directories from polluting load-path.

| File | Required By | Available Via |
|------|-------------|---------------|
| `+corelib.el` | early-init.el | `lisp/` |
| `+load-incrementally.el` | early-init.el | `lisp/` |
| `theme-lib.el` | early-init.el | `modules/theme/` (on load-path) |

## Makefile Target

```makefile
test-integration:
	@./scripts/integration-test.sh
```

## scripts/integration-test.sh

```bash
#!/usr/bin/env bash
set -euo pipefail

SOCKET="${TMPDIR:-/tmp}/emacs-integration-test-$$"
cleanup() { emacsclient -s "$SOCKET" --eval '(kill-emacs)' 2>/dev/null || true; }
trap cleanup EXIT

echo "Starting Emacs daemon..."
emacs -nw --daemon="$SOCKET" 2>&1 | head -50

echo "Running integration checks..."

check() {
  local name="$1" expr="$2"
  if emacsclient -s "$SOCKET" --eval "$expr" | grep -q '^t$'; then
    echo "✓ $name"
  else
    echo "✗ $name"
    exit 1
  fi
}

check "Evil mode in scratch" \
  '(with-current-buffer "*scratch*" (eq evil-state (quote normal)))'

check "Theme loaded" \
  '(not (null custom-enabled-themes))'

check "Leader keys bound" \
  '(keymapp (key-binding (kbd "SPC") nil t))'

check "Core autoloads" \
  '(and (fboundp (quote +theme-update)) (fboundp (quote +compile-project)))'

echo "All integration checks passed."
```

## Acceptance Criteria

- [ ] `make test-integration` passes
- [ ] No "Cannot open load file" errors during startup
- [ ] Evil mode active in all initial buffers
- [ ] Theme applied after init
- [ ] Leader keymap functional
