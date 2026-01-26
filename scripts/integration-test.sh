#!/usr/bin/env bash
# Run integration tests against a live Emacs daemon.
# Usage: ./scripts/integration-test.sh
set -euo pipefail

cd "$(dirname "$0")/.."

SOCKET="${TMPDIR}em-test-$$"

cleanup() {
  emacsclient -s "$SOCKET" --eval '(kill-emacs)' 2>/dev/null || true
}
trap cleanup EXIT

echo "Starting Emacs daemon..."
if ! emacs -nw --daemon="$SOCKET" 2>&1 | head -50; then
  echo "Failed to start Emacs daemon"
  exit 1
fi

# Wait for daemon to be ready
for i in {1..10}; do
  if emacsclient -s "$SOCKET" --eval 't' >/dev/null 2>&1; then
    break
  fi
  sleep 0.5
done

echo "Running integration checks..."
failed=0

check() {
  local name="$1" expr="$2"
  local result
  result=$(emacsclient -s "$SOCKET" --eval "$expr" 2>&1)
  if echo "$result" | grep -q '^t$'; then
    echo "✓ $name"
  else
    echo "✗ $name"
    echo "  Expected: t"
    echo "  Got: $result"
    failed=1
  fi
}

# IC-3: Evil mode active in scratch buffer
check "Evil mode in scratch" \
  '(with-current-buffer "*scratch*" (eq evil-state (quote normal)))'

# IC-4: Theme loaded
check "Theme loaded" \
  '(not (null custom-enabled-themes))'

# IC-5: Leader keys bound
check "Leader keys bound" \
  '(keymapp (key-binding (kbd "SPC") nil t))'

# IC-6: Core packages installed
check "Core packages installed" \
  '(cl-every (function elpaca-get) (quote (evil vertico consult)))'

# IC-7: Core autoloads registered
check "Core autoloads" \
  '(and (fboundp (quote +theme-update)) (fboundp (quote +compile-pp-parser)))'

if [[ $failed -eq 0 ]]; then
  echo "All integration checks passed."
else
  echo "Some integration checks failed."
  exit 1
fi
