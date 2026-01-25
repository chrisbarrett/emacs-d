#!/usr/bin/env bash
set -euo pipefail

# Install git pre-commit hooks
# This script should be run once after cloning the repository

cd "$(dirname "$0")/.."

cat > .git/hooks/pre-commit << 'EOF'
#!/usr/bin/env bash
# Pre-commit hook that runs pre-commit via nix-shell
# This ensures pre-commit is available regardless of system installation

exec nix-shell -p pre-commit --run "pre-commit run --hook-stage pre-commit"
EOF

chmod +x .git/hooks/pre-commit

echo "Pre-commit hook installed"
