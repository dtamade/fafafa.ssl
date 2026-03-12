#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PRECOMMIT_CMD="bash scripts/run_minimal_ci_gate.sh --pre-commit-minimal"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate pre-commit docs contract"

for doc in "README.md" "docs/AGENTS.md"; do
  TARGET="$ROOT_DIR/$doc"
  if [[ ! -f "$TARGET" ]]; then
    fail "missing doc file: $doc"
  fi

  if ! grep -Fq "$PRECOMMIT_CMD" "$TARGET"; then
    fail "doc missing pre-commit minimal quick command: $doc"
  fi
done

echo "[PASS] minimal ci gate pre-commit docs contract passed"
