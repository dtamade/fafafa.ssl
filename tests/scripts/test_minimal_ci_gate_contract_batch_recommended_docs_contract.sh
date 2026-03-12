#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RECOMMENDED_CMD="bash scripts/run_minimal_ci_gate.sh --fast-local --skip-warning-noise-governance-batch --with-minimal-gate-contract-batch"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate recommended docs contract"

for doc in "README.md" "docs/AGENTS.md"; do
  TARGET="$ROOT_DIR/$doc"
  if [[ ! -f "$TARGET" ]]; then
    fail "missing doc file: $doc"
  fi

  if ! grep -Fq "$RECOMMENDED_CMD" "$TARGET"; then
    fail "doc missing recommended minimal gate command: $doc"
  fi
done

echo "[PASS] minimal ci gate recommended docs contract passed"
