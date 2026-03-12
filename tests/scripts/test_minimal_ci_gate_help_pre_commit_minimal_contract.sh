#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate help pre-commit minimal contract"

OUT="$(bash "$SCRIPT" --help 2>&1)"

if [[ "$OUT" != *"--pre-commit-minimal"* ]]; then
  echo "$OUT"
  fail "help should include --pre-commit-minimal option"
fi

if [[ "$OUT" != *"fast-local + skip-warning + contract-batch"* ]]; then
  echo "$OUT"
  fail "help should describe pre-commit-minimal equivalence"
fi

echo "[PASS] minimal ci gate help pre-commit minimal contract passed"
