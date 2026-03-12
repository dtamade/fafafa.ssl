#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate help preset precedence note contract"

OUT="$(bash "$SCRIPT" --help 2>&1)"

if [[ "$OUT" != *"后出现的 preset 覆盖前者"* ]]; then
  echo "$OUT"
  fail "help should explain preset precedence (last-flag-wins)"
fi

echo "[PASS] minimal ci gate help preset precedence note contract passed"
