#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] focused ocsp regression drift summary contract"

OUT="$(cd "$ROOT_DIR" && bash "$SCRIPT" 2>&1)"

if [[ "$OUT" != *"[INFO] ocsp summary:"* ]]; then
  echo "$OUT"
  fail "ocsp focused contract should emit drift summary snapshot"
fi

if [[ "$OUT" != *"passed="* ]] || [[ "$OUT" != *"failed="* ]]; then
  echo "$OUT"
  fail "drift summary should include passed/failed counters"
fi

echo "[PASS] focused ocsp regression drift summary contract passed"
