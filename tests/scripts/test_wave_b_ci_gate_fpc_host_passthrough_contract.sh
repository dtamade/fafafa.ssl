#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_ci_gate.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave_b ci gate fpc host passthrough contract"

OUT="$(
  cd /tmp
  FAFAFA_FPC_EXE="contract-fpc" \
  bash "$SCRIPT" --dry-run --skip-examples --modules PKCS7 2>&1
)"

if [[ "$OUT" != *"python3 scripts/compile_all_modules.py --unit-output-dir"*"--fpc-exe 'contract-fpc'"* ]]; then
  echo "$OUT"
  fail "compile step should passthrough --fpc-exe override"
fi

if [[ "$OUT" != *"FAFAFA_FPC_EXE='contract-fpc'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_FPC_EXE"
fi

echo "[PASS] wave_b ci gate fpc host passthrough contract passed"
