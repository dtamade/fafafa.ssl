#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

BIN="tmp/test_algorithm_availability_contract"
LOG="/tmp/test_algorithm_availability_contract.log"

if ! fpc -Fu./src tests/test_algorithm_availability.pas -o"$BIN" >/tmp/test_algorithm_availability_contract.compile.log 2>&1; then
  echo '[INFO] compile output:'
  sed -n '1,260p' /tmp/test_algorithm_availability_contract.compile.log || true
  fail 'test_algorithm_availability should compile'
fi

if ! "./$BIN" >"$LOG" 2>&1; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_algorithm_availability should run successfully'
fi

if ! rg -F --quiet -- '[PASS] algorithm availability validation completed' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_algorithm_availability should print success marker'
fi

echo '[PASS] test_algorithm_availability stays green at runtime'
