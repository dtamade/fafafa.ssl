#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

BIN="tmp/test_benchmark_crypto_contract"
LOG="/tmp/test_benchmark_crypto_contract.log"

if ! fpc -Fu./src tests/performance/benchmark_crypto.pas -o"$BIN" >/tmp/test_benchmark_crypto_contract.compile.log 2>&1; then
  echo '[INFO] compile output:'
  sed -n '1,260p' /tmp/test_benchmark_crypto_contract.compile.log || true
  fail 'benchmark_crypto should compile'
fi

if ! "./$BIN" >"$LOG" 2>&1; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'benchmark_crypto should run successfully'
fi

if ! rg -F --quiet -- '[PASS] benchmark crypto program completed' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'benchmark_crypto should print completion marker'
fi

echo '[PASS] benchmark_crypto stays green at runtime'
