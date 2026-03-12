#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] openssl lib canonical imports contract"

mapfile -t HITS < <(
  cd "$ROOT_DIR"
  rg -n "fafafa\.ssl\.openssl\.backed" src tests examples \
    --glob '*.pas' \
    --glob '*.lpr' \
    --glob '!src/fafafa.ssl.openssl.backed.pas' \
    --glob '!src/fafafa.ssl.openssl.lib.pas' || true
)

if (( ${#HITS[@]} > 0 )); then
  printf '%s
' "${HITS[@]}" | sed -n '1,40p'
  fail "active Pascal source should use fafafa.ssl.openssl.lib instead of fafafa.ssl.openssl.backed"
fi

echo "[PASS] openssl lib canonical imports contract passed"
