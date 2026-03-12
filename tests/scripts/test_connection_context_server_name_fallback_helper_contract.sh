#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILES=(
  "src/fafafa.ssl.openssl.connection.pas"
  "src/fafafa.ssl.freepascal.connection.pas"
  "src/fafafa.ssl.winssl.connection.pas"
  "src/fafafa.ssl.mbedtls.connection.pas"
)

HELPER='GetLegacyContextDefaultServerName'

for f in "${FILES[@]}"; do
  rg -F --quiet -- "$HELPER" "$f" || {
    echo "[FAIL] missing shared fallback helper usage in $f"
    exit 1
  }

done

if rg -n 'AContext\.GetServerName' "${FILES[@]}"; then
  echo '[FAIL] connection constructors should not duplicate direct deprecated AContext.GetServerName fallback logic'
  exit 1
fi

rg -F --quiet -- "function $HELPER: string;" src/fafafa.ssl.connection.base.pas || {
  echo "[FAIL] missing shared fallback helper declaration in src/fafafa.ssl.connection.base.pas"
  exit 1
}

rg -F --quiet -- "function TBaseSSLConnection.$HELPER: string;" src/fafafa.ssl.connection.base.pas || {
  echo "[FAIL] missing shared fallback helper implementation in src/fafafa.ssl.connection.base.pas"
  exit 1
}

echo '[PASS] connection fallback logic is centralized in shared helper'
