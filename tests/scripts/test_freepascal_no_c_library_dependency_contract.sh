#!/usr/bin/env bash
# Phase 0 contract: FreePascal backend must not depend on any C library
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

echo "[TEST] FreePascal backend no C library dependency contract"

FP_FILES=(
  src/fafafa.ssl.freepascal.connection.pas
  src/fafafa.ssl.freepascal.context.pas
  src/fafafa.ssl.freepascal.context.material.pas
  src/fafafa.ssl.freepascal.lib.pas
  src/fafafa.ssl.freepascal.session.pas
  src/fafafa.ssl.freepascal.earlydatareplay.pas
  src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas
  src/fafafa.ssl.freepascal.earlydatareplay.dirstore.pas
  src/fafafa.ssl.tls13.aead.pas
  src/fafafa.ssl.tls13.appschedule.pas
  src/fafafa.ssl.tls13.bigint.pas
  src/fafafa.ssl.tls13.chacha20poly1305.pas
  src/fafafa.ssl.tls13.clienthello.pas
  src/fafafa.ssl.tls13.clienthello.parser.pas
  src/fafafa.ssl.tls13.ecdsa.pas
  src/fafafa.ssl.tls13.finished.pas
  src/fafafa.ssl.tls13.keyschedule.pas
  src/fafafa.ssl.tls13.parser.pas
  src/fafafa.ssl.tls13.posthandshake.pas
  src/fafafa.ssl.tls13.primitives.pas
  src/fafafa.ssl.tls13.recordcrypto.pas
  src/fafafa.ssl.tls13.servercertificate.pas
  src/fafafa.ssl.tls13.servercertverify.pas
  src/fafafa.ssl.tls13.serverhello.pas
  src/fafafa.ssl.tls13.wire.pas
  src/fafafa.ssl.tls13.x25519.pas
)

BANNED_PATTERNS=(
  "fafafa.ssl.openssl"
  "fafafa.ssl.mbedtls"
  "fafafa.ssl.wolfssl"
  "fafafa.ssl.winssl"
)

FAILED=0

for file in "${FP_FILES[@]}"; do
  if [[ ! -f "$file" ]]; then
    continue
  fi
  for pattern in "${BANNED_PATTERNS[@]}"; do
    if rg -F --quiet "$pattern" "$file" 2>/dev/null; then
      echo "[FAIL] $file depends on $pattern"
      rg -n -F "$pattern" "$file"
      FAILED=1
    fi
  done
done

if [[ $FAILED -eq 1 ]]; then
  echo ""
  echo "[FAIL] FreePascal/TLS1.3 code must not depend on any C-library backend"
  exit 1
fi

echo "[PASS] FreePascal backend has no C library dependencies"
