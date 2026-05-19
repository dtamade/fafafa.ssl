#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
MBEDTLS_LIB="$ROOT_DIR/src/fafafa.ssl.mbedtls.lib.pas"
MBEDTLS_CERT="$ROOT_DIR/src/fafafa.ssl.mbedtls.certificate.pas"
MBEDTLS_DOC="$ROOT_DIR/docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

require_fixed() {
  local file="$1"
  local needle="$2"
  local message="$3"
  if grep -Fq -- "$needle" "$file"; then
    echo "[PASS] $message"
  else
    fail "$message"
  fi
}

require_absent() {
  local file="$1"
  local needle="$2"
  local message="$3"
  if grep -Fq -- "$needle" "$file"; then
    fail "$message"
  else
    echo "[PASS] $message"
  fi
}

echo "[TEST] MbedTLS Ed25519 capability doc truth contract"

require_fixed "$MBEDTLS_LIB" \
  "sslKexRSA, sslKexDHE_RSA, sslKexECDHE_RSA, sslKexECDHE_ECDSA" \
  "MbedTLS capability record must keep the current shipped key-exchange families"
require_fixed "$MBEDTLS_CERT" \
  "Result := 'RSA';  // 默认" \
  "MbedTLS certificate public-key algorithm surface must still return the current RSA default"
require_fixed "$MBEDTLS_CERT" \
  "Result := 'SHA256withRSA';  // 默认" \
  "MbedTLS certificate signature-algorithm surface must still return the current RSA default"
require_fixed "$MBEDTLS_DOC" \
  '| Ed25519 | ❌ 当前 capability 不发布 | 当前 backend 没有 published Ed25519-specific capability / metadata surface；`GetPublicKeyAlgorithm` / `GetSignatureAlgorithm` 当前仍返回 RSA 默认值；不要把上游 MbedTLS 3.x 理论能力当成 fafafa.ssl 当前 backend truth |' \
  "MbedTLS dedicated matrix must stop projecting upstream Ed25519 theory as current published backend truth"
require_absent "$MBEDTLS_DOC" \
  "| Ed25519 | ⚠️ 部分 | MbedTLS 3.x |" \
  "MbedTLS dedicated matrix must stop describing Ed25519 as partial support"

echo "[PASS] MbedTLS Ed25519 capability doc truth contract passed"
