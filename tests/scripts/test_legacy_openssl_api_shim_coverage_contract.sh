#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] legacy openssl api shim coverage contract"

legacy_units=(
  fafafa.ssl.openssl.aes
  fafafa.ssl.openssl.aria
  fafafa.ssl.openssl.bio
  fafafa.ssl.openssl.blake2
  fafafa.ssl.openssl.bn
  fafafa.ssl.openssl.buffer
  fafafa.ssl.openssl.chacha
  fafafa.ssl.openssl.cmac
  fafafa.ssl.openssl.cms
  fafafa.ssl.openssl.comp
  fafafa.ssl.openssl.conf
  fafafa.ssl.openssl.consts
  fafafa.ssl.openssl.core
  fafafa.ssl.openssl.ct
  fafafa.ssl.openssl.des
  fafafa.ssl.openssl.dso
  fafafa.ssl.openssl.ec
  fafafa.ssl.openssl.ecdh
  fafafa.ssl.openssl.ecdsa
  fafafa.ssl.openssl.engine
  fafafa.ssl.openssl.err
  fafafa.ssl.openssl.evp
  fafafa.ssl.openssl.kdf
  fafafa.ssl.openssl.legacy_ciphers
  fafafa.ssl.openssl.lhash
  fafafa.ssl.openssl.modes
  fafafa.ssl.openssl.obj
  fafafa.ssl.openssl.ocsp
  fafafa.ssl.openssl.param
  fafafa.ssl.openssl.pem
  fafafa.ssl.openssl.pkcs
  fafafa.ssl.openssl.pkcs12
  fafafa.ssl.openssl.pkcs7
  fafafa.ssl.openssl.provider
  fafafa.ssl.openssl.rand
  fafafa.ssl.openssl.rsa
  fafafa.ssl.openssl.seed
  fafafa.ssl.openssl.sha3
  fafafa.ssl.openssl.sm
  fafafa.ssl.openssl.srp
  fafafa.ssl.openssl.ssl
  fafafa.ssl.openssl.stack
  fafafa.ssl.openssl.store
  fafafa.ssl.openssl.thread
  fafafa.ssl.openssl.ts
  fafafa.ssl.openssl.txt_db
  fafafa.ssl.openssl.types
  fafafa.ssl.openssl.ui
  fafafa.ssl.openssl.x509
)

missing=0
for unit in "${legacy_units[@]}"; do
  if rg -F --quiet "$unit" "$ROOT_DIR/src" "$ROOT_DIR/tests" "$ROOT_DIR/examples" \
      --glob '*.pas' --glob '*.lpr'; then
    unit_file="$ROOT_DIR/src/${unit}.pas"
    if [[ ! -f "$unit_file" ]]; then
      echo "[INFO] missing shim for active legacy import: $unit"
      missing=1
    fi
  fi
done

if [[ "$missing" -ne 0 ]]; then
  fail "active legacy OpenSSL imports should have shim units under src/"
fi

echo "[PASS] legacy openssl api shim coverage contract passed"
